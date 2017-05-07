%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Utility functions to convert AST coming from parser to a set of nested
%%%   dictionaries.
%%%
%%%   This is the place where semantic-level parts of TOML specification are
%%%   implemented.
%%%
%%% @todo precise error reporting
%%% @todo eliminate descending over and over again to set consequent keys in
%%%   a section (i.e. descend once to open a section and again to close it on
%%%   new section/EOF; remember about subsections)
%%% @end
%%%---------------------------------------------------------------------------

-module(toml_dict).

-export([build_store/1]).
-export([fold/3]).
-export([format_error/1]).

-export_type([store/0, store_array/0]).
-export_type([jsx_object/0, jsx_list/0, jsx_value/0, scalar/0, datetime/0]).

%%%---------------------------------------------------------------------------
%%% data types

%%----------------------------------------------------------
%% common types {{{

-type line() :: pos_integer().

-type scalar() ::
    string()
  | integer()
  | float()
  | boolean()
  | datetime().

-type datetime() ::
    {datetime, calendar:datetime(), TZ :: string()}
  | {datetime, calendar:datetime()}
  | {date, calendar:date()}
  | {time, calendar:time()}.
%% `TZ' is `"Z"' or has format `"[+-]HH:MM"'

%% }}}
%%----------------------------------------------------------
%% types coming from AST {{{

-type ast_section_header() :: {table, line(), ast_section_name()}
                            | {array_table, line(), ast_section_name()}.

-type ast_section_name() :: [string(), ...].

-type ast_key_value() :: {key, line(), ast_key(), ast_value()}.

-type ast_key() :: string().

-type ast_value() :: scalar() | ast_array() | ast_inline_table().

-type ast_array() :: {array, [ast_value()]}.

-type ast_inline_table() :: {inline_table, [ast_key_value()]}.

%% }}}
%%----------------------------------------------------------
%% value store {{{

-opaque store() :: term().
%% Mapping from {@type store_key()} to {@type store_value()}.

-type store_key() :: string().
%% Compatible with {@type ast_key()}.

-type store_value() ::
    {line(), key, scalar()}
  | {line(), key, {array, store_array()}}
  | {line(), object, store()}
  | {line(), section, store()}
  | {line(), auto_section, store()}
  | {line(), array_section, [store()]}.

-type store_array() ::
    {empty, []}
  | {string, [string(), ...]}
  | {integer, [integer(), ...]}
  | {float, [float(), ...]}
  | {boolean, [boolean(), ...]}
  | {datetime, [datetime(), ...]}
  | {array, [store_array(), ...]}
  | {object, [jsx_object(), ...]}.

%% }}}
%%----------------------------------------------------------
%% jsx-like types {{{

-type jsx_object() :: [{}] | [{binary(), jsx_value()}, ...].

-type jsx_list() :: [jsx_value()].

-type jsx_value() :: binary() | scalar() | jsx_list() | jsx_object().
%% NOTE: `string()' doesn't occur, there's `binary()' instead.
%%
%% NOTE: `datetime()' is not quite jsx-compatible.

%% }}}
%%----------------------------------------------------------
%% errors {{{

-type error_location() ::
  {Path :: [string(), ...], CurLine :: line(), PrevLine :: line()}.

% this error type doesn't cover inline sections (objects) and inline arrays
-type error() ::
    {descent, key, error_location()}
  | {section, key | section | array_section, error_location()}
  | {array_section, key | section | auto_section, error_location()}
  | {key, key | section | auto_section | array_section, error_location()}.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------

%% @doc Build value store out of a list of directives that come from parsing.

-spec build_store([ast_section_header() | ast_key_value()]) ->
  {ok, store()} | {error, Reason}
  when Reason :: term().

build_store(Directives) ->
  try
    Store = build_store(Directives, [], empty_store()),
    {ok, Store}
  catch
    throw:Reason ->
      {error, Reason}
  end.

%% @doc Interpret AST directives one by one, building value store.
%%
%%   Function throws a reason (`erlang:throw()') on error.

-spec build_store([ast_section_header() | ast_key_value()],
                  [] | ast_section_name(), store()) ->
  store().

build_store([{table, Line, SectionName} | Rest] = _Directives,
            _CurrentSection, Store) ->
  NewValues = add_section(SectionName, [], Line, Store),
  build_store(Rest, SectionName, NewValues);

build_store([{array_table, Line, SectionName} | Rest] = _Directives,
            _CurrentSection, Store) ->
  NewValues = add_array_section(SectionName, [], Line, Store),
  build_store(Rest, SectionName, NewValues);

build_store([{key, Line, Key, Value} | Rest] = _Directives,
            CurrentSection, Store) ->
  NewValues = set(CurrentSection, CurrentSection, Key, Value, Line, Store),
  build_store(Rest, CurrentSection, NewValues);

build_store([] = _Directives, _CurrentSection, Store) ->
  Store.

%%%---------------------------------------------------------------------------
%%% setters for nested dictionaries structure
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% empty_store() {{{

%% @doc Create an empty value store.

-spec empty_store() ->
  store().

empty_store() ->
  dict:new().

%% }}}
%%----------------------------------------------------------
%% set(SectionName, ErrorPath, Key, Value, Line, Store) {{{

%% @doc Set a value under a specified key in specified section in value store.

-spec set([] | ast_section_name(), [string()],
          ast_key(), ast_value(), line(), store()) ->
  store().

set([] = _SectionName, ErrorPath, Key, Value, Line, Store) ->
  ValueType = typeof(Value),
  case dict:find(Key, Store) of
    {ok, {PrevLine, object, _Value}} ->
      ErrorLocation = {lists:reverse(ErrorPath), Line, PrevLine},
      erlang:throw({key, key, ErrorLocation});
    {ok, {PrevLine, Type, _Value}} when Type /= object ->
      ErrorLocation = {lists:reverse(ErrorPath), Line, PrevLine},
      erlang:throw({key, Type, ErrorLocation});
    error when ValueType == object ->
      dict:store(Key, {Line, object, build_object(Value)}, Store);
    error when ValueType == array ->
      dict:store(Key, {Line, key, {array, build_array(Value)}}, Store);
    error ->
      dict:store(Key, {Line, key, Value}, Store)
  end;

set([Name | Rest] = _SectionName, ErrorPath, Key, Value, Line, Store) ->
  % XXX: descent here cannot encounter anything but sections, because the same
  % descent was already performed previously, when the section that this key
  % is in was opened
  case dict:find(Name, Store) of
    {ok, {PrevLine, section, SubStore}} ->
      NewSubStore = set(Rest, ErrorPath, Key, Value, Line, SubStore),
      dict:store(Name, {PrevLine, section, NewSubStore}, Store);
    {ok, {PrevLine, auto_section, SubStore}} ->
      NewSubStore = set(Rest, ErrorPath, Key, Value, Line, SubStore),
      dict:store(Name, {PrevLine, auto_section, NewSubStore}, Store);
    {ok, {PrevLine, array_section, [SubStore | RestStores]}} ->
      NewSubStore = set(Rest, ErrorPath, Key, Value, Line, SubStore),
      NewSubStoreList = [NewSubStore | RestStores],
      dict:store(Name, {PrevLine, array_section, NewSubStoreList}, Store)
  end.

%% }}}
%%----------------------------------------------------------
%% add_section(Path, Line, Store) {{{

%% @doc Add a new section to value store.

-spec add_section(ast_section_name(), [string()], line(), store()) ->
  store().

add_section([Name] = _SectionName, ErrorPath, Line, Store) ->
  case dict:find(Name, Store) of
    {ok, {PrevLine, section, _SubStore}} ->
      ErrorLocation = {lists:reverse(ErrorPath), Line, PrevLine},
      erlang:throw({section, section, ErrorLocation});
    {ok, {_PrevLine, auto_section, SubStore}} ->
      % automatically defined section, change its type and definition line
      dict:store(Name, {Line, section, SubStore}, Store);
    {ok, {PrevLine, array_section, _SubStore}} ->
      ErrorLocation = {lists:reverse(ErrorPath), Line, PrevLine},
      erlang:throw({section, array_section, ErrorLocation});
    {ok, {PrevLine, object, _PrevValue}} ->
      ErrorLocation = {lists:reverse(ErrorPath), Line, PrevLine},
      erlang:throw({section, key, ErrorLocation});
    {ok, {PrevLine, key, _PrevValue}} ->
      ErrorLocation = {lists:reverse(ErrorPath), Line, PrevLine},
      erlang:throw({section, key, ErrorLocation});
    error ->
      % not defined, add an empty section
      NewSubStore = empty_store(),
      dict:store(Name, {Line, section, NewSubStore}, Store)
  end;

add_section([Name | Rest] = _SectionName, ErrorPath, Line, Store) ->
  case dict:find(Name, Store) of
    {ok, {PrevLine, section, SubStore}} ->
      NewSubStore = add_section(Rest, [Name | ErrorPath], Line, SubStore),
      dict:store(Name, {PrevLine, section, NewSubStore}, Store);
    {ok, {PrevLine, auto_section, SubStore}} ->
      NewSubStore = add_section(Rest, [Name | ErrorPath], Line, SubStore),
      dict:store(Name, {PrevLine, auto_section, NewSubStore}, Store);
    {ok, {PrevLine, array_section, [SubStore | RestStores]}} ->
      NewSubStore = add_section(Rest, [Name | ErrorPath], Line, SubStore),
      NewSubStoreList = [NewSubStore | RestStores],
      dict:store(Name, {PrevLine, array_section, NewSubStoreList}, Store);
    {ok, {PrevLine, object, _PrevValue}} ->
      ErrorLocation = {lists:reverse(ErrorPath), Line, PrevLine},
      erlang:throw({descent, key, ErrorLocation});
    {ok, {PrevLine, key, _PrevValue}} ->
      ErrorLocation = {lists:reverse(ErrorPath), Line, PrevLine},
      erlang:throw({descent, key, ErrorLocation});
    error ->
      NewSubStore = add_section(Rest, [Name | ErrorPath], Line, empty_store()),
      dict:store(Name, {Line, auto_section, NewSubStore}, Store)
  end.

%% }}}
%%----------------------------------------------------------
%% add_array_section(Path, Line, Store) {{{

%% @doc Add a new section to value store.

-spec add_array_section(ast_section_name(), [string()], line(), store()) ->
  store().

add_array_section([Name] = _SectionName, ErrorPath, Line, Store) ->
  case dict:find(Name, Store) of
    {ok, {PrevLine, section, _SubStore}} ->
      ErrorLocation = {lists:reverse(ErrorPath), Line, PrevLine},
      erlang:throw({array_section, section, ErrorLocation});
    {ok, {PrevLine, auto_section, _SubStore}} ->
      ErrorLocation = {lists:reverse(ErrorPath), Line, PrevLine},
      erlang:throw({array_section, auto_section, ErrorLocation});
    {ok, {PrevLine, array_section, SubStores}} ->
      NewSubStoreList = [empty_store() | SubStores],
      dict:store(Name, {PrevLine, array_section, NewSubStoreList}, Store);
    {ok, {PrevLine, object, _PrevValue}} ->
      ErrorLocation = {lists:reverse(ErrorPath), Line, PrevLine},
      erlang:throw({array_section, key, ErrorLocation});
    {ok, {PrevLine, key, _PrevValue}} ->
      ErrorLocation = {lists:reverse(ErrorPath), Line, PrevLine},
      erlang:throw({array_section, key, ErrorLocation});
    error ->
      dict:store(Name, {Line, array_section, [empty_store()]}, Store)
  end;

add_array_section([Name | Rest] = _SectionName, ErrorPath, Line, Store) ->
  case dict:find(Name, Store) of
    {ok, {PrevLine, section, SubStore}} ->
      NewSubStore = add_array_section(Rest, [Name | ErrorPath], Line, SubStore),
      dict:store(Name, {PrevLine, section, NewSubStore}, Store);
    {ok, {PrevLine, auto_section, SubStore}} ->
      NewSubStore = add_array_section(Rest, [Name | ErrorPath], Line, SubStore),
      dict:store(Name, {PrevLine, auto_section, NewSubStore}, Store);
    {ok, {PrevLine, array_section, [SubStore | RestStores]}} ->
      NewSubStore = add_array_section(Rest, [Name | ErrorPath], Line, SubStore),
      NewSubStoreList = [NewSubStore | RestStores],
      dict:store(Name, {PrevLine, array_section, NewSubStoreList}, Store);
    {ok, {PrevLine, object, _PrevValue}} ->
      ErrorLocation = {lists:reverse(ErrorPath), Line, PrevLine},
      erlang:throw({descent, key, ErrorLocation});
    {ok, {PrevLine, key, _PrevValue}} ->
      ErrorLocation = {lists:reverse(ErrorPath), Line, PrevLine},
      erlang:throw({descent, key, ErrorLocation});
    error ->
      NewSubStore = add_array_section(Rest, [Name | ErrorPath], Line,
                                      empty_store()),
      dict:store(Name, {Line, auto_section, NewSubStore}, Store)
  end.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% AST to value store structures converters (for complex values)
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% build_object() {{{

%% @doc Convert inline table AST fragment to a value store.
%%
%%   Arrays in the table are converted with {@link build_array/1}, inner
%%   inline objects are converted recursively.

-spec build_object(ast_inline_table()) ->
  store().

build_object({inline_table, KeyValues} = _Object) ->
  lists:foldl(
    fun({key, Line, Key, Value}, Store) ->
      ValueType = typeof(Value),
      case dict:find(Key, Store) of
        {ok, {PrevLine, key, _PrevValue}} ->
          erlang:throw({duplicate, PrevLine}); % TODO: more precise error
        error when ValueType == object ->
          dict:store(Key, {Line, object, build_object(Value)}, Store);
        error when ValueType == array ->
          dict:store(Key, {Line, key, {array, build_array(Value)}}, Store);
        error ->
          Entry = {Line, key, Value},
          dict:store(Key, Entry, Store)
      end
    end,
    dict:new(),
    KeyValues
  ).

%% }}}
%%----------------------------------------------------------
%% build_array() {{{

%% @doc Convert array AST fragment to store's array representation.
%%
%%   Array of arrays is converted recursively.
%%
%%   Array of objects is converted to list of jsx-like structures.

-spec build_array(ast_array()) ->
  store_array().

build_array({array, []} = _Value) ->
  {empty, []};
build_array({array, [HE | Rest] = Elements} = _Value) ->
  Type = typeof(HE),
  case lists:all(fun(E) -> Type == typeof(E) end, Rest) of
    true when Type == object ->
      {Type, [store_to_jsx(build_object(E)) || E <- Elements]};
    true when Type == array ->
      {Type, [build_array(E) || E <- Elements]};
    true ->
      {Type, Elements};
    false ->
      % TODO: more precise error (which element, expected vs. encountered
      % type, array line number, mismatched element line number)
      erlang:throw(array_type_mismatch)
  end.

%% }}}
%%----------------------------------------------------------
%% store_to_jsx() {{{

%% @doc Convert a value store to a jsx-like structure.

-spec store_to_jsx(store()) ->
  jsx_object().

store_to_jsx(Store) ->
  case dict:size(Store) of
    0 -> [{}];
    _ -> dict:fold(fun store_to_jsx_fold/3, [], Store)
  end.

%% @doc Fold worker for {@link store_to_jsx/1}.

-spec store_to_jsx_fold(store_key(), store_value(), jsx_object()) ->
  jsx_object().

store_to_jsx_fold(K, {_Line, T, V}, Acc) ->
  Key = unicode:characters_to_binary(K),
  Value = case V of
    Stores when T == array_section ->
      lists:reverse([store_to_jsx(S) || S <- Stores]);
    Store when T == section; T == auto_section; T == object ->
      store_to_jsx(Store);
    {array, Array} ->
      array_to_jsx(Array);
    String when is_list(String) ->
      unicode:characters_to_binary(String);
    _ -> V
  end,
  [{Key, Value} | Acc].

%% @doc Convert a store representation of an array to a jsx-like structure.

-spec array_to_jsx(store_array()) ->
  jsx_list().

array_to_jsx({object, Object} = _Array) ->
  % objects in an array are already jsx-like structures (see build_array/1)
  Object;
array_to_jsx({array, Arrays} = _Array) ->
  [array_to_jsx(A) || A <- Arrays];
array_to_jsx({string, Strings} = _Array) ->
  [unicode:characters_to_binary(S) || S <- Strings];
array_to_jsx({_Type, Values} = _Array) ->
  % empty, int, float, bool, datetime stay as they are (though datetime is not
  % quite jsx-compatible)
  Values.

%% }}}
%%----------------------------------------------------------

%% @doc Determine type of an AST fragment that represents a value (RHS).

-spec typeof(ast_value()) ->
  string | integer | float | boolean | datetime | array | object.

typeof(Value) when is_list(Value) -> string;
typeof(Value) when is_integer(Value) -> integer;
typeof(Value) when is_float(Value) -> float;
typeof(Value) when is_boolean(Value) -> boolean;
typeof({datetime, _, _} = _Value) -> datetime;
typeof({datetime, _} = _Value) -> datetime;
typeof({date, _} = _Value) -> datetime;
typeof({time, _} = _Value) -> datetime;
typeof({array, _} = _Value) -> array;
typeof({inline_table, _} = _Value) -> object.

%%%---------------------------------------------------------------------------
%%% error formatting
%%%---------------------------------------------------------------------------

%% @doc Prepare a human-readable error message out of an error.

-spec format_error(term()) ->
  string().

%% errors extracted directly from `erlang:throw()'
format_error({duplicate, PrevLine} = _Reason) ->
  "key already defined in line " ++ integer_to_list(PrevLine);
format_error({type_mismatch, PrevLine} = _Reason) ->
  "section defined with a different type in line " ++ integer_to_list(PrevLine);
format_error(array_type_mismatch = _Reason) ->
  "array elements are of different type";

format_error(_Reason) ->
  "unrecognized error".

%%%---------------------------------------------------------------------------
%%% value store traversal
%%%---------------------------------------------------------------------------

%% @doc Traverse sections ("tables") and keys set in TOML file.
%%
%%   All keys of the section and all of the subsections are visited before
%%   moving to a sibling section. `Fun' is called with `section' argument
%%   before a call for any key or subsection. Otherwise, the traversal order
%%   is unspecified.
%%
%%   Array sections are passed as they were arrays of inline sections.

-spec fold(Fun, AccIn, store()) ->
  AccOut
  when Fun :: fun((Path, Key, Value | section, AccIn) -> AccOut),
       Path :: [string()],
       Key :: string(),
       Value :: {string, string()}
              | {integer, integer()}
              | {float, float()}
              | {boolean, boolean()}
              | {datetime, datetime()}
              | {array, store_array()},
       AccIn :: term(),
       AccOut :: term().

fold(Fun, AccIn, Store) when is_function(Fun, 4) ->
  {_, _, AccOut} = dict:fold(fun dict_fold_traverse/3, {Fun,[],AccIn}, Store),
  AccOut.

%% @doc Workhorse for {@link fold/3}.
%%   Intended to be passed to {@link dict:fold/3}.

-spec dict_fold_traverse(store_key(), store_value(),
                         {fun(), [string()], term()}) ->
  {fun(), [string()], term()}.

dict_fold_traverse(Key, {_Line, key, {array, Values}}, {Fun, Path, Acc}) ->
  NewAcc = Fun(Path, Key, {array, Values}, Acc),
  {Fun, Path, NewAcc};
dict_fold_traverse(Key, {_Line, key, Value}, {Fun, Path, Acc}) ->
  NewAcc = Fun(Path, Key, {typeof(Value), Value}, Acc),
  {Fun, Path, NewAcc};
dict_fold_traverse(Key, {_Line, object, SubStore}, {Fun, Path, Acc}) ->
  NewAcc = Fun(Path, Key, section, Acc),
  {_, _, NewAcc1} =
    dict:fold(fun dict_fold_traverse/3, {Fun, Path ++ [Key], NewAcc}, SubStore),
  {Fun, Path, NewAcc1};
dict_fold_traverse(Key, {_Line, section, SubStore}, {Fun, Path, Acc}) ->
  NewAcc = Fun(Path, Key, section, Acc),
  {_, _, NewAcc1} =
    dict:fold(fun dict_fold_traverse/3, {Fun, Path ++ [Key], NewAcc}, SubStore),
  {Fun, Path, NewAcc1};
dict_fold_traverse(Key, {_Line, auto_section, SubStore}, {Fun, Path, Acc}) ->
  NewAcc = Fun(Path, Key, section, Acc),
  {_, _, NewAcc1} =
    dict:fold(fun dict_fold_traverse/3, {Fun, Path ++ [Key], NewAcc}, SubStore),
  {Fun, Path, NewAcc1};
dict_fold_traverse(Key, {_Line, array_section, SubStores}, {Fun, Path, Acc}) ->
  Values = lists:reverse([store_to_jsx(Store) || Store <- SubStores]),
  NewAcc = Fun(Path, Key, {array, {object, Values}}, Acc),
  {Fun, Path, NewAcc}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker

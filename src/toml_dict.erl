%%%---------------------------------------------------------------------------
%%% @private
%%% @doc
%%%   Utility functions to convert AST coming from parser to a set of nested
%%%   dictionaries.
%%%
%%% @todo array tables
%%% @todo precise error reporting
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

-type ast_section_header() :: {table, line(), ast_section_name()}.
% also: `{array_table, line(), ast_section_name()}'

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
  | {line(), section, store()}. % TODO: array_section

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
%% JSX-like types {{{

-type jsx_object() :: [{}] | [{binary(), jsx_value()}, ...].

-type jsx_list() :: [jsx_value()].

-type jsx_value() :: binary() | scalar() | jsx_list() | jsx_object().
%% NOTE: `string()' doesn't occur, there's `binary()' instead.
%%
%% NOTE: `datetime()' is not quite JSX-compatible.

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
  NewValues = add_section(SectionName, Line, Store),
  build_store(Rest, SectionName, NewValues);

%build_store([{array_table, Line, SectionName} | Rest] = _Directives,
%            _CurrentSection, Store) ->
%  NewValues = add_array_section(...),
%  build_store(Rest, SectionName, NewValues);

build_store([{key, Line, Key, Value} | Rest] = _Directives,
            CurrentSection, Store) ->
  NewValues = set(CurrentSection, Key, Value, Line, Store),
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
%% set(SectionName, Key, Value, Line, Store) {{{

%% @doc Set a value under a specified key in specified section in value store.

-spec set([] | ast_section_name(), ast_key(), ast_value(), line(), store()) ->
  store().

set([] = _SectionName, Key, Value, Line, Store) ->
  ValueType = typeof(Value),
  case dict:find(Key, Store) of
    {ok, {PrevLine, _Type, _Value}} ->
      erlang:throw({duplicate, PrevLine});
    error when ValueType == object ->
      dict:store(Key, {Line, object, build_object(Value)}, Store);
    error when ValueType == array ->
      dict:store(Key, {Line, key, {array, build_array(Value)}}, Store);
    error ->
      dict:store(Key, {Line, key, Value}, Store)
  end;

set([Name | Rest] = _SectionName, Key, Value, Line, Store) ->
  case dict:find(Name, Store) of
    {ok, {PrevLine, section, SubStore}} ->
      NewSubStore = set(Rest, Key, Value, Line, SubStore),
      dict:store(Name, {PrevLine, section, NewSubStore}, Store);
    %{ok, {PrevLine, array_section, SubStore}} ->
    %  TODO: what to do here? how to append to most recent object?
    {ok, {PrevLine, object, _PrevValue}} ->
      erlang:throw({duplicate, PrevLine}); % TODO: different error
    {ok, {PrevLine, key, _PrevValue}} ->
      erlang:throw({duplicate, PrevLine}); % TODO: different error
    error ->
      NewSubStore = set(Rest, Key, Value, Line, empty_store()),
      % TODO: mark the section as autodefined
      dict:store(Name, {Line, section, NewSubStore}, Store)
  end.

%% }}}
%%----------------------------------------------------------
%% add_section(Path, Line, Store) {{{

%% @doc Add a new section to value store.

-spec add_section(ast_section_name(), line(), store()) ->
  store().

add_section([Name] = _SectionName, Line, Store) ->
  case dict:find(Name, Store) of
    {ok, {_PrevLine, section, _SubStore}} ->
      % OK, already defined
      Store;
    {ok, {PrevLine, array_section, _SubStore}} ->
      erlang:throw({type_mismatch, PrevLine}); % TODO: different error
    {ok, {PrevLine, object, _PrevValue}} ->
      erlang:throw({duplicate, PrevLine}); % TODO: different error
    {ok, {PrevLine, key, _PrevValue}} ->
      erlang:throw({duplicate, PrevLine}); % TODO: different error
    error ->
      % not defined, add an empty section
      NewSubStore = empty_store(),
      dict:store(Name, {Line, section, NewSubStore}, Store)
  end;

add_section([Name | Rest] = _SectionName, Line, Store) ->
  case dict:find(Name, Store) of
    {ok, {PrevLine, section, SubStore}} ->
      NewSubStore = add_section(Rest, Line, SubStore),
      dict:store(Name, {PrevLine, section, NewSubStore}, Store);
    %{ok, {PrevLine, array_section, SubStore}} ->
    %  TODO: what to do here? how to append to most recent object?
    {ok, {PrevLine, object, _PrevValue}} ->
      erlang:throw({duplicate, PrevLine}); % TODO: different error
    {ok, {PrevLine, key, _PrevValue}} ->
      erlang:throw({duplicate, PrevLine}); % TODO: different error
    error ->
      NewSubStore = add_section(Rest, Line, empty_store()),
      % TODO: mark the section as autodefined
      dict:store(Name, {Line, section, NewSubStore}, Store)
  end.

%% }}}
%%----------------------------------------------------------
%% add_array_section(Path, Line, Store) (TODO) {{{

%add_array_section(...) ->
%  ...

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
%%   Array of objects is converted to list of JSX-like structures (see
%%   {@link object_to_jsx/1}).

-spec build_array(ast_array()) ->
  store_array().

build_array({array, []} = _Value) ->
  {empty, []};
build_array({array, [HE | Rest] = Elements} = _Value) ->
  Type = typeof(HE),
  case lists:all(fun(E) -> Type == typeof(E) end, Rest) of
    true when Type == object ->
      {Type, [object_to_jsx(E) || E <- Elements]};
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
%% object_to_jsx() {{{

%% @doc Convert inline table AST fragment to a JSX-like structure.

-spec object_to_jsx(ast_inline_table()) ->
  jsx_object().

object_to_jsx(Object) ->
  Store = build_object(Object),
  store_to_jsx(Store).

%% @doc Convert a value store to a JSX-like structure.

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
    Store when T == object -> store_to_jsx(Store);
    {array, Array} -> array_to_jsx(Array);
    String when is_list(String) -> unicode:characters_to_binary(String);
    _ -> V
  end,
  [{Key, Value} | Acc].

%% @doc Convert a store representation of an array to a JSX-like structure.

-spec array_to_jsx(store_array()) ->
  jsx_list().

array_to_jsx({object, Object} = _Array) ->
  % objects in an array are already JSX-like structures (see build_array/1)
  Object;
array_to_jsx({array, Arrays} = _Array) ->
  [array_to_jsx(A) || A <- Arrays];
array_to_jsx({string, Strings} = _Array) ->
  [unicode:characters_to_binary(S) || S <- Strings];
array_to_jsx({_Type, Values} = _Array) ->
  % empty, int, float, bool, datetime stay as they are (though datetime is not
  % quite JSX-compatible)
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
%%   moving to a sibling section.
%%
%%   `Fun' is called with `section' argument before a call for any key or
%%   subsection.
%%
%%   Otherwise, the order of the traversal is unspecified.

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
  {Fun, Path, NewAcc1}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker

%%%---------------------------------------------------------------------------
%%% @doc
%%%   TOML parser module.
%%% @end
%%%---------------------------------------------------------------------------

-module(toml).

%% parser wrappers
-export([read_file/1, read_file/2]).
-export([parse/1, parse/2]).
%% explaining errors
-export([format_error/1]).
%% data accessors
-export([get_value/3, get_value/4, exists/2, exists/3]).
-export([keys/2, sections/2, foldk/4, folds/4]).
%-export([to_list/1, to_list/2]).

-export_type([config/0, section/0, key/0, toml_value/0]).
-export_type([toml_array/0, datetime/0]).
-export_type([jsx_object/0, jsx_list/0, jsx_value/0]).
-export_type([validate_fun/0, validate_fun_return/0]).
-export_type([validate_location/0, validate_error/0]).
-export_type([toml_error/0, semantic_error/0]).
-export_type([semerr_redefinition/0, semerr_inline/0]).
-export_type([semerr_data_location/0, semerr_location/0]).

%%%---------------------------------------------------------------------------
%%% data types

%%----------------------------------------------------------
%% main types {{{

-opaque config() :: {toml, term()}.
%% A tuple with atom `toml' being its first element.

-type section() :: [string()].
%% Name of a section ("table" in TOML's terms). Root section is denoted by
%% empty list (`[]').

-type key() :: string().
%% Name of a value in a section.

-type toml_value() ::
    {string, string()}
  | {integer, integer()}
  | {float, float()}
  | {boolean, boolean()}
  | {datetime, datetime()}
  | {array, toml_array()}
  | {data, term()}.
%% Value stored under {@type key()}, along with its type.
%%
%% Custom Erlang structure returned by validation function ({@type
%% validate_fun()}) is denoted by {@type @{data, Data@}}.
%%
%% Array of values is doubly typed, first as an array, and then with data type
%% of its content, e.g. `{array, {string, ["one", "two", "three"]}}'. See
%% {@type toml_array()} for details.

%% }}}
%%----------------------------------------------------------
%% auxiliary types {{{

-type toml_array() ::
    {empty, []}
  | {string, [string(), ...]}
  | {integer, [integer(), ...]}
  | {float, [float(), ...]}
  | {boolean, [boolean(), ...]}
  | {datetime, [datetime(), ...]}
  | {array, [toml_array(), ...]}
  | {object, [jsx_object(), ...]}.
%% Representation of array's content.

-type datetime() ::
    {datetime, calendar:datetime(), TZ :: string()}
  | {datetime, calendar:datetime()}
  | {date, calendar:date()}
  | {time, calendar:time()}.
%% RFC 3339 timestamp (with or without timezone), date, or time.
%%
%% `TZ' is either a `"Z"' (the same as `"+00:00"') or has format
%% `"[+-]HH:MM"'.

-type jsx_object() :: [{}] | [{binary(), jsx_value()}, ...].
%% Object (inline section/table) representation, jsx-style.

-type jsx_list() :: [jsx_value()].
%% Array representation, jsx-style.

-type jsx_value() :: binary()
                   | integer()
                   | float()
                   | boolean()
                   | datetime()
                   | jsx_list()
                   | jsx_object().
%% Arbitrary value (scalar/array/object), jsx-style. {@type datetime()} is not
%% really jsx-compatible, and there's no `null'.

%% }}}
%%----------------------------------------------------------
%% validation function {{{

-type validate_fun() ::
  fun((section(), key(), toml_value() | section, Arg :: term()) ->
        validate_fun_return()).
%% Key validation callback. This callback is specified at configuration
%% parsing time and has a chance to further verify validity of a value or even
%% convert it already to its intended form, e.g. listen address
%% `"<host>:<port>"' can be immediately converted to `{Host,Port}' tuple.
%%
%% <b>NOTE</b>: Array section ("array of tables" in TOML's terms) is passed
%% as an array of objects, i.e.
%% {@type @{array, @{object, [jsx_object(), ...]@}@}}.
%%
%% Since it's not allowed to have a section and key of the same name,
%% subsections themselves are also subject to validation. Validation function
%% can return `ok', `{ok,_}', or `ignore' to accept the section name (the
%% three values have the same result; any data from `{ok,Data}' is ignored)
%% and `{error,_}' to reject the name.

-type validate_fun_return() :: ok | {ok, Data :: term()} | ignore
                             | {error, validate_error()}.
%% Expected return values from {@type validate_fun()}.
%%
%% {@type @{ok, Data@}} results in the {@type toml_value()} of `{data, Data}'.
%% See {@link get_value/3}.
%%
%% {@type @{error, Reason :: validate_error()@}} is reported by
%% {@link read_file/2} and {@link parse/2} as
%% {@type @{error, @{validate, Where :: validate_location(), Reason@}@}}.

-type validate_error() :: term().
%% Error returned by {@type validate_fun()}. See {@type validate_fun_return()}
%% for details.

%% }}}
%%----------------------------------------------------------
%% errors {{{

-type toml_error() :: {tokenize, Line :: pos_integer()}
                    | {parse, Line :: pos_integer()}
                    | {semantic, semantic_error()}
                    | {bad_return, validate_location(), Result :: term()}
                    | {validate, validate_location(), validate_error()}.
%% Error in processing TOML.

-type validate_location() ::
  {Section :: [string()], Key :: string(), Line :: pos_integer()}.
%% Location information of validation error (see {@type validate_fun()}).

-type semantic_error() :: semerr_redefinition() | semerr_inline().
%% Data-level error, meaning that data represented by TOML config is forbidden
%% by TOML specification.

-type semerr_redefinition() ::
    {auto_section, key, semerr_location()}
  | {section, key | section | array_section, semerr_location()}
  | {array_section, key | section | auto_section, semerr_location()}
  | {key, key | section | auto_section | array_section, semerr_location()}.
%% Error signifying that a key/section was already defined, either explicitly
%% or implicitly. The structure of the error follows convention of
%% {@type @{Type, PreviousType, semerr_location()@}}.
%%
%% `auto_section' in `Type' means that there already exists a key with the
%% same name as one of the parent sections of the current section.
%%
%% `auto_section' in `PreviousType' means that the section was not defined
%% explicitly, but earlier sections restrict how it could look like (i.e.
%% a subsection was already defined).

-type semerr_inline() ::
    {duplicate, Key :: string(), semerr_data_location(), semerr_location()}
  | {type_mismatch,
      {Pos :: pos_integer(), OffendingType :: atom(), ExpectedType :: atom()},
      semerr_data_location(), semerr_location()}.
%% Error signifying that inline object has two keys of the same name or an
%% inline array has elements of different types.
%%
%% `Pos' is a 1-based index in the array, `ExpectedType' is data type of the
%% first array element, and `OffendingType' is the type of the first element
%% that doesn't match.

-type semerr_data_location() ::
  [pos_integer() | string()].
%% Location of a semantic error in inline data (arrays and objects). The
%% location is a path specified in terms appropriate for respective data
%% types: key for objects, 1-based index for arrays.

-type semerr_location() ::
  {Path :: [string(), ...], CurLine :: pos_integer(),
    PrevLine :: pos_integer()}.
%% Location information of semantic error. `Path' is name of the offending
%% section and, if applicable, key.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% parser wrappers
%%%---------------------------------------------------------------------------

%% @doc Parse a TOML file on disk.

-spec read_file(file:filename()) ->
  {ok, config()} | {error, ReadError | toml_error()}
  when ReadError :: file:posix() | badarg | terminated | system_limit.

read_file(File) ->
  case file:read_file(File) of
    {ok, Content} -> parse(Content);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Parse a TOML file on disk.
%%
%%   Each of the keys in the file is passed through a validation callback that
%%   can accept the key, reject it, make it skipped, or further parse its
%%   value for later retrieval.

-spec read_file(file:filename(), {validate_fun(), Arg :: term()}) ->
  {ok, config()} | {error, ReadError | toml_error()}
  when ReadError :: file:posix() | badarg | terminated | system_limit.

read_file(File, {Fun, _Arg} = Validate) when is_function(Fun, 4) ->
  case file:read_file(File) of
    {ok, Content} -> parse(Content, Validate);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Parse a TOML config from a string.

-spec parse(string() | binary() | iolist()) ->
  {ok, config()} | {error, toml_error()}.

parse(String) ->
  parse(String, {fun accept_all/4, []}).

%% @doc Parse a TOML config from a string.
%%
%%   Each of the keys in the config is passed through a validation callback
%%   that can accept the key, reject it, make it skipped, or further parse its
%%   value for later retrieval.

-spec parse(string() | binary() | iolist(), {validate_fun(), Arg :: term()}) ->
  {ok, config()} | {error, toml_error()}.

parse(String, {Fun, Arg} = _Validate) when is_function(Fun, 4) ->
  % the grammar assumes that the input ends with newline character
  case toml_lexer:tokenize(String) of
    {ok, Tokens, _EndLine} ->
      case toml_parser:parse(Tokens) of
        {ok, Result} ->
          build_config(Result, Fun, Arg);
        {error, {LineNumber, _ParserModule, _Message}} ->
          {error, {parse, LineNumber}}
      end;
    {error, {LineNumber, _LexerModule, _Message}, _} ->
      {error, {tokenize, LineNumber}}
  end.

%% @doc Default validation function that accepts all values.

-spec accept_all(section(), key(), toml_value(), term()) ->
  ok.

accept_all(_Section, _Key, _Value, _Arg) ->
  ok.

%%----------------------------------------------------------
%% build_config() {{{

%% @doc Convert AST coming from parser to a config representation.

-spec build_config([term()], validate_fun(), term()) ->
  {ok, config()} | {error, Reason}
  when Reason :: {semantic, term()}
               | {validate, Where :: validate_location(), validate_error()}
               | {bad_return, Where :: validate_location(), term()}.

build_config(Directives, Fun, Arg) ->
  case toml_dict:build_store(Directives) of
    {ok, Store} ->
      EmptyConfig = dict:store([], empty_section(), dict:new()),
      try toml_dict:fold(fun build_config/4, {Fun, Arg, EmptyConfig}, Store) of
        {_, _, Config} -> {ok, {toml, Config}}
      catch
        throw:{bad_return, {Section, Key}, Result} ->
          Line = toml_dict:find_line(Section, Key, Store),
          {error, {bad_return, {Section, Key, Line}, Result}};
        throw:{validate, {Section, Key}, Reason} ->
          Line = toml_dict:find_line(Section, Key, Store),
          {error, {validate, {Section, Key, Line}, Reason}}
      end;
    {error, Reason} ->
      {error, {semantic, Reason}}
  end.

%% @doc Fold workhorse for {@link build_config/1}.

-spec build_config(section(), key(), section | toml_value(), term()) ->
  term().

build_config(Section, Key, section = _Value, {ValidateFun, Arg, Config}) ->
  case ValidateFun(Section, Key, section, Arg) of
    ok -> ok;
    {ok, _Data} -> ignore;
    ignore -> ok;
    {error, Reason} -> erlang:throw({validate, {Section, Key}, Reason});
    Result -> erlang:throw({bad_return, {Section, Key}, Result})
  end,
  NewConfig = dict:update(
    Section,
    fun({Keys, SubSects}) -> {Keys, [Key | SubSects]} end,
    Config
  ),
  {ValidateFun, Arg, dict:store(Section ++ [Key], empty_section(), NewConfig)};
build_config(Section, Key, {_T, _V} = Value, {ValidateFun, Arg, Config}) ->
  % NOTE: array value from `toml_dict' is compatible with this module
  NewConfig = case ValidateFun(Section, Key, Value, Arg) of
    ok ->
      dict:update(
        Section,
        fun({Keys, SubSects}) -> {dict:store(Key, Value, Keys), SubSects} end,
        Config
      );
    {ok, Data} ->
      Value1 = {data, Data},
      dict:update(
        Section,
        fun({Keys, SubSects}) -> {dict:store(Key, Value1, Keys), SubSects} end,
        Config
      );
    ignore ->
      Config;
    {error, Reason} ->
      erlang:throw({validate, {Section, Key}, Reason});
    Result ->
      erlang:throw({bad_return, {Section, Key}, Result})
  end,
  {ValidateFun, Arg, NewConfig}.

%% @doc Create a value for an empty section.

empty_section() ->
  KeyValues = dict:new(),
  SubSections = [],
  {KeyValues, SubSections}.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% explaining errors
%%%---------------------------------------------------------------------------

%% @doc Prepare a human-readable error message out of an error.

-spec format_error(Reason :: term()) ->
  string().

format_error({validate, {_Section, _Key, _Line} = _Where, Reason}) ->
  % TODO: use `Where' (error location)
  unicode:characters_to_list([
    "validation error: ",
    io_lib:print(Reason, 1, 16#ffffffff, -1)
  ]);
format_error({bad_return, {_Section, _Key, _Line} = _Where, Result}) ->
  % TODO: use `Where' (error location)
  unicode:characters_to_list([
    "unexpected value from validation function: ",
    io_lib:print(Result, 1, 16#ffffffff, -1)
  ]);
format_error({semantic, Reason}) ->
  toml_dict:format_error(Reason);
format_error({parse, Line}) ->
  "syntax error in line " ++ integer_to_list(Line);
format_error({tokenize, Line}) ->
  "unexpected character in line " ++ integer_to_list(Line);
format_error(Reason) when is_atom(Reason) ->
  file:format_error(Reason);
format_error(Reason) ->
  unicode:characters_to_list([
    "unrecognized error: ",
    io_lib:print(Reason, 1, 16#ffffffff, -1)
  ]).

%%%---------------------------------------------------------------------------
%%% data accessors
%%%---------------------------------------------------------------------------

%% @doc Get tagged value from config.

-spec get_value(section(), key(), config()) ->
  toml_value() | none | section.

get_value(Section, Key, {toml, Store} = _Config) ->
  case dict:find(Section, Store) of
    {ok, {KeyValues, SubSections}} ->
      case dict:find(Key, KeyValues) of
        {ok, {_T,_V} = Value} ->
          Value;
        error ->
          case lists:member(Key, SubSections) of
            true -> section;
            false -> none
          end
      end;
    error ->
      none
  end.

%% @doc Get tagged value from config.
%%   If the key doesn't exist, specified default is returned.

-spec get_value(section(), key(), config(), toml_value()) ->
  toml_value() | section.

get_value(Section, Key, {toml, _} = Config, Default) ->
  case get_value(Section, Key, Config) of
    none -> Default;
    Any -> Any
  end.

%% @doc Check if the section exists.
%%   If there is a key under the specified name, `false' is returned.

-spec exists(section(), config()) ->
  boolean().

exists(Section, {toml, Store} = _Config) ->
  dict:is_key(Section, Store).

%% @doc Check if the key exists.
%%   If there is a section under the specified name, `false' is returned.

-spec exists(section(), key(), config()) ->
  boolean().

exists(Section, Key, {toml, Store} = _Config) ->
  case dict:find(Section, Store) of
    {ok, {KeyValues, _SubSections}} -> dict:is_key(Key, KeyValues);
    error -> false
  end.

%% @doc List keys of a section.
%%
%%   Only keys that correspond to scalars or arrays are returned. Subsections
%%   (which include inline sections) are omitted.
%%
%%   `none' is returned when `Section' is neither an explicitly defined
%%   section, a section introduced implicitly by defining its subsection, nor
%%   an inline section.

-spec keys(section(), config()) ->
  [key()] | none.

keys(Section, {toml, Store} = _Config) ->
  case dict:find(Section, Store) of
    {ok, {KeyValues, _SubSections}} -> dict:fetch_keys(KeyValues);
    error -> none
  end.

%% @doc List direct subsections of a section.
%%
%%   `none' is returned when `Section' is neither an explicitly defined
%%   section, a section introduced implicitly by defining its subsection, nor
%%   an inline section.

-spec sections(section(), config()) ->
  [key()] | none.

sections(Section, {toml, Store} = _Config) ->
  case dict:find(Section, Store) of
    {ok, {_KeyValues, SubSections}} -> SubSections;
    error -> none
  end.

%% @doc Traverse all the values set in a section.

-spec foldk(section(), Fun, AccIn, config()) ->
  AccOut
  when Fun :: fun((section(), key(), toml_value(), AccIn) -> AccOut),
       AccIn :: term(),
       AccOut :: term().

foldk(Section, Fun, Acc, {toml, Store} = _Config) when is_function(Fun, 4) ->
  case dict:find(Section, Store) of
    {ok, {KeyValues, _SubSections}} ->
      TravAcc = {Section, Fun, Acc},
      {_, _, Result} = dict:fold(fun traverse_keys/3, TravAcc, KeyValues),
      Result;
    error ->
      Acc
  end.

%% @doc Workhorse for {@link foldk/4}.

traverse_keys(Key, Value, {Section, Fun, FunAcc}) ->
  NewFunAcc = Fun(Section, Key, Value, FunAcc),
  {Section, Fun, NewFunAcc}.

%% @doc Traverse the direct subsections of a section.

-spec folds(section(), Fun, AccIn, Config :: config()) ->
  AccOut
  when Fun :: fun((config(), section(), AccIn) -> AccOut),
       AccIn :: term(),
       AccOut :: term().

folds(Section, Fun, Acc, {toml, Store} = Config) when is_function(Fun, 3) ->
  case dict:find(Section, Store) of
    {ok, {_KeyValues, SubSections}} ->
      traverse_sections(SubSections, Section, Config, Fun, Acc);
    error ->
      Acc
  end.

%% @doc Workhorse for {@link folds/4}.

traverse_sections([] = _SubSections, _Section, _Config, _Fun, Acc) ->
  Acc;
traverse_sections([Name | Rest] = _SubSections, Section, Config, Fun, Acc) ->
  NewAcc = Fun(Config, Section ++ [Name], Acc),
  traverse_sections(Rest, Section, Config, Fun, NewAcc).

%to_list({toml, Store} = _Config) ->
%  'TODO'.

%to_list(Section, {toml, Store} = _Config) ->
%  'TODO'.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker

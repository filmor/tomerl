%%%---------------------------------------------------------------------------
%%% @doc
%%% @end
%%%---------------------------------------------------------------------------

-module(toml).

%% parser wrappers
-export([read_file/1]).
-export([parse/1]).
%% explaining errors
-export([format_error/1]).
%% data accessors
-export([get_value/3, get_value/4, exists/2, exists/3]).
-export([keys/2, sections/2, foldk/4, folds/4]).
%-export([to_list/1, to_list/2]).

-export_type([config/0, section/0, key/0, value/0]).
-export_type([datetime/0, toml_array/0]).
-export_type([jsx_object/0, jsx_list/0, jsx_value/0]).
-export_type([parse_error/0]).

%%%---------------------------------------------------------------------------

-type config() :: {toml, term()}.

-type section() :: [string()].

-type key() :: string().

-type value() ::
    {string, string()}
  | {integer, integer()}
  | {float, float()}
  | {boolean, boolean()}
  | {datetime, datetime()}
  | {array, toml_array()}
  | {data, term()}.

-type toml_array() ::
    {empty, []}
  | {string, [string(), ...]}
  | {integer, [integer(), ...]}
  | {float, [float(), ...]}
  | {boolean, [boolean(), ...]}
  | {datetime, [datetime(), ...]}
  | {array, [toml_array(), ...]}
  | {object, [jsx_object(), ...]}.

-type jsx_object() :: [{}] | [{binary(), jsx_value()}, ...].

-type jsx_list() :: [jsx_value()].

-type jsx_value() :: binary()
                   | integer()
                   | float()
                   | boolean()
                   | datetime()
                   | jsx_list()
                   | jsx_object().

-type datetime() ::
    {datetime, calendar:datetime(), TZ :: string()}
  | {datetime, calendar:datetime()}
  | {date, calendar:date()}
  | {time, calendar:time()}.
%% `TZ' is either a `"Z"' (the same as `"+00:00"') or has format
%% `"[+-]HH:MM"'.

-type parse_error() :: {parse, Line :: pos_integer()}
                     | {tokenize, Line :: pos_integer()}
                     | {semantic, term()}.

%%%---------------------------------------------------------------------------
%%% parser wrappers
%%%---------------------------------------------------------------------------

%% @doc Parse a TOML file on disk.

-spec read_file(file:filename()) ->
  {ok, config()} | {error, ReadError | parse_error()}
  when ReadError :: file:posix() | badarg | terminated | system_limit.

read_file(File) ->
  case file:read_file(File) of
    {ok, Content} -> parse(Content);
    {error, Reason} -> {error, Reason}
  end.

%% @doc Parse a TOML config from a string.

-spec parse(string() | binary() | iolist()) ->
  {ok, config()} | {error, parse_error()}.

parse(String) ->
  % the grammar assumes that the input ends with newline character
  case toml_lexer:tokenize(String) of
    {ok, Tokens, _EndLine} ->
      case toml_parser:parse(Tokens) of
        {ok, Result} ->
          build_config(Result);
        {error, {LineNumber, _ParserModule, _Message}} ->
          {error, {parse, LineNumber}}
      end;
    {error, {LineNumber, _LexerModule, _Message}, _} ->
      {error, {tokenize, LineNumber}}
  end.

%%----------------------------------------------------------
%% build_config() {{{

%% @doc Convert AST coming from parser to a config representation.

-spec build_config([term()]) ->
  {ok, config()} | {error, {semantic, term()}}.

build_config(Directives) ->
  case toml_dict:build_store(Directives) of
    {ok, Store} ->
      EmptyConfig = dict:store([], empty_section(), dict:new()),
      Config = toml_dict:fold(fun build_config/4, EmptyConfig, Store),
      {ok, {toml, Config}};
    {error, Reason} ->
      {error, {semantic, Reason}}
  end.

%% @doc Fold workhorse for {@link build_config/1}.

-spec build_config(section(), key(), section | value(), term()) ->
  term().

build_config(Section, Key, section = _Value, Config) ->
  NewConfig = dict:update(
    Section,
    fun({Keys, SubSects}) ->
      {dict:store(Key, section, Keys), [Key | SubSects]}
    end,
    Config
  ),
  dict:store(Section ++ [Key], empty_section(), NewConfig);
build_config(Section, Key, {_T, _V} = Value, Config) ->
  % NOTE: array value from `toml_dict' is compatible with this module
  dict:update(
    Section,
    fun({Keys, SubSects}) -> {dict:store(Key, Value, Keys), SubSects} end,
    Config
  ).

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
  value() | none | section.

get_value(Section, Key, {toml, Store} = _Config) ->
  case dict:find(Section, Store) of
    {ok, {KeyValues, _SubSections}} ->
      case dict:find(Key, KeyValues) of
        {ok, {_T,_V} = Value} -> Value;
        {ok, section} -> section;
        error -> none
      end;
    error ->
      none
  end.

%% @doc Get tagged value from config.
%%   If the key doesn't exist, specified default is returned.

-spec get_value(section(), key(), config(), value()) ->
  value() | section.

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
    {ok, {KeyValues, _SubSections}} ->
      case dict:find(Key, KeyValues) of
        {ok, {_T,_V}} -> true;
        {ok, section} -> false;
        error -> false
      end;
    error ->
      false
  end.

%% @doc List keys of a section.
%%
%%   Only keys that correspond to scalars or arrays are returned. Subsections
%%   (which include inline sections) are omitted.

-spec keys(section(), config()) ->
  [key()] | none.

keys(Section, {toml, Store} = _Config) ->
  case dict:find(Section, Store) of
    {ok, {KeyValues, _SubSections}} ->
      dict:fold(
        fun (_K, section, Acc) -> Acc; (K, {_T,_V}, Acc) -> [K | Acc] end,
        [],
        KeyValues
      );
    error ->
      none
  end.

%% @doc List direct subsections of a section.

-spec sections(section(), config()) ->
  [key()] | none.

sections(Section, {toml, Store} = _Config) ->
  case dict:find(Section, Store) of
    {ok, {_KeyValues, SubSections}} -> SubSections;
    error -> none
  end.

%% @doc Fold over keys of a section.

-spec foldk(section(), fun(), term(), config()) ->
  term().

foldk(_Path, _Fun, _Acc, _Config) ->
  'TODO'.

%% @doc Fold over direct subsections of a section.

-spec folds(section(), fun(), term(), config()) ->
  term().

folds(_Path, _Fun, _Acc, _Config) ->
  'TODO'.

%to_list(_Config) ->
%  'TODO'.

%to_list(_Path, _Config) ->
%  'TODO'.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker

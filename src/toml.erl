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
-export([set_value/4, set_default/4, delete/2, delete/3, update/4, update/5]).

-export_type([config/0, section/0, key/0, value/0]).
-export_type([toml_datetime/0, toml_array/0, toml_object/0]).
-export_type([parse_error/0, line/0]).

%%%---------------------------------------------------------------------------

-type config() :: term().

-type section() :: [string()].

-type key() :: string().

-type value() ::
    {string, string()}
  | {integer, integer()}
  | {float, float()}
  | {boolean, boolean()}
  | {datetime, toml_datetime()}
  | {array, toml_array()}
  | {data, term()}.

-type toml_array() ::
    {Type :: empty, []}
  | {Type :: string, [string(), ...]}
  | {Type :: integer, [integer(), ...]}
  | {Type :: float, [float(), ...]}
  | {Type :: boolean, [boolean(), ...]}
  | {Type :: datetime, [toml_datetime(), ...]}
  | {Type :: array, [toml_array(), ...]}
  | {Type :: object, [toml_object(), ...]}.

-type toml_object() :: list().
%% JSX-style objects.
%% TODO: be more specific

-type toml_datetime() :: tuple().
%% TODO: be more specific

-type line() :: pos_integer().

-type parse_error() :: {parse, line()} | {tokenize, line()} | {semantic, term()}.

%%%---------------------------------------------------------------------------
%%% parser wrappers
%%%---------------------------------------------------------------------------

-spec read_file(file:filename()) ->
  {ok, config()} | {error, ReadError | parse_error()}
  when ReadError :: file:posix() | badarg | terminated | system_limit.

read_file(File) ->
  case file:read_file(File) of
    {ok, Content} -> parse(Content);
    {error, Reason} -> {error, Reason}
  end.

-spec parse(string() | binary() | iolist()) ->
  {ok, config()} | {error, parse_error()}.

parse(String) ->
  % the grammar assumes that the input ends with newline character
  case toml_lexer:tokenize(String) of
    {ok, Tokens, _EndLine} ->
      case toml_parser:parse(Tokens) of
        {ok, Result} ->
          case toml_dict:build_store(Result) of
            {ok, V} -> {ok, V};
            {error, Reason} -> {error, {semantic, Reason}}
          end;
        {error, {LineNumber, _ParserModule, _Message}} ->
          {error, {parse, LineNumber}}
      end;
    {error, {LineNumber, _LexerModule, _Message}, _} ->
      {error, {tokenize, LineNumber}}
  end.

%%%---------------------------------------------------------------------------
%%% explaining errors
%%%---------------------------------------------------------------------------

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

get_value(_Path, _Key, _Config) ->
  'TODO'.

%% @doc Get tagged value from config.
%%   If the key doesn't exist, specified default is returned.

-spec get_value(section(), key(), config(), value()) ->
  value() | section.

get_value(_Path, _Key, _Config, _Default) ->
  'TODO'.

%% @doc Check if the section exists.

-spec exists(section(), config()) ->
  boolean().

exists(_Path, _Config) ->
  'TODO'.

%% @doc Check if the key exists.

-spec exists(section(), key(), config()) ->
  boolean().

exists(_Path, _Key, _Config) ->
  'TODO'.

%% @doc List keys of a section.

-spec keys(section(), config()) ->
  [key()].

keys(_Path, _Config) ->
  'TODO'.

%% @doc List direct subsections of a section.

-spec sections(section(), config()) ->
  [key()].

sections(_Path, _Config) ->
  'TODO'.

%% @doc Fold over keys of a section.

-spec foldk(section(), fun(), term(), config()) ->
  'TODO'.

foldk(_Path, _Fun, _Acc, _Config) ->
  'TODO'.

%% @doc Fold over direct subsections of a section.

-spec folds(section(), fun(), term(), config()) ->
  'TODO'.

folds(_Path, _Fun, _Acc, _Config) ->
  'TODO'.

%to_list(_Config) ->
%  'TODO'.

%to_list(_Path, _Config) ->
%  'TODO'.

%% @doc Set (replace) a value for a key.
%%   Sections get replaced, too.

-spec set_value(section(), key(), value(), config()) ->
  Old :: value() | none.

set_value(_Path, _Key, _Value, _Config) ->
  'TODO'.

%% @doc Set a value for an undefined key.
%%   If the key has a value (or there's a section of this name), noth

-spec set_default(section(), key(), value(), config()) ->
  set | exists.

set_default(_Path, _Key, _Value, _Config) ->
  'TODO'.

-spec delete(section(), config()) ->
  ok.

delete(_Path, _Config) ->
  'TODO'.

-spec delete(section(), key(), config()) ->
  Old :: value() | none.

delete(_Path, _Key, _Config) ->
  'TODO'.

-spec update(section(), key(), fun(), config()) ->
  Old :: value().

update(_Path, _Key, _Fun, _Config) ->
  'TODO'.

-spec update(section(), key(), fun(), term(), config()) ->
  Old :: value() | none.

update(_Path, _Key, _Fun, _Initial, _Config) ->
  'TODO'.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker

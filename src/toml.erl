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
-export([]).

-export_type([config/0, parse_error/0]).

%%%---------------------------------------------------------------------------

-type config() :: term().

-type parse_error() :: term().

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
  case toml_lexer:string(unicode:characters_to_list([String, $\n])) of
    {ok, Tokens, _EndLine} ->
      case toml_parser:parse(Tokens) of
        {ok, Result} ->
          {ok, Result};
        {error, {_LineNumber, _ParserModule, _Message}} ->
          {error, {parse, badarg}}
      end;
    {error, {_LineNumber, _LexerModule, _Message}, _} ->
      {error, {parse, badarg}}
  end.

%%%---------------------------------------------------------------------------
%%% explaining errors
%%%---------------------------------------------------------------------------

-spec format_error(Reason :: term()) ->
  string().

format_error({parse, _}) -> "parse error";
format_error(Reason) when is_atom(Reason) -> file:format_error(Reason);
format_error(Reason) ->
  unicode:characters_to_list([
    "unrecognized error: ",
    io_lib:print(Reason, 1, 16#ffffffff, -1)
  ]).

%%%---------------------------------------------------------------------------
%%% data accessors
%%%---------------------------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker

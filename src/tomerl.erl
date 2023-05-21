%%%---------------------------------------------------------------------------
%%% @doc
%%%   TOML parser module.
%%% @end
%%%---------------------------------------------------------------------------

-module(tomerl).

-ignore_xref([
    read_file/1,
    parse/1,
    get/2
]).
-export([
    read_file/1,
    parse/1,
    get/2
]).

-export_type([
    section/0,
    maybe_number/0,
    value/0
]).

-type section() :: #{binary() => value()}.
%% A TOML table

-type value() ::
    maybe_number()
    | binary()
    | boolean()
    | date()
    | [value()]
    | section().
%% Valid TOML values

-type date() :: tomerl_datetime:t().
%% `tomerl' specific date type to handle all cases supported by the TOML format

-type maybe_number() ::
    number()
    | nan
    | infinity
    | negative_infinity.
%% Number type extended by nan and infinity values

-type toml_error() ::
    {tokenize, Line :: pos_integer()}
    | {parse, Line :: pos_integer()}.
%% Error in processing TOML

%% @doc Parse a TOML file
%%
%% This is equivalent to reading a file using `file:read_file/1' and passing the
%% result on to `parse/1'. The returned error may be either an error as returned
%% by `file:read_file/1' or one returned by `parse/1'.
-spec read_file(file:name_all()) ->
    {ok, section()} | {error, ReadError | toml_error()}
when
    ReadError :: file:posix() | badarg | terminated | system_limit.
read_file(File) ->
    case file:read_file(File) of
        {ok, Content} -> parse(Content);
        {error, Reason} -> {error, Reason}
    end.

%% @doc Parse TOML data from a string or `iodata'
%%
%% The TOML data is parsed and returned as a nested map with `binary()' keys,
%% see `section/0'. The returned errors from the parser and tokenizer
%% respectively currently only contain the line of the error without any further
%% information. This may change in the future.
-spec parse(string() | binary() | iolist()) ->
    {ok, section()} | {error, toml_error()}.
parse(String) ->
    try
        case tomerl_lexer:tokenize(String) of
            {ok, Tokens, _EndLine} ->
                case tomerl_parser:parse(Tokens) of
                    {ok, Result} ->
                        tomerl_convert:do(Result);
                    {error, {LineNumber, _ParserModule, _Message}} ->
                        {error, {parse, LineNumber}}
                end;
            {error, {LineNumber, _LexerModule, _Message}, _} ->
                {error, {tokenize, LineNumber}}
        end
    catch
        error:Reason:_St ->
            % {error, Reason, _St}
            {error, Reason}
    end.

%% @doc Utility function to access a value or sub-table by path
%%
%% TOML data as returned by `parse/1' or `read_file/1' is represented as a
%% nested map. As it is a common pattern to have to access a particular entry by
%% path, this function implements the necessary loop:
%%
%% ```
%% {ok, T} = tomerl:parse("a.b.c = 1"),
%% {ok, 1} = tomerl:get(T, [a, b, c]).
%% '''
-spec get(section(), [string() | binary() | atom()]) ->
    {ok, value()} | {error, not_found}.
get(Section, [H | T]) ->
    case maps:find(ensure_binary(H), Section) of
        {ok, Subsection} when T =:= [] orelse is_map(Subsection) ->
            get(Subsection, T);
        _ ->
            {error, not_found}
    end;
get(Value, []) ->
    {ok, Value}.

ensure_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
ensure_binary(Binary) when is_binary(Binary) ->
    Binary;
ensure_binary(List) when is_list(List) ->
    list_to_binary(List).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(SIMPLE,
    "[section]\nkey = 1\n[section.sub_section]\nkey.sub_key = \"string\""
).

simple_parse_test() ->
    Expected = #{
        <<"section">> => #{
            <<"key">> => 1,
            <<"sub_section">> => #{
                <<"key">> => #{
                    <<"sub_key">> => <<"string">>
                }
            }
        }
    },

    ?assertMatch({ok, Expected}, tomerl:parse(?SIMPLE)).

getter_test() ->
    NestedMap = #{
        <<"a">> => #{
            <<"b">> => #{
                <<"c">> => 1
            },
            <<"d">> => 2
        },
        <<"e">> => 3
    },

    ?assertEqual({ok, 1}, get(NestedMap, [a, b, c])),
    ?assertEqual({ok, 1}, get(NestedMap, [a, <<"b">>, c])),
    ?assertEqual({ok, 1}, get(NestedMap, [a, b, "c"])),
    ?assertEqual({ok, 1}, get(NestedMap, [<<"a">>, <<"b">>, <<"c">>])),
    ?assertEqual({error, not_found}, get(NestedMap, [a, b, c, d])),
    ?assertEqual({error, not_found}, get(NestedMap, [a, b, d, e])),
    ?assertEqual({ok, 3}, get(NestedMap, [e])).

-endif.

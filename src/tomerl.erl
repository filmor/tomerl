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
    | [value()]
    | section().
%% Valid TOML values

-type maybe_number() ::
    number()
    | nan
    | infinity
    | negative_infinity.
%% Number type extended by nan and infinity values

%%----------------------------------------------------------
%% errors {{{

-type toml_error() ::
    {tokenize, Line :: pos_integer()}
    | {parse, Line :: pos_integer()}
    | {semantic, semantic_error()}
    | {bad_return, validate_location(), Result :: term()}.
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
    | {type_mismatch, {Pos :: pos_integer(), OffendingType :: atom(), ExpectedType :: atom()},
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
    {Path :: [string(), ...], CurLine :: pos_integer(), PrevLine :: pos_integer()}.
%% Location information of semantic error. `Path' is name of the offending
%% section and, if applicable, key.

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% parser wrappers
%%%---------------------------------------------------------------------------

%% @doc Parse a TOML file on disk
-spec read_file(file:name_all()) ->
    {ok, section()} | {error, ReadError | toml_error()}
when
    ReadError :: file:posix() | badarg | terminated | system_limit.

read_file(File) ->
    case file:read_file(File) of
        {ok, Content} -> parse(Content);
        {error, Reason} -> {error, Reason}
    end.

%% @doc Parse a TOML config from a string
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

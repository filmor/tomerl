-module(toml_test).

-export([main/1]).


main(Args) ->
    Input = list_to_binary(lists:reverse(read_all([]))),

    Args1 = case Args of [] -> ["test"]; _ -> Args end,

    case Args1 of
        ["test"] ->
            {ok, Tokens, _} = toml_lexer:tokenize(Input),
            {ok, Result} = toml_parser:parse(Tokens),
            {ok, Res} = toml_convert:do(Result),
            Json = to_json(Res),

            io:format("~s~n", [jsx:encode(Json)]);
        ["convert"] ->
            {ok, Tokens, _} = toml_lexer:tokenize(Input),
            {ok, Result} = toml_parser:parse(Tokens),
            {ok, Res} = toml_convert:do(Result),

            io:format("~p~n", [Res]);
        ["lex"] ->
            {ok, Tokens, _} = toml_lexer:tokenize(Input),
            io:format("~p~n", [Tokens]);
        ["parse"] ->
            {ok, Tokens, _} = toml_lexer:tokenize(Input),
            {ok, Result} = toml_parser:parse(Tokens),
            io:format("~p~n", [Result])
    end.


read_all(Acc) ->
    case io:get_line("") of
        eof -> Acc;
        Bin -> read_all([Bin | Acc])
    end.


to_json(Map) when is_map(Map) ->
    maps:fold(
        fun (K, V, Res) ->
            Res#{ K => to_json(V) }
        end,
        #{},
        Map
    );

to_json([]) ->
    [];

to_json([H|T]) ->
    [to_json(H) | to_json(T)];

to_json(Value) when is_binary(Value) ->
    #{ type => string, value => Value };

to_json(Value) ->
    Type = if
        is_float(Value) -> float;
        is_integer(Value) -> integer
    end,
    #{ type => Type, value => list_to_binary(io_lib:format("~p", [Value])) }.
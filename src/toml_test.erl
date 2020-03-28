-module(toml_test).

-export([main/1]).


main(Args) ->
    Input = list_to_binary(lists:reverse(read_all([]))),

    case Args of
        ["test"] ->
            {ok, Tokens, _} = toml_lexer:tokenize(Input),
            {ok, Result} = toml_parser:parse(Tokens),
            {ok, Store} = toml_dict:build_store(Result),

            F = fun (A, B, C, D) -> to_json(A, B, C, D) end,

            Json = toml_dict:fold(F, #{}, Store),

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


to_json(Path, Key, Obj, Acc) ->
    set(Path, Key, convert_value(Obj), Acc).


set([H|T], Key, Value, Acc) ->
    Current = maps:get(H, Acc, #{}),
    Acc#{ H => set(T, Key, Value, Current) };
set([], Key, Value, Acc) ->
    Acc#{ Key => Value }.


convert_value(section) ->
    #{};
convert_value({_, _} = Other) ->
    {Type, Value} = case Other of
        {string, S} ->
            {string, unicode:characters_to_binary(S, unicode, utf8)};
        {array, Vals} when is_list(Vals) ->
            {array, lists:map(fun (V) -> convert_value(V) end, Vals)};
        {X, Float} when X =:= float; X =:= integer ->
            {X, list_to_binary(io_lib:format("~p", [Float]))}
    end,
    #{ type => Type, value => Value };
convert_value(Value) ->
    Type = if
        is_binary(Value) -> string;
        is_float(Value) -> float;
        is_integer(Value) -> integer
    end,
    #{ type => Type, value => Value }.


-module(toml_test).

-export([main/1]).


main(_Args) ->
    Input = list_to_binary(lists:reverse(read_all([]))),

    {ok, Tokens, _} = toml_lexer:tokenize(Input),
    {ok, Result} = toml_parser:parse(Tokens),
    {ok, Store} = toml_dict:build_store(Result),

    F = fun (A, B, C, D) -> to_json(A, B, C, D) end,

    Json = toml_dict:fold(F, #{}, Store),

    io:format("~s~n", [jsx:encode(Json)]),

    ok.


read_all(Acc) ->
    case io:get_line("") of
        eof -> Acc;
        Bin -> read_all([Bin | Acc])
    end.


to_json(Path, Key, Obj, Acc) ->
    set(Path, Key, convert_value(Obj), Acc).


set([H|T], Key, Value, Acc) ->
    H0 = list_to_binary(H),
    Current = maps:get(H0, Acc, #{}),
    Acc#{ H0 => set(T, Key, Value, Current) };
set([], Key, Value, Acc) ->
    Acc#{ list_to_binary(Key) => Value }.


convert_value(section) ->
    #{};
convert_value(Other) ->
    {Type, Value} = case Other of
        {string, S} ->
            {string, unicode:characters_to_binary(S, unicode, utf8)};
        {array, Vals} when is_list(Vals) ->
            {array, lists:map(fun (V) -> convert_value(V) end, Vals)};
        {X, Float} when X =:= float; X =:= integer ->
            {X, list_to_binary(io_lib:format("~p", [Float]))}
    end,
    #{ type => Type, value => Value }.


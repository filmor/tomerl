-module(tomerl_test).

-export([
    main/1,
    to_json/1
]).

main(Args) ->
    Input = list_to_binary(lists:reverse(read_all([]))),

    Args1 = case Args of [] -> ["test"]; _ -> Args end,

    case Args1 of
        ["test"] ->
            {ok, Res} = tomerl:parse(Input),
            Json = to_json(Res),

            io:format("~s~n", [jsx:encode(Json)]);
        ["convert"] ->
            {ok, Res} = tomerl:parse(Input),
            io:format("~p~n", [Res]);
        ["lex"] ->
            {ok, Tokens, _} = tomerl_lexer:tokenize(Input),
            io:format("~p~n", [Tokens]);
        ["parse"] ->
            {ok, Tokens, _} = tomerl_lexer:tokenize(Input),
            {ok, Result} = tomerl_parser:parse(Tokens),
            io:format("~p~n", [Result])
    end.


read_all(Acc) ->
    case io:get_line("") of
        eof -> Acc;
        Bin -> read_all([Bin | Acc])
    end.


-spec to_json(tomerl:section()) -> jsx:term().
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
    #{ <<"type">> => <<"string">>, <<"value">> => Value };

to_json(Value) when is_boolean(Value) ->
    #{ <<"type">> => <<"bool">>, <<"value">> => atom_to_binary(Value, utf8) };

to_json(nan) ->
    #{ <<"type">> => <<"float">>, <<"value">> => <<"nan">> };

to_json(negative_nan) ->
    #{ <<"type">> => <<"float">>, <<"value">> => <<"-nan">>};

to_json(infinity) ->
    #{ <<"type">> => <<"float">>, <<"value">> => <<"inf">>};

to_json(negative_infinity) ->
    #{ <<"type">> => <<"float">>, <<"value">> => <<"-inf">>};

to_json({{Y, M, D}, {H, Mi, S}}) ->
    #{
        <<"type">> => <<"datetime-local">>,
        <<"value">> => iolist_to_binary(
            io_lib:format(
                "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B",
                [Y, M, D, H, Mi, S]
            )
        )
    };

to_json({{{Y, M, D}, {H, Mi, S}}, Tz}) ->
    #{
        <<"type">> => <<"datetime">>,
        <<"value">> => iolist_to_binary(
            io_lib:format(
                "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B~s",
                [Y, M, D, H, Mi, S, Tz]
            )
        )
    };

to_json({Y, M, D}) when Y > 24 ->
    #{
        <<"type">> => <<"date">>,
        <<"value">> => list_to_binary(
            io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])
        )
    };

to_json({H, Mi, S}) ->
    #{
        <<"type">> => <<"time">>,
        <<"value">> => list_to_binary(
            io_lib:format("~2..0B:~2..0B:~2..0B", [H, Mi, S])
        )
    };

to_json(Value) ->
    Type = if
        is_float(Value) -> <<"float">>;
        is_integer(Value) -> <<"integer">>
    end,
    #{ <<"type">> => Type, <<"value">> => iolist_to_binary(io_lib:format("~p", [Value])) }.
%% @private
-module(tomerl_test).

-export([
    main/1,
    to_json/1,
    reformat_json/1
]).

main(Args) ->
    Input = list_to_binary(lists:reverse(read_all([]))),

    Args1 = case Args of [] -> ["test"]; _ -> Args end,

    case Args1 of
        ["test"] ->
            {ok, Res} = tomerl:parse(Input),
            Json = to_json(Res),

            io:format("~s~n", [jsone:encode(Json)]);
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


reformat_json(JsonData) when is_list(JsonData) ->
    [reformat_json(E) || E <- JsonData];
reformat_json(JsonData) when is_map(JsonData) ->
    case maps:find(<<"type">>, JsonData) of
        {ok, <<"float">>} ->
            % Parse float and reformat
            case maps:get(<<"value">>, JsonData) of
                <<"+inf">> ->
                    value(float, "inf");
                E when E =:= <<"nan">>; E =:= <<"inf">>; E =:= <<"-inf">> ->
                    % unchanged
                    JsonData;
                Val ->
                    to_json(binary_to_float(Val))
            end;
        {ok, _T} ->
            JsonData;
        _ ->
            maps:map(
                fun (_K, V) -> reformat_json(V) end,
                JsonData
            )
    end;
reformat_json(JsonData) ->
    JsonData.


-spec to_json(tomerl:section()) -> map().
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
    value(string, Value);

to_json(Value) when is_boolean(Value) ->
    value(bool, atom_to_list(Value));

to_json(nan) -> value(float, "nan");
to_json(infinity) -> value(float, "inf");
to_json(negative_infinity) -> value(float, "-inf");

to_json(Value) when is_float(Value), abs(Value) >= 1.0e7 orelse abs(Value) =< 1.0e-3 ->
    value(float, "~e", [Value]);

to_json(Value) when is_float(Value) ->
    value(float, float_to_list(Value, [compact, {decimals, 10}]));

to_json(Value) when is_integer(Value) ->
    value(integer, "~p", [Value]);

to_json(Value) ->
    case tomerl_datetime:type(Value) of
        undefined ->
            error({unknown_value_type, Value});
        Else ->
            dt_value(Else, Value)
    end.


dt_value(Kind, Value) ->
    Kind1 = case Kind of
        datetime_offset -> datetime;
        datetime -> "datetime-local";
        time -> "time-local";
        date -> "date-local";
        _ -> Kind
    end,
    value(Kind1, tomerl_datetime:format(Value)).


value(Type, Value) when is_list(Type) ->
    value(list_to_binary(Type), Value);
value(Type, Value) when is_atom(Type) ->
    value(atom_to_binary(Type, utf8), Value);
value(Type, Value) ->
    #{ <<"type">> => Type, <<"value">> => iolist_to_binary(Value) }.

value(Type, Format, Args) ->
    value(Type, io_lib:format(Format, Args)).
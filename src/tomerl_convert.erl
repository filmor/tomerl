-module(tomerl_convert).

-export([
    do/1
]).

-type key() :: [binary()].

-record(state, {
    array_tables = [] :: [key()],
    tables = #{} :: #{ key() => pos_integer() },
    result = #{}
}).

-type ast() :: [
    {table, non_neg_integer(), key(), tomerl:section()} |
    {array_table, non_neg_integer(), key(), tomerl:section()}
].

%% @doc Convert the parse result to a combined single table
-spec do(ast()) -> {ok, tomerl:section()} | {error, _}.
do(Ast) ->
    try
        State = lists:foldl(
            fun do/2,
            #state{},
            Ast
        ),

        {ok, reverse_lists(State#state.result)}
    catch
        _:Reason ->
            {error, {semantic, Reason}}
    end.


do({table, Line, Key, Table}, State) ->
    % 1. Ensure that a map exists at *Key (watch out for arrays on the way!)
    %    Arrays should be easy enough to traverse: The respective key must be in
    %    array_tables, then just pass through the last entry (or better: first
    %    and revert at the end?)
    % 2. Set current_table
    Tables = State#state.tables,
    
    % Check if redefined
    State1 =
    case maps:find(Key, Tables) of
        {ok, OtherLine} ->
            error({redefining_table, OtherLine, Line});
        _ ->
            State#state{tables=Tables#{ Key => Line }}
    end,

    do_set(table, Key, Table, State1);

do({array_table, _Line, Key, Table}, State) ->
    % 1. Ensure that *Key maps to an array
    % 2. Append a new empty object to the array
    % 3. Add Key to the currently known "array_tables"
    % 4. Set Key as current_table
    AT = ordsets:add_element(lists:reverse(Key), State#state.array_tables),

    KeysToDrop = [K || K <- maps:keys(State#state.tables), lists:prefix(Key, K)],

    T = maps:without(KeysToDrop, State#state.tables),

    State1 = do_set(array, Key, Table, State),
    State1#state{array_tables=AT, tables=T};

do(Entry, State) ->
    error({Entry, State}).


do_set(Type, Key, Value, State) ->
    New = do_set(
        Type, [], Key, State#state.result, Value, State#state.array_tables
    ),
    State#state{result = New}.

do_set(array, Current, [], List, Value, ArrayTables) when is_list(List) ->
    case ordsets:is_element(Current, ArrayTables) of
        true ->
            [Value | List];
        false ->
            error({fixed_array, lists:reverse(Current)})
    end;

do_set(table, _Current, [], Map, Value, _ArrayTables) when is_map(Map) ->
    recursive_merge(Map, Value);

do_set(Type, Current, Key, [HList|TList], Value, ArrayTables) ->
    case ordsets:is_element(Current, ArrayTables) of
        true ->
            [do_set(Type, Current, Key, HList, Value, ArrayTables) | TList];
        false ->
            error({fixed_array, Key})
    end;

do_set(Type, Current, [H|Tail], Map, Value, ArrayTables) when is_map(Map) ->
    case maps:find(H, Map) of
        {ok, Next} ->
            Map#{ H => do_set(Type, [H|Current], Tail, Next, Value, ArrayTables)};
        _ ->
            Value1 = case Type of array -> [Value]; table -> Value end,
            Map#{ H => new(Tail, Value1)}
    end.


new(L, Value) ->
    lists:foldr(
        fun (Comp, Res) ->
            #{Comp => Res}
        end,
        Value,
        L
    ).


recursive_merge(Map1, Map2) ->
    maps:fold(
        fun (K, V, Res) ->
            maps:update_with(
                K,
                fun (OldV) when is_map(OldV), is_map(V) ->
                        recursive_merge(OldV, V);
                    (OldV) ->
                        error({overwriting, OldV, V})
                end,
                V,
                Res
            )
        end,
        Map1,
        Map2
    ).


reverse_lists(Map) when is_map(Map) ->
    maps:map(fun (_K, V) -> reverse_lists(V) end, Map);
reverse_lists(List) when is_list(List) ->
    lists:foldl(fun (V, Acc) -> [reverse_lists(V) | Acc] end, [], List);
reverse_lists(V) ->
    V.
-module(toml_convert).

-export([
    do/1
]).

-type key() :: [binary()].

-record(state, {
    array_tables = [] :: [key()],
    tables = #{} :: #{ key() => pos_integer() },
    result = #{}
}).


do(Ast) ->
    State = lists:foldl(
        fun do/2,
        #state{},
        Ast
    ),

    State1 = flush(State),

    {ok, State1#state.result}.


do({table, Line, Key, Table}, State) ->
    % 1. Ensure that a map exists at *Key (watch out for arrays on the way!)
    %    Arrays should be easy enough to traverse: The respective key must be in
    %    array_tables, then just pass through the last entry (or better: first
    %    and revert at the end?)
    % 2. Set current_table
    Tables = State#state.tables,
    State1 =
    case maps:find(Key, Tables) of
        {ok, OtherLine} ->
            error({redefining_table, OtherLine, Line});
        _ ->
            State#state{tables=Tables#{ Key => Line }}
    end,

    do_set(Key, Table, State1);

do({array_table, _Line, Key, Table}, State) ->
    % 1. Ensure that *Key maps to an array
    % 2. Append a new empty object to the array
    % 3. Add Key to the currently known "array_tables"
    % 4. Set Key as current_table
    AT = ordsets:add_element(Key, State#state.array_tables),
    State#state{array_tables=AT};

do(Entry, State) ->
    error({Entry, State}).


do_set(Key, Value, State) ->
    New = do_set([], Key, State#state.result, Value, State#state.array_tables),
    State#state{result = New}.


do_set(Current, [H|Tail], Map, Value, ArrayTables) ->
    case maps:find(H, Map) of
        {ok, Next} when is_map(Next) ->
            Map#{ H => do_set([H|Current], Tail, Next, Value, ArrayTables)};
        {ok, _Next} ->
            error({not_supported, _Next, Value});
        _ ->
            Map#{ H => new(Tail, Value)}
    end;

do_set(_Current, [Key], Map, Value, _ArrayTables) when is_map(Map) ->
    Map#{ Key => Value };

do_set(_Current, [], Map, Value, _ArrayTables) when is_map(Map) ->
    Value;

do_set(Current, [], _Other, _Value, _ArrayTables) ->
    error({value_set, lists:reverse(Current), _Other}).


new(L, Value) ->
    lists:foldr(
        fun (Comp, Res) ->
            #{Comp => Res}
        end,
        Value,
        L
    ).

flush(State) ->
    State.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

set_test() ->
    X = do_set([<<"a">>, <<"b">>, <<"c">>], #{}, value),
    ?assertMatch(X, #{<<"a">> => #{<<"b">> => #{<<"c">> => value}}}).

overwrite_test() ->
    ?assertError(
        _,
        do_set([<<"a">>, <<"b">>], #{<<"a">> => #{<<"b">> => some_value}}, other_value)
    ).

-endif.
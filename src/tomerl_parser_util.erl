-module(tomerl_parser_util).

-export([
    to_binary/1,
    new_section/0,
    section_to_map/1,
    update_section/2,
    new_section/1
]).

-record(section, {
    map = #{}
}).

new_section() ->
    #section{}.

new_section(Entry) ->
    update_section(Entry, new_section()).

section_to_map(Map) when is_map(Map) ->
    Map;
section_to_map(#section{map = Map}) ->
    Map;
section_to_map(Value) ->
    Value.

update_section({key_value, [H], {Value, _Line}}, Map) ->
    case maps:is_key(H, Map) of
        true ->
            % TODO: Show line number in error
            error(overwritten);
        _ ->
            Map#{H => Value}
    end;
update_section({key_value, [H | T], Value}, Map) ->
    Map#{H => update_section({key_value, T, Value}, maps:get(H, Map, #{}))}.

to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom);
to_binary(List) ->
    case unicode:characters_to_binary(List) of
        {error, _Parsed, _RestData} ->
            % TODO: Show line number in error
            error(invalid_string);
        Res when is_binary(Res) ->
            Res
    end.

-module(compliance_tests).
-include_lib("eunit/include/eunit.hrl").

-spec discover(file:name_all()) -> [{Name :: list(), File :: list(), Json :: list()}].
discover(Directory) ->
    Files0 = filelib:wildcard("*/*.toml", Directory),
    Files = filelib:wildcard("*.toml", Directory) ++ Files0,

    lists:map(
        fun(Filename) ->
            Root = filename:rootname(Filename),
            Filename1 = filename:join(Directory, Filename),
            Json = lists:flatten([filename:rootname(Filename1), ".json"]),
            {Root, Filename1, Json}
        end,
        Files
    ).

valid_test_() ->
    [
        {
            Name,
            fun() ->
                {ok, JsonFile} = file:read_file(Json),
                JsonData = jsone:decode(JsonFile),
                JsonData1 = tomerl_test:reformat_json(JsonData),
                {ok, TomlData} = tomerl:read_file(Toml),
                ConvertedToml = tomerl_test:to_json(TomlData),
                ?assertEqual(JsonData1, ConvertedToml)
            end
        }
     || {Name, Toml, Json} <- discover("test/toml-test/tests/valid")
    ].

invalid_test_() ->
    [
        {
            Name,
            fun() ->
                ?assertNotMatch({ok, _}, tomerl:read_file(Toml))
            end
        }
     || {Name, Toml, _} <- discover("test/toml-test/tests/invalid"),
        not skip_invalid(Name)
    ].

skip_invalid(RootName) ->
    case lists:flatten(string:replace(RootName, "\\", "/")) of
        % Discussion: https://github.com/toml-lang/toml/issues/846
        % We might implement rejecting these, but apart from sticking to
        % the spec I don't really see the point.
        "inline-table/add" -> true;
        "table/append-with-dotted-keys-1" -> true;
        "table/append-with-dotted-keys-2" -> true;
        "table/duplicate-key-dotted-table" -> true;
        "table/duplicate-key-dotted-table2" -> true;
        _ -> false
    end.

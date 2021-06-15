-module(compliance_tests).
-include_lib("eunit/include/eunit.hrl").

-spec discover(file:name_all()) -> [{Name::list(), File::list(), Json::list()}].
discover(Directory) ->
    Files = filelib:wildcard(lists:flatten([Directory, "/*.toml"])),

    lists:map(
        fun (Filename) ->
            Root = filename:rootname(filename:basename(Filename)),
            Json = lists:flatten([filename:rootname(Filename), ".json"]),
            {Root, Filename, Json}
        end,
        Files
    ).

valid_test_() ->
    [
        {
            Name,
            fun () ->
                {ok, JsonFile} = file:read_file(Json),
                JsonData = jsone:decode(JsonFile),
                JsonData1 = tomerl_test:reformat_json(JsonData),
                {ok, TomlData} = tomerl:read_file(Toml),
                ConvertedToml = tomerl_test:to_json(TomlData),
                ?assertEqual(JsonData1, ConvertedToml)
            end
        }
        ||
        {Name, Toml, Json} <- discover("test/toml-test/tests/valid")
    ].

invalid_test_() ->
    [
        {
            Name,
            fun () ->
                ?assertNotMatch({ok, _}, tomerl:read_file(Toml))
            end
        }
        ||
        {Name, Toml, _} <- discover("test/toml-test/tests/invalid"),
        % Skip mixed array type rejection, not implemented here
        string:prefix(Name, <<"array-mixed-types-">>) =:= nomatch
    ].

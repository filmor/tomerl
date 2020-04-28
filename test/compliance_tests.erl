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
                JsonData = jsx:decode(JsonFile, [return_maps]),
                {ok, TomlData} = tomerl:read_file(Toml),
                ConvertedToml = tomerl_test:to_json(TomlData),
                ?assertEqual(JsonData, ConvertedToml)
            end 
        }
        ||
        {Name, Toml, Json} <- discover("test/test_suite/valid")
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
        {Name, Toml, _} <- discover("test/test_suite/invalid")
    ].
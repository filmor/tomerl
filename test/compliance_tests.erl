-module(compliance_tests).
-include_lib("eunit/include/eunit.hrl").

-define(VERSION, "1.1.0").

-record(test, {
    root :: string(),
    filename :: string(),
    json :: string(),
    kind :: valid | invalid
}).

-spec discover() -> [#test{}].
discover() ->
    Directory = "test/toml-test/tests",
    {ok, Files0} = file:read_file(Directory ++ "/files-toml-" ++ ?VERSION),
    Files = string:split( Files0, "\n", all),

    lists:filtermap(
        fun
            (<<>>) ->
                false;
            (Filename0) ->
                Filename = binary_to_list(Filename0),
                Root = filename:rootname(Filename),
                Filename1 = filename:join(Directory, Filename),
                Json = lists:flatten([filename:rootname(Filename1), ".json"]),
                {true, #test{
                    root = Root,
                    filename = Filename1,
                    json = Json,
                    kind =
                        case string:prefix(Filename, "valid/") of
                            nomatch ->
                                invalid;
                            _ ->
                                valid
                        end
                }}
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
     || #test{root = Name, filename = Toml, json = Json, kind = valid} <- discover()
    ].

invalid_test_() ->
    [
        {
            Name,
            fun() ->
                ?assertNotMatch({ok, _}, tomerl:read_file(Toml))
            end
        }
     || #test{root = Name, filename = Toml, kind = invalid} <- discover(),
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

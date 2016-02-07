-module(load_utils_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SOURCE, "ebin\\src").

read_erlang_term_test_() ->
    [success_read_erlang_term_entry("{a, 4}.", "read {a, 4}", {a, 4}),
     success_read_erlang_term_entry("[1, 2, 3].", "read [1, 2, 3]", [1, 2, 3]),
     failed_read_erlang_term_entry("a", "read without final dot"),
     failed_read_erlang_term_entry("{a, 4}.abcd.", "read with rest on same line"),
     success_read_erlang_term_entry("{a, 4}.\nabcd.", "read with rest on separate line", {a, 4})].

read_strings_test_() ->
    [success_read_strings_entry("", "read empty source", []),
     success_read_strings_entry("aaa", "read single line source", ["aaa"]),
     success_read_strings_entry("aaa\nbbb", "read two line source", ["aaa", "bbb"]),
     success_read_strings_entry("aaa\nbbb\nccc", "read three line source", ["aaa", "bbb", "ccc"]),
     success_read_strings_entry("aaa\n\nccc", "read source with empty line", ["aaa", "", "ccc"])].

read_number_table_test_() ->
    [success_read_number_table_entry("", "read empty source", []),
     success_read_number_table_entry("1 2 3", "read single line source", [[1, 2, 3]]),
     success_read_number_table_entry("1 2 3\n4 5", "read two line source", [[1, 2, 3], [4, 5]]),
     success_read_number_table_entry("1 2 3\n4 5\n666", "read three line source", [[1, 2, 3], [4, 5], [666]]),
     success_read_number_table_entry("1 2 3\n\n666", "read source with empty line", [[1, 2, 3], [], [666]]),
     failed_read_number_table_entry("1 aaa 2", "read source with noninteger item")].

read_from_file_test_() ->
    [success_read_from_file_entry("", "read empty source", 0),
     success_read_from_file_entry("abc", "read nonempty source", 3)].

prepare_source(SourceData) ->
    AbsFilename = filename:absname(?SOURCE),
    ok = file:write_file(AbsFilename, SourceData).

delete_source(_State) ->
    AbsFilename = filename:absname(?SOURCE),
    ok = file:delete(AbsFilename).

success_read_erlang_term_entry(Source, Description, Expected) ->
    {setup, fun() -> prepare_source(Source) end, fun delete_source/1, [{Description, ?_assertEqual(Expected, load_utils:read_erlang_term(?SOURCE))}]}.

failed_read_erlang_term_entry(Source, Description) ->
    {setup, fun() -> prepare_source(Source) end, fun delete_source/1, [{Description, ?_assertError({badmatch, _}, load_utils:read_erlang_term(?SOURCE))}]}.

success_read_strings_entry(Source, Description, Expected) ->
    {setup, fun() -> prepare_source(Source) end, fun delete_source/1, [{Description, ?_assertEqual(Expected, load_utils:read_strings(?SOURCE))}]}.

success_read_number_table_entry(Source, Description, Expected) ->
    {setup, fun() -> prepare_source(Source) end, fun delete_source/1, [{Description, ?_assertEqual(Expected, load_utils:read_number_table(?SOURCE, " "))}]}.

failed_read_number_table_entry(Source, Description) ->
    {setup, fun() -> prepare_source(Source) end, fun delete_source/1, [{Description, ?_assertError(badarg, load_utils:read_number_table(?SOURCE, " "))}]}.

success_read_from_file_entry(Source, Description, Expected) ->
    Collector = fun CollectFun(IoDevice, Count) ->
        case file:read(IoDevice, 1) of
            {ok, _Data} -> CollectFun(IoDevice, Count + 1);
            eof -> Count
        end
    end,
    Loader = fun(IoDevice) -> Collector(IoDevice, 0) end,
    {setup, fun() -> prepare_source(Source) end, fun delete_source/1, [{Description, ?_assertEqual(Expected, load_utils:read_from_file(?SOURCE, Loader))}]}.
%% @author std-string

-module(load_utils).

-export([read_erlang_term/1, read_strings/1, read_number_table/2, read_from_file/2]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec read_erlang_term(Filename :: string()) -> term().
read_erlang_term(Filename) ->
    Loader = fun(IoDevice) -> {ok, Term} = io:read(IoDevice, ""), Term end,
    read_from_file(Filename, Loader).

-spec read_strings(Filename :: string()) -> [string()].
read_strings(Filename) ->
    Loader = fun(IoDevice) -> lists:reverse(read_string_list(IoDevice, [])) end,
    read_from_file(Filename, Loader).

-spec read_number_table(Filename :: string(), NumberDelimiter :: string()) -> [[integer()]].
read_number_table(Filename, NumberDelimiter) ->
    StringList = read_strings(Filename),
    lists:map(fun(String) -> lists:map(fun list_to_integer/1, string:tokens(String, NumberDelimiter)) end, StringList).

-spec read_from_file(Filename :: string(), Loader :: fun((file:io_device()) -> term())) -> term().
read_from_file(Filename, Loader) ->
    read_from_file_impl(file:open(Filename, [read]), Loader).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec read_from_file_impl(OpenResult :: {'ok', IoDevice :: file:io_device()} | {'error', Reason :: term()},
                          Loader :: fun((file:io_device()) -> term())) -> term().
read_from_file_impl({ok, IoDevice}, Loader) ->
    try Loader(IoDevice)
    after
        file:close(IoDevice)
    end;
read_from_file_impl({error, Reason}, _Loader) -> erlang:error(io_error, [Reason]).

-spec read_string_list(IoDevice :: file:io_device(), StringList :: [string()]) -> [string()].
read_string_list(IoDevice, StringList) ->
    Result = io:get_line(IoDevice, ""),
    case Result of
        eof -> StringList;
        {error, Reason} -> erlang:error(io_error, [Reason]);
        Result -> read_string_list(IoDevice, [string:strip(Result, right, $\n)] ++ StringList)
    end.
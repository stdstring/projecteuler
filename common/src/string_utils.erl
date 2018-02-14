%% @author std-string

-module(string_utils).

-export([format/2]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec format(FormatString :: string(), Args :: [term()]) -> string().
format(FormatString, Args) -> lists:flatten(io_lib:format(FormatString, Args)).

%% ====================================================================
%% Internal functions
%% ====================================================================

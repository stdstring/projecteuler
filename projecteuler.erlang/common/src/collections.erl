%% @author std-string

-module(collections).
-export([get_all_circular_shift/1, get_circular_shift/1]).

%% ====================================================================
%% API functions
%% ====================================================================

%% get all circularly shifted lists from original
-spec get_all_circular_shift(Source :: list()) -> [list()].
get_all_circular_shift([]) -> [[]];
get_all_circular_shift(Source) ->
    get_all_circular_shift_impl(Source, Source, []).

%% create circularly shifted list from original
-spec get_circular_shift(Source :: list()) -> list().
get_circular_shift([]) -> [];
get_circular_shift([Head | Tail]) -> Tail ++ [Head].

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec get_all_circular_shift_impl(Source :: list(), Current :: list(), Dest :: [list()]) -> [list()].
get_all_circular_shift_impl(Source, Source, []) ->
    [Head | Tail] = Source,
    get_all_circular_shift_impl(Source, get_circular_shift([Head | Tail]), [Source]);
get_all_circular_shift_impl(Source, Source, Dest) -> lists:reverse(Dest);
get_all_circular_shift_impl(Source, Current, Dest) ->
    [Head | Tail] = Current,
    get_all_circular_shift_impl(Source, get_circular_shift([Head | Tail]), [Current] ++ Dest).
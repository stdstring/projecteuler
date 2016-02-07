-module(collections).
-export([get_all_circular_shift/1, get_circular_shift/1]).

%% get all circularly shifted lists from original
-spec get_all_circular_shift(Source :: list()) -> [list()].
get_all_circular_shift([]) -> [[]];
get_all_circular_shift(Source) ->
    get_all_circular_shift(Source, Source, []).

%% create circularly shifted list from original
-spec get_circular_shift(Source :: list()) -> list().
get_circular_shift([]) -> [];
get_circular_shift([Head | Tail]) -> Tail ++ [Head].

-spec get_all_circular_shift(Source :: list(), Current :: list(), Dest :: [list()]) -> [list()].
get_all_circular_shift(Source, Source, []) ->
    [Head | Tail] = Source,
    get_all_circular_shift(Source, get_circular_shift([Head | Tail]), [Source]);
get_all_circular_shift(Source, Source, Dest) -> lists:reverse(Dest);
get_all_circular_shift(Source, Current, Dest) ->
    [Head | Tail] = Current,
    get_all_circular_shift(Source, get_circular_shift([Head | Tail]), [Current] ++ Dest).
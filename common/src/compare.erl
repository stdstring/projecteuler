%% @author std-string

-module(compare).
-export([compare_asc/2, compare_desc/2]).

-type compare_result() :: 'left' | 'equal' | 'right'.

%% ====================================================================
%% API functions
%% ====================================================================

-spec compare_asc(LValue :: term(), RValue :: term()) -> compare_result().
compare_asc(LValue, RValue) ->
    if
        LValue < RValue -> left;
        LValue == RValue -> equal;
        LValue > RValue -> right
    end.

-spec compare_desc(LValue :: term(), RValue :: term()) -> compare_result().
compare_desc(LValue, RValue) ->
    if
        LValue > RValue -> left;
        LValue == RValue -> equal;
        LValue < RValue -> right
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
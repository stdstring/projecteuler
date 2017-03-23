-module(mutable_array_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

single_array_test() ->
    ?assertEqual(ok, safe_init()),
    ?assertError(badarg, mutable_array:create(0, <<>>)),
    ?assertError(badarg, mutable_array:create(-1, <<>>)),
    ?assertError(badarg, mutable_array:create(2, 666)).

%% ====================================================================
%% Internal functions
%% ====================================================================

safe_init() ->
    case mutable_array:is_loaded() of
        false -> mutable_array:init();
        true -> ok
    end.
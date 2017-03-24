-module(mutable_array_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

create_badarg_test_() ->
    ?assertEqual(ok, safe_init()),
    [{"zero size", ?_assertError(badarg, mutable_array:create(0, <<>>))},
     {"negative size", ?_assertError(badarg, mutable_array:create(-1, <<>>))},
     {"non binary default value", ?_assertError(badarg, mutable_array:create(2, 666))}].

get_test_() ->
    ?assertEqual(ok, safe_init()),
    Array = mutable_array:create(6, <<13>>),
    mutable_array:set(Array, 1, <<44>>),
    mutable_array:set(Array, 4, <<33>>),
    [{"read value from cell with index 0", ?_assertEqual(<<13>>, mutable_array:get(Array, 0))},
     {"read value from cell with index 1", ?_assertEqual(<<44>>, mutable_array:get(Array, 1))},
     {"read value from cell with index 2", ?_assertEqual(<<13>>, mutable_array:get(Array, 2))},
     {"read value from cell with index 3", ?_assertEqual(<<13>>, mutable_array:get(Array, 3))},
     {"read value from cell with index 4", ?_assertEqual(<<33>>, mutable_array:get(Array, 4))},
     {"read value from cell with index 5", ?_assertEqual(<<13>>, mutable_array:get(Array, 5))},
     {"read value from cell with negative index", ?_assertError(badarg, mutable_array:get(Array, -1))},
     {"read value from cell with out of range index", ?_assertError(badarg, mutable_array:get(Array, 6))}].

set_test() ->
    ?assertEqual(ok, safe_init()),
    Array = mutable_array:create(6, <<13>>),
    ?assertEqual(<<13>>, mutable_array:get(Array, 1)),
    mutable_array:set(Array, 1, <<44>>),
    ?assertEqual(<<44>>, mutable_array:get(Array, 1)),
    mutable_array:set(Array, 4, <<33>>),
    ?assertEqual(<<44>>, mutable_array:get(Array, 1)),
    mutable_array:set(Array, 1, <<55>>),
    ?assertEqual(<<55>>, mutable_array:get(Array, 1)),
    ?assertEqual(<<33>>, mutable_array:get(Array, 4)),
    ?assertEqual(<<13>>, mutable_array:get(Array, 0)),
    ?assertEqual(<<13>>, mutable_array:get(Array, 2)),
    ?assertEqual(<<13>>, mutable_array:get(Array, 3)),
    ?assertEqual(<<13>>, mutable_array:get(Array, 5)).

several_array_test_()->
    ?assertEqual(ok, safe_init()),
    FirstArray = mutable_array:create(4, <<5>>),
    SecondArray = mutable_array:create(4, <<66>>),
    mutable_array:set(FirstArray, 1, <<99>>),
    mutable_array:set(SecondArray, 2, <<77>>),
    [{"read value from first array from cell with index 0", ?_assertEqual(<<5>>, mutable_array:get(FirstArray, 0))},
     {"read value from first array from cell with index 1", ?_assertEqual(<<99>>, mutable_array:get(FirstArray, 1))},
     {"read value from first array from cell with index 2", ?_assertEqual(<<5>>, mutable_array:get(FirstArray, 2))},
     {"read value from first array from cell with index 3", ?_assertEqual(<<5>>, mutable_array:get(FirstArray, 3))},
     {"read value from second array from cell with index 0", ?_assertEqual(<<66>>, mutable_array:get(SecondArray, 0))},
     {"read value from second array from cell with index 1", ?_assertEqual(<<66>>, mutable_array:get(SecondArray, 1))},
     {"read value from second array from cell with index 2", ?_assertEqual(<<77>>, mutable_array:get(SecondArray, 2))},
     {"read value from second array from cell with index 3", ?_assertEqual(<<66>>, mutable_array:get(SecondArray, 3))}].

%% ====================================================================
%% Internal functions
%% ====================================================================

%% TODO (std_string) : probably move into common place
safe_init() ->
    case mutable_array:is_loaded() of
        false -> mutable_array:init();
        true -> ok
    end.
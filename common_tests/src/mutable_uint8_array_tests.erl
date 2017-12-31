%% @author std-string

-module(mutable_uint8_array_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

create_badarg_test_() ->
    ?assertEqual(ok, mutable_uint8_array:init()),
    [{"zero size", ?_assertError(badarg, mutable_uint8_array:create(0, 1))},
     {"negative size", ?_assertError(badarg, mutable_uint8_array:create(-1, 1))},
     {"non integer default value", ?_assertError(badarg, mutable_uint8_array:create(2, iddqd))}].

get_test_() ->
    ?assertEqual(ok, mutable_uint8_array:init()),
    Array = mutable_uint8_array:create(6, 13),
    mutable_uint8_array:set(1, 44, Array),
    mutable_uint8_array:set(4, 33, Array),
    [{"read value from cell with index 0", ?_assertEqual(13, mutable_uint8_array:get(0, Array))},
     {"read value from cell with index 1", ?_assertEqual(44, mutable_uint8_array:get(1, Array))},
     {"read value from cell with index 2", ?_assertEqual(13, mutable_uint8_array:get(2, Array))},
     {"read value from cell with index 3", ?_assertEqual(13, mutable_uint8_array:get(3, Array))},
     {"read value from cell with index 4", ?_assertEqual(33, mutable_uint8_array:get(4, Array))},
     {"read value from cell with index 5", ?_assertEqual(13, mutable_uint8_array:get(5, Array))},
     {"read value from cell with negative index", ?_assertError(badarg, mutable_uint8_array:get(-1, Array))},
     {"read value from cell with out of range index", ?_assertError(badarg, mutable_uint8_array:get(6, Array))}].

set_test() ->
    ?assertEqual(ok, mutable_uint8_array:init()),
    Array = mutable_uint8_array:create(6, 13),
    ?assertEqual(13, mutable_uint8_array:get(1, Array)),
    mutable_uint8_array:set(1, 44, Array),
    ?assertEqual(44, mutable_uint8_array:get(1, Array)),
    mutable_uint8_array:set(4, 33, Array),
    ?assertEqual(44, mutable_uint8_array:get(1, Array)),
    mutable_uint8_array:set(1, 55, Array),
    ?assertEqual(55, mutable_uint8_array:get(1, Array)),
    ?assertEqual(33, mutable_uint8_array:get(4, Array)),
    ?assertEqual(13, mutable_uint8_array:get(0, Array)),
    ?assertEqual(13, mutable_uint8_array:get(2, Array)),
    ?assertEqual(13, mutable_uint8_array:get(3, Array)),
    ?assertEqual(13, mutable_uint8_array:get(5, Array)).

several_array_test_()->
    ?assertEqual(ok, mutable_uint8_array:init()),
    FirstArray = mutable_uint8_array:create(4, 5),
    SecondArray = mutable_uint8_array:create(4, 66),
    mutable_uint8_array:set(1, 99, FirstArray),
    mutable_uint8_array:set(2, 77, SecondArray),
    [{"read value from first array from cell with index 0", ?_assertEqual(5, mutable_uint8_array:get(0, FirstArray))},
     {"read value from first array from cell with index 1", ?_assertEqual(99, mutable_uint8_array:get(1, FirstArray))},
     {"read value from first array from cell with index 2", ?_assertEqual(5, mutable_uint8_array:get(2, FirstArray))},
     {"read value from first array from cell with index 3", ?_assertEqual(5, mutable_uint8_array:get(3, FirstArray))},
     {"read value from second array from cell with index 0", ?_assertEqual(66, mutable_uint8_array:get(0, SecondArray))},
     {"read value from second array from cell with index 1", ?_assertEqual(66, mutable_uint8_array:get(1, SecondArray))},
     {"read value from second array from cell with index 2", ?_assertEqual(77, mutable_uint8_array:get(2, SecondArray))},
     {"read value from second array from cell with index 3", ?_assertEqual(66, mutable_uint8_array:get(3, SecondArray))}].

%% ====================================================================
%% Internal functions
%% ====================================================================
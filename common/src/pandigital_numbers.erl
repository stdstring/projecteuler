%% @author std-string

-module(pandigital_numbers).
-export([is_pandigital/1]).

-include("pandigital_def.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

-spec is_pandigital(Number :: pos_integer()) -> boolean().
is_pandigital(Number) ->
    if
        (?PANDIGITAL2_MIN =< Number) and (Number =< ?PANDIGITAL2_MAX) -> is_pandigital_impl(Number, 0, 2#110);
        (?PANDIGITAL3_MIN =< Number) and (Number =< ?PANDIGITAL3_MAX) -> is_pandigital_impl(Number, 0, 2#1110);
        (?PANDIGITAL4_MIN =< Number) and (Number =< ?PANDIGITAL4_MAX) -> is_pandigital_impl(Number, 0, 2#11110);
        (?PANDIGITAL5_MIN =< Number) and (Number =< ?PANDIGITAL5_MAX) -> is_pandigital_impl(Number, 0, 2#111110);
        (?PANDIGITAL6_MIN =< Number) and (Number =< ?PANDIGITAL6_MAX) -> is_pandigital_impl(Number, 0, 2#1111110);
        (?PANDIGITAL7_MIN =< Number) and (Number =< ?PANDIGITAL7_MAX) -> is_pandigital_impl(Number, 0, 2#11111110);
        (?PANDIGITAL8_MIN =< Number) and (Number =< ?PANDIGITAL8_MAX) -> is_pandigital_impl(Number, 0, 2#111111110);
        (?PANDIGITAL9_MIN =< Number) and (Number =< ?PANDIGITAL9_MAX) -> is_pandigital_impl(Number, 0, 2#1111111110);
        (?PANDIGITAL10_MIN =< Number) and (Number =< ?PANDIGITAL10_MAX) -> is_pandigital_impl(Number, 0, 2#1111111111);
        true -> false
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec is_pandigital_impl(Number :: non_neg_integer(), Result :: non_neg_integer(), ExpectedValue :: pos_integer()) -> boolean().
is_pandigital_impl(0, ExpectedValue, ExpectedValue) -> true;
is_pandigital_impl(0, _Result, _ExpectedValue) -> false;
is_pandigital_impl(Number, Result, ExpectedValue) ->
    is_pandigital_impl(Number div 10, Result bor (1 bsl (Number rem 10)), ExpectedValue).
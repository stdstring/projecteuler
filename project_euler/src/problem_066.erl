%% @author std-string

%% Consider quadratic Diophantine equations of the form:
%% x^2 – D * y^2 = 1
%% For example, when D = 13, the minimal solution in x is 649^2 – 13 * 180^2 = 1.
%% It can be assumed that there are no solutions in positive integers when D is square.
%% By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:
%% 3^2 – 2 * 2^2 = 1
%% 2^2 – 3 * 1^2 = 1
%% 9^2 – 5 * 4^2 = 1
%% 5^2 – 6 * 2^2 = 1
%% 8^2 – 7 * 3^2 = 1
%% Hence, by considering minimal solutions in x for D <= 7, the largest x is obtained when D = 5.
%% Find the value of D <= 1000 in minimal solutions of x for which the largest value of x is obtained.

-module(problem_066).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(NUMBER_START, 2).

%% ====================================================================
%% API functions
%% ====================================================================

get_check_data() ->
    [{7, 5}, {1000, 661}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(MaxNumber) ->
    process_numbers(?NUMBER_START, MaxNumber, {0, 0}).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_numbers(Number :: integer(), MaxNumber :: integer(), SavedData :: {SavedNumber :: integer(), SavedMaxX :: integer()}) -> integer().
process_numbers(Number, MaxNumber, {SavedNumber, _SavedMaxX}) when Number > MaxNumber -> SavedNumber;
process_numbers(Number, MaxNumber, {SavedNumber, SavedMaxX}) ->
    case pell_equation:find_first_solution(Number, 1) of
        undef -> process_numbers(Number + 1, MaxNumber, {SavedNumber, SavedMaxX});
        {X, _Y} when X > SavedMaxX -> process_numbers(Number + 1, MaxNumber, {Number, X});
        {_X, _Y} -> process_numbers(Number + 1, MaxNumber, {SavedNumber, SavedMaxX})
    end.
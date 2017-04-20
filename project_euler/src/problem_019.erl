%% @author std-string

%% You are given the following information, but you may prefer to do some research for yourself.
%% 1 Jan 1900 was a Monday. Thirty days has September, April, June and November.
%% All the rest have thirty-one, Saving February alone, which has twenty-eight, rain or shine. And on leap years, twenty-nine.
%% A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
%% How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

-module(problem_019).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(USUAL_YEAR, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]).
-define(LEAP_YEAR, [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]).

-type process_result() :: {DayNumber :: pos_integer(), Count :: non_neg_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() ->
    [{none, 171}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(none) ->
    %% 1900 is not the leap year
    %% 1 Jan 1900 - Monday => InitDayNumber = 1
    InitDayNumber = 1,
    StartDayNumber = (InitDayNumber + 365) rem 7,
    {_DayNumber, Count} = correct_last_sunday(solve_impl(1901, 2000, {StartDayNumber, 0})),
    Count.

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec solve_impl(CurrentYear :: pos_integer(), FinishYear :: pos_integer(), Result :: process_result()) -> process_result().
solve_impl(CurrentYear, FinishYear, Result) when CurrentYear > FinishYear -> Result;
solve_impl(CurrentYear, FinishYear, Result) ->
    Year = case is_leap_year(CurrentYear) of
        true -> ?LEAP_YEAR;
        false -> ?USUAL_YEAR
    end,
    solve_impl(CurrentYear + 1, FinishYear, process_year(Year, Result)).

-spec process_year(MonthsList :: [pos_integer()], Result :: process_result()) -> process_result().
process_year([], Result) -> Result;
process_year([Month | Rest], {DayNumber, Count}) ->
    NewDayNumber = (DayNumber + Month) rem 7,
    case NewDayNumber of
        0 ->  process_year(Rest, {NewDayNumber, Count + 1});
        _Other -> process_year(Rest, {NewDayNumber, Count})
    end.

-spec is_leap_year(Year :: pos_integer()) -> boolean().
is_leap_year(Year) ->
    (Year rem 4 == 0) and ((Year rem 100 /= 0) or (Year rem 400 == 0)).

-spec correct_last_sunday(Result :: process_result()) -> process_result().
correct_last_sunday({0, Count}) -> {0, Count - 1};
correct_last_sunday(Result) -> Result.
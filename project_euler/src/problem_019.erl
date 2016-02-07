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

get_check_data() ->
    [{none, 171}].

prepare_data(_ModuleSourceDir, Input) -> Input.

solve(none) ->
    %% 1900 is not the leap year
    %% 1 Jan 1900 - Monday => InitRem = 1
    InitRem = 1,
    StartRem = (InitRem + 365) rem 7,
    {Rem, Count} = solve_impl(1901, 2000, StartRem, 0),
    correct_last_sunday(Rem, Count).

solve_impl(CurrentYear, FinishYear, CurrentRem, Count) when CurrentYear > FinishYear -> {CurrentRem, Count};
solve_impl(CurrentYear, FinishYear, CurrentRem, Count) ->
    Year = case is_leap_year(CurrentYear) of
        true -> ?LEAP_YEAR;
        false -> ?USUAL_YEAR
    end,
    {NewRem, NewCount} = process_year(Year, CurrentRem, Count),
    solve_impl(CurrentYear + 1, FinishYear, NewRem, NewCount).

process_year([], Rem, Count) -> {Rem, Count};
process_year([Month | Rest], Rem, Count) ->
    NewRem = (Rem + Month) rem 7,
    if
        NewRem == 0 -> process_year(Rest, NewRem, Count + 1);
        NewRem /= 0 -> process_year(Rest, NewRem, Count)
    end.

is_leap_year(Year) ->
    (Year rem 4 == 0) and ((Year rem 100 /= 0) or (Year rem 400 == 0)).

correct_last_sunday(0, Count) -> Count - 1;
correct_last_sunday(_Rem, Count) -> Count.
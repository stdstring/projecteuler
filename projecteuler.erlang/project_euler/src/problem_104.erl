%% @author std-string

%% The Fibonacci sequence is defined by the recurrence relation:
%% F(n) = F(n−1) + F(n−2), where F(1) = 1 and F(2) = 1.
%% It turns out that F(541), which contains 113 digits, is the first Fibonacci number for which the last nine digits are 1-9 pandigital (contain all the digits 1 to 9, but not necessarily in order).
%% And F(2749), which contains 575 digits, is the first Fibonacci number for which the first nine digits are 1-9 pandigital.
%% Given that F(k) is the first Fibonacci number for which the first nine digits AND the last nine digits are 1-9 pandigital, find k.

-module(problem_104).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(BOTTOM_BORDER, 100000000).
-define(TOP_BORDER, 1000000000).

-type border_data() :: {TopBorder :: pos_integer(), Divider :: pos_integer()}.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{none, 329468}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(none) -> process(1, 1, 2, {?TOP_BORDER, 1}).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process(CurrentFib :: pos_integer(), PrevFib :: pos_integer(), FibNumber :: pos_integer(), BorderData :: border_data()) -> pos_integer().
process(CurrentFib, PrevFib, FibNumber, BorderData) when CurrentFib =< ?BOTTOM_BORDER ->
    NextFib = gen_next_fibonacci(CurrentFib, PrevFib),
    process(NextFib, CurrentFib, FibNumber + 1, BorderData);
process(CurrentFib, PrevFib, FibNumber, {TopBorder, Divider}) ->
    case check_number(CurrentFib, Divider) of
        true -> FibNumber;
        false ->
            NextFib = gen_next_fibonacci(CurrentFib, PrevFib),
            process(NextFib, CurrentFib, FibNumber + 1, calc_border_data(NextFib, {TopBorder, Divider}))
    end.

-spec check_number(Number ::pos_integer(), Divider :: pos_integer()) -> boolean().
check_number(Number, Divider) ->
    Left = Number div Divider,
    case pandigital_numbers:is_pandigital(Left) of
        false -> false;
        true ->
            Right = Number rem ?TOP_BORDER,
            pandigital_numbers:is_pandigital(Right)
    end.

-spec gen_next_fibonacci(Current :: pos_integer(), Prev :: pos_integer()) -> pos_integer().
gen_next_fibonacci(Current, Prev) -> Current + Prev.

-spec calc_border_data(Number :: pos_integer(), BorderData :: border_data()) -> border_data().
calc_border_data(Number, {TopBorder, Divider}) when Number >= TopBorder -> {TopBorder * 10, Divider * 10};
calc_border_data(_Number, BorderData) -> BorderData.
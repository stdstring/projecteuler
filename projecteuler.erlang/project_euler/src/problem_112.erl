%% @author std-string

%% Working from left-to-right if no digit is exceeded by the digit to its left it is called an increasing number; for example, 134468.
%% Similarly if no digit is exceeded by the digit to its right it is called a decreasing number; for example, 66420.
%% We shall call a positive integer that is neither increasing nor decreasing a "bouncy" number; for example, 155349.
%% Clearly there cannot be any bouncy numbers below one-hundred, but just over half of the numbers below one-thousand (525) are bouncy.
%% In fact, the least number for which the proportion of bouncy numbers first reaches 50% is 538.
%% Surprisingly, bouncy numbers become more and more common and by the time we reach 21780 the proportion of bouncy numbers is equal to 90%.
%% Find the least number for which the proportion of bouncy numbers is exactly 99%.

-module(problem_112).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-type order() :: 'undef' | 'asc' | 'desc'.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{0.5, 538}, {0.9, 21780}, {0.99, 1587000}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve(BouncyNumbersProportion) -> process_number(100, 0, 99, BouncyNumbersProportion).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_number(Number :: pos_integer(),
                     BouncyNumbersCount :: non_neg_integer(),
                     TotalCount :: pos_integer(),
                     BouncyNumbersProportion :: float()) -> pos_integer().
process_number(Number, BouncyNumbersCount, TotalCount, BouncyNumbersProportion) ->
    case is_bouncy_number(Number) of
        false -> process_number(Number + 1, BouncyNumbersCount, TotalCount + 1, BouncyNumbersProportion);
        true ->
            CurrentProportion = (BouncyNumbersCount + 1) / (TotalCount + 1),
            if
                CurrentProportion < BouncyNumbersProportion -> process_number(Number + 1, BouncyNumbersCount + 1, TotalCount + 1, BouncyNumbersProportion);
                CurrentProportion >= BouncyNumbersProportion -> Number
            end
    end.

-spec is_bouncy_number(Number :: pos_integer()) -> boolean().
is_bouncy_number(Number) -> is_bouncy_number_impl(numbers:get_digits(Number), undef, undef).

-spec is_bouncy_number_impl(Digits :: numbers:digits(), LastDigit :: numbers:digit() | 'undef', DigitsOrder :: order()) -> boolean().
is_bouncy_number_impl([], _LastDigit, _Direction) -> false;
is_bouncy_number_impl([Digit | DigitsRest], undef, undef) -> is_bouncy_number_impl(DigitsRest, Digit, undef);
is_bouncy_number_impl([Digit2 | DigitsRest], Digit1, undef) ->
    if
        Digit1 < Digit2 -> is_bouncy_number_impl(DigitsRest, Digit2, asc);
        Digit1 == Digit2 -> is_bouncy_number_impl(DigitsRest, Digit2, undef);
        Digit1 > Digit2 -> is_bouncy_number_impl(DigitsRest, Digit2, desc)
    end;
is_bouncy_number_impl([Digit2 | DigitsRest], Digit1, asc) when Digit1 =< Digit2 -> is_bouncy_number_impl(DigitsRest, Digit2, asc);
is_bouncy_number_impl([Digit2 | _DigitsRest], Digit1, asc) when Digit1 > Digit2 -> true;
is_bouncy_number_impl([Digit2 | DigitsRest], Digit1, desc) when Digit1 >= Digit2 -> is_bouncy_number_impl(DigitsRest, Digit2, desc);
is_bouncy_number_impl([Digit2 | _DigitsRest], Digit1, desc) when Digit1 < Digit2 -> true.
%% @author std-string

%% Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0, where each "_" is a single digit.

-module(problem_206).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(INF, 101010103).
-define(SUP, 138902661).

-type check_result() :: {'true', NumberRest :: pos_integer()} | 'false'.

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{none, 1389019170}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

%% notes:
%% Square = Number * Number = 1_2_3_4_5_6_7_8_9_0
%% Min(Square) = 1020304050607080900
%% Max(Square) = 1929394959697989990
%% Min(Number) = 1010101010,...
%% Max(Number) = 1389026623,...
%% Square = Number * Number = 1_2_3_4_5_6_7_8_900 due to properties of the square numbers
%% Let Number = 10 * Number' => Number * Number = (Number' * Number') * 100 => Square = Square' * 100
%% Square' = 1_2_3_4_5_6_7_8_9
%% Min(Number') = 101010101,...
%% Max(Number') = 138902662,...
%% Square' = 1_2_3_4_5_6_7_809, 1_2_3_4_5_6_7_829, 1_2_3_4_5_6_7_849, 1_2_3_4_5_6_7_869, 1_2_3_4_5_6_7_889  due to properties of the square numbers
%% Number' = XXXXXXXX3, XXXXXXXX7 due to Square' finished on digit 9
-spec solve(PreparedInput :: term()) -> term().
solve(none) -> 10 * process_number(?INF, 4).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_number(Number :: pos_integer(), Delta :: 4 | 6) -> pos_integer().
process_number(Number, _Delta) when Number > ?SUP -> error(logic_error);
process_number(Number, Delta) ->
    Square = Number * Number,
    case check_number(Square) of
        true -> Number;
        %% Delta = 4, 6
        false -> process_number(Number + Delta, 10 - Delta)
    end.

-spec check_number(Number :: pos_integer()) -> boolean().
check_number(Number) ->
    case check_last_digits(Number) of
        {true, NumberRest} -> check_digits(NumberRest, 8);
        false -> false
    end.

-spec check_last_digits(Number :: pos_integer()) -> check_result().
check_last_digits(Number) ->
    Rem = Number rem 100,
    if
        Rem == 09; Rem == 29; Rem == 49; Rem == 69; Rem == 89 -> {true, Number div 100};
        true -> false
    end.

-spec check_digits(Number :: pos_integer(), LastDigit :: 1..8) -> boolean().
check_digits(1, 1) -> true;
check_digits(Number, LastDigit) ->
    ActualDigit = Number rem 10,
    if
        ActualDigit == LastDigit -> check_digits(Number div 100, LastDigit - 1);
        ActualDigit /= LastDigit -> false
    end.

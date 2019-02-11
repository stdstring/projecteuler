%% @author std-string

%% Consider all integer combinations of a^b for 2 <= a <= 5 and 2 <= b <= 5:
%% 2^2=4, 2^3=8, 2^4=16, 2^5=32
%% 3^2=9, 3^3=27, 3^4=81, 3^5=243
%% 4^2=16, 4^3=64, 4^4=256, 4^5=1024
%% 5^2=25, 5^3=125, 5^4=625, 5^5=3125
%% If they are then placed in numerical order, with any repeats removed, we get the following sequence of 15 distinct terms:
%% 4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125
%% How many distinct terms are in the sequence generated by ab for 2 <= a <= 100 and 2 <= b <= 100?

-module(problem_029).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(MIN_BASE, 2).
-define(MIN_EXP, 2).

-type power_set() :: sets:set(PowerResult :: pos_integer()).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{{5, 5}, 15}, {{100, 100}, 9183}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve({MaxBase, MaxExp}) ->
    DestStorage = process_base(?MIN_BASE, MaxBase, MaxExp, sets:new()),
    sets:size(DestStorage).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec process_base(Base :: pos_integer(), MaxBase :: pos_integer(), MaxExp :: pos_integer(), Storage :: power_set()) -> power_set().
process_base(Base, MaxBase, _MaxExp, Storage) when Base > MaxBase -> Storage;
process_base(Base, MaxBase, MaxExp, Storage) ->
    NewStorage = process_exp(Base, ?MIN_EXP, MaxExp, Storage),
    process_base(Base + 1, MaxBase, MaxExp, NewStorage).

-spec process_exp(Base :: pos_integer(), Exp :: pos_integer(), MaxExp :: pos_integer(), Storage :: power_set()) -> power_set().
process_exp(_Base, Exp, MaxExp, Storage) when Exp > MaxExp -> Storage;
process_exp(Base, Exp, MaxExp, Storage) ->
    Value = numbers:power(Base, Exp),
    NewStorage = sets:add_element(Value, Storage),
    process_exp(Base, Exp + 1, MaxExp, NewStorage).
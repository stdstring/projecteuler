%% @author std-string

%% Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:
%% Triangle : T(n) = n * (n + 1) / 2, T(n) = 1, 3, 6, 10, 15, ...
%% Pentagonal : P(n) = n * (3n − 1) / 2, P(n) = 1, 5, 12, 22, 35, ...
%% Hexagonal : H(n) = n * (2n − 1), H(n) = 1, 6, 15, 28, 45, ...
%% It can be verified that T(285) = P(165) = H(143) = 40755.
%% Find the next triangle number that is also pentagonal and hexagonal.

-module(problem_045).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{{1, 1, 1}, 40755}, {{285, 165, 143}, 1533776805}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(_ModuleSourceDir, Input) -> Input.

-spec solve(PreparedInput :: term()) -> term().
solve({_TriangleN, PentagonalN, HexagonalN}) -> process_numbers(PentagonalN + 1, HexagonalN).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% notes: for T(n) = n * (n + 1) / 2 exists substitution n = 2t - 1
%% T(n) = n * (n + 1) / 2 = (2t - 1) * (2t - 1 + 1) / 2 = (2t - 1) * 2t / 2 = t * (2t - 1) = H(t)
%% so we can exclude triangle numbers
-spec process_numbers(PentagonalN :: pos_integer(), HexagonalN :: pos_integer()) -> pos_integer().
process_numbers(PentagonalN, HexagonalN) ->
    PentagonalNumber = calc_pentagonal_number(PentagonalN),
    HexagonalNumber = calc_hexagonal_number(HexagonalN),
    if
        PentagonalNumber == HexagonalNumber -> HexagonalNumber;
        PentagonalNumber < HexagonalNumber -> process_numbers(PentagonalN + 1, HexagonalN);
        HexagonalNumber < PentagonalNumber -> process_numbers(PentagonalN, HexagonalN + 1)
    end.

-spec calc_pentagonal_number(N :: pos_integer()) -> pos_integer().
calc_pentagonal_number(N) -> N * (3 * N - 1) div 2.

-spec calc_hexagonal_number(N :: pos_integer()) -> pos_integer().
calc_hexagonal_number(N) -> N * (2 * N - 1).
%% @author std-string

-module(pell_equation_tests).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Test functions
%% ====================================================================

find_positive_first_solution_test_() ->
    [create_find_first_solution_entry(1, 1, undef),
     create_find_first_solution_entry(2, 1, {3, 2}),
     create_find_first_solution_entry(3, 1, {2, 1}),
     create_find_first_solution_entry(4, 1, undef),
     create_find_first_solution_entry(5, 1, {9, 4}),
     create_find_first_solution_entry(6, 1, {5, 2}),
     create_find_first_solution_entry(7, 1, {8, 3}),
     create_find_first_solution_entry(8, 1, {3, 1}),
     create_find_first_solution_entry(9, 1, undef),
     create_find_first_solution_entry(10, 1, {19, 6}),
     create_find_first_solution_entry(11, 1, {10, 3}),
     create_find_first_solution_entry(12, 1, {7, 2}),
     create_find_first_solution_entry(13, 1, {649, 180}),
     create_find_first_solution_entry(14, 1, {15, 4}),
     create_find_first_solution_entry(15, 1, {4, 1}),
     create_find_first_solution_entry(16, 1, undef),
     create_find_first_solution_entry(17, 1, {33, 8}),
     create_find_first_solution_entry(18, 1, {17, 4}),
     create_find_first_solution_entry(19, 1, {170, 39}),
     create_find_first_solution_entry(20, 1, {9, 2}),
     create_find_first_solution_entry(21, 1, {55, 12}),
     create_find_first_solution_entry(22, 1, {197, 42}),
     create_find_first_solution_entry(23, 1, {24, 5}),
     create_find_first_solution_entry(24, 1, {5, 1}),
     create_find_first_solution_entry(25, 1, undef),
     create_find_first_solution_entry(26, 1, {51, 10}),
     create_find_first_solution_entry(27, 1, {26, 5}),
     create_find_first_solution_entry(28, 1, {127, 24}),
     create_find_first_solution_entry(29, 1, {9801, 1820}),
     create_find_first_solution_entry(30, 1, {11, 2}),
     create_find_first_solution_entry(31, 1, {1520, 273}),
     create_find_first_solution_entry(32, 1, {17, 3}),
     create_find_first_solution_entry(33, 1, {23, 4}),
     create_find_first_solution_entry(34, 1, {35, 6}),
     create_find_first_solution_entry(35, 1, {6, 1}),
     create_find_first_solution_entry(36, 1, undef),
     create_find_first_solution_entry(37, 1, {73, 12}),
     create_find_first_solution_entry(38, 1, {37, 6}),
     create_find_first_solution_entry(39, 1, {25, 4}),
     create_find_first_solution_entry(40, 1, {19, 3}),
     create_find_first_solution_entry(41, 1, {2049, 320}),
     create_find_first_solution_entry(42, 1, {13, 2}),
     create_find_first_solution_entry(43, 1, {3482, 531}),
     create_find_first_solution_entry(44, 1, {199, 30}),
     create_find_first_solution_entry(45, 1, {161, 24}),
     create_find_first_solution_entry(46, 1, {24335, 3588}),
     create_find_first_solution_entry(47, 1, {48, 7}),
     create_find_first_solution_entry(48, 1, {7, 1}),
     create_find_first_solution_entry(49, 1, undef),
     create_find_first_solution_entry(50, 1, {99, 14}),
     create_find_first_solution_entry(51, 1, {50, 7}),
     create_find_first_solution_entry(52, 1, {649, 90}),
     create_find_first_solution_entry(53, 1, {66249, 9100}),
     create_find_first_solution_entry(54, 1, {485, 66}),
     create_find_first_solution_entry(55, 1, {89, 12}),
     create_find_first_solution_entry(56, 1, {15, 2}),
     create_find_first_solution_entry(57, 1, {151, 20}),
     create_find_first_solution_entry(58, 1, {19603, 2574}),
     create_find_first_solution_entry(59, 1, {530, 69}),
     create_find_first_solution_entry(60, 1, {31, 4}),
     create_find_first_solution_entry(61, 1, {1766319049, 226153980}),
     create_find_first_solution_entry(62, 1, {63, 8}),
     create_find_first_solution_entry(63, 1, {8, 1}),
     create_find_first_solution_entry(64, 1, undef),
     create_find_first_solution_entry(65, 1, {129, 16}),
     create_find_first_solution_entry(66, 1, {65, 8}),
     create_find_first_solution_entry(67, 1, {48842, 5967}),
     create_find_first_solution_entry(68, 1, {33, 4}),
     create_find_first_solution_entry(69, 1, {7775, 936}),
     create_find_first_solution_entry(70, 1, {251, 30}),
     create_find_first_solution_entry(71, 1, {3480, 413}),
     create_find_first_solution_entry(72, 1, {17, 2}),
     create_find_first_solution_entry(73, 1, {2281249, 267000}),
     create_find_first_solution_entry(74, 1, {3699, 430}),
     create_find_first_solution_entry(75, 1, {26, 3}),
     create_find_first_solution_entry(76, 1, {57799, 6630}),
     create_find_first_solution_entry(77, 1, {351, 40}),
     create_find_first_solution_entry(78, 1, {53, 6}),
     create_find_first_solution_entry(79, 1, {80, 9}),
     create_find_first_solution_entry(80, 1, {9, 1}),
     create_find_first_solution_entry(81, 1, undef),
     create_find_first_solution_entry(82, 1, {163, 18}),
     create_find_first_solution_entry(83, 1, {82, 9}),
     create_find_first_solution_entry(84, 1, {55, 6}),
     create_find_first_solution_entry(85, 1, {285769, 30996}),
     create_find_first_solution_entry(86, 1, {10405, 1122}),
     create_find_first_solution_entry(87, 1, {28, 3}),
     create_find_first_solution_entry(88, 1, {197, 21}),
     create_find_first_solution_entry(89, 1, {500001, 53000}),
     create_find_first_solution_entry(90, 1, {19, 2}),
     create_find_first_solution_entry(91, 1, {1574, 165}),
     create_find_first_solution_entry(92, 1, {1151, 120}),
     create_find_first_solution_entry(93, 1, {12151, 1260}),
     create_find_first_solution_entry(94, 1, {2143295, 221064}),
     create_find_first_solution_entry(95, 1, {39, 4}),
     create_find_first_solution_entry(96, 1, {49, 5}),
     create_find_first_solution_entry(97, 1, {62809633, 6377352}),
     create_find_first_solution_entry(98, 1, {99, 10}),
     create_find_first_solution_entry(99, 1, {10, 1}),
     create_find_first_solution_entry(100, 1, undef),
     create_find_first_solution_entry(101, 1, {201, 20}),
     create_find_first_solution_entry(102, 1, {101, 10}),
     create_find_first_solution_entry(103, 1, {227528, 22419}),
     create_find_first_solution_entry(104, 1, {51, 5}),
     create_find_first_solution_entry(105, 1, {41, 4}),
     create_find_first_solution_entry(106, 1, {32080051, 3115890}),
     create_find_first_solution_entry(107, 1, {962, 93}),
     create_find_first_solution_entry(108, 1, {1351, 130}),
     create_find_first_solution_entry(109, 1, {158070671986249, 15140424455100}),
     create_find_first_solution_entry(110, 1, {21, 2}),
     create_find_first_solution_entry(111, 1, {295, 28}),
     create_find_first_solution_entry(112, 1, {127, 12}),
     create_find_first_solution_entry(113, 1, {1204353, 113296}),
     create_find_first_solution_entry(114, 1, {1025, 96}),
     create_find_first_solution_entry(115, 1, {1126, 105}),
     create_find_first_solution_entry(116, 1, {9801, 910}),
     create_find_first_solution_entry(117, 1, {649, 60}),
     create_find_first_solution_entry(118, 1, {306917, 28254}),
     create_find_first_solution_entry(119, 1, {120, 11}),
     create_find_first_solution_entry(120, 1, {11, 1}),
     create_find_first_solution_entry(121, 1, undef),
     create_find_first_solution_entry(122, 1, {243, 22}),
     create_find_first_solution_entry(123, 1, {122, 11}),
     create_find_first_solution_entry(124, 1, {4620799, 414960}),
     create_find_first_solution_entry(125, 1, {930249, 83204}),
     create_find_first_solution_entry(126, 1, {449, 40}),
     create_find_first_solution_entry(127, 1, {4730624, 419775}),
     create_find_first_solution_entry(128, 1, {577, 51})].

find_negative_pell_equation_first_solution_test_() ->
     [create_find_first_solution_entry(1, -1, undef),
      create_find_first_solution_entry(2, -1, {1, 1}),
      create_find_first_solution_entry(3, -1, undef),
      create_find_first_solution_entry(4, -1, undef),
      create_find_first_solution_entry(5, -1, {2, 1}),
      create_find_first_solution_entry(6, -1, undef),
      create_find_first_solution_entry(7, -1, undef),
      create_find_first_solution_entry(8, -1, undef),
      create_find_first_solution_entry(9, -1, undef),
      create_find_first_solution_entry(10, -1, {3, 1}),
      create_find_first_solution_entry(11, -1, undef),
      create_find_first_solution_entry(12, -1, undef),
      create_find_first_solution_entry(13, -1, {18, 5}),
      create_find_first_solution_entry(14, -1, undef),
      create_find_first_solution_entry(15, -1, undef),
      create_find_first_solution_entry(16, -1, undef),
      create_find_first_solution_entry(17, -1, {4, 1}),
      create_find_first_solution_entry(18, -1, undef),
      create_find_first_solution_entry(19, -1, undef),
      create_find_first_solution_entry(20, -1, undef),
      create_find_first_solution_entry(21, -1, undef),
      create_find_first_solution_entry(22, -1, undef),
      create_find_first_solution_entry(23, -1, undef),
      create_find_first_solution_entry(24, -1, undef),
      create_find_first_solution_entry(25, -1, undef),
      create_find_first_solution_entry(26, -1, {5, 1}),
      create_find_first_solution_entry(27, -1, undef),
      create_find_first_solution_entry(28, -1, undef),
      create_find_first_solution_entry(29, -1, {70, 13}),
      create_find_first_solution_entry(30, -1, undef),
      create_find_first_solution_entry(31, -1, undef),
      create_find_first_solution_entry(32, -1, undef),
      create_find_first_solution_entry(33, -1, undef),
      create_find_first_solution_entry(34, -1, undef),
      create_find_first_solution_entry(35, -1, undef),
      create_find_first_solution_entry(36, -1, undef),
      create_find_first_solution_entry(37, -1, {6, 1}),
      create_find_first_solution_entry(38, -1, undef),
      create_find_first_solution_entry(39, -1, undef),
      create_find_first_solution_entry(40, -1, undef),
      create_find_first_solution_entry(41, -1, {32, 5}),
      create_find_first_solution_entry(42, -1, undef),
      create_find_first_solution_entry(43, -1, undef),
      create_find_first_solution_entry(44, -1, undef),
      create_find_first_solution_entry(45, -1, undef),
      create_find_first_solution_entry(46, -1, undef),
      create_find_first_solution_entry(47, -1, undef),
      create_find_first_solution_entry(48, -1, undef),
      create_find_first_solution_entry(49, -1, undef),
      create_find_first_solution_entry(50, -1, {7, 1}),
      create_find_first_solution_entry(51, -1, undef),
      create_find_first_solution_entry(52, -1, undef),
      create_find_first_solution_entry(53, -1, {182, 25}),
      create_find_first_solution_entry(54, -1, undef),
      create_find_first_solution_entry(55, -1, undef),
      create_find_first_solution_entry(56, -1, undef),
      create_find_first_solution_entry(57, -1, undef),
      create_find_first_solution_entry(58, -1, {99, 13}),
      create_find_first_solution_entry(59, -1, undef),
      create_find_first_solution_entry(60, -1, undef),
      create_find_first_solution_entry(61, -1, {29718, 3805}),
      create_find_first_solution_entry(62, -1, undef),
      create_find_first_solution_entry(63, -1, undef),
      create_find_first_solution_entry(64, -1, undef),
      create_find_first_solution_entry(65, -1, {8, 1}),
      create_find_first_solution_entry(66, -1, undef),
      create_find_first_solution_entry(67, -1, undef),
      create_find_first_solution_entry(68, -1, undef),
      create_find_first_solution_entry(69, -1, undef),
      create_find_first_solution_entry(70, -1, undef),
      create_find_first_solution_entry(71, -1, undef),
      create_find_first_solution_entry(72, -1, undef),
      create_find_first_solution_entry(73, -1, {1068, 125}),
      create_find_first_solution_entry(74, -1, {43, 5}),
      create_find_first_solution_entry(75, -1, undef),
      create_find_first_solution_entry(76, -1, undef),
      create_find_first_solution_entry(77, -1, undef),
      create_find_first_solution_entry(78, -1, undef),
      create_find_first_solution_entry(79, -1, undef),
      create_find_first_solution_entry(80, -1, undef),
      create_find_first_solution_entry(81, -1, undef),
      create_find_first_solution_entry(82, -1, {9, 1}),
      create_find_first_solution_entry(83, -1, undef),
      create_find_first_solution_entry(84, -1, undef),
      create_find_first_solution_entry(85, -1, {378, 41}),
      create_find_first_solution_entry(86, -1, undef),
      create_find_first_solution_entry(87, -1, undef),
      create_find_first_solution_entry(88, -1, undef),
      create_find_first_solution_entry(89, -1, {500, 53}),
      create_find_first_solution_entry(90, -1, undef),
      create_find_first_solution_entry(91, -1, undef),
      create_find_first_solution_entry(92, -1, undef),
      create_find_first_solution_entry(93, -1, undef),
      create_find_first_solution_entry(94, -1, undef),
      create_find_first_solution_entry(95, -1, undef),
      create_find_first_solution_entry(96, -1, undef),
      create_find_first_solution_entry(97, -1, {5604, 569}),
      create_find_first_solution_entry(98, -1, undef),
      create_find_first_solution_entry(99, -1, undef),
      create_find_first_solution_entry(100, -1, undef),
      create_find_first_solution_entry(101, -1, {10, 1}),
      create_find_first_solution_entry(102, -1, undef),
      create_find_first_solution_entry(103, -1, undef),
      create_find_first_solution_entry(104, -1, undef),
      create_find_first_solution_entry(105, -1, undef),
      create_find_first_solution_entry(106, -1, {4005, 389}),
      create_find_first_solution_entry(107, -1, undef),
      create_find_first_solution_entry(108, -1, undef),
      create_find_first_solution_entry(109, -1, {8890182, 851525}),
      create_find_first_solution_entry(110, -1, undef),
      create_find_first_solution_entry(111, -1, undef),
      create_find_first_solution_entry(112, -1, undef),
      create_find_first_solution_entry(113, -1, {776, 73}),
      create_find_first_solution_entry(114, -1, undef),
      create_find_first_solution_entry(115, -1, undef),
      create_find_first_solution_entry(116, -1, undef),
      create_find_first_solution_entry(117, -1, undef),
      create_find_first_solution_entry(118, -1, undef),
      create_find_first_solution_entry(119, -1, undef),
      create_find_first_solution_entry(120, -1, undef),
      create_find_first_solution_entry(121, -1, undef),
      create_find_first_solution_entry(122, -1, {11, 1}),
      create_find_first_solution_entry(123, -1, undef),
      create_find_first_solution_entry(124, -1, undef),
      create_find_first_solution_entry(125, -1, {682, 61}),
      create_find_first_solution_entry(126, -1, undef),
      create_find_first_solution_entry(127, -1, undef),
      create_find_first_solution_entry(128, -1, undef)].
%% 122, 125, 130

%% ====================================================================
%% Internal functions
%% ====================================================================

create_find_first_solution_entry(D, C, Expected) ->
    Description = lists:flatten(io_lib:format("first solution of x^2 - ~p * y^2 = ~p", [D, C])),
    {Description, ?_assertEqual(Expected, pell_equation:find_first_solution(D, C))}.
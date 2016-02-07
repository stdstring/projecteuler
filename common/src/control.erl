-module(control).
-export([for/3, for/4, for/5]).

%% 0-based index
-spec for(Count :: non_neg_integer(), State :: term(), IterationFun :: fun((non_neg_integer(), term()) -> term())) -> term().
for(Count, State, IterationFun) -> for(0, Count - 1, 1, State, IterationFun).

%% index in [From, To]
-spec for(From :: non_neg_integer(), To :: non_neg_integer(), State :: term(), IterationFun :: fun((non_neg_integer(), term()) -> term())) -> term().
for(From, To, State, IterationFun) -> for_impl(From, To, 1, State, IterationFun).

%% index in [From, To]
-spec for(From :: non_neg_integer(), To :: non_neg_integer(), Step :: non_neg_integer(), State :: term(), IterationFun :: fun((non_neg_integer(), term()) -> term())) -> term().
for(From, To, Step, State, IterationFun) -> for_impl(From, To, Step, State, IterationFun).

-spec for_impl(Index :: non_neg_integer(), To :: non_neg_integer(), Step :: non_neg_integer(), State :: term(), IterationFun :: fun((non_neg_integer(), term()) -> term())) -> term().
for_impl(Index, To, _Step, State, _IterationFun) when Index > To -> State;
for_impl(Index, To, Step, State, IterationFun) ->
    for_impl(Index + Step, To, Step, IterationFun(Index, State), IterationFun).
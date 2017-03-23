%% @author std-string

-module(mutable_array).

-export([init/0, create/2, get/2, set/3, is_loaded/0]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec init() -> 'ok' | {'error', {Reason :: 'load_failed' | 'bad_lib' | 'load' | 'reload' | 'upgrade' | 'old_code', Text :: string()}}.
init() -> erlang:load_nif("MutableArrayNif", 0).

-spec create(Size :: integer(), DefaultValue :: binary()) -> binary().
create(_Size, _DefaultValue) -> error(not_loaded).

-spec get(Array :: binary(), Index :: non_neg_integer()) -> binary().
get(_Array, _Index) -> error(not_loaded).

-spec set(Array :: binary(), Index :: non_neg_integer(), Value :: binary()) -> binary().
set(_Array, _Index, _Value) -> error(not_loaded).

-spec is_loaded() -> boolean().
is_loaded() -> false.

%% ====================================================================
%% Internal functions
%% ====================================================================
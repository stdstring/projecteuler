%% @author std-string

-module(mutable_uint8_array).

-export([init/0, create/2, get/2, set/3, is_loaded/0, size/1]).

-type mutable_uint8_array() :: binary().

%% ====================================================================
%% API functions
%% ====================================================================

-spec init() -> 'ok' | {'error', {Reason :: 'load_failed' | 'bad_lib' | 'load' | 'reload' | 'upgrade' | 'old_code', Text :: string()}}.
init() ->
    case is_loaded() of
        false -> unsafe_init();
        true -> ok
    end.

-spec create(Size :: pos_integer(), DefaultValue :: binary()) -> mutable_uint8_array().
create(_Size, _DefaultValue) -> error(not_loaded).

-spec get(Index :: non_neg_integer(), Array :: mutable_uint8_array()) -> binary().
get(_Index, _Array) -> error(not_loaded).

-spec set(Index :: non_neg_integer(), Value :: binary(), Array :: mutable_uint8_array()) -> mutable_uint8_array().
set(_Index, _Value, _Array) -> error(not_loaded).

-spec is_loaded() -> boolean().
is_loaded() -> false.

-spec size(Array :: mutable_uint8_array()) -> pos_integer().
size(_Array) -> error(not_loaded).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec unsafe_init() -> 'ok' | {'error', {Reason :: 'load_failed' | 'bad_lib' | 'load' | 'reload' | 'upgrade' | 'old_code', Text :: string()}}.
unsafe_init() ->
    ModulePath = code:which(?MODULE),
    ModuleDir = filename:dirname(ModulePath),
    erlang:load_nif(filename:join(ModuleDir, "MutableUInt8ArrayNif"), 0).
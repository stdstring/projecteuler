-module(numerical_task_behaviour).

-callback get_check_data() -> [{Input :: term(), Output :: term()}].

-callback prepare_data(ModuleSourceDir :: string(), Input :: term()) -> PreparedInput :: term().

-callback solve(PreparedInput :: term()) -> Result :: term().
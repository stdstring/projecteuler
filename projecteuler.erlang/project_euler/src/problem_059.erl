%% @author std-string

%% Each character on a computer is assigned a unique code and the preferred standard is ASCII (American Standard Code for Information Interchange).
%% For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.
%% A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each byte with a given value, taken from a secret key.
%% The advantage with the XOR function is that using the same encryption key on the cipher text, restores the plain text;
%% for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.
%% For unbreakable encryption, the key is the same length as the plain text message, and the key is made up of random bytes.
%% The user would keep the encrypted message and the encryption key in different locations, and without both "halves", it is impossible to decrypt the message.
%% Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key.
%% If the password is shorter than the message, which is likely, the key is repeated cyclically throughout the message.
%% The balance for this method is using a sufficiently long password key for security, but short enough to be memorable.
%% Your task has been made easy, as the encryption key consists of three lower case characters.
%% Using problem_059.dat, a data file containing the encrypted ASCII codes, and the knowledge that the plain text must contain common English words,
%% decrypt the message and find the sum of the ASCII values in the original text.

-module(problem_059).
-export([get_check_data/0, prepare_data/2, solve/1]).

-behaviour(numerical_task_behaviour).

-define(DELIMITER, ",").
-define(CHECK_DATA, ["The ", " the "]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec get_check_data() -> [{Input :: term(), Output :: term()}].
get_check_data() -> [{"problem_059.dat", 107359}].

-spec prepare_data(ModuleSourceDir :: string(), Input :: term()) -> term().
prepare_data(ModuleSourceDir, Filename) ->
    Loader = fun(IoDevice) ->
        {ok, [Data]} = io:fread(IoDevice, "", "~s"),
        lists:map(fun list_to_integer/1, string:tokens(Data, ?DELIMITER))
    end,
    load_utils:read_from_file(filename:join(ModuleSourceDir, Filename), Loader).

-spec solve(PreparedInput :: term()) -> term().
solve(Data) ->
    DataSize = length(Data),
    Result = [{Message, [Char1, Char2, Char3]} || Char1 <- lists:seq($a, $z),
                                                  Char2 <- lists:seq($a, $z),
                                                  Char3 <- lists:seq($a, $z),
                                                  Message <- [decrypt(Data, DataSize, [Char1, Char2, Char3])],
                                                  check_data(Message, ?CHECK_DATA)],
    [{Message, _Key}] = Result,
    lists:sum(Message).

%% ====================================================================
%% Internal functions
%% ====================================================================

-spec decrypt(Data :: [pos_integer()], DataSize :: pos_integer(), Key :: string()) -> [pos_integer()].
decrypt(Data, DataSize, Key) ->
    RepeatedKey = repeat_key(Key, DataSize),
    xor_data(Data, RepeatedKey, []).

-spec check_data(Message :: [pos_integer()], CheckData :: [string()]) -> boolean().
check_data(_Message, []) -> false;
check_data(Message, [CheckItem | CheckRest]) ->
    case string:str(Message, CheckItem) of
        0 -> check_data(Message, CheckRest);
        _Other -> true
    end.

-spec repeat_key(Key :: string(), SourceSize :: pos_integer()) -> string().
repeat_key(Key, SourceSize) ->
    KeySize = length(Key),
    repeat_key(Key, SourceSize, KeySize).

-spec repeat_key(Key :: string(), SourceSize :: pos_integer(), KeySize :: pos_integer()) -> string().
repeat_key(Key, SourceSize, KeySize) when SourceSize rem KeySize == 0 ->
    string:copies(Key, SourceSize div KeySize);
repeat_key(Key, SourceSize, KeySize) ->
    string:copies(Key, SourceSize div KeySize) ++ string:substr(Key, 1, SourceSize rem KeySize).

-spec xor_data(Source :: [pos_integer()], Key :: [pos_integer()], Dest :: [pos_integer()]) -> [pos_integer()].
xor_data([], [], Dest) -> lists:reverse(Dest);
xor_data([SourceChar | SourceRest], [KeyChar | KeyRest], Dest) ->
    xor_data(SourceRest, KeyRest, [SourceChar bxor KeyChar] ++ Dest).
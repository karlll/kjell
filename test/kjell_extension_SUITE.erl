%%%-------------------------------------------------------------------
%%% @author karl l <karl@ninjacontrol.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(kjell_extension_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [
     load_extension,
     activate_ext_point,
     non_registered_ext_point,
     cmd_ok,
     cmd_error,
     cmd_undef,
     get_exts,
     get_ext,
     get_ext_empty,
     has_extension,
     get_command_ext,
     get_all_command_exts
    ].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
load_extension() -> 
    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
load_extension(Config) -> 
    kjell_profile:start_link(),
    kjell_extension:start_link(),
    DataDir = proplists:get_value(data_dir,Config),
    ok = load_exts(DataDir),
    kjell_profile:stop(),
    ok.


activate_ext_point() ->
    [].

activate_ext_point(Config) ->
    kjell_profile:start_link(),
    kjell_extension:start_link(),
    DataDir = proplists:get_value(data_dir,Config),
    ok = load_exts(DataDir),

    ExpectedStr = "(output) Extension got : test\n",
    {ok, OutStr} = kjell_extension:activate(shell_output_line,"test"),
    ct:pal("Got result = ~p",[OutStr]),
    true = string:equal(ExpectedStr, OutStr),
    kjell_profile:stop(),
    ok.


non_registered_ext_point() ->
    [].

non_registered_ext_point(Config) ->
    kjell_profile:start_link(),
    kjell_extension:start_link(),
    DataDir = proplists:get_value(data_dir,Config),
    ok = load_exts(DataDir),
    ExpectedStr = "test",
    OutStr = kjell_extension:activate(non_registered,"test"),
    ct:pal("Got result = ~p",[OutStr]),
    true = string:equal(ExpectedStr, OutStr),
    kjell_profile:stop(),
    ok.

cmd_ok() ->
    [].

cmd_ok(Config) ->
    kjell_profile:start_link(),
    kjell_extension:start_link(),
    DataDir = proplists:get_value(data_dir,Config),
    ExpectedStr = "(cmd) Extension got : \"test\"\n",
    ok = load_exts(DataDir),
    {ok,OutStr} = kjell_extension:activate({command,cmd},"test"),
    ct:pal("Got result = ~p",[OutStr]),
    true = string:equal(ExpectedStr, OutStr),
    ct:pal("State = ~p",[kjell_profile:state()]),
    kjell_profile:stop(),
    ok.

cmd_error() ->
    [].

cmd_error(Config) ->
    kjell_profile:start_link(),
    kjell_extension:start_link(),
    DataDir = proplists:get_value(data_dir,Config),
    ExpectedStr = "Error message",
    ok = load_exts(DataDir),
    {error, OutStr} = kjell_extension:activate({command,cmd_error},"test"),
    ct:pal("Got result = ~p",[OutStr]),
    true = string:equal(ExpectedStr, OutStr),
    kjell_profile:stop(),
    ok.


cmd_undef() ->
    [].

cmd_undef(Config) ->
    kjell_profile:start_link(),
    kjell_extension:start_link(),
    DataDir = proplists:get_value(data_dir,Config),
    ok = load_exts(DataDir),
    {error, undefined} = kjell_extension:activate({command,cmd_undef},[]),
    kjell_profile:stop(),
    ok.

get_exts() ->
    [].

get_exts(Config) ->
    kjell_profile:start_link(),
    kjell_extension:start_link(),
    DataDir = proplists:get_value(data_dir,Config),
    ok = load_exts(DataDir),
    Cmds = kjell_extension:get_extensions(command),
    ct:pal("Got extension = ~p",[Cmds]),
    kjell_profile:stop(),
    kjell_extension:stop(),
    ok.

get_ext() ->
    [].

get_ext(Config) ->
    kjell_profile:start_link(),
    kjell_extension:start_link(),
    DataDir = proplists:get_value(data_dir,Config),
    ok = load_exts(DataDir),
    Cmd = kjell_extension:get_extension(command),
    ct:pal("Got extension = ~p",[Cmd]),
    kjell_profile:stop(),
    kjell_extension:stop(),
    ok.

get_ext_empty() ->
    [].

get_ext_empty(Config) ->
    kjell_profile:start_link(),
    kjell_extension:start_link(),
    DataDir = proplists:get_value(data_dir,Config),
    ok = load_exts(DataDir),
    [] = kjell_extension:get_extension(undefined),
    [] = kjell_extension:get_extensions(undefined),
    kjell_profile:stop(),
    kjell_extension:stop(),
    ok.


has_extension() ->
    [].
has_extension(Config) ->
    kjell_profile:start_link(),
    kjell_extension:start_link(),
    DataDir = proplists:get_value(data_dir,Config),
    ok = load_exts(DataDir),
    true = kjell_extension:has_extensions({command,cmd}),
    false = kjell_extension:has_extensions({command,undef_cmd}),
    true = kjell_extension:has_extensions(shell_output_line),
    kjell_profile:stop(),
    kjell_extension:stop(),
    ok.

get_command_ext() ->
    [].
get_command_ext(Config) ->
    kjell_profile:start_link(),
    kjell_extension:start_link(),
    DataDir = proplists:get_value(data_dir,Config),
    ok = load_exts(DataDir),
    {{test_cmd,cmd_error},"Test command 2"} = kjell_extension:get_command_extension(cmd_error),
    undefined = kjell_extension:get_command_extension(cmd_undef),
    {{test_cmd,cmd},_} = kjell_extension:get_command_extension(cmd), % return only one
    kjell_profile:stop(),
    kjell_extension:stop(),
    ok.

get_all_command_exts() ->
    [].
get_all_command_exts(Config) ->
    kjell_profile:start_link(),
    kjell_extension:start_link(),
    DataDir = proplists:get_value(data_dir,Config),
    ok = load_exts(DataDir),
    ExpectedCmdExts = 
        [
            {{test_cmd,cmd_error},"Test command 2"},
            {{test_cmd,cmd},"Test command"},
            {{test_extension2,cmd}, "Test command 2 (test_cmd contains the same command)"}
        ],
    ExpectedCmdExts = kjell_extension:get_all_command_extensions(),
    kjell_profile:stop(),
    kjell_extension:stop(),
    ok.

%
%

load_exts(Path) ->
    ok = kjell_profile:load_profile(filename:join(Path,"kjell.config")),
    ExtensionDir = kjell_profile:get_value(ext_dir),
    FullExtensionPath = filename:join(Path,ExtensionDir),
    ct:pal("Extension Dir = ~p",[FullExtensionPath]),
    kjell_extension:init_extensions(FullExtensionPath),
    ct:pal("All = ~p",[ets:all()]),
    ok.



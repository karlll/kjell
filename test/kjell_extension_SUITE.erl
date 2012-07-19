%%%-------------------------------------------------------------------
%%% @author a2l <a2l@ultramonkey2>
%%% @copyright (C) 2012, a2l
%%% @doc
%%%
%%% @end
%%% Created : 16 Jul 2012 by a2l <a2l@ultramonkey2>
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
    [load_extension,
     activate_ext_point,
     non_registered_ext_point].

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
    DataDir = proplists:get_value(data_dir,Config),
    ok = load_exts(DataDir),
    kjell_profile:stop(),
    ok.


activate_ext_point() ->
    [].

activate_ext_point(Config) ->
    kjell_profile:start_link(),
    DataDir = proplists:get_value(data_dir,Config),
    ok = load_exts(DataDir),

    ExpectedStr = "(output) Extension got : test\n",
    OutStr = kjell_extension:activate(shell_output_line,"test"),
    ct:pal("Got result = ~p",[OutStr]),
    true = string:equal(ExpectedStr, OutStr),
    kjell_profile:stop(),
    ok.


non_registered_ext_point() ->
    [].

non_registered_ext_point(Config) ->
    kjell_profile:start_link(),
    DataDir = proplists:get_value(data_dir,Config),
    ok = load_exts(DataDir),
    ExpectedStr = "test",
    OutStr = kjell_extension:activate(non_registered,"test"),
    ct:pal("Got result = ~p",[OutStr]),
    true = string:equal(ExpectedStr, OutStr),
    kjell_profile:stop(),
    ok.


load_exts(Path) ->
    ok = kjell_profile:load_profile(filename:join(Path,"kjell.config")),
    ExtensionDir = kjell_profile:get_value(ext_dir),
    FullExtensionPath = filename:join(Path,ExtensionDir),
    ct:pal("Extension Dir = ~p",[FullExtensionPath]),
    kjell_extension:init_extensions(FullExtensionPath),
    ct:pal("All = ~p",[ets:all()]),
    ok.


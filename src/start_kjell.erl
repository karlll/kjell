%%%-------------------------------------------------------------------
%%% @author karl l <karl@ninjacontrol.com>
%%% @doc
%%%   
%%% @end
%%%-------------------------------------------------------------------

-module(start_kjell).

-compile([export_all]).
-export([start/0]).

-define(TTY_DRV,'tty_sl -e -c').
-define(CFG_DIR,".kjell").
-define(CFG_FILE,"kjell.config").
-define(EXT_DIR,"extensions").
-define(COL_PROF,"default.profile").

start() ->
    ok = setup(),
    k_user_drv:start([?TTY_DRV,{kjell,start,[init]}]),
    ok.

setup() ->
    ok = config(),
    ok.

config() ->
    config(get_cfg).

config(get_cfg) ->
    case get_cfg_dir() of
	{ok,CfgDir} ->
	    case get_cfg_file(CfgDir) of
		{ok,CfgFile} ->
		    kjell_profile:start_link(),
		    Cfg = filename:join(CfgDir,CfgFile),
		    ok = kjell_profile:load_profile(Cfg),
		    ok = kjell_profile:set_value(cfg_dir,CfgDir),
		    config(get_ext_dir,CfgDir);
	        {error,not_found} ->
		    case create_default_cfg_file(CfgDir) of
			ok -> 
	    		    kjell_profile:start_link(),
			    Cfg = filename:join(CfgDir,?CFG_FILE),
			    create_default_colprof(CfgDir),
			    ok = kjell_profile:load_profile(Cfg),
			    ok = kjell_profile:set_value(cfg_dir,CfgDir),
			    config(get_ext_dir,CfgDir);

			{error,Reason} ->
			    {error,Reason}
		    end
	    end;
	{error,Reason} ->
	    {error,Reason}
    end.

config(get_ext_dir,CfgDir) ->
    ExtDirCfg = case kjell_profile:get_value(ext_dir) of
		     undefined ->
			 ?EXT_DIR;
		     Val ->
			 Val
		 end,
    ExtDir = case filename:pathtype(ExtDirCfg) of
		 relative ->
		     filename:join(CfgDir,ExtDirCfg);
		 _Other ->
		     ExtDirCfg
	     end,
    case filelib:is_dir(ExtDir) of
	true ->
	    %% start extension handler
	    ok = kjell_extension:init_extensions(ExtDir),
	    ok;
	false ->
	    % creating default ext dir
	    case create_default_ext_dir(CfgDir) of
		{ok,NewExtDir} ->
		    %% start extension handler
		    ok = kjell_extension:init_extensions(NewExtDir),
		    %% update configuration
		    ok = kjell_profile:set_value(ext_dir,NewExtDir),
		    ok;
		{error,Reason} ->
		    {error,Reason}
	    end
    end.


get_cfg_file(CfgDir) ->
    CfgFile = filename:join(CfgDir,?CFG_FILE),
    case filelib:is_regular(CfgFile) of
	true ->
	    {ok,CfgFile};
	false ->
	    {error,not_found}
    end.

get_cfg_dir() ->
    CfgDir = case init:get_argument(home) of
		  {ok,[PathList]} ->
		      Path = hd(PathList),
		      filename:join(Path,?CFG_DIR);
		  _ ->
		      TmpDir = case os:type() of
				   vxworks ->
				       "/tmp";
				   {unix, _} ->
				       "/tmp";
				   {win32,_} ->
				       TempDir = case os:getenv("TEMP") of
						     false ->
							 "."; 
						     Tmp ->
							 Tmp
						 end,
				       TempDir;
				    _ -> % try current dir
				       "."
			       end,
		      filename:join(TmpDir,?CFG_DIR)
	      end,
    case filelib:ensure_dir(CfgDir) of
	ok ->
	    case file:make_dir(CfgDir) of
		ok -> 
		    {ok, CfgDir};
		{error,eexist} ->
		    {ok, CfgDir}; % already exists
		{error, Reason} ->
		    Msg = io_lib:format("Error creating config dir = ~s : ~s",[CfgDir,Reason]), 
		    {error,Msg}
	    end;
			
	{error, Reason} ->
	    Msg = io_lib:format("Error in path = ~s : ~s~n",[CfgDir,Reason]), 
	    {error,Msg}
    end.
		      

create_default_cfg_file(Path)->	        
    CfgFile = filename:join(Path,?CFG_FILE),
    case file:open(CfgFile,[write]) of
	{ok, File} ->
	    try lists:map(fun(L) -> file:write(File,lists:flatten(io_lib:format("~p.~n",[L]))) end, 
			  default_config()) of
		_Res -> 
		    ok
	    catch
		Class:Reason ->
		    Msg = io_lib:format("Error writing default config file : ~p : ~p~n",[Class,Reason]),
		    {error,lists:flatten(Msg)}
	    end;
	{error,Reason} ->
	       Msg = io_lib:format("Error opening default config file : ~p~n",[Reason]),
	       {error,lists:flatten(Msg)}
    end.

create_default_ext_dir(Path)->
    ExtPath = filename:join(Path,?EXT_DIR),
    case filelib:ensure_dir(ExtPath) of
	ok ->
	    case file:make_dir(ExtPath) of
		ok -> 
		    {ok, ExtPath};
		{error,eexist} ->
		    {ok, ExtPath}; % already exists
		{error, Reason} ->
		    Msg = io_lib:format("Error creating extension dir = ~s : ~s~n",[ExtPath,Reason]), 
		    {error,Msg}
	    end;
			
	{error, Reason} ->
	    Msg = io_lib:format("Error in path = ~s : ~s~n",[ExtPath,Reason]), 
	    {error,Msg}
    end.



create_default_colprof(Path)->
    ProfFile = filename:join(Path,?COL_PROF),
    case file:open(ProfFile,[write]) of
	{ok, File} ->
	    try lists:map(fun(L) -> file:write(File,lists:flatten(io_lib:format("~p.~n",[L]))) end, 
			  default_col_prof()) of
		_Res -> 
		    ok
	    catch
		Class:Reason ->
		    Msg = io_lib:format("Error writing default color profile : ~p : ~p~n",[Class,Reason]),
		    {error,lists:flatten(Msg)}
	    end;
	{error,Reason} ->
	       Msg = io_lib:format("Error opening default color profile : ~p~n",[Reason]),
	       {error,lists:flatten(Msg)}
    end.

%
% default config
%

default_col_prof() ->
    [ 
      %% Text classes 
      %% { class, {text_attrib, foreground, background}}

      {string,{none,yellow,none}},
      {digits,{none,green,none}},
      {keyword,{bright,magenta,none}},
      {warning,{dim,red,none}},
      {error,{underscore,red,none}},
      {term,{none,cyan,none}},
      {record_name,{none,magenta,none}},
      {record_field,{none,blue,none}},
      {function,{none,magenta,none}},

      %% Character colors

      %% $[
      {lst_start,{none,white,none}},
      %% $]
      {lst_end,{none,white,none}},
      %% ${
      {tpl_start,{none,blue,none}},
      %% $}
      {tpl_end,{none,blue,none}},
      %% "<<"
      {bstr_start,{none,blue,none}},
      %% ">>"
      {bstr_end,{none,blue,none}},

      %% "[]"
      {empty_lst,{none,yellow,none}},
      %% "{}"
      {empty_tpl,{none,yellow,none}},
      %% "<<>>"
      {empty_bstr,{none,yellow,none}},

      %% "..."
      {elipsis,{dim,white,none}},
      %% $,
      {comma,{none,white,none}},
      %% " = "
      {equal,{none,white,none}},
      %% $:
      {colon,{none,white,none}},
      %% $|
      {pipe,{none,white,none}}

    ].
			
default_config() ->		    
    [
     {ext_dir,?EXT_DIR},
     {color_profile,?COL_PROF}
    ].

			      
		      

    

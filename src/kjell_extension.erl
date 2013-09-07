%%%-------------------------------------------------------------------
%%% @author karl l <karl@ninjacontrol.com>
%%% @doc
%%% kjell extensions
%%% @end
%%%-------------------------------------------------------------------
-module(kjell_extension).

-compile([export_all]).


-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE,stop).
    
%%%===================================================================
%%% API
%%%===================================================================

init_extensions(ExtensionPath)->							     
	gen_server:call(?MODULE,{init_extensions, ExtensionPath}).

activate({command,Cmd},InData) ->
	gen_server:call(?MODULE,{activate, {command, Cmd},InData});

activate(ExtPoint,InData) ->
	gen_server:call(?MODULE,{activate,ExtPoint,InData}).

-spec get_extension(ExtPoint::term()) -> {Module::term(),Function::term(),Desc::list()} | [].
get_extension(ExtPoint) ->
	case get_extensions(ExtPoint) of
		[] -> 
			[];
		Exts ->
			hd(Exts)
	end.

-spec get_extensions(ExtPoint::term()) -> Extensions::list() | [].
get_extensions(ExtPoint) ->
	gen_server:call(?MODULE,{get_extensions,ExtPoint}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.


handle_call({init_extensions,ExtensionPath}, _From, State) ->

	io:format("DEBUG: init_extensions.~n"),

    ets:new(ext_pts,[named_table]),
    ets:new(ext_desc,[named_table]),

    case load_extensions(ExtensionPath) of
		{ok, Exts} ->
		    ok = register_extensions(Exts);
		_Other ->
		    io:format("No extensions loaded.~n")
    end,
    {reply, ok, State};

handle_call({activate,{command,Cmd},InData}, _From, State) ->

    Command = get_cmd_ext(Cmd),
    case Command of
		undefined ->
	    	{reply, {error, undefined}, State};
      	CmdModFuns ->
	    	{reply, apply_extension({command,CmdModFuns},InData), State}
    end;

handle_call({activate,ExtPoint,InData}, _From, State)->
    Extensions = ets:lookup(ext_pts,ExtPoint),
    case Extensions of 
	[] ->
	    {reply, InData, State};
	Extensions ->
	    case point_type(ExtPoint) of
			single ->
			    {reply, apply_extension(hd(Extensions),InData), State};
			multiple ->
			    {reply, apply_extensions(hd(Extensions),InData), State}; % TODO : implement multiple extensions
			none ->
			    {reply, InData, State}
	    end
    end;

handle_call({get_extensions,ExtPoint}, _From, State)->
    ExtensionData = ets:lookup(ext_pts,ExtPoint),
    case ExtensionData of 
	[] ->
	    {reply, [], State};
	[{ExtPoint,Extensions}] when is_tuple(Extensions) ->
		{reply, [Extensions], State};
	[{ExtPoint,Extensions}] when is_list(Extensions) ->
		{reply, Extensions, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State}.




%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

compile_extension(File, OutDir) ->
    compile_extension(File,OutDir,[]).

compile_extension(File, OutDir, Options) ->
    Verbose = lists:member(verbose,Options),
    Strict = lists:member(strict,Options),
    case compile:file(File,[return_errors, return_warnings, {outdir,OutDir}]) of
	{ok, ExtModule, []} ->
	    {ok,ExtModule};
	{ok, ExtModule, Warnings} ->
	    case Verbose of
		true -> io:format("compile_extension: compiling ~p, got warning(s) = ~p~n",[File,Warnings]);
		false -> ok
	    end,
	    case Strict of
		false -> {ok, ExtModule};
		true -> {error, Warnings}
	    end;
	{error, Error, []} ->
	    case Verbose of
		true -> io:format("compile_extension: compiling ~p, got error(s) = ~p~n",[File,Error]);
		false -> ok
	    end,
	    {error,Error};
	{error, Error, Warnings} ->
	    case Verbose of
		true -> io:format("compile_extension: compiling ~p, got error(s) = ~p, warning(s) = ~p~n",[File,Error,Warnings]);
		false -> ok
	    end,
	    {error,Error}
    end.  

get_modules(Path)->
    case file:list_dir(Path) of
	{ok,FileList} ->
	    L = lists:filter(fun(File) -> filename:extension(File) == ".erl" end,
			     FileList),
	    {ok,L};
	{error,Reason} ->
	    {error,Reason}
    end.

load_extensions(Path) ->
    case get_modules(Path) of
	{error,Reason} ->
	    io:format("No extensions loaded. Error while reading directory ~p: \"~p\"~n",[Path,Reason]),
	    {ok, []};
	{ok,FileList} ->
		% Compile the files
	    CompdList = lists:map(fun(File) -> 
					  case compile_extension(filename:join(Path,File),Path) of
					      {ok,ExtModule} ->
						  ExtModule;
					      {error,Error} ->
						  io:format("Error while compiling ~p (~p).~n",[File,Error]),
						  none
					  end
				  end, FileList),
						% Select valid extensions
	    io:format("DEBUG : Adding code path = ~p~n",[Path]),
	    true = code:add_patha(Path),
	    ExtList = lists:filter(fun(CompdItem) -> 
					   case CompdItem  of
					       none ->
						   false; % Failed compilation, remove
					       Mod ->
						   Exports = proplists:get_value(exports,apply(Mod,module_info,[])),

						   case lists:member({extends,0},Exports) of
						       true ->
							   io:format("DEBUG: ~p is an extension~n", [CompdItem]),
							   true;
						       false ->
							   false
						   end
					   end
				   end, CompdList),

	    {ok,ExtList}
    end.

register_extensions(ExtList) ->
    register_extensions(ExtList,[],[]).

register_extensions([ExtMod|T],ExtPtRegister,ExtModDescList) ->
     ExtendsData = apply(ExtMod,extends,[]),
     {ExtModDesc,ExtsList} = ExtendsData,
    NewExtPtRegister = lists:concat([ExtsList,ExtPtRegister]),
    register_extensions(T,NewExtPtRegister,[ExtModDesc|ExtModDescList]);

register_extensions([],ExtPtRegister,ExtModDescList) ->
    CExtPtRegister = compact_lst(ExtPtRegister),
    lists:map(fun(Pt) -> {P,ExtMods} = Pt,
			 io:format("Inserting P = ~p , ExtMods = ~p~n",[P,ExtMods]),
			 ets:insert_new(ext_pts,{P,ExtMods})
	      end, CExtPtRegister),
    ets:insert_new(ext_desc,{loaded_exts,ExtModDescList}),
    ok.


get_cmd_ext(Cmd) ->
    Commands = ets:lookup(ext_pts,command),
    case Commands of
	[] ->
	    undefined;
	List when is_list(List) ->
	    
	    {command,Extensions} = hd(List),
	    case Extensions of
		T when is_tuple(T) ->
		    {{ExtMod,Fun},_} = T,
		    case Fun of
			Cmd ->
			    T;
			_ -> undefined
		    end;
		L when is_list(L) ->
		    Filter = fun({{ExtMod,Fun}, _}) ->
				     case Fun of
					 Cmd ->
					     true;
					 _ ->
					     false
				     end
			     end,
		    Cs = lists:filter(Filter,Extensions),
		    case Cs of
			[] ->
			    undefined;
			_ ->
			    Cs
		    end
	    end
		       
    end.
	 
			     





apply_extension({ExtPt,Exts},InData) when is_tuple(Exts) ->
    {{M,F},Desc} = Exts,
    Res = apply(M,F,[InData]),
    Res;

apply_extension({ExtPt,Exts},InData) when is_list(Exts) ->
    {{M,F},Desc} = hd(Exts),
    Res = apply(M,F,[InData]),
    Res.


apply_extensions(Extensions,InData)->								 
   InData. %% todo





point_type(shell_input_line) ->	single;
point_type(shell_output_line) -> single;
point_type(prompt) -> single;
point_type(startup) -> single;
point_type(startup_msg) -> single;
point_type(Other) -> none.



%% Transform list 
%% from [{A,B,C},{A,D,E}] -> [{A,[{B,C},{D,E}]}]
compact_lst(Lst) ->
    compact_lst(Lst,[]).
compact_lst([{Tag,ItemA,ItemB}|T],Acc) ->
    case lists:flatten(proplists:get_all_values(Tag,Acc)) of
	[] ->
	    compact_lst(T,[{Tag,{ItemA,ItemB}}|Acc]);
	L ->
	    compact_lst(T,[ {Tag,[{ItemA,ItemB}|L]}|proplists:delete(Tag,Acc)])
    end;
compact_lst([],Acc) ->
    lists:reverse(Acc).

	    
    

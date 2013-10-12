%%%-------------------------------------------------------------------
%%% @author karl l <karl@ninjacontrol.com>
%%% @doc
%%% kjell profile settings
%%% @end
%%%-------------------------------------------------------------------
-module(kjell_profile).

-compile([export_all]).
-include("kjell.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(LOG, kjell_log).

-record(state, {text_attr = [], %% TODO: remove?
	       settings = []}).

%% flags

-define(FLAG_NO_COLOR, no_color). 

%%%===================================================================
%%% API
%%%===================================================================

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
    

%% Applies color / formatting to the provided string
%% corresponding to the given class in the current profile.
q(Class,Str) ->
    case gen_server:call(?MODULE,{q_str,Class,Str}) of
	{ok,undefined} ->
	    Str;
	{ok,FStr} ->
	    FStr
    end.


load_profile(File) when is_list(File) ->
    gen_server:call(?MODULE,{load_profile,File});
load_profile(default) ->
    gen_server:call(?MODULE,{load_profile,default}).

get_value(Key) ->
    case gen_server:call(?MODULE,{get_value,Key}) of
	{ok, Value} ->
	    Value;
	Other -> Other
    end.
    
set_value(Key,Value) ->
    gen_server:call(?MODULE,{set_value,Key,Value}).



state() ->
    gen_server:call(?MODULE,state).

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


									    
		

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------


handle_call({load_profile, File}, _From, State) when is_list(File) ->
    OldSettings = State#state.settings,
    case file:consult(File) of
	{ok, Terms} ->
	    TxtFmtStrs = case proplists:get_value(color_profile,Terms) of
			     undefined -> % undefined, load default profile
				 	?LOG:debug("Load default color profile"),
				 	format_strs(default_colors(ansi));
			     ColProf ->
				 	ProfDir = filename:dirname(File),
				 	ColProfFile = filename:join(ProfDir,ColProf),
				 	?LOG:debug("Use color profile ~p", [ColProfFile]),
				 case file:consult(ColProfFile) of
				     {ok,ColTerms} ->
					 	format_strs(ColTerms);
				     {error,Reason} ->
					 	?LOG:error("Error reading color profile ~p: ~p~n",[ColProfFile,Reason]),
					 	format_strs(default_colors(ansi)) % error,  use default color profile
				 end
			 end,
	    
 	    {reply, ok, State#state{settings=[{text_attr,TxtFmtStrs}|Terms] ++ OldSettings}};

	{error,Reason} ->
	    ?LOG:error("Error loading profile ~p: ~p ~n",[File,Reason]),
	    default_profile(State)
    end;

handle_call({load_profile, default}, _From, State) ->
    default_profile(State);

handle_call({q_str,Class,Str}, _From, State) ->
    DisableFlag = proplists:get_value(?FLAG_NO_COLOR,State#state.settings),
    TxtAttr = proplists:get_value(text_attr,State#state.settings),
    Res = case DisableFlag of 
	      true ->
		  	Str;
	      _ ->
			  case proplists:get_value(Class,TxtAttr) of
			      undefined ->
				  	undefined;
			      FmtStr ->
				  	lists:flatten(io_lib:format(FmtStr,[Str]))
			  end
	  end,
    {reply, {ok, Res}, State};

handle_call({get_value,Key}, _From, State) ->
    {reply, {ok, proplists:get_value(Key,State#state.settings)}, State};

handle_call({set_value,Key,Value}, _From, State) ->
    OldSettings = State#state.settings,
    NewSettings = [ {Key,Value} | proplists:delete(Key,OldSettings) ],
    {reply, ok, State#state{settings=NewSettings}};

handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(state, _From, State) ->
    {reply, {ok, State}, State}.


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

    
% map color_profile tuples to format strings
format_strs(TextAttrs) ->
    F = fun(AttrTuple) ->
		{Class,{TxtAttr,FgColor,BgColor}} = AttrTuple,

		% filter the 'none':s
		Attrs = lists:filter(fun(A) -> {_,V} = A, V =/= none end,
				     [{text_attr,TxtAttr},
				      {fg_color, FgColor},
				      {bg_color, BgColor}]),

		FmtStr = etcol:t([ { Attrs, "~ts"},
				   { [{text_attr,reset}], ""}
				 ]),
		{Class, FmtStr}
	end,
    lists:map(F,TextAttrs).


default_colors(ansi)->
    ?DEFAULT_COLORS.
    
default_profile(State) ->
    TxtFmtStrs = format_strs(default_colors(ansi)),
    OldSettings = State#state.settings,
    Settings=[{text_attrs,TxtFmtStrs}| OldSettings],
    {reply, ok, State#state{settings=Settings}}.

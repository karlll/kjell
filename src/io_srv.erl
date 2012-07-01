%%% @author karl l <karl@ninjacontrol.com>
%%% @doc
%%%
%%% @end

-module(io_srv).


%% {io_request, From, ReplyAs, Request}
%% {io_reply, ReplyAs, Reply}

%%% Request
% output request

%% {put_chars, Encoding, Characters}
%% {put_chars, Encoding, Module, Function, Args}
%% {put_chars, Characters}
%% {put_chars, Module, Function, Args}


%%    The server replies to the client with an io_reply where the Reply element is one of:

%% ok
%% {error, Error}


% input request

% {get_until, Encoding, Prompt, Module, Function, ExtraArgs}
%     Module, Function, ExtraArgs denotes a function and arguments to determine when enough data is written. 
%     The function should take two additional arguments, the last state, and a list of characters. The function should return one of:

%{done, Result, RestChars}
%{more, Continuation}

% The function should be called like apply(Module, Function, [ State, Data | ExtraArgs ]) 


% {get_chars, Encoding, Prompt, N}


% {get_line, Encoding, Prompt}



%% The server replies to the client with an io_reply where the Reply element is one of:

%% Data
%% eof
%% {error, Error}

%%  -  Data is the characters read, in either list or binary form (depending on the io_server mode, see below).
%%  -  Error describes the error to the client, which may do whatever it wants with it. The Erlang io-module typically returns it as is.
%%  -  eof is returned when input end is reached and no more data is available to the client process.

%% For backward compatibility the following Requests should also be handled by an io_server (these messages should not be present after R15B of OTP):

%% {get_until, Prompt, Module, Function, ExtraArgs}
%% {get_chars, Prompt, N}
%% {get_line, Prompt}

-export([start_link/1, init/0, loop/1]).

-export([out/4]).
-export([out/5, exec_proc/4]).
-record(state, 
	{
	}
       ).

start_link(Name) ->

    Pid = spawn_link(?MODULE,init,[]),
    true = register(Name,Pid),
    %% test
    %% unlink(Pid),
    %% 
    Pid.

init() ->
    
    ?MODULE:loop(#state{}).

loop(State) ->
    receive

	{io_request, From, ReplyAs, Request} ->

	    case req(Request,From,ReplyAs,State) of
		
		{ok,State} ->
		    ?MODULE:loop(State);
		{error,Reason,State} ->
		    exit

	    end;

	_Unknown ->
	    exit(unknown)
    end.


req({put_chars, Encoding, Characters},
    From, ReplyAs, State ) -> 
    Reply = start_worker(?MODULE,out,[Encoding,Characters]),

    From ! {io_reply, ReplyAs, Reply},

    {ok, State};

req({put_chars, Encoding, Module, Function, Args},
    From, ReplyAs, State) ->
    Reply = start_worker(?MODULE,out,[Encoding, Module, Function,Args]),

    From ! {io_reply, ReplyAs, Reply},
    
    {ok, State};

req({put_chars, Characters},
    From, ReplyAs, State) ->
    Reply = start_worker(?MODULE,out,[Characters]),

    From ! {io_reply, ReplyAs, Reply},

    {ok, State};

req({put_chars, Module, Function, Args},
    From, ReplyAs, State) ->
    Reply = start_worker(?MODULE,out,[Module, Function, Args]),

    From ! {io_reply, ReplyAs, Reply},

    {ok, State}.



%
% Output workers
%

out(Pid,Characters) ->

    try io:format("Got characters=~p",[Characters]) of
	_ -> Pid ! {ok, self()}
    catch
	Class:Exception ->
    	    Pid ! {error, self(), {Class,Exception}}
    end.
	     
	     

out(Pid,Encoding,Characters)->

    try io:format("Got Encoding=~p,Characters=~p",[Encoding,Characters]) of
	_ ->  Pid ! {ok, self()}

    catch
	Class:Exception ->
    	    Pid ! {Class, self(), Exception}

    end.

out(Pid,Module, Function,Args) ->
    % io:format("Calling Module=~p, Fun=~p w. Args = ~p~n",[Module,Function,Args]),
    try apply(Module, Function, Args) of
	Res -> io:format("~p",[Res]),
	       Pid ! {ok, self()}
    catch
	Class:Exception ->
	    Pid ! {Class, self(), Exception}
    end.

out(Pid,Encoding,Module, Function,Args) ->
   % io:format("Encoding = ~p . Calling Module=~p, Fun=~p w. Args = ~p~n",[Encoding,Module,Function,Args]),
 
   try apply(Module, Function, Args) of
   	    Res -> io:format("# ~s",[text:t(string,Res)]),
   		   Pid ! {ok, self()}
    catch

   	Class:Exception ->
   	    Pid ! {Class, self(), Exception}

    end.

    


%% req({get_until, Encoding, Prompt, Module, Function, ExtraArgs},
%%     From, ReplyAs, State) ->    
%% 		    ok;  

%% req({get_chars, Encoding, Prompt, N},
%%     From, ReplyAs, State) ->
%% 		    ok;


%% req({get_line, Encoding, Prompt},
%%     From, ReplyAs, State) ->
%%     ok;

%% req({get_until, Prompt, Module, Function, ExtraArgs},
%%     From, ReplyAs, State) ->
%%     ok.

%% req({get_chars, Prompt, N},
%%     From, ReplyAs, State) ->
%%     ok.

%% req({get_line, Prompt},
%%     From, ReplyAs, State) ->
%%     ok.




start_worker(Module,Fun,Args) ->
    Pid = spawn(?MODULE,exec_proc,[self(),Module,Fun,Args]),
    receive
	{ok, Pid} ->
	    ok;
	{ok, Pid, Result} ->
	    {ok,Result};
	{error, Pid, Reason} ->
	    {error, Reason};
	_ -> 
	    exit(unexpected_error)
    end.
	       



%% Fun should take at least 1 argument,
%% Fun(Pid), which is the invoking process to which
%% the result shall be sent (if any)
%%
%% The result should be
%% {ok,Pid,Result}
%% {error,Pid,Error}
%%
exec_proc(Pid,Module,Fun,Args)->
    process_flag(trap_exit,true),
    {ChildPid,Ref} = spawn_monitor(Module,Fun,[self() | Args ]),
    receive
	{'DOWN',Ref,process,ChildPid,normal} ->
	    Pid ! {ok, self()};
	{'DOWN',Ref,process,ChildPid,Error} ->
	    Pid ! {error,self(),Error};
	{ok,ChildPid} ->
	    receive 
		{'DOWN',Ref,process,ChildPid,normal} -> ok; % 
		_ -> Pid ! {error, self(), {unexpected_message}}
	    end,
	    Pid ! {ok,self()};
	{ok,ChildPid,Result} ->
	    receive 
		{'DOWN',Ref,process,ChildPid,normal} -> ok; % 
		_ -> Pid ! {error, self(), {unexpected_message}}
	    end,
	    Pid ! {ok,self(), Result};
	{error,ChildPid,Error} ->
	    receive 
		{'DOWN',Ref,process,ChildPid,normal} -> ok; % 
		_ -> Pid ! {error, self(), {unexpected_message}}
	    end,
	    Pid ! {error,self(),Error};	  

	_ ->
	    Pid ! {error,self(),{unknown_message}}
    end.

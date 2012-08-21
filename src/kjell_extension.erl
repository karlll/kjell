%%%-------------------------------------------------------------------
%%% @author karl l <karl@ninjacontrol.com>
%%% @doc
%%% kjell extensions
%%% @end
%%%-------------------------------------------------------------------
-module(kjell_extension).

-compile([export_all]).

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
    ets:new(ext_pts,[named_table]),
    ets:new(ext_desc,[named_table]),
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
	 
			     

activate({command,Cmd},InData)->
    Command = get_cmd_ext(Cmd),
    case Command of
	undefined ->
	    {error, undefined};
        CmdModFuns ->
	    apply_extension({command,CmdModFuns},InData)
    end;

activate(ExtPoint,InData)->
    Extensions = ets:lookup(ext_pts,ExtPoint),
    case Extensions of 
	[] ->
	    InData;
	Extensions ->
	    case point_type(ExtPoint) of
		single ->
		    apply_extension(hd(Extensions),InData);
		multiple ->
		    apply_extensions(hd(Extensions),InData);
		none ->
		    InData
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

init_extensions(ExtensionPath)->							     

    case load_extensions(ExtensionPath) of
	{ok, Exts} ->
	    ok = register_extensions(Exts);
	_Other ->
	    io:format("No extensions loaded.~n")
    end,
    ok.




point_type(shell_input_line) ->	single;
point_type(shell_output_line) -> single;
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

	    
    

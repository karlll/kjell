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

    NewRegTmp = lists:map(fun(P) -> 
				  {Point,ModFun,ExtPointDesc} = P,
				  case proplists:get_value(Point,ExtPtRegister) of
				      undefined -> %add new
					  {Point,{ModFun,ExtPointDesc}};
				      OldVal -> % add to existing
					  {Point,{[{ModFun,ExtPointDesc},OldVal]}}
				  end 
			  end, ExtsList),
    NR_Keys = proplists:get_keys(NewRegTmp),
    R_Keys = proplists:get_keys(ExtPtRegister),
						% Get all keys from R not in NR
    SubR = sets:subtract(sets:from_list(R_Keys),sets:from_list(NR_Keys)),
    SubRegister = [ {K,proplists:get_value(K,ExtPtRegister)} || K <- sets:to_list(SubR) ],
    NewRegister = lists:concat([NewRegTmp,SubRegister]),
    register_extensions(T,NewRegister,[ExtModDesc|ExtModDescList]);

register_extensions([],ExtPtRegister,ExtModDescList) ->
    ets:new(ext_pts,[named_table]),
    ets:new(ext_desc,[named_table]),
    lists:map(fun(Pt) -> {P,ExtMods} = Pt,
			 io:format("Inserting P = ~p , ExtMods = ~p~n",[P,ExtMods]),
			 ets:insert_new(ext_pts,{P,ExtMods})
	      end, ExtPtRegister),
    ets:insert_new(ext_desc,{loaded_exts,ExtModDescList}),
    ok.



activate(ExtPoint,InData)->
    Extensions = ets:lookup(ext_pts,ExtPoint),
    case point_type(ExtPoint) of
	single ->
	    apply_extension(hd(Extensions),InData);
	multiple ->
	    apply_extensions(Extensions,InData);
	none ->
	    InData
    end.


apply_extension(Extension,InData)->							
    {ExtPt,{{ExtMod,Fun}, Desc}} = Extension,
    {ok, Res} = apply(ExtMod,Fun,[InData]),
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
point_type(command) -> multiple;
point_type(Other) -> none.


t() ->
    A = load_extensions("."),
    B = register_extensions(A),
    ok.

%%%-------------------------------------------------------------------
%%% @author karl l <karl@ninjacontrol.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(text).
-export([test/0, t/2]).

%-define(ESC,0x1B).

%% Font Set G0		<ESC>(
%% Set default font.
%% Font Set G1		<ESC>)
%% Set alternate font.

%% Set Attribute Mode	<ESC>[{attr1};...;{attrn}m


text_attr(reset)-> 0;
text_attr(bright)-> 1;
text_attr(dim)-> 2;
text_attr(underscore)-> 3;
text_attr(blink)-> 4;
text_attr(reverse)-> 5;
text_attr(hidden)-> 6.


fg_color(black) -> 30;	
fg_color(red) -> 31;
fg_color(green) -> 32;
fg_color(yellow) -> 33;
fg_color(blue) -> 34;
fg_color(magenta) -> 35;
fg_color(cyan) -> 36;
fg_color(white) -> 37.

bg_color(black) ->  40;
bg_color(red) ->  41;
bg_color(green) ->  42;
bg_color(yellow) ->  43;
bg_color(blue) ->  44;
bg_color(magenta) ->  45;
bg_color(cyan) ->  46;
bg_color(white) ->  47.


test() ->
    io:format("\e[31mtesting\e[0m").




%
%
% "This is a test string"
%
%

t(Class,Str) ->
    {Attr,Fg,Bg} = p(Class),
    AttrC = case Attr of
		none ->
		    "";
		A -> 
		    "[" ++ integer_to_list(text_attr(A)) ++ "m"
	    end,
    FgC = case Fg of
		none ->
		    "";
		F -> 
		    "[" ++ integer_to_list(fg_color(F)) ++ "m"
	    end,
    BgC = case Bg of
		none ->
		    "";
		B -> 
		    "[" ++ integer_to_list(bg_color(B)) ++ "m"
	    end,
    "\e" ++ AttrC ++ FgC ++ BgC ++ Str ++ "\e[0m".
    
			     
		      
    

p(Class,text_attrib) ->
    {Attrib,_,_} =  p(Class),
    Attrib;
p(Class,foreground) ->
    {_,Foreground,_} = p(Class),
    Foreground;
p(Class,background) ->
    {_,_,Background} = p(Class),
    Background.
p(Class) ->
    proplists:get_value(Class,profile()).


profile() ->
    [
     %% { class, {text_attrib, foreground, background}}
     {string,{bright,yellow,none}},
     {digits,{bright,green,none}},
     {keyword,{bright,cyan,none}}
    ].


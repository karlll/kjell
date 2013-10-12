%%%-------------------------------------------------------------------
%%% @author karl l <karl@ninjacontrol.com>
%%% @doc Small terminal color library
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(etcol).

-define(ANSI_CTRL_CHARS,"\e[").
-define(ANSI_CLEAR,"\e[0m").

-export([t/1,test/0]).

-export([text_attr/2,fg_color/2,bg_color/2]).

t(Lines) ->
    t_a(Lines). % only ANSI, for now.


ctrl_sequence(ansi,Attrs) ->
    ctrl_sequence(ansi,Attrs,[]).
ctrl_sequence(ansi,[{F,Attr}|T],Acc) -> 
    ctrl_sequence(ansi,T,[apply(?MODULE,F,[ansi|[Attr]])| Acc]);
ctrl_sequence(ansi,[],Acc) ->    
    lists:concat([ ?ANSI_CTRL_CHARS , string:join(lists:reverse(Acc),";") , "m"]).


t_a(Lines) ->
    t_a(Lines,[]).
t_a([Line|T], Acc) ->
    {Attrs,Str} = Line,
    t_a(T,[string:concat(ctrl_sequence(ansi,Attrs),Str)|Acc]);
t_a([],Acc) ->
    lists:flatten(lists:reverse(Acc)).



text_attr(ansi,reset)-> "0";
text_attr(ansi,bright)-> "1";
text_attr(ansi,dim)-> "2";
text_attr(ansi,underscore)-> "4";
text_attr(ansi,blink)-> "5";
text_attr(ansi,reverse)-> "7";
text_attr(ansi,hidden)-> "8";
text_attr(ansi,alt_font_1)-> "11";
text_attr(ansi,alt_font_2)-> "12";
text_attr(ansi,alt_font_3)-> "13";
text_attr(ansi,alt_font_4)-> "14";
text_attr(ansi,alt_font_5)-> "15";
text_attr(ansi,alt_font_6)-> "16";
text_attr(ansi,alt_font_7)-> "17";
text_attr(ansi,alt_font_8)-> "18";
text_attr(ansi,alt_font_9)-> "19";
text_attr(ansi,framed)-> "51";
text_attr(ansi,encircled)-> "52";
text_attr(ansi,overlined)-> "52".


fg_color(ansi,black) -> "30";   
fg_color(ansi,red) -> "31";
fg_color(ansi,green) -> "32";
fg_color(ansi,yellow) -> "33";
fg_color(ansi,blue) -> "34";
fg_color(ansi,magenta) -> "35";
fg_color(ansi,cyan) -> "36";
fg_color(ansi,white) -> "37";
fg_color(ansi,default_fg) -> "39".

bg_color(ansi,black) ->  "40";
bg_color(ansi,red) ->  "41";
bg_color(ansi,green) ->  "42";
bg_color(ansi,yellow) ->  "43";
bg_color(ansi,blue) ->  "44";
bg_color(ansi,magenta) ->  "45";
bg_color(ansi,cyan) ->  "46";
bg_color(ansi,white) ->  "47";
bg_color(ansi,default_bg) -> "49".

% ****************************************************************

test() ->
    
    Colors = [ black, red, green, yellow, blue, magenta, cyan, white],
    Attrs = [ bright, dim, underscore, blink, reverse, hidden],
    
    AllCombos = [ {A,F,B} || A <- Attrs, F <- Colors, B <- lists:reverse(Colors) ],
    
    Disp = fun(AttrTuple) ->
            {Attr,Fg,Bg} = AttrTuple,
            {[{text_attr,Attr},
                    {fg_color,Fg},
                    {bg_color,Bg}], lists:concat([atom_to_list(Fg), 
                        " on ", 
                        atom_to_list(Bg), 
                        " (",
                        atom_to_list(Attr),
                        ")", ?ANSI_CLEAR, "\n"])}
    end,
    
    AllCombosAttr = lists:map(Disp,AllCombos),
    AllCombosStr = t_a(AllCombosAttr),
    io:format("~ts",[AllCombosStr]).


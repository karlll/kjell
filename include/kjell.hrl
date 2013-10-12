%%% @author karl l <karl@ninjacontrol.com>
%%% @doc
%%%
%%% @end


-define(VERSION,"0.2.0").
-define(BANNER,"Kjell v." ++ ?VERSION).

-define(DEFAULT_COLORS,
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
      {prompt,{none,green,none}},

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
      {pipe,{none,white,none}},

      % Needed for kjell-prompt extension
      {prompt_start,{reverse,yellow,none}},
      {prompt_text,{dim,black,yellow}},
      {prompt_end,{none,yellow,none}}

    ]).


-define(EDLIN,edlin).

% logging

% log level definitions
-define(LL_OFF,-1).
-define(LL_ERROR, 1).
-define(LL_WARN, 2).
-define(LL_INFO, 3).
-define(LL_DEBUG, 4).

% log level
-define(LL,?LL_INFO).




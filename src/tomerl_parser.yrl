Nonterminals
  toml
  root_section section_list
  section section_name section_body
  key key_component value key_value
  array value_list nls maybe_space
  inline_table inline_kv_list
.

Terminals
  '[' ']' '{' '}'
  '.' '=' ','
  nl space
  datetime_tz local_datetime local_date local_time bool maybe_key float integer
  bare_key basic_string literal_string basic_string_ml literal_string_ml
.

Rootsymbol toml.

%%%---------------------------------------------------------------------------

toml -> root_section              : '$1'.
toml -> root_section section_list : lists:flatten(['$1', lists:reverse('$2')]).
toml ->              section_list : lists:flatten(lists:reverse('$1')).

root_section -> section_body : [{table, 1, [], '$1'}].

section_list -> section : ['$1'].
section_list -> section_list section : ['$2' | '$1'].

%%----------------------------------------------------------
%% sections (a.k.a. tables)

% XXX: the only places where 'space' is reported is before "[" or "]"
section -> maybe_space '[' section_name maybe_space ']' nl :
  [{table, line('$2'), '$3', #{}}].
section -> maybe_space '[' section_name maybe_space ']' nl section_body :
  [{table, line('$2'), '$3', '$7'}].
section -> maybe_space '[' '[' section_name maybe_space ']' ']' nl :
  [{array_table, line('$2'), '$4', #{}}].
section -> maybe_space '[' '[' section_name maybe_space ']' ']' nl section_body :
  [{array_table, line('$2'), '$4', '$9'}].

section_name -> key : '$1'.
% section_name -> section_name '.' key : ['$3' | '$1'].

section_body -> nl : #{}.
section_body -> key_value nl : tomerl_parser_util:to_map('$1', #{}).
section_body -> section_body nl : '$1'.
section_body -> section_body key_value nl : tomerl_parser_util:to_map('$2', '$1').

%%----------------------------------------------------------

key_value -> key '=' value : {key_value, '$1', '$3'}.

%%----------------------------------------------------------

key -> key_component         : '$1'.
key -> key_component '.' key : '$1' ++ '$3'.

key_component -> bare_key       : key_string_values('$1').
key_component -> basic_string   : key_string_values('$1').
key_component -> literal_string : key_string_values('$1').
key_component -> maybe_key      : key_string_values('$1').
key_component -> bool           : key_string_values('$1').

%%----------------------------------------------------------

value -> basic_string      : string_value('$1').
value -> basic_string_ml   : string_value('$1').
value -> literal_string    : string_value('$1').
value -> literal_string_ml : string_value('$1').
value -> maybe_key         : value('$1').
value -> integer           : value('$1').
value -> float             : value('$1').
value -> bool              : value('$1').
value -> datetime_tz       : value('$1').
value -> local_datetime    : value('$1').
value -> local_date        : value('$1').
value -> local_time        : value('$1').
value -> array        : '$1'.
value -> inline_table : '$1'.

%%----------------------------------------------------------

array -> maybe_space '[' nls                        maybe_space ']' :
  {[], line('$2')}.
array -> maybe_space '[' nls value_list nls         maybe_space ']' :
  {'$4', line('$2')}.
array -> maybe_space '[' nls value_list nls ',' nls maybe_space ']' :
  {'$4', line('$2')}.

value_list -> value : [element(1, '$1')].
value_list -> value_list nls ',' nls value : [element(1, '$5') | '$1'].

nls -> '$empty'.
nls -> nls nl.

% XXX: the only places where 'space' is reported is before "[" or "]"
maybe_space -> '$empty'.
maybe_space -> space.

%%----------------------------------------------------------

inline_table -> '{' nls '}' : {#{}, line('$1')}.
inline_table -> '{' nls inline_kv_list nls '}' : {'$3', line('$1')}.
inline_table -> '{' nls inline_kv_list ',' nls'}' : {'$3', line('$1')}.

inline_kv_list -> key_value : tomerl_parser_util:to_map('$1', #{}).
inline_kv_list -> inline_kv_list ',' nls key_value : tomerl_parser_util:to_map('$4', '$1').

%%----------------------------------------------------------

%%%---------------------------------------------------------------------------

Erlang code.

-ignore_xref([
    format_error/1,
    parse_and_scan/1
]).

line({_, Line}) ->
  Line;
line({_, Line, _}) ->
  Line.

value({maybe_key, Line, {_Type, _RawValue, ParsedValue}}) ->
  {ParsedValue, Line};

value({_TermName, Line, Value}) ->
  {Value, Line}.

-spec string_value(_) -> {binary(), pos_integer()}.
string_value({maybe_key, Line, {_Type, RawValue, _}}) ->
  {tomerl_parser_util:to_binary(RawValue), Line};
string_value({_, Line, Val}) ->
  {tomerl_parser_util:to_binary(Val), Line}.

-spec key_string_values(_) -> [binary()].
key_string_values({maybe_key, _, _} = Token) ->
  % Special handling for maybe_keys as they can contain dots
  {Str, _} = string_value(Token),
  string:lexemes(Str, ".");
key_string_values(Token) ->
  {Str, _} = string_value(Token),
  [Str].

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang

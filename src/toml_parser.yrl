%%%---------------------------------------------------------------------------

Header
  "%%% @private"
.

Nonterminals
  toml
  root_section section_list
  section section_name section_body
  key value key_value
  array value_list nls maybe_space
  inline_table inline_kv_list
.

Terminals
  '[' ']' '{' '}'
  '.' '=' ','
  nl space
  datetime_tz local_datetime local_date local_time
  bool key_float float key_integer integer bare_key
  basic_string literal_string
  basic_string_ml literal_string_ml
.

Rootsymbol toml.

%%%---------------------------------------------------------------------------

toml -> root_section              : '$1'.
toml -> root_section section_list : lists:flatten(['$1', lists:reverse('$2')]).
toml ->              section_list : lists:flatten(lists:reverse('$1')).

root_section -> section_body : lists:reverse('$1').

section_list -> section : ['$1'].
section_list -> section_list section : ['$2' | '$1'].

%%----------------------------------------------------------
%% sections (a.k.a. tables)

% XXX: the only places where 'space' is reported is before "[" or "]"
section -> maybe_space '[' section_name maybe_space ']' nl section_body :
  [{table, line('$2'), lists:reverse('$3')} | lists:reverse('$7')].
section -> maybe_space '[' '[' section_name maybe_space ']' ']' nl section_body :
  [{array_table, line('$2'), lists:reverse('$4')} | lists:reverse('$9')].

section_name -> key : ['$1'].
section_name -> section_name '.' key : ['$3' | '$1'].

section_body -> nl : [].
section_body -> key_value nl : ['$1'].
section_body -> section_body nl : '$1'.
section_body -> section_body key_value nl : ['$2' | '$1'].

%%----------------------------------------------------------

key_value -> key '=' value : {key, '$1', '$3'}.

%%----------------------------------------------------------

key -> basic_string : value('$1').
key -> bare_key     : value('$1').
key -> bool         : atom_to_list(value('$1')).
key -> key_integer  : value('$1', raw).
key -> key_float    : value('$1', raw).
key -> local_date   : value('$1', raw).

%%----------------------------------------------------------

value -> basic_string      : {line('$1'), value('$1')}.
value -> basic_string_ml   : {line('$1'), value('$1')}.
value -> literal_string    : {line('$1'), value('$1')}.
value -> literal_string_ml : {line('$1'), value('$1')}.
value -> key_integer       : {line('$1'), value('$1', parsed)}.
value -> key_float         : {line('$1'), value('$1', parsed)}.
value -> integer           : {line('$1'), value('$1')}.
value -> float             : {line('$1'), value('$1')}.
value -> bool              : {line('$1'), value('$1')}.
value -> datetime_tz       : {line('$1'), {datetime, element(1, value('$1')), element(2, value('$1'))}}.
value -> local_datetime    : {line('$1'), {datetime, value('$1')}}.
value -> local_date        : {line('$1'), {date, value('$1', parsed)}}.
value -> local_time        : {line('$1'), {time, value('$1')}}.
value -> array        : '$1'.
value -> inline_table : '$1'.

%%----------------------------------------------------------

array -> maybe_space '[' nls                        maybe_space ']' :
  {line('$2'), {array, []}}.
array -> maybe_space '[' nls value_list nls         maybe_space ']' :
  {line('$2'), {array, lists:reverse('$4')}}.
array -> maybe_space '[' nls value_list nls ',' nls maybe_space ']' :
  {line('$2'), {array, lists:reverse('$4')}}.

value_list -> value : ['$1'].
value_list -> value_list nls ',' nls value : ['$5' | '$1'].

nls -> '$empty'.
nls -> nls nl.

% XXX: the only places where 'space' is reported is before "[" or "]"
maybe_space -> '$empty'.
maybe_space -> space.

%%----------------------------------------------------------

inline_table -> '{' '}' :
  {line('$1'), {inline_table, []}}.
inline_table -> '{' inline_kv_list '}' :
  {line('$1'), {inline_table, lists:reverse('$2')}}.

% NOTE: as per spec and reference grammar, trailing comma is not allowed
inline_kv_list -> key_value : ['$1'].
inline_kv_list -> inline_kv_list ',' key_value : ['$3' | '$1'].

%%----------------------------------------------------------

%%%---------------------------------------------------------------------------

Erlang code.

line({_TermName, Line}) ->
  Line;
line({_TermName, Line, _Value}) ->
  Line.

value({_TermName, _Line, {RawValue, _ParsedValue}}, raw = _Element) ->
  RawValue;
value({_TermName, _Line, {_RawValue, ParsedValue}}, parsed = _Element) ->
  ParsedValue.

value({_TermName, _Line, Value}) ->
  Value.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang

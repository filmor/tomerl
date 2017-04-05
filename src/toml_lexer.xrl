% @private
% Pity the "private" marker doesn't work at all.

Definitions.

SP = [\s\t]
COMMENT = #[^\n]*

HEX = [0-9a-fA-F]
HEX4 = {HEX}{HEX}{HEX}{HEX}
HEX8 = {HEX}{HEX}{HEX}{HEX}{HEX}{HEX}{HEX}{HEX}
CHAR = [^\\"\x00-\x1f]
ESC_CHAR = \\[btnfr"\\\\]
U4 = \\u{HEX4}
U8 = \\U{HEX8}

BARE_KEY = [a-zA-Z0-9_-]+
LITERAL_STRING = '[^\']*'
BASIC_STRING = "({CHAR}|{ESC_CHAR}|{U4}|{U8})*"
BASIC_STRING_ML = """("?"?({CHAR}|\r?\n|\\{SP}*\r?\n|{ESC_CHAR}|{U4}|{U8}))*(\\{SP}+)?"""
LITERAL_STRING_ML = '''('?'?[^'])*'''

POS_INTEGER = ([0-9]|[1-9](_?[0-9])+)
INTEGER = [+-]?{POS_INTEGER}
KEY_INTEGER = -?{POS_INTEGER}

FRACTION = \.[0-9](_?[0-9])*
EXPONENT = [eE]{INTEGER}
FLOAT = {INTEGER}({FRACTION}|{FRACTION}{EXPONENT}|{EXPONENT})
KEY_FLOAT = -?{POS_INTEGER}[eE]-?{POS_INTEGER}

YEAR  = ([0-9][0-9][0-9][0-9])
MONTH = (0[1-9]|1[0-2])
DAY   = (0[1-9]|[12][0-9]|3[01])
HOUR = ([01][0-9]|2[0-3])
MIN  = ([0-5][0-9])
SEC  = ([0-5][0-9]|60)
DATE = {YEAR}-{MONTH}-{DAY}
TIME = {HOUR}:{MIN}:{SEC}(\.[0-9]+)?
TZOFFSET = (Z|[+-]{HOUR}:{MIN})

%%%---------------------------------------------------------------------------

Rules.

% NOTE: to tell apart "[[ foo ]]" (valid) and "[ [ foo ] ]" (invalid), spaces
% preceding a bracket need to be reported, but to simplify grammar in general,
% they get skipped in all the other places
{SP}+\[ : {token, {space, TokenLine}, "["}.
{SP}+\] : {token, {space, TokenLine}, "]"}.
\[ : {token, {'[', TokenLine}}.
\] : {token, {']', TokenLine}}.
\{ : {token, {'{', TokenLine}}.
\} : {token, {'}', TokenLine}}.
\. : {token, {'.', TokenLine}}.
=  : {token, {'=', TokenLine}}.
,  : {token, {',', TokenLine}}.
\r?\n : {token, {nl, TokenLine}}.

{DATE}T{TIME}{TZOFFSET} : {token, {datetime_tz, TokenLine, datetime_tz(TokenChars)}}.
{DATE}T{TIME} : {token, {local_datetime, TokenLine, local_datetime(TokenChars)}}.
% local date is a valid bare key
{DATE} : {token, {local_date, TokenLine, {TokenChars, local_date(TokenChars)}}}.
{TIME} : {token, {local_time, TokenLine, local_time(TokenChars)}}.

% these two are valid bare keys
true  : {token, {bool, TokenLine, true}}.
false : {token, {bool, TokenLine, false}}.

{KEY_FLOAT} : {token, {key_float, TokenLine, {TokenChars, to_float(TokenChars)}}}.
{KEY_INTEGER} : {token, {key_integer, TokenLine, {TokenChars, to_integer(TokenChars)}}}.
{FLOAT} : {token, {float, TokenLine, to_float(TokenChars)}}.
{INTEGER} : {token, {integer, TokenLine, to_integer(TokenChars)}}.
{BARE_KEY} : {token, {bare_key, TokenLine, TokenChars}}.

{BASIC_STRING}   : {token, {basic_string, TokenLine, basic_string(TokenChars)}}.
{LITERAL_STRING} : {token, {literal_string, TokenLine, literal_string(TokenChars)}}.
{BASIC_STRING_ML}   : {token, {basic_string_ml, TokenLine, basic_string_ml(TokenChars)}}.
{LITERAL_STRING_ML} : {token, {literal_string_ml, TokenLine, literal_string_ml(TokenChars)}}.

{SP}+     : skip_token.
{COMMENT} : skip_token.

%%%---------------------------------------------------------------------------

Erlang code.

-export([tokenize/1]).

%% @doc Lexer entry point.

-spec tokenize(string() | binary() | iolist()) ->
  {ok, [term()], integer()} | {error, term(), integer()}.

tokenize(String) ->
  string(unicode:characters_to_list([String, $\n])).

%%----------------------------------------------------------

to_integer("+" ++ String) ->
  to_integer(String);
to_integer("-" ++ String) ->
  -to_integer(String);
to_integer(String) ->
  list_to_integer([C || C <- String, C /= $_]).

to_float(String) ->
  StripUnderscore = [C || C <- String, C /= $_],
  case string:chr(StripUnderscore, $.) of
    0 ->
      % fix the float for Erlang's parsing function by inserting the missing
      % fraction part just before "e" marker
      % NOTE: we're here because either "." or "[eE]" was found in the number,
      % and we know the "." is missing
      [A, B] = string:tokens(StripUnderscore, "eE"),
      list_to_float(A ++ ".0e" ++ B);
    _ ->
      list_to_float(StripUnderscore)
  end.

datetime_tz(String) ->
  StrLen = length(String),
  case lists:last(String) of
    $Z ->
      Timezone = "Z",
      {local_datetime(string:substr(String, 1, StrLen - 1)), Timezone};
    _ ->
      Timezone = string:substr(String, 1 + StrLen - 6, 6),
      {local_datetime(string:substr(String, 1, StrLen - 6)), Timezone}
  end.

local_datetime(String) ->
  [Date, Time] = string:tokens(String, "T"),
  {local_date(Date), local_time(Time)}.

local_date(String) ->
  [Y, M, D] = string:tokens(String, "-"),
  {list_to_integer(Y), list_to_integer(M), list_to_integer(D)}.

local_time(String) ->
  [HH, MM, SS] = string:tokens(String, ":"),
  case string:tokens(SS, ".") of
    [_] ->
      {list_to_integer(HH), list_to_integer(MM), list_to_integer(SS)};
    [S, _Frac] ->
      % TODO: encode `Frac'
      {list_to_integer(HH), list_to_integer(MM), list_to_integer(S)}
  end.

literal_string(String) ->
  string:substr(String, 2, length(String) - 2).

literal_string_ml(String) ->
  case string:substr(String, 4, length(String) - 6) of
    "\r\n" ++ Result -> Result;
    "\n" ++ Result -> Result;
    Result -> Result
  end.

basic_string(String) ->
  esc_codes(string:substr(String, 2, length(String) - 2)).

basic_string_ml(String) ->
  case string:substr(String, 4, length(String) - 6) of
    "\r\n" ++ Result -> esc_codes(Result);
    "\n" ++ Result -> esc_codes(Result);
    Result -> esc_codes(Result)
  end.

esc_codes("") -> "";
esc_codes("\\b"  ++ Rest) -> [$\b | esc_codes(Rest)];
esc_codes("\\t"  ++ Rest) -> [$\t | esc_codes(Rest)];
esc_codes("\\n"  ++ Rest) -> [$\n | esc_codes(Rest)];
esc_codes("\\f"  ++ Rest) -> [$\f | esc_codes(Rest)];
esc_codes("\\r"  ++ Rest) -> [$\r | esc_codes(Rest)];
esc_codes("\\\"" ++ Rest) -> [$\" | esc_codes(Rest)];
esc_codes("\\\\" ++ Rest) -> [$\\ | esc_codes(Rest)];
esc_codes("\\u"  ++ [C1, C2, C3, C4 | Rest]) ->
  [u([C4, C3, C2, C1]) | esc_codes(Rest)];
esc_codes("\\U"  ++ [C1, C2, C3, C4, C5, C6, C7, C8 | Rest]) ->
  [u([C8, C7, C6, C5, C4, C3, C2, C1]) | esc_codes(Rest)];
esc_codes("\\" ++ Rest) ->
  % XXX: "dangling" backslash can only occur in multiline basic strings
  % (regexps are designed so), so this clause will never be called for basic
  % string
  esc_codes(skip_spaces(Rest));
esc_codes([C | Rest]) ->
  [C | esc_codes(Rest)].

skip_spaces(" " ++ Rest) -> skip_spaces(Rest);
skip_spaces("\t" ++ Rest) -> skip_spaces(Rest);
skip_spaces("\n" ++ Rest) -> skip_spaces(Rest);
skip_spaces("\r\n" ++ Rest) -> skip_spaces(Rest);
skip_spaces(String) -> String.

u([]) -> 0;
u([C | Rest]) when C >= $0 andalso C =< $9 ->      (C - $0) + 16 * u(Rest);
u([C | Rest]) when C >= $A andalso C =< $F -> (10 + C - $A) + 16 * u(Rest);
u([C | Rest]) when C >= $a andalso C =< $f -> (10 + C - $a) + 16 * u(Rest).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang

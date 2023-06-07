-module(tomerl_datetime).

-ignore_xref([
    format/1,
    millisecond/1,
    fractional/1,
    new_time/4,
    offset/1,
    to_calendar/1,
    type/1
]).
-export([
    new_time/3,
    new_time/4,
    new_time/5,
    new_date/3,
    new_datetime/2,

    with_offset/2,

    to_calendar/1,
    millisecond/1,
    fractional/1,
    offset/1,

    type/1,
    format/1
]).

-type year() :: 1000..9999.
-type month() :: 1..12.
-type day() :: 1..31.
-type hour() :: 0..23.
-type minute() :: 0..59.
-type second() :: 0..60.
-type millisecond() :: 0..999.

-type offset() :: z | integer().
% Timezone offset in minutes or `z'

-record(date, {
    year :: year(),
    month :: month(),
    day :: day()
}).

-record(time, {
    hour :: hour(),
    minute :: minute(),
    second :: second()
}).

-record(time_frac, {
    hour :: hour(),
    minute :: minute(),
    second :: second(),
    fractional :: non_neg_integer(),
    exponent :: pos_integer()
}).

-record(datetime, {
    date :: date(),
    time :: time()
}).

-record(datetime_offset, {
    date :: date(),
    time :: time(),
    offset :: offset()
}).

-opaque date() :: #date{}.
-opaque time() :: #time{} | #time_frac{}.
-opaque datetime() :: #datetime{}.
-opaque datetime_offset() :: #datetime_offset{}.
-type t() :: date() | time() | datetime() | datetime_offset().

-export_type([
    date/0,
    time/0,
    datetime/0,
    datetime_offset/0,
    year/0,
    month/0,
    day/0,
    hour/0,
    minute/0,
    second/0,
    millisecond/0,
    offset/0,
    t/0
]).

-spec new_time(hour(), minute(), second()) -> time().
new_time(H, M, S) ->
    #time{hour = H, minute = M, second = S}.

-spec new_time(hour(), minute(), second(), millisecond()) -> time().
new_time(H, M, S, Ms) ->
    new_time(H, M, S, Ms, 3).

-spec new_time(hour(), minute(), second(), non_neg_integer(), pos_integer()) -> time().
new_time(H, M, S, Frac, FracExp) ->
    #time_frac{
        hour = H,
        minute = M,
        second = S,
        fractional = Frac,
        exponent = FracExp
    }.

-spec new_date(year(), month(), day()) -> date().
new_date(Y, M, D) ->
    #date{year = Y, month = M, day = D}.

-spec new_datetime(date(), time()) -> datetime().
new_datetime(Date, Time) ->
    #datetime{date = Date, time = Time}.

-spec new_datetime(date(), time(), offset()) -> datetime_offset().
new_datetime(Date, Time, Offset) ->
    #datetime_offset{date = Date, time = Time, offset = Offset}.

-spec with_offset(datetime(), offset()) -> datetime_offset().
with_offset(#datetime{date = Date, time = Time}, Offset) ->
    new_datetime(Date, Time, Offset).

-spec type(_) -> time | date | datetime | datetime_offset | undefined.
type(#time{}) ->
    time;
type(#time_frac{}) ->
    time;
type(#date{}) ->
    date;
type(#datetime{}) ->
    datetime;
type(#datetime_offset{}) ->
    datetime_offset;
type(_) ->
    undefined.

%% @doc Convert a tomerl date, time, or datetime object to the format used by
%%      Erlang's calendar module, dropping the timezone offset and millisecond
%%      information
-spec to_calendar
    (time()) -> calendar:time();
    (date()) -> calendar:date();
    (datetime()) -> calendar:datetime();
    (datetime_offset()) -> calendar:datetime().
to_calendar(#time{hour = H, minute = M, second = S}) ->
    {H, M, S};
to_calendar(#time_frac{hour = H, minute = M, second = S}) ->
    {H, M, S};
to_calendar(#date{year = Y, month = M, day = D}) ->
    {Y, M, D};
to_calendar(#datetime{date = Date, time = Time}) ->
    {to_calendar(Date), to_calendar(Time)};
to_calendar(#datetime_offset{date = Date, time = Time}) ->
    {to_calendar(Date), to_calendar(Time)}.

%% @doc Get the timezone offset of a datetime
-spec offset
    (datetime_offset()) -> offset();
    (datetime()) -> undefined.
offset(#datetime_offset{offset = Offset}) ->
    Offset;
offset(#datetime{}) ->
    undefined.

%% @doc Get the millisecond information of an object
-spec millisecond(datetime() | datetime_offset() | time()) -> millisecond().
millisecond(#datetime{time = Time}) ->
    millisecond(Time);
millisecond(#datetime_offset{time = Time}) ->
    millisecond(Time);
millisecond(#time{}) ->
    0;
millisecond(#time_frac{fractional = Frac, exponent = Exp}) ->
    RefExp = 3,
    if
        Exp >= 1, Exp < RefExp ->
            Frac * int_pow(10, RefExp - Exp);
        Exp =:= RefExp ->
            Frac;
        Exp > RefExp ->
            Frac div int_pow(10, Exp - RefExp)
    end.

-spec fractional(datetime() | datetime_offset() | time()) ->
    {FractionalPart :: non_neg_integer(), Exponent :: pos_integer()}.
fractional(#datetime{time = Time}) ->
    fractional(Time);
fractional(#datetime_offset{time = Time}) ->
    fractional(Time);
fractional(#time{}) ->
    {0, 1};
fractional(#time_frac{fractional = Frac, exponent = Exp}) ->
    {Frac, Exp}.

%% @doc Format the date, time, or datetime in ISO8601 format
-spec format(T :: t()) -> iolist().
format(#time{hour = H, minute = M, second = S}) ->
    io_lib:format("~2..0B:~2..0B:~2..0B", [H, M, S]);
format(#time_frac{hour = H, minute = M, second = S, fractional = Frac, exponent = Exp}) ->
    Exp1 = max(3, Exp),
    Frac1 = round(Frac * math:pow(10, Exp1 - Exp)),
    Format = ("~2..0B:~2..0B:~2..0B.~" ++ integer_to_list(Exp1) ++ "..0B"),
    io_lib:format(Format, [H, M, S, Frac1]);
format(#date{year = Y, month = M, day = D}) ->
    io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D]);
format(#datetime{date = Date, time = Time}) ->
    [format(Date), $T, format(Time)];
format(#datetime_offset{date = Date, time = Time, offset = z}) ->
    [format(Date), $T, format(Time), $Z];
format(#datetime_offset{date = Date, time = Time, offset = Offset}) ->
    Sign =
        if
            Offset < 0 -> $-;
            true -> $+
        end,
    Offset1 = abs(Offset),
    Minutes = Offset1 rem 60,
    Hours = Offset1 div 60,
    Formatted = io_lib:format("~2..0B:~2..0B", [Hours, Minutes]),
    [format(Date), $T, format(Time), Sign, Formatted].

-spec int_pow(Base :: integer(), Exponent :: non_neg_integer()) -> integer().
int_pow(B, E) when B =:= 0 andalso E =:= 0; E < 0 ->
    error(badarg);
int_pow(_B, 0) ->
    1;
int_pow(B, 1) ->
    B;
int_pow(B, E) when E rem 2 =:= 0 ->
    int_pow(B * B, E div 2);
int_pow(B, E) ->
    B * int_pow(B, E - 1).

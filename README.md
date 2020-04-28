TOML Parser for Erlang
======================

[![CI](https://github.com/filmor/tomerl/workflows/CI/badge.svg)](https://github.com/filmor/tomerl/actions)
[![Hex](https://img.shields.io/hexpm/v/tomerl)](https://hex.pm/packages/tomerl)
[![TOML](https://img.shields.io/badge/TOML-1.0.0--rc1-blue)](https://github.com/toml-lang/toml)

`tomerl` is an Erlang library for parsing
[TOML 1.0.0-rc1](https://github.com/toml-lang/toml) data, forked from [toml](https://github.com/dozzie/toml)
by Stanisław Klekot.

The documentation at [Hexdocs](https://hexdocs.pm/tomerl) is updated on release, it can be generated locally via `rebar3 edoc`.

Usage Example
-------------

Assuming an input file called `config.toml` with the following content:

```toml
lipsum = "lorem ipsum dolor sit amet"

[apples]
count = 2

[berry.black]
has_some = true
```

the data can be read in Erlang like this:

```erlang
{ok, Data} = tomerl:read_file("config.toml").

>>> Data = #{
    <<"lipsum">> => <<"lorem ipsum dolor sit amet">>,
    <<"apples">> => #{ <<"count">> => 2 },
    <<"berry">> => #{ <<"black">> => #{ <<"has_some">> => true }}
}.
```

To access the data, there is a simple `get` function that accepts lists of strings, binaries and atoms:

```erlang
{ok, true} = tomerl:get(Data, [berry, black, has_some]),
{ok, 2} = tomerl:get(Data, ["apples", <<"count">>]),
{error, not_found} = tomerl:get(Data, [something, undefined]).
```

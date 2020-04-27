TOML Parser for Erlang
======================

`tomerl` is an Erlang library application for parsing
[TOML](https://github.com/toml-lang/toml) data. It supports parsing version 1.0.0-rc1 of the TOML specification.

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

Documentation
-------------

`toml` is documented using EDoc. A local copy is generated using `rebar3 doc`
in the  `./doc/` directory. The documentation at 
at <https://hexdocs.pm/toml-erlang> is update on release.

Known limitations
-----------------

* Types conveying time only store second precision (fraction of a second is
  truncated)

Contact and License
-------------------

This library is based on the initial work by Stanislaw Klekot <dozzie at jarowit.net>.
It is distributed under the 3-clause BSD license. Check the COPYING file for
details.

[toml]: https://github.com/toml-lang/toml

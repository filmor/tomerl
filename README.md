TOML parser for Erlang
======================

`toml` is an Erlang library application for parsing [TOML][toml] configuration
language. It supports parsing 0.4.0 version of the TOML specification.

Usage example
-------------

Let's assume an input file called `config.toml` has following content:

    lipsum = "lorem ipsum dolor sit amet"

    [apples]
    count = 2

    [berry.black]
    has_some = true

Configuration from this file can be retrieved in following manner:

    {ok, Config} = toml:read_file("config.toml").
    {string, Val1} = toml:get_value([], "lipsum", Config).
    {integer, Val2} = toml:get_value(["apples"], "count", Config).
    {boolean, Val3} = toml:get_value(["berry", "black"], "has_some", Config).
    none = toml:get_value([], "oranges", Config).

Known limitations
-----------------

* Types conveying time only store 1s precision (fraction of a second is
  truncated)

Contact and License
-------------------

`toml` library is written by Stanislaw Klekot <dozzie at jarowit.net>.
The primary distribution point is <http://dozzie.jarowit.net/>.

`toml` library is distributed under 3-clause BSD license. See COPYING file for
details.

[toml]: https://github.com/toml-lang/toml

TOML parser for Erlang
======================

`toml` is an Erlang library application for parsing [TOML][toml] configuration
language. It supports parsing 0.4.0 version of the TOML specification.

Since TOML is a format for configuration files instead of general purpose
serialization, `toml` library focuses on reading the configuration parameters.

`toml:parse/2` and `toml:read_file/2` allow user to provide a validation
callback, which can check correctness of a value (e.g. if a listen address in
<i>listen="host:port"</i> has proper format) and even convert such values to
a usable Erlang data structure. All this happens at the time of reading
a configuration file, so any errors can be rejected in uniform manner.

In smaller scale, `toml:get_value/3` returns values as tuples tagged with
value type. This allows to fail early and with clear error on an obviously
invalid parameter:

    # [spool]
    # directory = 1024
    {string, SpoolDir} = toml:get_value(["spool"], "directory", Config).

Configuration usually consists of flags and parameters and not of complex,
nested data structures, so `toml` leans towards simple sections (or
<i>tables</i>, how TOML calls them) and simple access to values in them.
Still, all the TOML specification is supported, so nested structures are
available.

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

Documentation
-------------

`toml` is documented using EDoc. A local copy is generated with `make doc`
command to `./doc/` directory. An already generated online copy is available
at <http://dozzie.jarowit.net/api/erlang-toml/>.

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

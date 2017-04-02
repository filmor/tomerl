TOML parser for Erlang
======================

`toml` is an Erlang library application for parsing [TOML][toml] configuration
language. It supports parsing 0.4.0 version of the TOML specification.

Known limitations
-----------------

* Types conveying time only store 1s precision (fraction of a second is
  truncated)
* Nested arrays formatting requires whitespaces around `"["` and `"]"`
  characters to work properly

Contact and License
-------------------

`toml` library is written by Stanislaw Klekot <dozzie at jarowit.net>.
The primary distribution point is <http://dozzie.jarowit.net/>.

`toml` library is distributed under 3-clause BSD license. See COPYING file for
details.

[toml]: https://github.com/toml-lang/toml

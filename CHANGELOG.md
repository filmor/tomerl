# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased
### Added
- Support for sub-millisecond precision in datetimes
- Use [`rebar3_ex_doc`](https://github.com/starbelly/rebar3_ex_doc) for documentation
  generation

### Changed
- Updated compliance test suite and respective eunit generator
- Allow leading zeros in float exponents
- Allow lower-case date/time separator and Z
- Handle "float-like" keys (numeric keys with a single dot)
- Fix handling of control characters (allow literal tab character, disallow control
  characters in comments)
- Fix trailing space parsing for multiline strings

## [0.5.0] - 2021-06-15
### Changed
- Convert `-nan` to `nan` instead of `negative_nan` (slightly different from
  toml-elixir!)
- Update the test-suite using `git subtree`, will be kept up to date this way
- Extend CI to include xref, dialyzer and format checking
- Fix multiple smaller parsing bugs, mostly about rejecting invalid input
- Handle keywords (like `false`) in key position in tables correctly

## [0.4.0] - 2020-04-28
### Added
- Simple test escript `tomerl_test` to investigate partial parsing and as an
  endpoint to [toml-test](https://github.com/BurntSushi/toml-test)
- Implement support for non-finite floats (`inf`, `nan`) in the same way as
  [toml-elixir](https://github.com/bitwalker/toml-elixir)
- Support for [TOML 1.0.0-rc1](https://github.com/toml-lang/toml/blob/master/versions/en/toml-v1.0.0-rc.1.md)
- Test suite
- Simple datetime functionality

### Removed
- `toml_dict` as an intermediary representation, we parse directly into a "map
  of maps" now
- Validation function support

### Changed
- Renamed to `tomerl`
- Switch to [`rebar3`](https://rebar3.org)
- Allow a space as the datetime separator (instead of only `T`)
- Allow mixed type arrays

## [0.3.0] - 2017-10-28
### Changed
- Settled and documented location for errors reported by validation
  function
- Validation function is now also called for sections.

## [0.2.0] - 2017-05-08
### Added
- Added and documented precise location for semantic errors (duplicate keys
  and similar).

## [0.1.0] - 2017-04-08
### Added
- Initial implementation

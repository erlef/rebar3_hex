rebar3 Hex
===========

Providers for interacting with the Erlang package manager [hex.pm](https://hex.pm/).


Usage
------

Add to your projects `rebar.config`:

```erlang
{plugins, [{rebar3_hex, {git, "https://github.com/tsloughter/rebar3_hex.git", {branch, "master"}}}]}.
```

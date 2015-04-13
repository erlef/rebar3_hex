rebar3 Hex Providers
=========================

Providers for interacting with the Erlang package manager [hex.pm](https://hex.pm/).


Usage
------

Add to your projects `rebar.config`:

```erlang
{plugins, [{rebar3_hex, {git, "https://github.com/tsloughter/rebar3_hex.git", {branch, "master"}}}]}.
```

Commands
--------

* `hex config <key> [<value>]`
* `hex info [<package> [<version>]]`
* `hex key [remove key_name|list]`
* `hex publish`
* `hex user [register|whoami|auth|deauth|reset_password]`


TODO
----

* `hex docs`
* `hex owners`
* `hex search`

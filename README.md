rebar3 Hex Providers
=========================

Providers for interacting with the Erlang package manager [hex.pm](https://hex.pm/).


Usage
------

Add to your projects `rebar.config`:

```erlang
{plugins, [rebar3_hex]}.
```

Commands
--------

* `hex config <key> [<value>]`
* `hex docs`
* `hex info [<package> [<version>]]`
* `hex key [remove key_name|list]`
* `hex publish`
* `hex owners [add <package> <email>|remove <package> <email>|list <package>]`
* `hex user [register|whoami|auth|deauth|reset_password]`
* `hex search <term>`

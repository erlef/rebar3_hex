rebar3 Hex Providers
=========================

Providers for interacting with the Erlang package manager [hex.pm](https://hex.pm/).


Usage
------

Add to your global rebar3 config in `~/.config/rebar3/rebar.config`:

```erlang
{plugins, [rebar3_hex]}.
```

Usage
--------

* `rebar3 hex cut [-i major|minor|patch]`
* `rebar3 hex docs`
* `rebar3 hex publish`
* `rebar3 hex repo auth <repo> generate`
* `rebar3 hex repo auth <repo> --key <repo key>`
* `rebar3 hex owner [add <package> <email>|remove <package> <email>|list <package>]`
* `rebar3 hex user [register|whoami|auth|deauth|reset_password]`
* `rebar3 hex search <term>`

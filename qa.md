# rebar3_hex QA

- [Prep](#prep)
- [User tests (happy path)](#user-tests--happy-path-)
  * [Register](#register)
  * [Login](#login)
  * [Whoami](#whoami)
  * [reset_password](#reset-password)
  * [Deauth](#deauth)
- [User tests (unhappy path)](#user-tests--unhappy-path-)
- [Publish tests (happy path)](#publish-tests--happy-path-)
- [Publish tests (unhappy path)](#publish-tests--unhappy-path-)
- [Cut tests (happy path)](#cut-tests--happy-path-)
- [Cut tests (unhappy path)](#cut-tests--unhappy-path-)
- [Docs test (happy path)](#docs-test--happy-path-)
- [Docs test (unhappy path)](#docs-test--unhappy-path-)
- [Retire tests (happy path)](#retire-tests--happy-path-)
- [Retire tests (unhappy path)](#retire-tests--unhappy-path-)
- [Revert tests (happy path)](#revert-tests--happy-path-)
- [Revert tests (unhappy path)](#revert-tests--unhappy-path-)
- [Repo tests (happy path)](#repo-tests--happy-path-)
- [Repo tests (unhappy path)](#repo-tests--unhappy-path-)
- [Key tests (happy path)](#key-tests--happy-path-)
- [Key tests (unhappy path)](#key-tests--unhappy-path-)
- [Owner tests (happy path)](#owner-tests--happy-path-)
- [Owner tests (unhappy path)](#owner-tests--unhappy-path-)
- [Search - happy path](#search---happy-path)
- [Search - unhappy path](#search---unhappy-path)

## Prep

1. Ensure you have a postgres instance running with a `postgres` as a superuser and password `postgres`. Alternatively edit config/dev.exs and adjust accordingly to your existing setup.

1. Get and setup an instance of hexpm :

        git clone https://github.com/hexpm/hexpm.git
        mix deps.get
        mix deps.compile
        mix ecto.setup
        cd assets && npm install


1.  comment out the billing report in hexpm

        perl -i -pe 's/{Hexpm.Billing.Report/#$&/' lib/hexpm/application.ex

1. Start hexpm:

        mix phx.server

1. Open another terminal and git a copy of truecoat or alternatively create your own rebar3 test app

       git https://github.com/starbelly/truecoat.git

## User tests (happy path)

### Register

        1. rebar3 hex user register

        $ rebar3 hex user register
        By registering an account on Hex.pm you accept all our policies and terms of service found at https://hex.pm/policies

        Username: ([])> jerry
        Email: ([])> jerry@foo.bar
        Account Password:
        Account Password (confirm):
        Registering...
        You are required to confirm your email to access your account, a confirmation email has been sent to jerry@foo.bar
        Then run `rebar3 hex auth -r hexpm` to create and configure api tokens locally.

1. View the console

1. Look back to your console for the verification link and copy it
   Example:

        %Bamboo.Email{assigns: %{email: "jerry@foo.bar", key: "2b65aadfecde389abc442147b6a51499", username: "jerry"}, attachments: [], bcc: [], cc: [], from: {"Hex.pm", "noreply@hex.pm"}, headers: %{}, html_body: "<!DOCTYPE html>\n<html>\n  <head>\n    <meta charset=\"utf-8\">\n  </head>\n  <body>\n\n<p>\nTo begin using your email, we require you to verify your email address.</p>\n\n<p>\n    You can do so by following <a href=\"http://localhost:4000/email/verify?username=jerry&amp;email=jerry%40foo.bar&amp;key=2b65aadfecde389abc442147b6a51499\">this link</a> or by pasting this link in your web browser: http://localhost:4000/email/verify?username=jerry&amp;email=jerry%40foo.bar&amp;key=2b65aadfecde389abc442147b6a51499\n</p>\n\n    <p>\n      -- <br>\n      Hex.pm\n    </p>\n  </body>\n</html>\n", private: %{html_layout: {HexpmWeb.EmailView, "layout.html"}, text_layout: {HexpmWeb.EmailView, "layout.text"}, view_module: HexpmWeb.EmailView, view_template: :verification}, subject: "Hex.pm - Email verification", text_body: "\nTo begin using your email, we require you to verify your email address.\nYou can do so by following this link:\n\nhttp://localhost:4000/email/verify?username=jerry&email=jerry%40foo.bar&key=2b65aadfecde389abc442147b6a51499\n-- Hex.pm\n", to: [{"jerry", "jerry@foo.bar"}]}

1. Run curl on the link you copied or just paste it in your browser

        $ curl -L "http://localhost:4000/email/verify?username=jerry&email=jerry%40foo.bar&key=bfa19fdc7331b794b2fe5ee5f48d304d"
        <!DOCTYPE html>
        <html lang="en" prefix="og: http://ogp.me/ns#">
        <head>
        <meta charset="utf-8">
        <meta http-equiv="X-UA-Compatible" content="IE=edge">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <meta name="description" content="A package manager for the Erlang ecosystem">

        <title>Hex</title>
        ...
### Login

1. Provided the steps in register have been successfully completed try to login using rebar3_hex

        $ rebar3 hex user auth
        Username: ([])> jerry
        Account Password:
        You have authenticated on Hex using your account password. However, Hex requires you to have a local password that applies only to this machine for security purposes. Please enter it.
        Local Password:
        Local Password (confirm):
        Local Password (confirm): Generating all keys...

### Whoami

1. Provided the steps in login have been successfully completed run the whomami command

        $ rebar3 hex whoami
        hexpm : jerry (jerry@foo.bar)

### reset_password

1. Provided the steps in whoami have been successfully completed run the reset_password sub command

        $ rebar3 hex user reset_password
        Username or Email: ([])> jerry
        Email with reset link sent

1. Go over to the console for your running hexpm instance and grab the reset link.

        %Bamboo.Email{assigns: %{key: "6dd4338b0d2049c2477d40f954318b87", username: "jerry"}, attachments: [], bcc: [], cc: [], from: {"Hex.pm", "noreply@hex.pm"}, headers: %{}, html_body: "<!DOCTYPE html>\n<html>\n  <head>\n    <meta charset=\"utf-8\">\n  </head>\n  <body>\n\n<p>Reset your Hex.pm password</p>\n\n<p>\nWe heard you&#39;ve lost your password to Hex.pm. Sorry about that!</p>\n\n<p>\n  You can chose a new password by following <a href=\"http://localhost:4000/password/new?username=jerry&amp;key=6dd4338b0d2049c2477d40f954318b87\">this link</a> or by pasting the link below in your web browser.\n</p>\n\n<p>\nhttp://localhost:4000/password/new?username=jerry&amp;key=6dd4338b0d2049c2477d40f954318b87</p>\n\n<p>\nOnce this is complete, your existing keys may be invalidated, you will need to regenerate them by running:\n  <pre>mix hex.user auth</pre>\n\nand entering your username and password.</p>\n\n    <p>\n      -- <br>\n      Hex.pm\n    </p>\n  </body>\n</html>\n", private: %{html_layout: {HexpmWeb.EmailView, "layout.html"}, text_layout: {HexpmWeb.EmailView, "layout.text"}, view_module: HexpmWeb.EmailView, view_template: :password_reset_request}, subject: "Hex.pm - Password reset request", text_body: "\nReset your Hex.pm password\nWe heard you've lost your password to Hex.pm. Sorry about that!\nYou can chose a new password by following this link\n\nhttp://localhost:4000/password/new?username=jerry&key=6dd4338b0d2049c2477d40f954318b87\nOnce this is complete, your existing keys may be invalidated, you will need to regenerate them by running:\nmix hex.user auth\nand entering your username and password.\n-- Hex.pm\n", to: [{"jerry", "jerry@foo.bar"}]}

1. Visit the link copied from the console in your browser and reset the user's password

1. Follow steps in logging into to ensure you can login with your new password

### Deauth

1. Provided you have successfully completed register and login steps execute the deauth sub-command :


        $ rebar3 hex user deauth
        User `jerry` removed from the local machine. To authenticate again, run `rebar3 hex user auth` or create a new user with `rebar3 hex user register`

1. Ensure the config has been removed from your local hex.config file:

         $ cat ~/.config/rebar3/hex.config
         #{<<"hexpm">> => #{}}.

## User tests (unhappy path)

## Publish tests (happy path)

## Publish tests (unhappy path)

## Cut tests (happy path)

## Cut tests (unhappy path)

## Docs test (happy path)

## Docs test (unhappy path)

## Retire tests (happy path)

## Retire tests (unhappy path)

## Revert tests (happy path)

## Revert tests (unhappy path)

## Repo tests (happy path)

## Repo tests (unhappy path)

## Key tests (happy path)

## Key tests (unhappy path)

## Owner tests (happy path)

## Owner tests (unhappy path)

## Search - happy path

## Search - unhappy path

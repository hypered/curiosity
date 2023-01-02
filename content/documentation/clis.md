---
title: Curiosity
---

# Command-line interfaces

Curiosity is mainly a web application, `cty serve`, but it also comes with
additional CLIs (command-line interfaces).

The documentation for these CLIs is available as [man
pages](https://en.wikipedia.org/wiki/Man_page), and the man pages are also
rendered as HTML pages:

- [`curiosity(7)`](/documentation/clis/curiosity.7)
- [`cty(1)`](/documentation/clis/cty.1)

It is also possible to submit individual commands through a [web
interface](/run).

# Asciinema demo

An interactive shell session where we show `cty` has been recorded with
[asciinema](https://asciinema.org/). It is visible on [its own
page](/documentation/asciinema).

# Quick start

This quick start section assumes you have SSH access to the Linux user `alice`
at `smartcoop.sh`, but also applies to the other possible
[environments](/documentation/environments):

```
$ ssh alice@smartcoop.sh
Last login: Thu Sep 22 18:24:30 2022 from xxx.xxx.xxx.xxx
Welcome to the Curiosity environment.
Run `man curiosity` for the manual.
[alice@curiosity-1:~]$
```

For simplicity, we write the prompt `[alice@curiosity-1:~]$` as `$` in the
following text.

In addition to the documentation available online at
[`smartcoop.sh`](https://smartcoop.sh/documentation), information is also
available locally in man pages and as part of the `cty` command-line
executable:

```
$ man curiosity
$ man cty
$ cty --help
```

Note: Type `q` to exit from a man page.

By default, `cty` commands are using two files: `curiosity.log` and
`state.log`. The former contains logs of what the system is doing. The later
contains a JSON representation of the state of the system. It is read when a
command starts, then written back when a command exits.

For the sake of this section, if those files exist, we first remove them to
start from a clean slate:

```
$ ls
curiosity.log  state.json

$ rm curiosity.log state.json

$ ls

```

The first real `cty` command we can use is `cty init`. Its purpose is to create
an empty state: i.e. a `state.json` file with just enough data to make it
useful for other `cty` commands, but no data from a user perspective (e.g. no
user profiles, no invoices, and so on).

```
$ cty init
State file 'state.json' created.

$ ls
state.json

```

To have a look at the current state, it's possible to directly open the
`state.json` file. But `cty` offers some commands to explore it. For instance
you can try the following:

```
$ cty state
$ cty state --hs
```

Note: each `cty` command accepts the `--help` option. Running `cty state
--help` would allow you to discover that it also accepts the `--hs` option.

Since `cty state` uses the JSON format, it can also be paired with something
like `jq`:

```
$ cty state | jq .
$ cty state | jq ._dbUserProfiles
```

Some commands have sub-commands. For instance with `cty user --help`, you can
see that it supports a `create` sub-command. And you can discover how to create
a new user with `cty user create --help`:

```
$ cty user create alice a alice@example.com --accept-tos
User created: USER-1
$ cty user create mila m mila@example.com --accept-tos
User created: USER-2
$ cty state | jq '._dbUserProfiles | .[] | {username: ._userProfileCreds._userCredsName, email: ._userProfileEmailAddr}'
{
  "username": "alice",
  "email": "alice@example.com"
}
{
  "username": "mila",
  "email": "mila@example.com"
}
```

To avoid repeating the same commands again and again when exploring a specific
situation, `cty` supports running scripts: text files containing commands. As
an example, to re-create exactly the same state as above, the following text
file can be used with `cty run`:

```
$ cat create-users.txt
reset
user create alice a alice@example.com --accept-tos
user create mila m mila@example.com --accept-tos

$ cty run create-users.txt
1: reset
Resetting to the empty state.
2: user create alice a alice@example.com --accept-tos
User created: USER-1
3: user create mila m mila@example.com --accept-tos
User created: USER-2
```

Note: the `reset` command makes the state empty, resulting in the same state as
`cty init`.

Such scripts are used to create a [test suite](/documentation/tests),
demonstrating the behavior of Curiosity. They are located in the [`/scenarios`
directory](https://github.com/hypered/curiosity/tree/main/scenarios) within the
GitHub repository. They are also listed [in the
documentation](/documentation/scenarios#scenarios) from where they can be run
and explored.

To expose the state file using a web interface, `cty serve` can be used. By
default it uses port 9000. In the specific case of the `smartcoop.sh` machine,
it can be reached by using the [`play.smartcoop.sh`](https://play.smartcoop.sh)
sub-domain:

```
$ cty serve
^C
```

Note: type `ctrl-c` to stop the web server. This is visible above as `^C`.

# For developers

See also:

- [Command line interface guidelines](https://clig.dev/)
- [Best practices for inclusive
  CLIs](https://seirdy.one/posts/2022/06/10/cli-best-practices/)

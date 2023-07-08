---
title: Curiosity
---

# Application state

In a traditional [three-tier
architecture](https://en.wikipedia.org/wiki/Multitier_architecture)
application, the data tier, which stores the complete state of the system, is a
relational database such as PostgreSQL.

In Curiosity, the complete state of the application is maintained in-memory
using [Software transactional
memory](https://en.wikipedia.org/wiki/Software_transactional_memory).

From the web interface, the complete application state can be browsed at
[`/state`](/state) and [`/state.json`](/state.json). The first link uses the
internal state representation to format the data and the second link uses JSON.
A graphical, albeit partial, representation is also available at
[`/state.svg`](/state.svg).

From the command-line, the complete state can be viewed with the `cty state`
command, and from the REPL, it is available with `state`. Further drilling down
the returned data can be done on the command-line for instance with
[`jq`](https://stedolan.github.io/jq/).

Such complete representations are currently only practical for little data.
We'll probably have to revisit the means to query it when we have to deal with
bigger volumes.

# Application state management

While Curiosity maintains the state only in-memory during nomal operations, it
is possible to write the state to disk, or read it from disk. Many `cty`
commands accept the `--state <file.json>`, `--socket <file.sock>`, and
`--memory` options to decide how the state is managed.

By default, commands such as `cty repl` and `cty serve` are operating as if
`--state state.json` was given. This causes the state to be read from the
`state.json` file upon startup, and written back to end with the program exits.

With the `--memory` option, the commands are not using a file at all. Their
initial state is empty, and is simply discarded when they shut down.

The `--socket` option, can be used to operate against the state of a running
server. This feature is currently incomplete.

Another currently incomplete feature allows to call `cty` commands across SSH.
In that case, the machine is configured so that they operate against the
running server.

For commands that expect a state file, whether it is their default behavior or
they are instructed to do so, the state file must exit. The command to create a
state file is `cty init`.

# Virtual machine state

The virtual machine shipped with the prototype (and featured at
[`cty-1.hypered.systems`](https://cty-1.hypered.systems)) is configured to start with an initial
state, called [`state-0`](/documentation/state-0). This is the same state used
in automated [test scenarios](/documentation/scenarios).

# See also

- [Sent emails](/documentation/emails)
- [Quotations](/documentation/quotations)
- [Orders](/documentation/orders)

# Curiosity - A prototype application for Smart

This repository contains a prototype application for the
[Smart](https://github.com/smartcoop/) cooperative. What we hear about
"prototype" is written in `commence`'s
[README](https://github.com/hypered/commence#readme).

In particular, in addition of working features, we want

> a software artifact that documents how the real software should work,
> documents what the business logic is, and provides confidence in the business
> logic by virtue of having a working implementation.

A demonstration instance of Curiosity is running at
[smartcoop.sh](https://smartcoop.sh). It contains
[documentation](https://smartcoop.sh/documentation) that complements this
README. If you're non-technical, those links are a better starting point. The
rest of this README is intended for more technical profiles.

# Content

Curiosity offers multiple tools compiled as a single executable, called
[`cty`](#cty). The main command, `cty serve`, is a web application. In
particular it uses the `servant` and `stm` libraries. The `stm` library is used
instead of a regular relational database (e.g. PostgreSQL). This means the
whole state of the application is in memory instead of a "real" database.

In addition of the web application, some other commands are provided to better
demonstrate and explore the features of the application.

- `cty serve` runs the web application.
- `cty repl` is similar but exposes a
  [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)
  instead of an HTTP server to run commands and interact with the state.
- `cty run` interprets scripts containing commands (the same commands that the
  REPL uses).
- `cty sock` offers a text interface similar to `cty repl` but through a
  UNIX-domain socket, and accepts multiple clients.
- Other commands of `cty` are meant to be a client for the `cty sock` server.
  It can also be run against a local state file.
- `cty parse` is a helper command to play with the command parser used in `cty
  repl`.

**Note**: The commands that can be typed in the REPL are using the same syntax
used by `cty` itself. E.g. `cty state` typed in a shell, is similar to `state`
typed in the Curiosity REPL.

# Related repositories

- The prototype uses [`design-hs`](https://github.com/hypered/smart-design-hs/) as
  a component library to create HTML pages.

- It also uses [`commence`](https://github.com/hypered/commence) as a
  kind of base "framework" to organize the prototype.

# Tests

Tests covering (partially) the library can be run by entering the Nix shell and
using `cabal`:

```sh
$ nix-shell
$ cabal test --test-show-details=direct
```

The same thing can be accomplished with a convenience script:
`scripts/run-tests.sh`, which takes care of entering the Nix shell.

The test suite uses `hspec`, so its [command-line
options](https://hspec.github.io/options.html) apply.

Integration tests, which use the [Nix testing
infrastructure](https://nix.dev/tutorials/integration-testing-using-virtual-machines),
can be run by building a specific attribute (see below for an interactive
environment):

```sh
$ nix-build -A run-vm-tests
```

A [GitHub action](https://github.com/hypered/curiosity/actions) is used to run
the tests on every commit, and report the results on each Pull Request. It is
also used to populate a [Nix binary cache](#nix-binary-cache).

# `ghcid`

Reloading the code automatically upon changes can be done with `ghcid`. A
convenience script to run `ghcid` is `script/ghcid.sh`.

`ghcid` can also be instructed to run something when code loads successfully.
This is used to typically run tests, but can also be used to restart the
Curiosity HTTP server.

This looks like this (be sure to enter the `nix-shell` first):

```
$ ghcid --warnings --command scripts/ghci.sh --test ':main serve'
```

Note: use the
[`autoReload`](https://smartcoop.sh/haddock/Curiosity-Html-Misc.html#v:autoReload)
function defined in `Curiosity.Html.Misc` to cause an open web page to be
automatically refreshed when working on some HTML snippet.

The same mechanism can be used with the HSPec-based tests or the Golden tests:

```
$ ghcid --warnings --command scripts/ghci-spec.sh --test ':main'
$ ghcid --warnings --command scripts/ghci-scenarios.sh --test ':main'
```

# Running

There are multiple ways to run Curiosity's code. Building the `cty` binary and
executing it is only one way.

It is possible to load the code in GHCi, so that it is interpreted instead of
compiled. This provides a quick way to reload the code (with `:r`).

One way is to use `cabal` directly, but this exposes the `Curiosity.Command`
module, instead of `Main`:

```sh
$ nix-shell
$ cabal repl curiosity
```

Instead of using `cabal`, it's possible to use a helper script to call `:main`
within GHCi.

```sh
$ script/ghci.sh
ghci> :main --help
```

We can also use Cabal to run the example. The advantage compared to using Nix
to build the binary is that it is incremental (i.e. re-use already compiled but
not modified modules):

```sh
$ nix-shell
$ cabal run -- cty --help
```

(If necessary, the built executable is at
`dist-newstyle/build/x86_64-linux/ghc-9.0.2/curiosity-0.1.0.0/x/cty/build/cty/cty`.)

The binary can be built and run also this way, which bypass building the tests:

```sh
$ cabal build curiosity
$ dist-newstyle/build/x86_64-linux/ghc-9.0.2/curiosity-0.1.0.0/x/cty/build/cty/cty --help
```

We can build a binary with Nix:

```sh
$ nix-build -A binaries
$ result/bin/cty --help
```

Finally, some [environments](#environments) also contain a runnable executable
Curiosity.

# Environments

We can have a shell populated with the Curiosity binaries and man pages with:

```sh
$ nix-shell default.nix -A shell
$ cty --help
$ man cty
```

That shell also provides a `run-full-environment` script. It runs a local
environment where `cty serve` and an Nginx reverse proxy are running together:

```sh
$ run-full-environment
```

This is the closest environment we have that mimicks the complete [virtual
machine image](#virtual-machine-images) without actually running a VM. It is
based on a [procfile](https://devcenter.heroku.com/articles/procfile), run by
[hivemind](https://github.com/DarthSim/hivemind).

It is also possible to only run Nginx in that shell with `run-nginx`. This is
useful for instance to proxy a `cty serve` running from GHCi.

The above script is simply a tiny wrapper around the `run` attribute:

```sh
$ nix-build -A run
$ result/bin/run-full-environment
```

The `run-vm-tests` attribute lets you run tests based on a NixOS VM mirroring
the production setup, plus a client VM. You can run it either
non-interactively...:

```sh
$ nix-build -A run-vm-tests
```

... or interactively:

```sh
$ nix-build -A run-vm-tests.driverInteractive
$ result/bin/nixos-test-driver
> start_all( )
```

This opens two QEMU windows, one for the server, one for the client, and you
can use the root account to interactively log in the VMs. The client can access
the server using the `smartcoop.sh` domain name. You can read the relevant
[NixOS manual
section](https://nixos.org/manual/nixos/stable/index.html#sec-running-nixos-tests-interactively)
for more informations.

**Note**: these VMs are not connected to the internet, `smartcoop.sh` here
refers to the server VM, not the production machine.

And finally, we can run a local virtual machine running both `cty serve` and an
Nginx reverse proxy using QEMU ([see below](#qemu)):

```sh
$ nix-build -A runvm
$ result/bin/run-nixos-vm
```

The web application can be accessed at `127.0.0.1:8180`. A helper script is
provided to do the same: `scripts/runvm.sh`.

The virtual machine image running at `smartcoop.sh` is based on the above, and
can be built with:

```
$ nix-build -A image
```

# `cty`

`cty` is the main command-line tool to run the web application, interact
against a server running on the same host (through a UNIX-domain socket), or
against a state file. Note that modifying a state file used by a running server
should be avoided.

By default, `cty` interacts against a state file called `state.json`. Use the
`--state` option to override the file name. Use the `--socket` option to
instead interact against a running server.

```sh
$ cty init
State file 'state.json' created.

$ cty state
{...}

$ cty user get alice
No such user.

$ cty user signup alice secret alice@example.com --accept-tos
User created: USER-1
Signup confirmation email enqueued: EMAIL-1

$ cty user get USER-1
{...}
```

# Example REPL commands

The REPL can be quit with the `quit` command, or `Ctrl-d`:

```
$ cty repl
> quit
```

The REPL accepts commands similar to the `cty` sub-commands, including
`--help`.

```
> user signup alice secret alice@example.com --accept-tos
> --help
```

# Example scripts

Scripts (i.e. files containing commands) can be run with `cty run`. Exampe
scripts can be found within the repository, in the [`scenarios/`
directory](https://github.com/hypered/curiosity/tree/main/scenarios).

```
$ cty run scenarios/user-signup.txt
1: reset
State is now empty.
3: user signup alice secret alice@example.com --accept-tos
User created: USER-1
Signup confirmation email enqueued: EMAIL-1
4: user get USER-1 --short
USER-1 alice
5: quit
Exiting
```

Such scripts, together with their expected output, are used as a high-level
[testing mechanism](https://smartcoop.sh/documentation/tests).

# Nix binary cache

You can re-use the Curiosity binaries built by the CI through a custom Nix
binary cache.

On a NixOS system add the following snippet to your system configuration:

```nix
nix.settings = {
  substituters = [ "https://s3.eu-central-003.backblazeb2.com/curiosity-store/" "https://cache.nixos.org/" ];
  trusted-public-keys = [ "curiosity-store:W3LXUB+6DjtZkKV0gEfNXGtTjA+hMqjPUoK6mzzco+w=" "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];
}
```

On a non-NixOS system, you can edit the `/etc/nix/nix.conf` file and set the
`substituters` and `trusted-public-keys` configuration attributes to:

```
substituters = https://s3.eu-central-003.backblazeb2.com/curiosity-store/ https://cache.nixos.org/
trusted-public-keys = curiosity-store:W3LXUB+6DjtZkKV0gEfNXGtTjA+hMqjPUoK6mzzco+w= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

# Docker image

A Docker image can be built to experiment with the `cty` program, and have
access to man pages. By default, running the image will run Bash.

```sh
$ nix-build -A docker
$ docker load < result
$ docker run --privileged -it -p 9000:9000 curiosity:vsj9an994jjrw47kv9fj0icjs2f6xq6r
```

**Note**: the image tag will be different as it depends on the actual image
content.

**Note**: `-p 9000:9000` is necessary only if you want to run the web
applicatio with `cty serve`.

# Virtual machine images

## QEMU

A virtual machine image that can be run with QEMU can be built and run with:

```sh
$ nix-build -A runvm
$ result/bin/run-nixos-vm
```

Within the VM, Nginx is setup as a reverse-proxy in front of `cty serve`. You
should be able to confirm that with e.g.:

```sh
# curl -v 127.0.0.1
# systemctl status nginx
# systemctl status app
```

The VM contains other binaries to help interact with the system, and local
documentation is available as man pages:

```sh
# cty --help
# man curiosity
```

Use `ctrl-a x` to quit QEMU.

**Note**: when using the `run-nixos-vm` script, a disk file called
`nixos.qcow2` is created. It is re-used on the next call, and may create some
conflicts. Thus it is sometimes necessary to delete it before running the
script again.

## Digital Ocean

An image suitable for Digital Ocean can also be built:

```sh
$ nix-build -A image
$ ls result/
nixos.qcow2.gz
```

Its weight is about 870MB.

# Static documentation

The static documentation can be built by using the `content` Nix attribute. It
is better to name its result symlink `_site` instead of the default (`result`)
be cause this is what `cty serve` expects by default, as well as the
`serve-doc.sh` helper script.

```
$ nix-build -A content --out-link _site
$ scripts/serve-doc.sh
```

Documentation pages use the Nginx
[SSI](http://nginx.org/en/docs/http/ngx_http_ssi_module.html) feature, to embed
some HTML fragments that are generated by the `cty serve` backend. This is also
used to embed a navigation bar (so that a logout or a login link is provided
depending on if the backend notices that the user is logged in or not).

To get a static navigation bar within the generated HTML pages instead of using
the SSI feature, it is possible to use the `public` Nix attribute:

```
$ nix-build -A public --out-link _site
$ scripts/serve-doc.sh
```

# The `smartcoop.sh` host

These are raw notes about how `smartcoo.sh` was deployed. I (Thu) have used 4
scripts that come from my [nix-notes](https://github.com/noteed/nix-notes)
repository.

Those scripts require some environment variables for authentication, and some
configuration to point to the right services (DigitalOcean instead of AWS). I'm
using a `.envrc` file, and a `s3-config` file, that are not under version
control.

-   `upload-image.sh` is used to upload the DigitalOcean image to Spaces
    (similar to S3). This gives us a URL from where the image can be downloaded
    for the next step.

-   `import-image.sh` is used to import the image as a "custom image" into
    DigitalOcean. This requires a URL from which DO will download the file.
    This was provided by the previous step. The command returns quickly, but the
    image will be shown as "Pending" in the DO web interface for quite a while.

    ```sh
    $ scripts/import-image.sh
    ID           Name         Type      Distribution    Slug    Public    Min Disk
    110021225    curiosity    custom    Unknown OS              false     0
    ```

    The returned image ID is important for the next step.

-   `create-droplet.sh` is used to create a Drople (i.e. a VM) at DO. The image
    ID from the previous step is hard-coded in the script. In addition, a
    public SSH key of mine, already known by DO, is specified in the script to
    be copied in the Droplet. After that, I note its IP address.

    I've confirmed I could SSH into it:

    ```sh
    $ ssh root@146.190.30.165
    ```

    I've also confirmed that the same commands shown above for the QEMU case
    also work.

    It seems only 4.4GB are used on the 25GB disk.

-   `deploy.sh` is used to deploy changes to the Droplet, without needing to
    rebuild an image or create a new Droplet. Note that I specified
    `smartcoop.sh` within the script instead of its IP address. See below.

# The `smartcoop.sh` domain

I've bought the domain at Namecheap on 2022-06-08 and configured Namecheap to
use DO's name servers. I've created the `smartcoop.sh` domain manually within
the DO web interface (within the "curiosity" project). Then I've created an A
record for `@`, associated to the above IP address.

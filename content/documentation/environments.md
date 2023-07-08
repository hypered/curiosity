---
title: Curiosity
---


# Environments

Curiosity is mostly a single program, `cty`, and its main mode of operation is
to run as a web application by invoking `cty serve`. Nevertheless, as most
real-world applications, distributing a single binary is not enough: developing
or operating such programs require additions. For instance it is necessary to
provide documentation, and we suggest using a reverse-proxy such as Nginx to
expose Curiosity to the internet.

In addition of a development environment, Curiosity can be exercised through
three possible environments: a `nix-shell` -based environment, a virtual
machine image, or a Docker image.

## Development environment

Curiosity is developed using the [Nix package manager](/documentation/nix). It
provides a reliable way to specify dependencies and build our artifacts: the
`cty` binary of course, but also for instance virtual machine or Docker images.

One of the most useful tool of Nix is `nix-shell`. It runs a Bash shell in
which all the necessary dependencies to work on the project are automatically
made available. Once in such an environment, it is thus possible to compile
Curiosity's source code or use a common tool to format it.

To enter the development environment, one has only to run `nix-shell` within
a copy of the Curiosity repository:

```
$ nix-shell
```

## Running development environment

A second environment, also provided by `nix-shell`, is offered, this time not
to work directly on the project, but to try it:

```
$ nix-shell default.nix -A shell
```

Again, this runs a Bash shell, configured in a very specific way. It offers the
`cty` binary, but also access to man pages and Bash completion. It is also
configured such that `cty serve` knows by default where to find some resource
directories (documentation and example data files).

It is possible to enter the same shell without cloning the Git repository by
directly executing:

```
$ nix-shell \
  https://github.com/hypered/curiosity/archive/refs/heads/main.tar.gz \
  -A shell
```

## Virtual machine image

The official way Curiosity is distributed is as a virtual machine image. This
is the most complete environment and what should be used when deploying
Curiosity. In addition of what can be found in the other environments
(excluding the development environment), it has an Nginx reverse proxy, an SSH
server and more.

When running a virtual machine based on that image, the reverse proxy, the SSH
server and the Curiosity web application are automatically started.

An example deployment of this virtual machine image runs at
[cty-1.hypered.systems](https://cty-1.hypered.systems).

## Docker image

The third way Curiosity can be tried is through a Docker image. This
environment is similar to the running development environment. It has also
access to man pages or Bash completion, but the later are not enabled
automatically.

## Environment variables

When running `cty serve`, the location of the documentation, the example
data files, and the scenarios must be known by the application in order to
serve them. Those locations can be passed by using the `--static-dir`,
`--data-dir`, and `scenarios-dir` command-line options. For convenience, the
above environments contain three environment variables, `CURIOSITY_STATIC_DIR`,
`CURIOSITY_DATA_DIR`, and `CURIOSITY_SCENARIOS_DIR` that are set to the right
values. Those environment variables take precedence over the command-line
options.

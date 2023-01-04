---
title: Curiosity
---


# Nix

Curiosity is developed using the [Nix package
manager](https://nixos.org/guides/how-nix-works.html). Nix provides a reliable
way to specify project dependencies and build our artifacts, whether it is [our
`cty` binary](/documentation/clis), or even Curiosity's main deliverable: the
complete virtual machine image running at
[`smartcoop.sh`](https://smartcoop.sh).

Once you have Nix installed and a clone of the [Curiosity
repository](https://github.com/hypered/curiosity), building the `cty` binary
can be achieved with:

```
$ nix-build -A binaries
```

The result is made available in the `result/` symlink.

# NixOS

NixOS is a Linux distribution based on Nix. A complete NixOS-based machine can
be described declaratively, using Nix. Just like we can build the `cty` binary
with the above command, we can build the complete system with:

```
$ nix-build -A toplevel
```

Whenever we change something in the Curiosity repository, we can build a new
toplevel and activate it on a given machine (e.g. the machine running at
[`smartcoop.sh`](https://smartcoop.sh)).

# Binary cache

To avoid building the same artifacts multiple times on different machines, Nix
makes it possible to use a binary cache, i.e. a place where results can be
written to, so that other machines can download them, without unnecessary
rebuilds. Curiosity's binary cache is populated by its continuous integration
setup (which uses [GitHub
workflows](https://docs.github.com/en/actions/using-workflows)), which is
responsible to build the toplevel on every change.

To configure your own NixOS system to use our binary cache, you can add the
following values to your `configuration.nix` file:

```nix
nix.settings.substituters = [
  "https://cache.nixos.org/"
  "https://s3.eu-central-003.backblazeb2.com/curiosity-store/"
];
nix.settings.trusted-public-keys = [
  "curiosity-store:W3LXUB+6DjtZkKV0gEfNXGtTjA+hMqjPUoK6mzzco+w="
];
```

On a non-NixOS system, you can edit the `/etc/nix/nix.conf` file and set the
`substituters` and `trusted-public-keys` configuration attributes to:

```
substituters = https://cache.nixos.org/ https://s3.eu-central-003.backblazeb2.com/curiosity-store/
trusted-public-keys = curiosity-store:W3LXUB+6DjtZkKV0gEfNXGtTjA+hMqjPUoK6mzzco+w=
```

# Environments

In addition of producing reliably build artifacts, Nix can be used to provide
environments. This can be a development environment (where development tools
are present, e.g. a compiler, a code formatter, ...), a testing environment,
and so on. You can read more on the [dedicated documentation
page](/documentation/environments).

# Nix attributes

Names such as `binaries` or `toplevel` in the above sections are _attributes_
in the main Nix expression file (`default.nix`). Those and others are
documented a [dedicated page](/documentation/attributes).

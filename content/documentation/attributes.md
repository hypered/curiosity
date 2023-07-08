---
title: Curiosity
---


# Nix attributes

This page explains the different Nix attributes that can be found in
`default.nix`. This is useful for developers to better explore the project
(instead of only building the main artifact, a virtual machine image).

Note: for an overview of how Nix is used in this project, see
[Nix](/documentation/nix).

# `cty`

```
$ nix-build -A binaries
$ ls result/bin/
cty
```

`cty` is the program implementing Curiosity. It provides a [command-line
interface](/documentation/clis) to initialize, modify and explore Curiosity's
state. It is also the web application exposed behing Nginx when the complete
system is run (normally using the virtual machine image).

# Static web site content

```
nix-build -A content --out-link _site
```

The static site content of Curiosity mainly comprises the HTML documentation
pages (generated from Markdown source files), and some assets for the web site
(fonts, CSS, some JavaScript, ...). It also contains the
[asciicast](https://asciinema.org/) file and the
[Stork](https://stork-search.net/)-based search index (powering the [search
page](/documentation/search).

Once built, the result can be served with any web server, e.g.:

```
$ nix-shell -p busybox --run 'httpd -f -p 9000 -h _site'
```

There is no index page (that page is provided by `cty serve`). You can navigate
directly to the documentation page, e.g.:
[`http://127.0.0.1:9000/documentation.html`](http://127.0.0.1:9000/documentation.html).

Note: when using the `content` attribute, the resulting pages are using the
Nginx [SSI](http://nginx.org/en/docs/http/ngx_http_ssi_module.html) feature, to
embed a dynamic navigation bar (so that a logout or a login link is provided
depending on if the backend notices that the user is logged in or not). In
other words, unless Nginx is used to serve the `_site/` directory (and is
configured to enable SSI), such a navigation bar will not be visible. See below
for a different version of the `content` attribute.

Note: in the virtual machine image, Nginx is also configured to serve static
files (but not the documentation) directly. I.e., requesting
`/static/css/main.css` will not trigger a call to the `cty serve` backend.

# Static web site content (alternative)

```
nix-build -A public --out-link _site
```

Instead of the `content` attribute explained above, it is possible to use the
`public` attribute` It is very similar to `content`, but embed a static
navigation bar (that shows that you're not logged in).

# Static JSON files

```
nix-build -A data
```

In addition of the web site content (see above), additional files are available
so that they can be exploited through the web interface. Those files are simply
a copy of the `data/` directory from the Curiosity repository.

# Static scenario scripts

```
nix-build -A scenarios
```

This is similar to the `data` attribute, but this time it provides a copy of
the `scenarios/` directory.

# Stork indexes

```
$ nix-build -A indexes
ls result/indexes/
content.st  stork.css
```

To power the documentation [search page](/documentation/search), an index is
built using [Stork](https://stork-search.net/). This is done using the source
Markdown files (but converted to an even simpler text format). The `indexes`
attribute makes it possible to only build the index.

Note: these files are also served directly by Nginx in the Curiosity virtual
machine image.

# Haddock documentation

```
$ nix-build -A haddock
ls -1 result-doc/share/doc/curiosity-0.1.0.0/html/
...
index.html
...
```

The `cty` program is written in Haskell. Haddock is the name of the
documentation tool that extracts specially formatted comments from the Haskell
source code and generates HTML pages. The `haddock` attribute can used to build
those documentation pages.

Again, those files can be served with any web server, e.g.:

```
$ nix-shell -p busybox --run \
  'httpd -f -p 9000 -h result-doc/share/doc/curiosity-0.1.0.0/html/'
```

Note: these files are also served directly by Nginx in the Curiosity virtual
machine image.

# Man pages

```
$ nix-build -A man-pages
$ ls result/share/man/*
result/share/man/man1:
cty.1.gz

result/share/man/man7:
curiosity.7.gz
```

[Man pages](/documentation/man-pages) can be built with the `man-pages`
attribute. The resulting files can be open with the `man` program.

# Complete system

```
$ nix-build -A toplevel
$ ls result
activate               dry-activate        init                    kernel-modules  sw
append-initrd-secrets  etc                 init-interface-version  kernel-params   system
bin                    extra-dependencies  initrd                  nixos-version   systemd
configuration-name     firmware            kernel                  specialisation
```

The complete system, as found in the virtual machine image or on the machine
running [`cty-1.hypered.systems`](https://cty-1.hypered.systems), can be built with the
`toplevel` attribute.

Since NixOS is based on a Linux kernel, we can find in the results an
[initrd](https://en.wikipedia.org/wiki/Initial_ramdisk), a kernel, some kernel
modules, the kernel [command-line
parameters](https://docs.kernel.org/admin-guide/kernel-parameters.html) and
other files to construct a complete NixOS system.

Such a "toplevel" can be copied into a virtual machine image or copied to an
existing machine and then be "activated". (Using the `activate` script visible
in the results.) Such a way to deploy a (possibly new) system to a live machine
is used by the `scripts/deploy.sh` script found in the Curiosity repository.

# Virtual machine image

```
$ nix-build -A image
$ ls result/
nixos.qcow2.gz
```

A virtual machine image suitable for DigitalOcean can be built using the
`image` attribute. The result is an image in the
[qcow2](https://en.wikipedia.org/wiki/Qcow) image format, and compressed with
gzip.

Such an image can be uploaded somwhere (e.g. to S3) from which it can then be
imported in DigitalOcean as a custom image, then used to spin up a new virtual
machine. Once created, such a virtual machine can be updated by using the
`toplevel` attribute.

Note: see the `scripts/upload-image.sh` and `scripts/import-image.sh` for
commands to upload an image to S3 and importing it as a custom image.

Note: in addition of DigitalOcean, it is possible to easily build images for
other service providers or virtual machine managers. See a list in the [nixpkgs
repository](https://github.com/NixOS/nixpkgs/tree/master/nixos/modules/virtualisation).

# Local virtual machine

```
$ nix-build -A runvm
result/bin/run-nixos-vm
```

The toplevel can be run with a local QEMU virtual machine by building the
`runvm` attibute and running the resulting script.

Within the VM, you're automatically logged in as root. You can then see the
Curiosity systemd unit status with:

```
# systemctl status curiosity.service
```

Or you can query the `cty serve` backend through Nginx with:

```
# curl http://127.0.0.1
```

Outside the VM, you can access the web application on the 8180 port.

Note: use `Ctrl-a x` to cause QEMU to shutdown the virtual machine. You may
also want to remove the disk image created at `./nixos.qcow2`.

# Local environment

```
$ nix-build -A runenv
result/bin/run-full-environment
```

Instead of running a virtual machine, it is possible to run the `cty serve`
backend together with Nginx as reverse proxy with the `runenv` attribute and
running the resulting script.

Note: the `runenv` script uses [hivemind](https://github.com/DarthSim/hivemind)
to execute a simple [procfile](https://devcenter.heroku.com/articles/procfile)
containing two entries, one for `cty serve`, one for `nginx`.

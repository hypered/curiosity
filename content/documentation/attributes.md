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

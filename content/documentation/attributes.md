---
title: Curiosity
---


# Nix attributes

This page explains the different Nix attributes that can be found in
`default.nix`. This is useful for developers to better explore the project
(instead of only building the main artifact, a virtual machine image).

Note: for an overview of how Nix is used in this project, see
[Nix](/documentation/nix).

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

# Static web site content (alternative)

```
nix-build -A public --out-link _site
```

Instead of the `content` attribute explained above, it is possible to use the
`public` attribute` It is very similar to `content`, but embed a static
navigation bar (that shows that you're not logged in).

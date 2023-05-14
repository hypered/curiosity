---
title: Curiosity
---


# Man pages

In addition of HTML pages, parts of the documentation for Curiosity are
available as [man pages](https://en.wikipedia.org/wiki/Man_page).

Each command-line tool has its own man page. An easy way to find out what those
tools are, is to start typing `cty` at a virtual machine prompt then hit the
`TAB` key twice to see the proposed auto-completion. There is also a more
general man page available as `man curiosity`.

Assuming an SSH access to `cty.hypered.systems`, here is a possible session
demonstrating the above:

```
$ ssh root@cty.hypered.systems
[root@curiosity-1:~]# cty<TAB><TAB>
cty
[root@curiosity-1:~]# man cty
[root@curiosity-1:~]# man curiosity
```

For convenience, the man pages are also rendered as HTML pages:

- [`curiosity(7)`](/documentation/clis/curiosity.7)
- [`cty(1)`](/documentation/clis/cty.1)

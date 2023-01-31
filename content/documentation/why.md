---
title: Curiosity
---

# Why

This document can be seen as a longer version of the [about](/about) page. It
tries to explain with more context and from a more personnal perspective why I
([Thu](https://github.com/noteed)) created Curiosity the way it is.

# Timeline

This project, Curiosity, started in August 2022. It follows different
initiatives to try to radically improve [Smart Coop](https://smartbe.be/fr/)
in-house software; some of them happened when I was Smart's CTO, between July
2020 and June 2022. Among the pursued goals were (with no specific order):
reduce the number of bugs deployed to production, reduce the time to develop
new features (or fixes), reduce the complexity of the existing software (which
certainly influenced the previous points), improve internal knowledge of the
various moving pieces, etc.

Among the reasons for the current complexity is, from my perspective, how the
software grew over 20 years (since almost Smart inception, in 1998). Multiple
applications were created, one every few years, with often the intended goal of
replacing the previous application by something better. Unfortunately, although
it's probably something that happens very often in the software industry, the
new application doesn't end up replacing the previous application, but instead
both continue to exist side by side.

This means that today, Smart internal eco-system is comprised of multiple
operating systems, mutliple databases, multiple (non-identical) environments,
multiple programming languages, multiple ways to deploy to an environment, and
of course multiple applications that interact closely.

# Initiatives

I will not go through everything we did or try to reduce the complexity and
streamline software development. Instead I will talk about a common theme I
tried to follow.

I believe that Smart's internal software, from a feature set perspective, is
small and simple enough for a few persons to completely wrap their head around.
Most of its complexity doesn't reflect the business domain complexity, but
instead stem from programming decisions taken over time (e.g. use another
database migration mechanism for the new application than for the previous one,
or another authentication mechanism, or another way to produce PDFs, or a
frontend framework instead of the existing server-side framework, or another
newer frontend framework, ... while still keeping everything that came before).

Indeed, until around 2017-2018, it was possible for Smart to grow with less
than 10 developers. (At the end of 2019, they were about 20.)

Normally, with such a state of affairs, my first instinct would be to refactor
the code from the inside out: every time you work on some part of the code (to
introduce a new feature or fix a bug), try to crystallize its behavior by
adding tests and documentation, and make sure it's located in the right library
so that it can be re-used easily. In particular, try to weed out any similar
code or functionalities.

Instead, as this proved difficult (I believe most developers were afraid of
doing changes, had difficulties into adding tests, or were reluctant to touch
applications that they did not know, even if those applications were using the
same or similar code), I tried to move to a way of crystallizing the software
behavior from a more global perspective.

You can think of this change of perspective as something similar from going
from unit testing to end-to-end testing. But more importantly, you can think of
it as moving the burden from developers to business experts. Indeed, I claim
that the most important subject to overcome for Smart when it comes to internal
software development is not on the programming side (even if there are
important chanllenges there) but is to be able to express and master what the
software is supposed to do.

So the central theme I tried to follow was this: I wanted to expose the system
surface as a single unified "document" and I wanted to surface any internal
detail that matters to business experts (or anyone working closely with the
development teams). Doing so, I also wanted the "document" to be a shared and
central place to provoke discussions and a vehicule to think its evolution,
i.e. a prototype prefiguring the future of a real system.

By "document", I kind of hear a single PDF, or a book that I can hold in my
hand, which contains everything I need to know, nicely organized, with a
convenient table of content, index, glossary, table of symbols... In the case
of a software project, the "document" I had in mind was mostly a static website
that you can browse.

Two initiatives in particular really followed this common theme: the design
system, and CLIs. I will explain them a bit, because they help to undersand the
trajectory of this project, Curiosity.

## The design system initiative

We started to use this "unified view" approach from two different angles: at
the database level (although not presented as a website), and at the
application screens (or pages) level. I picture this double approach in my mind
as one layer at the top (the GUI), and one layer at the bottom (the databases),
sandwiching the bulk of the code in between. By massaging those two layers to
make them as smooth as possible, my hope was to also improve that bulk of code,
as if we applied pressure on it.

The "screens" initiative was really successful (I'm not saying the "databases"
initiative wasn't, but since I focus on the "document" approach, I will no
longer talk about the databases here). In 6 months (actually totalling 3
full-time months of work), a [design system](https://reference.smartcoop.sh/)
was created. (I'm linking to an older version of the design system, as used in
Curiosity. The official one is at
[design.smart.coop](https://design.smart.coop/).)

In addition of a design created in Figma, comprising [design
tokens](https://designsystem.digital.gov/design-tokens/), individual
components, and full-fledged example screens, the design system website
presents a reference implementation of the design. Re-creating a screen as
presented in that website amounts to simply linking to a CSS file (and also a
JS file) and making sure to emit the same HTML as the references.

The key point here is that a single site can be used as a central point to
discuss, think, and prototype Smart's applications. A single place can present
how the production screens are right now (you don't have to navigate a real
application, which may depend on access rights or other internal states, to
reach a page), how they might look like in the future, or how new screens could
look like. It also presents details that are important to designers or
developers.

Furthermore, because the design exists in Figma, in a reference implementation
(using [Pugs](https://pugjs.org/), making it very straightforward to create new
screens) and, over time, in the various technolgies used in the different
applications (e.g.  [Razor templates](https://github.com/smartcoop/design-cs)),
this means that a lot of different profiles are autonomous to create new
screens. For instance, a backend developer can quickly generate the same HTML
as the reference implementation (even more quickly once it is implemented in a
reusable package). Even if it's not perfect from a designer or UX persperctive,
those other roles can iterate on the design directly within the design system
(within the reference implementation examples, or within Figma, depending on
their skills). The result of their work can be applied in the production
system at a later date.

As a "measure" of the success of the design system initiative, I'll mention
that multiple Smart european partners started to use it, and that it was used
within Smart Belgium in both a very new application, and in an existing and
ancient application (if you use Smart's services, you'll have noticed that the
login and news pages have adopted the design system around June 2022). As
predicted, changing the design of the ancient application applied some
"pressure" and some of its code was improved at the same occasion.

## The CLI intitiative

Command-line interfaces (CLIs) seem prehistoric. If you're old enough, you may
have used them when you were kids, on the first PC of the familly. Nowadays,
they are used mostly by developers. With experiences in the Linux and Open
Source world, I'm used to CLIs and value them. Smart, maybe because it is a
Microsoft shop, is more used to graphical user interfaces (GUIs), be it native
desktop applications or web apps.

Although it's no longer "screens" or "documents" in the sense of a browsable
website, CLIs share a lot of the qualities of the theme I was after. Maybe
obviously, the "I" in GUI and CLI is the same: it stands for "Interface". This
is the surface through which you can interact (and observe) a system.

Similarly to a website that you can browse, [well-done
CLIs](https://clig.dev/) have a flag, (usually `-h` or `--help`) that serves as
an entry point to start discovering what the CLI can do. Nowadays, constructing
a CLI, as a programmer, will create such a flag and description almost
automatically.

Even if not necessary from an end-user perspective, additional commands can
quickly be added to expose (or "surface") internal aspects of the system to
make it quicker to interact with or observe it.

Maybe this is a stretch, but even if you're not a developer, imagining a new
command, its parameters, its effect on the system, and its outputs can be done
by someone close to the developement teams.

It's hard to quantify the success of this initiative. But since developers have
practiced it, some of them, even some that were reluctant at first, now turn to
create a CLI as a first approximation of what a feature should do, improve it,
then only create an equivalent graphical interface.

# Curiosity

The Curiosity initiative happened after my role as a CTO and this time, instead
of having a leadership role where developers were doing the actual programming
work, I had to do the progamming myself.

Furthermore, it was based mostly on a proposal I made (instead of an explicit
demand from Smart), itself following a [previous
description](https://github.com/noteed/start-servant) of what I wanted to mean
by "prototype". And although the project started in August 2022 and was planned
to go until January 2023 included, I had very few contacts with Smart after
mid-November 2022.

This means on one hand, I had a lot of freedom to push the project towards an
opinionated direction, the one exposed above, and on the other hand, the
project lacked a focus on more business-oriented features. For instance some
commands supported by Curiosity are exposed through SSH, a nice feature from a
technical perspective, but the concept of "invoice" is very light on the
details.

Compared to the design system, Curiosity has to implement features from the
business domain (so that people can simply use the system, instead of studying
it). In other words, this is more app-like, and since this "document" becomes
inherently dynamic, what I'm proposing is a living documentation.

> Living documentation is a dynamic method of system documentation that
> provides information that is current, accurate and easy to understand. \[...]
> Business stakeholders can review the documentation to ensure that it
> describes the desired behavior of the system from a logical standpoint.
> Developers can use the information to help them program only what is needed,
> making the code as lean as possible. \[...][1]

[1]: https://www.techtarget.com/searchsoftwarequality/definition/living-documentation

## CLI

Another key aspect of Curiosity is how, although it is presented as a website,
it is constructed very much around a CLI, the `cty` program (the web
application is simply a sub-command, `cty serve`). Since this is a text about
live documentation, I use the opportunity to display its `--help` message (and
note that it is rendered dynamically by the backend, by running the exact code
that is deployed on the server, not a copied version):

<pre><code><!--# include virtual="/help.txt" --></code></pre>

## Scenarios

Curiosity can interpret [test](/documentation/tests)
[scenarios](/documentation/scenarios) (that use the commands of the `cty` CLI
shown above) and render them as the following table:

<!--# include virtual="/partials/scenarios/user-signup" -->

We can view the command being run (preceded by its line number within the
scenario), and the CLI output, but we can also follow links in the columns on
the right where we can see the internal state evolve across the scenario.

## Screens

On the "screens" aspect, the [sign up
process](/documentation/processes/signup#web-interface)
can be described and explored in a documentation page that links to specific
screens (e.g. the signup form, the message shown upon succesful registration)
and also the specific email sent.

# Final thoughts

With Curiosity, I think I've shown how features can both exist in a live system
and be faithfully re-used in the documentation without duplication. With
additional business expert inputs, I believe complete (business) features could
have been implemented in the timeframe of the project. Curiosity is very small
and it's hard to prove that the approach would scale for the whole Smart
eco-system, but I think it would, and also that a production system could be
built this way (instead of merely a prototype).

I didn't talk about the more technical aspects of Curiosity, but it's written
very similarly to a real system. For instance, it is setup within a virtual
machine image, with a reverse-proxy, ready to be deployed in a cloud provider
or on-premises infrastructure, as demonstrated by
[`smartcoop.sh`](https://smartcoop.sh), and a basic continuous integration and
automated deployment pipeline exists.

Unfortunately Curiosity failed to attract a discussion centered around it.
Communications about the project, when originating from Smart, continued to be
expressed without talking about Curiosity's content per se or ability to
display its inner workings. I remained the only person trying to organise
content or thinking of new ways to display useful information (e.g. the
relationship between users, business units and legal entities rendered as SVG
diagrams).

I still think that such a prototype, powering a live documentation website, is
a poweful approach to master the developement of a software application.

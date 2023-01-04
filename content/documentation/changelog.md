---
title: Curiosity
---

# Changelog

This [changelog](https://en.wikipedia.org/wiki/Changelog) is a list of notable
changes (e.g. new features or bug fixes) made to the Curiosity project. This
helps people get a quick overview of recent evolutions of the project. Most
entries in the changelog link to Pull Requests (PRs), i.e. proposals by
developers (or more generally, contributors) to apply changes to the [code
base](/documentation/source).  PRs may contain additional details and
discussions, or provide historical context.

## 2023-01-04

**Introduce a [GitHub actions](https://github.com/hypered/curiosity/actions)
based CI job.** This job builds and tests Curiosity through its Nix build.

- We had to alter some niv sources to simplify the CI setup. See the
  [12956fe](https://github.com/hypered/curiosity/commit/12956fe85bf35aabbe56e05c17d7977490c2198f)
  commit message for more details.
- See [PR-108](https://github.com/hypered/curiosity/pull/108).

**Introduce a NixOS VM integration test.**

- The current test only only makes sure the production environment is able to
  serve a page, the static assets are correctly brotli-compressed and the Nginx
  SSI setup works as expected.
- A startup race condition pushed us to use the systemd notification mechanism.
  See the corresponding change in the
[2a412f1](https://github.com/hypered/curiosity/commit/2a412f16c3b912ca3305914f02aacd7efdf560b7)
  commit.
- Add some hardening options to the Curiosity systemd service.
- See [PR-107](https://github.com/hypered/curiosity/pull/107).

**Complete `cty sock`.**

- `cty-sock` was just echoing the received command (after parsing it).  Now it
  actually runs it, similarly to `cty run` or `cty repl`
- Make additional `cty <command>` work with the `--socket` option (`state`,
  `user get`,  `user signup`)
- The HTTP server opens the UNIX domain socket by default, and an option is
  offered to disable that behavior
- Some tests for commands serialisation/parsing are added
- `cty-sock` is now a subcommand, i.e. `cty sock`
- See [PR-111](https://github.com/hypered/curiosity/pull/111).

**Sending `cty` commands through SSH now works.**

- With this change, the user is now correctly set through SSH's ForceCommand,
  and the command is used with the `--socket` option. This means that a remote
  user with access to the server's `curiosity` user can now alter and look at the
  HTTP server's state. The `/curiosity.sock` file within the VM must be readable
  and writable by the curiosity user though .
- See [PR-112](https://github.com/hypered/curiosity/pull/112).

**Use a Nix binary cache.**

- We add a Nix post-build hook to the GitHub action build pipeline that pushes
  the produced store paths to a Backblaze object storage.
- We configure the object storage as a binary cache for the builder to speed up
  the build phase by caching the dependencies builds. The VM image is also
  configured to use the cache.
- See [PR-115](https://github.com/hypered/curiosity/pull/115).

**Code cleaning.**

- Rename some functions to be more consistent, remove unused code, and make
  compilation warning-free.
- No longer use `Commence.Runtime.Storage`. It was seldom used (i.e. only by
  some `UserProfile` operations, and in only some routes in `Server.hs`). This
  allows to move `checkCredentials` and `checkPassword` in the right modules
  (pure code in `Data` and STM-based code in `Core`).
- Split out `Runtime` modules.
- Apply HLint recommendations.
- See [PR-109](https://github.com/hypered/curiosity/pull/109),
  [PR-117](https://github.com/hypered/curiosity/pull/117), and
  [PR-121](https://github.com/hypered/curiosity/pull/121).

**Add a search feature to the documentation.** This adds a new page to query
the static documentation part of the site.

- This is based on [Stork](https://stork-search.net): the search index is a
  static file (`content.st`) built ahead of time, and it is queried by a
  companion JS library.
- The search results are currently not very precise but this uses an old
  version of Stork. The current one is 1.5 (which is also the JS library
  version we use, which could cause potential compatibility issues).
- The CSS are currently very crude and should probably be provided by the Smart
  design system.
- See [PR-118](https://github.com/hypered/curiosity/pull/118).

**Add Ormolu, together with treefmt.**

- This adds Ormolu as the Haskell source code formatter, replacing Brittany.
  Brittany is unmaintained since 2022-11-11. This also adds a `treefmt`
  configuration file to drive Ormolu. That configuration could be extended in
  the future to e.g. also format Nix expressions.
- See [PR-119](https://github.com/hypered/curiosity/pull/119).

**Add an automatic deployment step to the GH action.**

- This calls
  [`scripts/deploy.sh`](https://github.com/hypered/curiosity/blob/main/scripts/deploy.sh)
  after a succesful build when the impacted branch is `main`, so that
  [`smartcoop.sh`](https://smartcoop.sh) is deployed automatically.
- See [PR-125](https://github.com/hypered/curiosity/pull/125).

**Improve the run command.**

- The [`/run`](/run) page can access scenarios when using the `run` command.
- The output was not showing quotes around multi-word arguments. This is now
  fixed.
- In the HTML representation of a scenario output, links to individual states
  were broken when the scenario was calling another scenario (using the `run`
  command). This is now fixed.

See [PR-126](https://github.com/hypered/curiosity/pull/126).

**Link the reject command in the web interface.**

- A `reject` command was aleady present in the command-line interface. It is
  now visible behind the "3 dots" action button on the list of quotations.
- See [PR-127](https://github.com/hypered/curiosity/pull/127).

**Continue the quotation flow.**

- Call the "dry run" validation in the quotation flow. This simply calls the
  validation code, without impacting the system state.
- In the setup code, we rename Susie (the seller) to Mila for consistency, as
  Mila is defined to be within Alpha and One in state-0.
- `forms quotation new` can now requires a seller entity and a seller unit, and
  a buyer entity and a buyer unit. The quotation flow is updated to setup Alpha
  and One so they can be used later in the scenario.
- See [PR-123](https://github.com/hypered/curiosity/pull/123).

**Add an SVG representation of the state.** The SVG representation is a graph
that shows users, legal entities and business units as nodes. It also shows
some roles as edges.

- That representation is available at [`/state.svg`](/state.svg) (remember that
  there is already [`/state`](/state) and [`/state.json`](/state.json)).
- Similarly for executed scenarios, it is possible to see the SVG
  representation of the state after each step, e.g. in the [quotation
flow](https://smartcoop.sh/documentation/scenarios/quotation-flow).
- See [PR-128](https://github.com/hypered/curiosity/pull/128).

**Improve documentation.** This adds some improvements to the documentation and
README.

- In the README, re-organise some sections, adding some details, or fixing some
  non-up-to-date examples.
- An asciicast is added. This is mostly to show how asciicasts can be added in
  the future, but the example is a short introduction to the `cty` command-line
  interface.
- For local rendering of the static documentation (from Markdown files), it is
  now possible to get a static navigation bar (instead of the Nginx SSI-based
  one).
- Still fro local rendering, there is an automatic rebuild available with
  `entr`. It had not the static files (CSS, ...). This is now fixed.
- A [`/documentation/attributes`](/documentation/attributes) page is added. It
  documents all the interesting Nix attributes available in the `default.nix`
  file.
- A [`/documentation/why`](/documentation/why) page is added. It gives context
  and reasons to create Curiosity
- See [PR-110](https://github.com/hypered/curiosity/pull/110), and
  [PR-130](https://github.com/hypered/curiosity/pull/130).

## 2022-11-17

**Make profiles more complete.** This PR enriches user profiles, legal
entities, ...

- Turns `@` mentions into link within user profile bios or legal
  entities
- Make email addresses clickable
- Show advisors in user profile
- Add a registration date to the user profile
- Allow to link a user to a business unit
- Rename `cty business` to `cty unit`, and `cty legal` to `cty entity`
- Add a` IsSupervised` flag to entities, meaning their accounting is
  managed by the group operating Curiosity
- Add a `IsHost` flag to entities, meaning they can "host" business
  units
- Add additional users and entities to `state-0`
- See [PR-102](https://github.com/hypered/curiosity/pull/102)

**Add a `cty user invite` command.** This PR changes the user profile data type
so that having a username / password is optional. Instead, it is possible to be
"invited" (a token is used to authenticate instead). Accepting the invitation
(and providing a username and a password) is not yet implemented. The
invitation command is `cty user invite`.

To make things clearer, `cty user create` is renamed to `cty user
signup`. See [PR-103](https://github.com/hypered/curiosity/pull/103).

**Add a `cty quotation reject` command.** It was possible to "accept" a
quotation (thus creating an order). This PR adds the possibility to "reject" a
quotation, and the rejection accepts an optional comment. See [PR-104](https://github.com/hypered/curiosity/pull/104).

**Improve documentation.**

- The navigation bar in the documentation part of the web site now shows
  whether the user is logged in or not (just like in the rest of the
  site).
- The documentation part of the site was served with an aggressive cache
  control policy, which made the pages not refreshed after changes. This
  is now disabled.
- See [PR-105](https://github.com/hypered/curiosity/pull/105)

**Display individual emails.**

- From the list of emails at `/emails`, it is now possible to see each
  individual email.
- Emails have now a body (a template) and a title, defined by their
  template name. Those are visible in the individual email view.
- See [PR-105](https://github.com/hypered/curiosity/pull/105).

**Improve the signup flow.** This PRs improves the sign up flow:

- After sign up, a message is shown. It was using a Dialog component; it
  is now using a regular page. (The Dialog was using JavaScript, causing
  the browser to focus on the Smart logo, making a blue border appear.)
- The logs and `cty run` now makes clearly appear that an email is being
  sent during sign up.
- `cty reset` output is now slightly better.
- Scenarios have been renamed (e.g. `0.txt` is now `user-signup.txt`).
- A dedicated documentation page now exists for the sign up process.
- The echo pages were just showing text (as code). They now have a
  proper navigation bar too.
- During sign up, some validation code was executed. That code is moved
  to the `User` module.
- It is also expanded to check that the TOS have been accepted, and a
  scenario reflecting that is added.
- See [PR-106](https://github.com/hypered/curiosity/pull/106).

## 2022-11-08

**Document the main sale flow.** This adds a dedicated [documentation
page](https://smartcoop.sh/documentation/smart) written by Roger to document
some business matter, and in particular a Sale flow. To contrast that flow with
the existing `quotation-flow.golden`, this scenario is now rendered within a
[documentation page](/documentation/scenarios/quotation-flow). See
[PR-93](https://github.com/hypered/curiosity/pull/93).

**Add a processing thread to send emails.** A thread is now running in the HTTP
server and in the REPL to "send" emails. Currently it simply changes the email
state from "TODO" to "DONE". Commands are added to stop or start the thread, or
to run its logic manually. This can be used within the HTTP server or the REPL
when the thread is stopped, or with `cty` or `cty run`. See
[PR-94](https://github.com/hypered/curiosity/pull/94).

**Simplify the `Data.Db` data type.** The `Data.Db` type was parameterized by a
"runtime" type, propagated to `HaskDb` and `StmDb`. I think the parameter was
no longer necessary since I removed the `RuntimeHasStmDb` class in
`b9972e90281723a3b18f857bf7ddfb763a17e7e9`. See
[PR-95](https://github.com/hypered/curiosity/pull/95).

**Document golden testing, improve `cty run`.** This mainly adds a
[documentation page](/documentation/tests) to explain how golden testing works
in the Curiosity project. To do so, some new routes are added to be able to
embed scenarios and their golden files within the documentation page.

This also adds some flags to `cty run` to be able to control whether
regular output and/or the final state of the scenario should be printed to the
console.

A nice aspect of using the new flags is that `cty run`, when only reporting
the final state, should not retain in memory more than the runtime state, and
that displaying it should make sure its final state is fully evaluated. This
could be the basis for some benchmarking code (e.g. process a long scenario as
fast as possible). See [PR-96](https://github.com/hypered/curiosity/pull/96).

**Add a script to generate a code coverage report.** This currently only calls
the `cty` binary, and not the tests. The result is a set of HTML pages showing
the code that is never executed. See
[PR-97](https://github.com/hypered/curiosity/pull/97).

**Update the OpenSSL library from 3.0.5 to 3.0.7.** This fixes a vulnerability,
see [this blog
post](https://www.openssl.org/blog/blog/2022/11/01/email-address-overflows/).
See [PR-98](https://github.com/hypered/curiosity/pull/98).

**Augment the legal entity, user profile, and business unit types.** This PR is
based on an email from Roger titled 'pour les visuels web des fiches "legal"
"person", business unit"' from 2022-08-27.

- Add some notion of legal entity authorization: This makes it possible to
  add a user, specifying a role, to a list of "acting" users within the
  entity. Maybe this should be called "contacts" instead.
- Add some notion of user profile authorization.
- Add some notion of business unit authorization, and also notions of Scope
  Code, Holders, and Activity types.
- See [PR-99](https://github.com/hypered/curiosity/pull/99).

**Document the concepts of queues and steps:**

- Add command to show emails to send:
  - This adds `queue emails-to-send`
  - And now `queues` reports also the emails to send
- Now step `--all` also send emails; it was only "verifying" emails
  (i.e. set users as having a verified email address).
- Add a [scenario](/documentation/queues) for signup email and email
  verification. This shows that after a user is created, an email must be sent
  to that user, and their email address must be verified. After running `step
  --all`, queues are empty. (Currently, email verification is just  setting the
  "verified" bit to true, without actually checking some one-time token.)
- Add a `--dry` option to the step command, to show what would happen, but
  without actually doing it.
- Add a `time` command with a small scenario and a [documentation
  page](/documentation/time).
- See [PR-100](https://github.com/hypered/curiosity/pull/100).

**Modules reorganization.** This PR splits the `Curiosity.Runtime` module into
additional modules. See [PR-92](https://github.com/hypered/curiosity/pull/92).

**`procfile`-based environment.** This is a first step towards writing an
integration test suite.

We set up an environment running the Curiosity server behind an Nginx reverse
proxy. This environment is mostly provisioned using the various existing Nix
build artifacts.

While this setup does not provides an isolation level comparable to a VM-based
one, it's much more lightweight. See
[PR-101](https://github.com/hypered/curiosity/pull/101).

## 2022-10-21

**Add a "Quick start" section to the [CLIs
documentation](/documentation/clis).** In particular, this gives examples for
`cty --help`, `cty init`, `cty reset`, `cty state`, `cty user create`, `cty
run`, and `cty serve`. See
[PR-77](https://github.com/hypered/curiosity/pull/77).

**Web page to run `cty run` commands.** This adds a [web page](/run) with a
single field to run a command, similar to what `cty run` supports, and the
corresponding `POST` handler. If the user is logged in, their username is used.
Otherwise, the command is run as `--user nobody`. That `nobody` value currently
doesn't mean anything but could be useful/improved in the future to support
some anonymous commands. See
[PR-75](https://github.com/hypered/curiosity/pull/75).

**Complete the contract flow.** This is still not yet complete, but now
submitting the form triggers a validation function, same for the CLI.

- This adds a simple `CanCreateContracts` right.
- This adds a data type `CreateContractAll` to represent form data. Form data
  are kept in memory server-side to offer possibly multiple screens to view
  (and confirm) or edit the data.
- The final screen shows the data ready to be submitted, together with the
  result of the validation rules. Upon submission, validation rules are applied
  again before creating the "real" (as opposed to editable form data) contract.
- See [PR-76](https://github.com/hypered/curiosity/pull/76).

**Alternative contract form: a "simple" contract.** The end goal would be to
have a single contract form that accommodates two scenarios: what we call the
"simple" or "3in1" form, that generates invoices, work contracts, and expenses
from a single form, and the "business unit" or "Activité" case, where the
different documents are created through separate forms. (This was started in
the previous PR.) See [PR-78](https://github.com/hypered/curiosity/pull/78).

**Add the list of roles and list of countries to the documentation (re-using
the same data used in the application).** See
[PR-79](https://github.com/hypered/curiosity/pull/79).

**Show additional data in the documentation.**

- [Static data](/documentation/validation-data): we display roles, countries,
  and VAT rates (the username blocklist was already present).
- Static data: we display [permissions](/documentation/permissions) on their
  own page.
- [Live data](/documentation/live-data): we display the legal entities in the
  documentation since those are slowly changing data.
- See [PR-80](https://github.com/hypered/curiosity/pull/80).

**Start implementing the
[quotation flow](https://github.com/hypered/curiosity/blob/2ab9c94b040e9e39e3a5586b4026d062e552fbcb/scenarios/quotation-flow.golden).**
This means the following commands are added:

- `forms quotation new`
- `forms quotation validate`
- `forms quotation submit`
- `quotation sign`
- `invoice emit --from`
- `reminder send`
- `payment match`

And the follow new data types are added:

- [Quotation](/haddock/Curiosity-Data-Quotation.html)
- [Order](/haddock/Curiosity-Data-Order.html)
- [RemittanceAdv](/haddock/Curiosity-Data-RemittanceAdv.html)
- [Email](/haddock/Curiosity-Data-Email.html)

Unrelated to the above scenario, there is also a new
[Demand](/haddock/Curiosity-Data-Demand.html) data type (see its module
description) and the simple contract form has been augmented. See
[PR-81](https://github.com/hypered/curiosity/pull/81).

**Remove some build warnings.** See
[PR-82](https://github.com/hypered/curiosity/pull/82).

**Expose scenario's intermediate states.** This change adds the list of
available test scenarios to the [documentation](/documentation/scenarios).
Scenarios are displayed as they appear in a golden file (although they are run
live upon requesting the web page) and the intermediate states are linked after
each command. See [an example](/scenarios/quotation-flow). See
[PR-84](https://github.com/hypered/curiosity/pull/84).

**Start a better separation between pure/STM/IO code**.

- `Curiosity.Data` should provide only pure code (no `STM`, no `IO`).
- `Curiosity.Core` adds `STM` to that. There is no call to `atomically` there.
- `Curiosity.Runtime` adds IO to that, mainly for logging, and for making
  atomic transactions out of the STM operations defined in `Core`.

We're demonstrating that with the `reset` operation (but we haven't moved
all the `STM` or `IO` out of `Data` yet).

Moreover, a call to `runAppMSafe` is replaced by `runRunM`. This removes one
layer of `Either` case pattern matching, is great since it was obfuscating that
`readFullStmDbInHaskFromRuntime` was already safe. See
[PR-83](https://github.com/hypered/curiosity/pull/83).

**Avoid unnecessary re-builds.** The Haskell build is quite expensive; we
aggressively filter its source inputs to make sure that editor artifacts and
other changed files don't end up triggering a rebuild.

We take advantage of this filtering rework to move back the Haskell sources
filtering to the edge: we clean the derivation sources before sending it to
`cabal2nix` project lists. Doing this allows us to apply a package-specific
filter instead of a generic one. See
[PR-85](https://github.com/hypered/curiosity/pull/85).

**List quotation forms on the homepage.** See
[PR-87](https://github.com/hypered/curiosity/pull/87).

**Clean `Conf` data type.** This removes a function from the `Conf` data type,
and also bump the Commence dependency for similar reason. This exposes
command-line option to configure the logging (to `stdout`, to a file, or
disabled). See [PR-88](https://github.com/hypered/curiosity/pull/88).

**Fix links to forms in the documentation.** See
[PR-89](https://github.com/hypered/curiosity/pull/89).

**Complete the documenting handlers.** This adds mainly handlers for the
quotation routes, but also echo handlers for `User.Update` and
`SimpleContract.SubmitContract`, so that different forms are documented in the
same way. See [PR-90](https://github.com/hypered/curiosity/pull/90).

**Complete the quotation flow done through the web interface.** This branch
mainly completes the web interface version of the quotation flow (as visible in
`scenarios/quotation-flow.golden`). One of the main aspect is sending emails
during some operations, and thus this branch makes it possible to see the sent
emails. This also means a client is now necessary within the `Quotation` object
so that an email has a meaningful recipient.

To better see the complete flow, in addition of emails, we now display
quotation forms, quotations, and orders on the user homepage, and state is
added to the `Quotation` object to know if it is sent or signed.

An interesting aspect is that the "signed" state requires an order ID, making
it more difficult to forget to create an order in the same transaction that
sets the state to "signed". See
[PR-91](https://github.com/hypered/curiosity/pull/91).

## 2022-09-12

Any user or business unit has a public profile page visible at `/username` or
`/unitname`. The code was mostly done in the past, but it was difficult to
combine it with serving the static assets or documentation. This is now done.
[PR-73](See https://github.com/hypered/curiosity/pull/73).

A long-standing issue related to the log file was resolved. It appeared when
the log file was supposed to be rotated (after growing past some limit), but
the number of rotated log files to be kept was set to zero. Increasing that
number solved the problem. See
[PR-74](https://github.com/hypered/curiosity/pull/74).

## 2022-09-09

Start "user actions", with the email address to be verified as a first action.

- This change the rather crude homepage to a nicer datagrid (to display user
  profiles with an email address waiting to be verified).
- The datagrid has links to let user perform actions.
- A dedicated page to actually perform the action is added. (This acts as a
  confirmation page.)
- See [PR-61](https://github.com/hypered/curiosity/pull/61).

First attempt at creating an EDSL (Embedded domain-specific language). When
`cty run` was demonstrated, it was asked if it supported `if/then/else`
statements (it doens't). In the case something more powerful than `cty run` is
needed, one option would be to offer an EDSL. The "E" means that the "language"
is provided within a host language, in our case Haskell.

- An example script using the EDSL is [visible in the
  PR](https://github.com/hypered/curiosity/pull/64/files#diff-e1ec2e690f8583bcd58ec147dcad8962ab1492d6ddc33e6f4fa444a0b9e8c267R61-R76).
- It might be useful in the future to describe business rules a clearly as
  possible, while making them computer-readable (and executable).
- See [PR-64](https://github.com/hypered/curiosity/pull/64).

Add the main concepts of the prototype: legal entities, business units,
employment contracts, and invoices.

- Add HTML views, with exemple JSON files.
- Update the documentation to refer to them.
- Begin a form to create a contract. The state of the form is kept within the
  server, which allows us to have multiple pages if necessary. This is used
  here to add/edit/delete expenses as part of the main contract. (The form is not
  yet complete.)
- Introduce the idea of a set, called `state-0`, of well-known example objects
  that can be manipulated by test scenarios. See the
  [documentation](https://smartcoop.sh/documentation/scenarios).
- See [PR-63](https://github.com/hypered/curiosity/pull/63),
  [PR-68](https://github.com/hypered/curiosity/pull/68), and
  [PR-72](https://github.com/hypered/curiosity/pull/72).

Start to expose some data as UBL (in JSON). An example of legal entity can be
viewed as a UBL `PartyLegalEntity`.

- The structure of that representation is taken from
  [issue 39](https://github.com/hypered/curiosity/issues/39).
- See the example in the
  [documentation](https://smartcoop.sh/documentation/ubl).
- See [PR-65](https://github.com/hypered/curiosity/pull/65).

Add [Brotli compression](https://en.wikipedia.org/wiki/Brotli) to Nginx. This
was done as part of improvements to the [web.dev](https://web.dev/measure/)
metrics but was not yet merged.

- See [PR-66](https://github.com/hypered/curiosity/pull/66).

Some improvements to the code.

- See [PR-67](https://github.com/hypered/curiosity/pull/67) and
  [PR-58](https://github.com/hypered/curiosity/pull/58).

An auto-reload mechanism used during development is added.

- When used, a change to the source code causes an open web page to be
  automatically reloaded.
- See [PR-69](https://github.com/hypered/curiosity/pull/69).

Improve the `cty` command-line.

- Some features supported by `cty` are now available in `cty run` or `cty repl`
  and vice versa.
- In particular, `cty run` supports `run`ning scripts (i.e. a script can call
  other scripts). This is used in a script to set the system to `state-0`,
  mentioned above.
- See [PR-70](https://github.com/hypered/curiosity/pull/70).

Add a script to find broken links on [`smartcoop.sh`](https://smartcoop.sh).

- See [PR-71](https://github.com/hypered/curiosity/pull/71).

## 2022-08-23

A regular user (by opposition to a system user) is now available in the
virtual machine image.

- The user has access to the Curiosity binaries, Bash completion and man
  pages.
- The default options for `cty serve` and the options for the `cty serve`
  service are changed to no longer overlap (e.g. the default listen port is
  9000, but the service is configured to use 9100).
- There is now a `play.smartcoop.sh` domain, and Nginx is configured to
  forward its requests to port 9000. I.e. when the user decides to run
 `cty serve`, it's possible to access it on that new domain.
- `jq` is also available to further process some `cty` output.
- See [PR-52](https://github.com/hypered/curiosity/pull/52/) and following
  commits.

`cty run` and related commands are improved to be easier to use and more
useful, in particular to write golden tests.

- Scenarios can contain comments, starting with the `#` character.
- The output of `cty run` now shows the input line numbers and the commands
  being run.
- `cty user get` accepts a `--short` option to make its output less verbose.
- `cty run` understands a `reset` command, to make the state empty.
- Internally, new users are appended at the end of the list of users (instead
  of at the beginning).  This makes for a nicer default when displaying the
state.
- A new `cty users` command is added to filter users.
- `cty run` understands an `as` command, to change the default user for the
  next commands in the script.
- Enable some completion: for the filenames for `cty run`, and for user IDs
  (the later is really just a helper, and is not dependant on an actual state).
- See [PR-53](https://github.com/hypered/curiosity/pull/53/).

Introduce [golden
testing](https://ro-che.info/articles/2017-12-04-golden-tests), based on the
behavior of `cty run`.

- The underlying code of `cty run` can be used with the `tasty-golden` library.
- Scripts under the `scenarios/` directory now have an expected output (using
  the `.golden` extension).
- A new test program runs all scenarios, ensuring their output match the golden
  files.
- See [PR-54](https://github.com/hypered/curiosity/pull/54/).

Add concepts of business entities, legal entities, invoices, and employment
contract.

- Those contain only an ID for now.
- Commands to create instances of those concepts are added to `cty`, e.g.
  `cty invoice`.
- See [PR-56](https://github.com/hypered/curiosity/pull/56/).

Various improvements to the web pages, before we can expose additional features
already present in the CLI (e.g. set an email address as verified, or create a
new invoice).

- Add a dividing line in the user menu.
- Show the logged in username in the user menu.
- Fix the hamburger menu on the Markdown-based pages.
- The whole settings pages (including the main navigation header) are
  scrollable.
- Rename "logout" to "sign out".
- Add a side menu to the settings page.
- Make the main navigation header mobile friendly.
- See [PR-57](https://github.com/hypered/curiosity/pull/57/).

Improve some metrics, as measured by [web.dev](https://web.dev/measure/). The
first measures were 68, 93, 100, 85 (Performance, Accessibility, Best
Practices, SEO). Now they are 97, 100, 100, 100.

- Some static assets are now served directly by Nginx instead of its
  `cty serve` backend.
- Enable gzip compression.
- Remove unused JS and CSS files.
- Add a "description" meta tag and improve contrast of the footer link (this
  improves the SEO score).
- See also the previous PR.

## 2022-08-16

- Various elements usually required of any business application:
  - Add a simple "right" to user profiles to allow to "verify" email addresses.
    Without the right, the user cannot do the operation.
  - Start the concept of "queues", where actions waiting to be taken, either by
    users or by the system can be seen.
  - Add an operation to run the waiting actions.
  - Improve some `cty` aspects.
  - The above is mostly done on the CLI level, except for `/` route, now
    demonstrating the waiting actions (user email verification for now).
  - Add public profile view, and an "introspection" view.
  - See [PR-50](https://github.com/hypered/curiosity/pull/50/).
- Improve JSON generation by hiding null fields. See
  [PR-46](https://github.com/hypered/curiosity/pull/46).
- Improve the handling of data store transactions. See
  [PR-47](https://github.com/hypered/curiosity/pull/47).
- Improve build times by better filtering what is source code or not. See
  [PR-48](https://github.com/hypered/curiosity/pull/48).
- Offer a Docker image. See
  [PR-49](https://github.com/hypered/curiosity/pull/49).


## 2022-08-06

- Introduce a `Login` data type, instead of re-using the `Credentials` one. The
  user profile has been enriched with some fields. Those are supposed to be
  checkd or verified, leading to different access rights. They are visible in the
  user profile view, and additional example data are provided. See
  [PR-42](https://github.com/hypered/curiosity/pull/42) and a [new
  documentation page](/documentation/objects/users).
- Streamline some `cty` usage: some commands have nicer outputs or error
  reporting, and now support an explicit `--memory` option to not read or write
  any file, or talk to a UNIX-domain socket. See
  [PR-43](https://github.com/hypered/curiosity/pull/43).
- Beginning of a test suite for the library code. See
  [PR-44](https://github.com/hypered/curiosity/pull/44).

## 2022-08-02

- Expose the [Haddock documentation](/haddock/), add a favicon. See
  [PR-41](https://github.com/hypered/curiosity/pull/41).

## 2022-08-01

- Fix the User Id generation, used in the signup form and the `cty user create`
  command. See [PR-35](https://github.com/hypered/curiosity/pull/35).
- Add a blocklist for usernames. This is interesting because it shows how to
  use hard-coded data both in some business-logic and show those same data in
  the documentation. See [PR-36](https://github.com/hypered/curiosity/pull/36).
- The `smartcoop.sh` domain is now available in HTTPS. See
  [PR-37](https://github.com/hypered/curiosity/pull/37).

## 2022-07-31

This is the initial entry in this changelog. This is mostly a setup phase where
we tried to have a basis onto which we can implement future "business" feature
requests. URLs and links are the one existing when this entry is written, and
may become obsolete as the project evolves.

- Project
  - Setup repositories: `smart-design-hs`, `commence`, and `curiosity` (the
    last one is the main repository, the other are supporting libraries).
  - Buy and setup `smartcoop.sh`.
  - Rent a VM at DigitalOcean.
  - Automate the deployment of `smart-design-hs` to `design.smartcoop.sh`.
  - Scripts to deploy to our virtual machine image to `smartcoop.sh`. See demo
    environment below.
  - A local Nix shell is available with our binaries, Bash completion, and man
    pages to interact with them.
  - Expose useful things in `default.nix`, e.g. the Haddock documentation.

- Web site structure
  - [`/`](/): Landing page and homepage (i.e. when the user is logged in).
  - [`/about`](/about): About page.
  - [`/documentation`](/documentation): A set of Markdown files.
    - We made it possible to drop the `.html` from URLs.
  - `/views`: Shows our data in HTML. This uses the data from `/data`. See
    documentation.
  - `/forms`: Shows our HTML forms. See documentation.
  - `/messages`: Shows our feedback messages, upon user actions. See
    documentation.
  - `/data`: Shows our example JSON data. See documentation.
  - The complete internal state is available at `/state` and `/state.json`.
  - `robots.txt`, `humans.txt`, `security.txt` are present.
  - This re-uses the Smart design system.
  - This changelog.

- Basic user management
  - User sign up (username, password, email address). No validation yet.
    No confirmation email.
  - User login.
  - User logout.
  - User profile page that shows the above information, plus a display name.
  - Edit user profile page. This actually only changes the password, where it
    should be the display name instead.

- Command-line interfaces
  - `cty serve` is the web application.
  - `cty` is a command-line application designed to perform most of the same
    actions available on the web site. It can run those actions eitheir against
    its own local state, or the state of a running application.
  - `cty run` runs a script containing REPL commands.
  - `cty-sock` is intended as a target of `cty`, without running a web
    interface.
  - `cty parse` parses REPL commands.
  - `cty repl` allows to run multiple commands in a single session. This mostly
    avoids repeating the `cty` program name.
  - `cty-shell` is a small program running behind SSH, for the `curiosity`
    user. It exposes only some Curiosity-related command.
  - The syntax used in the REPL is the same as the `cty` commands themselves.
  - The complete internal state is available with `cty state`.

- Virtual machine image
  - Create a `configuration.nix` file to specify the content of our image.
  - The VM contains our binaries.
  - The VM contains our man-pages.
  - Bash completion for our binaries is setup within the VM.
  - The VM runs `cty serve`.
  - The VM runs `Nginx` as a reverse-proxy, in front of `cty serve`.
  - When running localy, ports 22 and 80 are forwareded to the host.
  - Create a `curiosity` user on the VM to be able to run `cty`, and in
    particular interact with `cty-sock`. This is currently wired to `cty parse`
    instead. TODO Document this and other things.
  - SSH configuration (using `cty-shell` and `ForceCommand`) to expose the
    above `curiosity` user.

- Demo environment
  - `smartcoop.sh` exposes a virtual machine (using the image defined above)
    running at DigitalOcean.

---
title: Curiosity
---

# Users

## Base user (Completion-0)

The "base" user is the initial state of the user object. To create it, the
system requires:

- a username
- a password
- an email address
- an acceptance of our TOS (using a mandatory checkbox)

The email address is necessary because it is used by the password reset process
(which involves sending a non-guessable URL be email).

Most features of the system are available to users "upgraded" from the "base"
status.

Bob

:   See [static example](/views/profile/bob-0.json) (—
    [JSON](/data/bob-0.json)), or [live example](/bob).

## Completion-1 user

When sufficient additional information are filled by the user, additional
services are available (e.g. registering to a training session or using
fee-based personal services).

The additional information are:

- Postal address
- Telephone number

Bob

:   See [static example](/views/profile/bob-1.json) (—
    [JSON](/data/bob-1.json)), or [live example](/bob).

## Completion-2 user

With addition information, the user can buy social shares, sign the CURU, and
register a nested organization (an "Activité").

The additional information are:

- EId

Bob

:   See [static example](/views/profile/bob-2.json) (—
    [JSON](/data/bob-2.json)), or [live example](/bob).

## Authorizations

See the [library documentation](/haddock/Curiosity-Data-User.html#t:Authorization).

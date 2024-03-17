---
title: Curiosity
---

# Test scenarios

One of the main goal of Curiosity is to precisely describe its behavior in
terms of test scenarios. To do so, Curiosity supports running scripts: they are
textual sequences of commands that mimic closely what can be done through the
web interface.

As part of Curiosity's [test suite](/documentation/tests), a [collection of
scenarios](#scenarios) are run and their results are compared to known sources
of truth, called "golden files". This makes sure that, as scenarios are
created, and as Curiosity evolves, the system continues to work as expected,
and few bugs can be introduced inadvertantly.

To manually run scenarios, see how to use the `cty run` [command-line
tool](/documentation/clis). If you're a developer, the script to ensure
scenarios match their corresponding golden files is visible [on
GitHub](https://github.com/hypered/curiosity/blob/main/tests/run-scenarios.hs).

# Example data

Entering enough data into the system to allow to execute (possibly manually,
through the web interface) a specific test scenario can be tedious if the
system is initially empty. Fortunately, scripts can also be used to generate
the necessary data and set the system state as desired.

A specific set of example data is available with Curiosity, called
[`state-0`](/documentation/state-0). (We envision that additional set of data
could be created in the future.)

# Validation data

The behavior of the system (in particular regarding its validation rules) is
often dependent on static data: data that are considered part of the system
definition. Those data can be for instance hard-coded in the source code. The
data driving the system are visible and explained in the documentation in the
[validation data page](/documentation/validation-data).

# Live data

In addition to the above static validation data, parts of the system depend on
live data. For instance, the rights a user has can evolve over time. The static
validation data are presented in the documentation, and usually the live data
are visible in the application itself.

Still, some live data can be considered as some kind of system configuration
and change slowly enough they deserve to be documented too. This is the case
for instance of the legal entities: in a production system, they would stay
almost always the same, but in a prototype, it is useful to be able to
dynamically add, edit, and remove them in specific test scenarios.

This kind of live data have their own [documentation
page](/documentation/live-data).

# Scenarios

The test suite of Curiosity contains the following scenarios (those are
clickable links). For each scenario, it is possible to view all the states
resulting of each of the commands the scenario executes.

<!--# include virtual="/partials/scenarios" -->

The list [as JSON](/partials/scenarios.json).

Some scenarios have additional documentation:

- [Quotation flow](/documentation/scenarios/quotation-flow)

# See also

- [Tests](/documentation/tests)

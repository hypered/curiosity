---
title: Curiosity
---

# `state-0`

We intend `state-0` to be a set of example data that can serve as common
knowledge among the team working with Curiosity. Here, we detail what it is
comprised of.

Note: sometimes, we provide links to specific pages of the web application.
When those pages are displaying the live application state, it is possible that
the underlying data have changed and are no longer how they were when the
system was initialized.

Note: if you are operating Curiosity yourself, you can initialize Curiosity's
state by running `cty run scenarios/state-0.txt`. Here is [an example
scenario](https://github.com/hypered/curiosity/blob/main/scenarios/user-signup.txt),
and its [corresponding golden
file](https://github.com/hypered/curiosity/blob/main/scenarios/user-signup.golden).

## Users

- [Alice](/alice)
- [Charlie](/charlie)
- [Chloe](/chloe)
- [Mila](/mila)

## Legal entities

- [One S.A.](/entity/one)
- [Two S.A.](/entity/two)
- [Three S.R.L.](/entity/three)
- [Four S.R.L.](/entity/four)

## Business units

- [Alpha](/alpha)

## Forms

Forms data are held separately from the rest of the regular objects in what we
call the _staging area_. This is a place in memory where form data can be
edited and eventually submitted. When they are submitted, they are validated
against various rules. Only if they pass such validation, data are recorded in
the regular state.

In the staging area, invalid data can reside. The user can amend the data as
much as they desire. Interestingly, this allows us to have example data in the
staging area that will fail validation, and this can be used to exemplify
validation rules.

### Simple contract

- [Amount `-100`](/forms/edit/simple-contract/confirm-simple-contract/TBPJLIUG)
- [Amount `0`](/forms/edit/simple-contract/confirm-simple-contract/HNONWPTG)
- [Amount `100`](/forms/edit/simple-contract/confirm-simple-contract/RZEMQMNF)

Note: the validation rules are visible in the [source
code](/haddock/src/Curiosity.Types.SimpleContract.html#validateCreateSimpleContract).
We should try to expose them more clearly, both in the documentation, and in
the code.

# See also

- [Validation rules](/documentation/validation)
- [Validation data](/documentation/validation-data)
- [Live data](/documentation/live-data)

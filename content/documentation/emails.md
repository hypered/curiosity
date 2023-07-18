---
title: Curiosity
---

# Email templates

All the emails that can be sent by the system are given by variants of the
`EmailTemplate` data type. See [their
documentation](/haddock/Curiosity-Types-Email.html#t:EmailTemplate).

# Sent emails

All the emails that are sent by the system go through an internal queue.
Sending an email, from the point of view of the system, can thus be done
atomically within a transaction: either the action that sends an email is done
completely, or not at all, including sending the email.

Another process should actually send the emails outside the system and drain
the queue.

The complete queue can be seen at [`/emails`](/emails) and
[`/emails.json`](/emails.json).

# See also

- [Application state](/documentation/state)
- [Quotations](/documentation/quotations)
- [Orders](/documentation/orders)

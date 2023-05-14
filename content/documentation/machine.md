---
title: Curiosity
---


# Virtual machine image

Curiosity is packaged as a virtual machine image. The image contains everything
(e.g. the web application, a reverse proxy, helper programs and documentation,
including this page).

The same image can be built for different environments. For instance we use KVM
for local development and the virtual machine available at
[`cty.hypered.systems`](//cty.hypered.systems) for demonstration purpose is
[deployed](/documentation/deployment) at DigitalOcean.

## Built with Nix

The build system used for Curiosity is [Nix](/documentation/nix). It ensures
that the complete system, or its different parts, can be built reliably from
source.

When the image is built for a local KVM, its ports 80 and 22 are forwarded to
the host ports 8180 and 8122.

When the machine is built for DigitalOcean, it uses Let's encrypt to get a
HTTPS certificate.

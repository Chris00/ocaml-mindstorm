![Continuous Integration](https://github.com/Chris00/ocaml-mindstorm/actions/workflows/main.yml/badge.svg)

Mindstorm
=========

Installation
------------

The easier way to install this library is by using [OPAM][]:

    opam install mindstorm

for the standard version and

    opam install mindstorm-lwt

for the LWT one.  If you prefer to compile and install by hand, make
sure you have [dune][] and the dependencies listed in
[src/dune](src/dune) and [lwt/dune](lwt/dune), then run

    dune build @install
    dune install mindstorm
    dune install mindstorm-lwt

[OPAM]: https://opam.ocaml.org/
[dune]: https://github.com/ocaml/dune


Usage
-----

Two modules are available: `Mindstorm` and `Mindstorm_lwt` with
essentially the same signatures, except that the second one—as its
name indicates—is to be used with `Lwt`.


Prerequisite under Unix
-----------------------

You need the package `libbluetooth-dev` (under Debian) or equivalent.
For the USB connection, you must install the package `libusb-1.0-0-dev`
(its presence should be automatically detected).

Prerequisite under Windows
--------------------------

Do not install the LEGO® fantom drivers.  (If you know how to make
this library work with the LEGO® drivers installed, submit a patch!)

Prerequisite under Mac OS X
---------------------------

At the moment, the package is not compatible with OSX.

Documentation
-------------

You can compile the HTML doc with

    make doc

and then point your browser to `_build/default/_doc/index.html`.
Alternatively, you can
[read it online](https://Chris00.github.io/ocaml-mindstorm/doc/).

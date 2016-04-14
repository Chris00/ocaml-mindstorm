[![Build Status](https://travis-ci.org/Chris00/ocaml-mindstorm.svg?branch=master)](https://travis-ci.org/Chris00/ocaml-mindstorm)

Mindstorm
=========

Installation
------------

The easier way to install this library is by using
[opam][http://opam.ocaml.org/]:

    opam install mindstorm

If you clone this repository, you must install `oasis` and then you can
compile the code with

	ocaml setup.ml -configure
	ocaml setup.ml -build

or with `make` (this latter possibility requires the library `Lwt` to
be present as it is meant for developers) and install with

	ocaml setup.ml -install

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

Xcode.

Compilation & Installation
--------------------------

The easier way to install this package it to use
[opam](http://opam.ocaml.org/):

    opam install mindstorm

If you downloaded the tarball, type:

    make
    make install

The installation requires `ocamlfind` and
[`cppo`](http://mjambon.com/cppo.html) (both installable via opam).

If you cloned the repository, you will additionally need
[oasis](http://oasis.forge.ocamlcore.org/) (also installable via opam).


Documentation
-------------

You can compile the HTML doc with

    make doc

and then point your browser to `API.docdir/index.html`.
Alternatively, you can
[read it online](http://ocaml-mindstorm.forge.ocamlcore.org/).

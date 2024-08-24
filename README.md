AIO -- ANSI Support
===================

This is a very simple module that provides terminal applications with a simple
means of controlling colour and cursor position. I wrote it for my needs and
so it is not exhaustive.

It is based around the contents of this page:
  https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797

Building
--------

    $ mmc --make libaio

Installation
------------

    $ sudo mmc --make libaio.install

There is a simple `main/2` predicate defined but it is commented out, so if
you wish to extend this module it can be useful to test locally, in which
case the build command is then:

    $ mmc --make aio

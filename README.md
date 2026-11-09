## INSTALLATION
    To install this module type the following:

```
      perl Makefile.PL
      make
      make test
      make install
```

Perl 5.18 or newer and a functional C compiler are required.
Graph analysis and Perl source emission use a mandatory XS engine.
Construction, options, entry points, and callback dispatch also use XS.
Perl supplies loading/export glue and integration with B::Deparse,
PadWalker, and the optional compression modules.

On XPerl with class snapshot helpers, native `class` objects preserve their
fields and ordinary blessed payloads. Reconstruction requires the class
definitions and XPerl helpers to be available and bypasses constructors.

## DEPENDENCIES

ExtUtils::MakeMaker configures and builds the distribution. Runtime
and test dependencies are listed in Makefile.PL.

Optional dependencies:

- PadWalker enables automatic variable names in DumpLex.
- Algorithm::Diff and Cpanel::JSON::XS enable additional test coverage.
- Compress::Zlib and MIME::Base64 enable compressed scalar output.

CI tests every stable Perl series from 5.18 through 5.44, plus a development
Perl built with the refcounted stack and a separate threaded build.

## AUTHOR AND COPYRIGHT
    Yves Orton, (demerphq)

    Copyright (C) 2003 Yves Orton  2003-2005

    This library is free software; you can redistribute it and/or
    modify it under the same terms as Perl itself.

    Contains code derived from works by Gisle Aas, Graham Barr,
    Jeff Pinyan, Richard Clamp, and Gurusamy Sarathy as well as
    material taken from the core.

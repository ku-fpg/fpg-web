We use tools, typically written in Haskell, to explore and validate our
ideas. The main research challenge we are addressing is improving the
high assurance narrative for functional languages. We release our tools
open source, under a BSD license, use git for version control, and
github for external contributions.

This page is a tool-centric view of our research. Please email us if you
are interested in contributing, or want more details about a specific
tool.

Haskell Domain Specific Languages (DSLs)
========================================

[Kansas Lava](/Tools/KansasLava "Kansas Lava")
-------------------------------------------------------

Kansas Lava is a Domain Specific Language (DSL) for expressing
hardware-oriented descriptions of computations, and is hosted inside the
language Haskell. Kansas Lava programs are descriptions of specific
hardware entities, the connections between them, and other computational
abstractions that can compile down to these entities. Large circuits
have be successfully expressed using Kansas Lava, and Haskell's powerful
abstraction mechanisms, as well as generic generative techniques, can be
applied to good effect to provide descriptions of highly efficient
circuits.

-   [Read
    more](/Tools/KansasLava "Read the rest of Kansas Lava.")

[KURE](/Tools/KURE "KURE")
-----------------------------------

The **Kansas University Rewrite Engine** (KURE) is a Haskell-hosted
Domain Specific Language (DSL) for writing transformation systems based
on rewrite strategies. When writing transformation systems, a
significant amount of engineering effort goes into setting up plumbing
to make sure that specific rewrite rules can fire. Systems like Stratego
and Strafunski provide most of this plumbing as infrastructure, allowing
the DSL user to focus on the rewrites rules. KURE is a strongly typed
strategy control language in the tradition of Stratego and Strafunski.
It is intended for writing reasonably efficient rewrite systems, makes
use of type families to provide a delimited generic mechanism for tree
rewriting, and provides support for efficient identity rewrite
detection.

-   [Read more](/Tools/KURE "Read the rest of KURE.")

[ChalkBoard](/Tools/ChalkBoard "ChalkBoard")
-----------------------------------------------------

ChalkBoard is a Haskell-hosted Domain Specific Language (DSL) for image
generation and processing. The basic structure is a chalk `Board`, a
two-dimensional canvas of values, typically colors. ChalkBoard provides
the usual image processing functions (masking, overlaying, function
mapping, cropping, warping, rotating, etc.) as well as a few more
unusual ones. Images can be imported into ChalkBoard as first-class
color `Board`s. ChalkBoard also provides combinators for drawing shapes
directly on boards. The system is based loosely on Pan, but the
principal image type, a `Board`, is abstract.

-   [Read
    more](/Tools/ChalkBoard "Read the rest of ChalkBoard.")

Tools
=====

[Haskell Program Coverage](/Tools/Hpc "Haskell Program Coverage")
--------------------------------------------------------------------------

Haskell Program Coverage (Hpc) is a high-fidelity code coverage tool for
Haskell, now in widespread use throughout the Haskell community. Hpc
includes tools that instrument Haskell programs to record program
coverage, run instrumented programs, and display the coverage
information obtained. It is included with the standard GHC distribution.

-   [Read
    more](/Tools/Hpc "Read the rest of Haskell Program Coverage.")

Group Efforts
=============

[Lambda Bridge](/Tools/LambdaBridge "Lambda Bridge")
-------------------------------------------------------------

The Lambda Bridge effort provides enabling technology for using
functional programming on FPGA fabrics and board. The majority of the
artifacts are shared documentation of ways to use FPGA board, and
libraries (software and hardware) that facilitate the use of FPGAs.

-   [Read
    more](/Tools/LambdaBridge "Read the rest of Lambda Bridge.")

Libraries
=========

[Haskell Object Observation Debugger (Hood)](/Tools/Hood "Haskell Object Observation Debugger (Hood)")
---------------------------------------------------------------------------------------------------------------

HOOD is a small post-mortem debugger for the lazy functional language
[Haskell](http://www.haskell.org). It is based on the concept of
observation of intermediate data structures, rather than the more
traditional stepping and variable examination paradigm used by
imperative language debuggers.

-   [Read
    more](/Tools/Hood "Read the rest of Haskell Object Observation Debugger (Hood).")

[IO Reification](/Tools/IOReification "IO Reification")
----------------------------------------------------------------

[data-reify](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/data-reify)
provided the ability to turn recursive structures into explicit graphs.
Many (implicitly or explicitly) recursive data structure can be given
this ability, via a type class instance.

-   [Read
    more](/Tools/IOReification "Read the rest of IO Reification.")

[Scotty](/Tools/Scotty "Scotty")
-----------------------------------------

A Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp.
Sinatra + Warp = Scotty.

Scotty is the cheap and cheerful way to write RESTful, declarative web
applications.

-   A page is as simple as defining the verb, url pattern, and Text
    content.
-   It is template-language agnostic. Anything that returns a Text value
    will do.
-   Conforms to WAI Application interface.
-   Uses very fast Warp webserver by default.

Hackage:
[http://hackage.haskell.org/package/scotty](http://hackage.haskell.org/package/scotty "http://hackage.haskell.org/package/scotty")

Github:
[https://github.com/xich/scotty](https://github.com/xich/scotty "https://github.com/xich/scotty")

-   [Read more](/Tools/Scotty "Read the rest of Scotty.")

Other Libraries
---------------

-   [dotgen](http://hackage.haskell.org/package/dotgen) A simple
    interface for building .dot graph files, for input into the dot and
    graphviz tools. It includes a monadic interface for building graphs.
-   [httpd-shed](http://hackage.haskell.org/package/httpd-shed) A
    trivial web server that can be used to build Ajax and other
    interactive applications in Haskell.
-   [io-reactive](http://hackage.haskell.org/package/io-reactive) An API
    for generating reactive objects that mimic the reactive model used
    used in the TIMBER programming language.
-   [marked-pretty](http://hackage.haskell.org/package/marked-pretty) A
    compatible extension to pretty that allows marking and scoping of
    the output document. A typical application is outputting pretty HTML
    with style attributes like bold and color.
-   [sized-types](http://hackage.haskell.org/package/sized-types)
    Providing indices, matrixes, sparse matrixes, and signed and
    unsigned bit vectors.
-   tumblescope A library for observing and manipulating mainly
    isomorphic views of a model.


ChalkBoard is a Haskell-hosted Domain Specific Language (DSL) for image
generation and processing. The basic structure is a chalk `Board`, a
two-dimensional canvas of values, typically colors. ChalkBoard provides
the usual image processing functions (masking, overlaying, function
mapping, cropping, warping, rotating, etc.) as well as a few more
unusual ones. Images can be imported into ChalkBoard as first-class
color `Board`s. ChalkBoard also provides combinators for drawing shapes
directly on boards. The system is based loosely on Pan, but the
principal image type, a `Board`, is abstract.

### ChalkBoard Links

-   [Download ChalkBoard](http://hackage.haskell.org/package/chalkboard)
-   [ChalkBoard
    Tutorial](http://www.ittc.ku.eduTools/ChalkBoard/Tutorial)
-   [ChalkBoard
    Documentation](http://www.ittc.ku.edusites/default/files/chalkboardhaddock/index.html)

Download
--------

We believe that ChalkBoard is a viable and useful research platform for
experimenting with applied functional programming. It is experimental,
and everything might change! The primary concepts (functor-based
transformations of images, OpenGL acceleration) will remain, but we are
still trying to balance and tune our observable sub-language.
Applicative functors are also sure to follow, and many more experiments
are needed.

ChalkBoard is available from the Haskell package server
[hackage](http://hackage.haskell.org/). The latest version of the core
ChalkBoard package is available
[here](http://hackage.haskell.org/package/chalkboard).

### Install Issues

The use of the DevIL image library might cause some issues on MacOSX. We
use `port install` to install this, which seems to require Xcode 3.1.

\

ChalkBoard Paper
----------------

2009

[Matlage, K.](/biblio/author/8), and [A.
Gill](/biblio/author/42), "[ChalkBoard: Mapping Functions to
Polygons](/biblio/view/11)", *21st International Symposium on
Implementation and Application of Functional Languages*: LNCS 6041,
11/2009. [Abstract](/node/11)

-   [Tagged](/biblio/export/tagged/11 "Click to download the EndNote Tagged formatted file")
-   [XML](/biblio/export/xml/11 "Click to download the XML formatted file")
-   [BibTex](/biblio/export/bibtex/11 "Click to download the BibTEX formatted file")
-   [Google
    Scholar](http://scholar.google.com/scholar?btnG=Search%2BScholar&as_q=%22ChalkBoard%3A%2BMapping%2BFunctions%2Bto%2BPolygons%22&as_sauthors=Matlage&as_occt=any&as_epq=&as_oq=&as_eq=&as_publication=&as_ylo=&as_yhi=&as_sdtAAP=1&as_sdtp=1 "Click to search Google Scholar for this entry")


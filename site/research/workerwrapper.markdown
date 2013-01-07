The worker/wrapper transformation is a technique for transforming a
computation of one type into a *worker* of a different type, together
with a *wrapper* that acts as an impedance matcher between the original
and new computations. The technique can be used to improve the
performance of functional programs by improving the choice of data
structures used.

Transformation Overview
-----------------------

[![](http://www.ittc.ku.edu/sites/default/files/WW.png)](http://www.ittc.ku.edu/csdl/fpg/sites/default/files/WW.png)

(click to enlarge)

This image is an abstract diagram explaining the worker/wrapper
transformation, where arrows signify usage. The (smaller) boxes are
computations that have a type, given as an incoming arrow on the left
hand side, and require the use of a second computation, given as an
outgoing arrow on the right hand side. These computational boxes can be
considered representing functions with higher-order arguments. **A** and
**B** should be read as providing a typed service by using the box to
their right to provide this service.

The **original computation** is a recursive function, that is it uses
itself, notated with a right arrow that arcs back to the type of the
computation, **A**. The ultimate result is a wrapper and an optimized
worker, which operates using a new type, **B**.

The worker/wrapper transformation operates as follows:

1.  A target type for the recursion is selected, called **B**.
2.  A **coercion chain** is constructed from two higher-order functions
    that coerce from **A** to **B**, and back to **A**.
3.  The worker/wrapper local preconditions are verified; one example is
    when **abs** and **rep** form an identity.
4.  The worker/wrapper transformation is applied, replacing then the
    original computation with the **post worker/wrapper chain**.
5.  This new formulation can be transformed and optimized using well
    understood local refinements into a *worker* that operates
    efficiently over **B**, and a *wrapper* **abs** that allows original
    users of **A** to call this new function.

This general pattern captures many possible transformations and
refinements.

Worker/Wrapper Theorem
----------------------

The transformation can be expressed formally, using the theory of
**least fixed points** over pointed ω-complete partial orders (the
semantic domain of Haskell).

### Worker/Wrapper Factorization Rule

If **comp :: A** is defined by **comp = fix body** for some **body ::
A→A**, and **rep :: A→B** and **abs :: B→A** satisfy any of the
worker/wrapper assumptions, then

**comp = abs work**

where **work :: B** is defined by

**work = fix (rep ο body ο abs)**

Alternatively, **work** can be defined as

**work = rep comp**

which avoids the need to define **comp** using an explicit fixed point.

### Worker/Wrapper Assumptions

There are several worker/wrapper assumptions that are sufficient to
justify worker/wrapper factorization. The three given in the
worker/wrapper paper are:

  ---------------------------- ---------------- ----------------------------------
  **abs ο rep**                **= id**         (**(A)** basic assumption)
  **abs ο rep ο body**         **= body**       (**(B)** body assumption)
  **fix (abs ο rep ο body)**   **= fix body**   (**(C)** fixed-point assumption)
  ---------------------------- ---------------- ----------------------------------

There is also another possible assumption, which uses the **fixed point
fusion** rule (hence the name fpf assumption) and only holds for a
strict **abs**:

  ----------------------------- ------------------ --------------------------
  **wrap ο rep ο body ο abs**   **= body ο abs**   (**(D)** fpf assumption)
  ----------------------------- ------------------ --------------------------

### Worker/Wrapper Fusion Rule

If any of the worker/wrapper assumptions **(A)**, **(B)** or **(C)**
hold, then:

**rep (abs work) = work**

Papers
------

2009

[Gill, A.](/biblio/author/42), and [G.
Hutton](/biblio/author/2), "[The worker/wrapper
transformation](/biblio/view/19)", *Journal of Functional
Programming*, vol. 19, no. 2: Cambridge University Press, pp. 227–251,
03/2009. [Abstract](/node/19)

-   [Tagged](/biblio/export/tagged/19 "Click to download the EndNote Tagged formatted file")
-   [XML](/biblio/export/xml/19 "Click to download the XML formatted file")
-   [BibTex](/biblio/export/bibtex/19 "Click to download the BibTEX formatted file")
-   [Google
    Scholar](http://scholar.google.com/scholar?btnG=Search%2BScholar&as_q=%22The%2Bworker%2Fwrapper%2Btransformation%22&as_sauthors=Gill&as_occt=any&as_epq=&as_oq=&as_eq=&as_publication=&as_ylo=&as_yhi=&as_sdtAAP=1&as_sdtp=1 "Click to search Google Scholar for this entry")

2010

[Hutton, G.](/biblio/author/2), [M.
Jaskelioff](/biblio/author/6), and [A.
Gill](/biblio/author/42), "[Factorising Folds for Faster
Functions](/biblio/view/10)", *Journal of Functional
Programming*, vol. 20, issue 3-4, 2010. [Abstract](/node/10)

-   [Tagged](/biblio/export/tagged/10 "Click to download the EndNote Tagged formatted file")
-   [XML](/biblio/export/xml/10 "Click to download the XML formatted file")
-   [BibTex](/biblio/export/bibtex/10 "Click to download the BibTEX formatted file")
-   [Google
    Scholar](http://scholar.google.com/scholar?btnG=Search%2BScholar&as_q=%22Factorising%2BFolds%2Bfor%2BFaster%2BFunctions%22&as_sauthors=Hutton&as_occt=any&as_epq=&as_oq=&as_eq=&as_publication=&as_ylo=&as_yhi=&as_sdtAAP=1&as_sdtp=1 "Click to search Google Scholar for this entry")

-   [Unboxed values as first class citizens in a non-strict functional
    language](http://citeseer.ist.psu.edu/jones91unboxed.html). Simon L
    Peyton Jones and John Launchbury. *Functional Programming Languages
    and Computer Architecture (FPCA '91)*, pp. 636–666, 1991.
-   [Work It, Wrap It, Fix It, Fold
    It.](http://www.ittc.ku.edu/~neil/publications.html) Neil Sculthorpe
    and Graham Hutton. Submitted to *Transactions on Programming
    Languages and Systems (TOPLAS)*.

Use Cases
---------

The Worker/Wrapper Transformation is surprisingly general! It has been
applied to the following application areas:

-   **Using strictness information**—was the original motivating example
    for this transformation.
-   **CPS translation**—is just using an alternative representation for
    computation.
-   **Memoization**—is using a data-structure to represent a function.
-   **Accumulation**—is often possible when the result is a monoid;
    worker/wrapper enables this change of type.
-   **Constructor specialization**—creates specialized workers and
    wrappers.
-   **Cross-function short-cut fusion**—uses worker/wrapper to transmit
    fusion opportunities over function boundaries.

We expect there are many others.

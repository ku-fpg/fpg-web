<div class="teaser">
        
We use functional programming to solve problems in ways that are
amicable to acceleration (GPUs, Multi-cores, FPGAs), and supports
assurance arguments (using semi-formal methods like equational
reasoning). As a group we make aggressive use of functional languages,
extending the technology where needed, and ultimately strive to close
gaps between high level specifications and highly efficient
implementations. We then deploy our new technologies into diverse
application areas, including telemetry, high performance computing and
real-time systems.

</div>


Our operating assumption is that a well written functional program can
perform effectively as a concise executable specification of a solution
to a problem. However, the scope and influence of our ideas is intended
to be broader than simply their use inside our functional language
community.


Overview of Research
====================

It is **possible** to write specification style, high-level
implementation (or model) in our language of choice, Haskell, as well as
in other languages. This model may be relatively efficient as a model,
but be completely unsuitable for deployment. The model may lack specific
fidelity details, or be intended for a platform with a small footprint,
or deployment may have unrealizable throughput requirements. All of
these concerns can be addressed, and addressed from within our language
of choice, **but at the cost of compromising the clarity of the original
specification.**

It is also **possible** to write very fast programming in Haskell, or
programs that use finite heap, so could be run on an embedded platform,
or can be captured using a technique called deep embedding, so they
could be cross compiled onto alternative platforms like FPGAs or
graphics cards. However, **each of these techniques fundamentally
compromised some level of clarity**, and therefore negating one of the
reasons to use a functional language in the first place.

As a group, we want to be able to use functional programming language
technology as a bridge between both these programming styles, or more
generally between specification and implementation. At the University of
Kansas, we are addressing this gap using two separate techniques.

-   First, we are looking at deep compiler optimizations which
    fundamentally revisit the optimizing possibilities of a Haskell
    program. At the heart of these principled optimizations is the
    [Worker Wrapper
    transformation](/Research/WorkerWrapper).
-   Second, we are looking at ways of expressing and implementing
    [Domain Specific
    Languages](/Research/DSLs) (DSLs)
    which are stylized libraries that give an engineered compromise
    between clarity, elegance and efficiency.

Separately, both these technologies address outstanding problems, and
ofter viable solutions. Ultimately we want to join both research
threads, and have the DSL technologies become the target of our
aggressive optimizations, closing the loop and bridging the gap between
specification and implementation.

Funding
=======

### Improving the Applicability of Haskell-Hosted Semi-Formal Models to High Assurance Development (2011-2013)

In engineering practice, models are an essential part of understanding
how to build complex systems. In this project, high-level models and
efficient implementations of computer systems will be developed
side-by-side under a single framework that bridges the gap between them
using a high degree of automation. This is possible due to the use of a
modern functional language for both the model and implementation, and
the deployment of a new and powerful general-purpose and semi-automatic
refinement technology.

The functional language Haskell has already enjoyed considerable success
as a platform for high-level modeling of complex systems with its
mathematical-style syntax, state-of-the-art type system, and powerful
abstraction mechanisms. In this project, Haskell will be used to express
a semi-formal model and an efficient implementation, taking the form of
two distinct expressions of computation with the same mathematical
foundation. The project develops tools and methodologies that use
transformations like the worker/wrapper transformation to construct
links between these models and implementations, lowering the cost of the
development of high-assurance software and hardware components in
application areas like security kernels and critical control systems.
Lowering the cost of linking semi-formal specifications and models to
real implementations will have considerable impact. For example,
Evaluation Assurance Level (EAL) 5 and 6 of the Common Criteria call for
semi-formal methods to construct such link, and this project addresses
keys part of this requirement.

### Efficient Hardware Implementation of Iterative FEC Decoders (2009-2011)

On this project, we are using Kansas Lava to generate efficient forward
error correcting codes. From a research point of view, we are trying to
answer the following questions:

-   Can we use use functional programming to complement and support the
    current development module of using MATLAB for a reference, and VHDL
    for an implementation.
-   Can we build a functional program that mitigates against the need to
    perform frequent refactorings when working in VHDL, as a suitable
    architecture is discovered.
-   Can we gain a stronger assurance of the relationship between the
    specification and implementation?
-   What are the weaknesses of using a system like Lava to implement a
    FEC, and what are the remaining research problems with using EDSLs
    as an architecture bridge.

Other Sponsors
--------------

#### Scottish Informatics and Computer Science Alliance (SICSA)

#### 

[![](http://www.ittc.ku.edu/csdl/fpg/sites/default/files/sicsa_logo.gif)](http://www.sicsa.ac.uk/)

#### International Foundation for Telemetering (IFT)

[![](http://www.ittc.ku.edu/csdl/fpg/sites/default/files/IFT_logo.gif)](http://www.telemetry.org/pages/ift/whoisift.php)

#### Xilinx

#### 

[![](http://www.ittc.ku.edu/csdl/fpg/sites/default/files/xilinx_logo.gif)](http://www.xilinx.com/)

Research Before KU
==================

[History](/Users/AndyGill/History "History")
-----------------------------------------------------

Andy Gill has been an active researcher in functional programming for 20
years. Before joining KU and forming the FPG, Andy worked on optimizing
functional languages at Edinburgh then Glasgow, running Haskell on
virtual machines at OGI, and high assurance software at Galois.

-   [Read
    more](/Users/AndyGill/History "Read the rest of History.")


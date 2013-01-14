
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
    [Worker Wrapper transformation](/Research/WorkerWrapper).
-   Second, we are looking at ways of expressing and implementing
    *Domain Specific Languages* (DSLs)
    which are stylized libraries that give an engineered compromise
    between clarity, elegance and efficiency.

Separately, both these technologies address outstanding problems, and
ofter viable solutions. Ultimately we want to join both research
threads, and have the DSL technologies become the target of our
aggressive optimizations, closing the loop and bridging the gap between
specification and implementation.

Summary of Active Projects
==========================

We have three projects that attempt to address our research objectives.

 * The [HERMIT Project](/projects/hermit) is building infrastructure for
   performing deep for 


 * The [Kansas Lava Project](/projects/kansas-lava) is ...
   It is intended as a target for refinements in the HERMIT project.

 * [The Sunroof Project](/projects/sunroof) is looking DSLs for web technologies,
   specifically deep embeddings for offshoring computation into
   the browser.

Funding
=======
### Improving the Applicability of Haskell-Hosted Semi-Formal Models to High Assurance Development

##### NSF (2011-2014)

<a href="/projects/hermit" class="teaser">More about the HERMIT project</a>

### Efficient Hardware Implementation of Iterative FEC Decoders

##### CTEIP (2009-2011)

<a href="/projects/hfec" class="teaser">More about the HFEC project</a>

### Other Sponsors

We would like to thank our other sponsors for supporting our research.

 * International Foundation for Telemetering (IFT) for 
   funding for hardware, travel and student scholarships.
 * Xilinx for software and hardware through the Xilinx University Program.
 * Altera for software and hardware through the Altera University Program.


KURE
----

<div class="teaser">
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
</div>

KURE was used (along with Template Haskell) to provide the basic rewrite
abilities inside HERA. It was rewritten once in late 2008, and again in
2012. The 2012 version is being used as the underlying rewrite engine by
HERMIT. Both the 2008 and 2012 versions are [available on Hackage](http://hackage.haskell.org/package/kure).

### Key Links

### Publications

* <div class="cite Farmer:12:HERMITinMachine"/>
* <div class="cite Gill:2009:KUREDSL"/>

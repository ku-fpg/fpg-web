KURE
----

<div class="teaser">
The **Kansas University Rewrite Engine** (KURE) is a Haskell-hosted Domain-Specific Language (DSL) for writing transformation systems based on rewrite strategies.
When writing transformation systems, a significant amount of engineering effort goes into setting up the infrastructure to direct queries and rewrites to the desired points in the structure.
Strategic programming languages provide most of this infrastructure, allowing the user to focus on the rewrites rules.
KURE is a strongly typed strategy control language in the tradition of Stratego and StrategyLib.
Its aim is to provide a simple interface for writing reasonably efficient rewrite systems while supporting complex traversal strategies.
</div>

KURE was used (along with Template Haskell) to provide the basic rewrite abilities inside HERA (the precursor to [HERMIT](software/hermit.html)).
It was rewritten once in late 2008, again in 2012, and the interface was revised in 2013.
The 2013 version is being used as the underlying rewrite engine by HERMIT.
The 2008, 2012 and 2013 versions are all [available on Hackage](http://hackage.haskell.org/package/kure).

### Key Links

* <http://hackage.haskell.org/package/kure>

### Publications

* <div class="cite Farmer:12:HERMITinMachine"/>
* <div class="cite Gill:2009:KUREDSL"/>

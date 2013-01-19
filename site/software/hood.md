<div class="teaser">

HOOD is a small post-mortem debugger for the lazy functional language
[Haskell](http://www.haskell.org). It is based on the concept of
observation of intermediate data structures, rather than the more
traditional stepping and variable examination paradigm used by
imperative language debuggers.

</div>

Features
--------

-   Observation of base types (Int, Bool, Float, etc)
-   Observation of both finite and infinite structures (Lists, trees,
    arrays, etc).
-   Observation of usage patterns for functions.
-   Observation of monadic actions, including IO actions.
-   Hooks to add observational capabilities for new base type and used
    defined types.
-   Programmable browsing capabilities - structure browsers can be coded
    and plugged in.
-   Includes a basic structure rendering package that uses a
    Haskell-like syntax.
-   Thread-safe observations are are supported.
-   Supports observations on exceptions (on certain compilers).

Examples
--------

Hood can observe data structures:

~~~~ {style="border: 1px solid black;"}
main = runO ex2

ex2 = print
      . reverse
      . (observe "intermediate")
      . reverse
      $ [0..9]
~~~~

Running this program gives this output:

~~~~ {style="border: 1px solid black;"}
[0,1,2,3,4,5,6,7,8,9]

-- intermediate
  9 : 8 : 7 : 6 : 5 : 4 : 3 : 2 : 1 : 0 : []
~~~~

Hood can also observe functions, showing both the arguments and result
of each call:

~~~~ {style="border: 1px solid black;"}
main = runO ex9

ex9 = print $ observe "foldl (+) 0 [1..4]" foldl (+) 0 [1..4]
~~~~

Running this program gives this output:

~~~~ {style="border: 1px solid black;"}
10

-- foldl (+) 0 [1..4]
  { \ { \ 0 1  -> 1
      , \ 1 2  -> 3
      , \ 3 3  -> 6
      , \ 6 4  -> 10
      } 0 (1 : 2 : 3 : 4 : []) 
       -> 10
  }
~~~~

Note that Hood preserves the type and strictness properties of the
function under observation. If an argument is not examined in the
function, it remains unevaluated. As an example:

~~~~ {style="border: 1px solid black;"}
ghci> runO $ print $ observe "sum xs" (\ xs ys -> sum xs) [0..2] [0..]
~~~~

Notice that ys is left unevaluated (denoted by the underscore):

~~~~ {style="border: 1px solid black;"}
3

-- sum xs
  { \ (0 : 1 : 2 : []) _  -> 3
  }
~~~~

History
-------

Hood was developed at OGI, in 1999, for GHC 4.X. We are looking into a
debugging toolkit for Haskell and Lava, so we ported Hood to GHC 6.X,
and re-released it on hackage. We hope you find it useful.

### Key Links

 *  <http://hackage.haskell.org/package/hood>
 
### HOOD Papers

 * <div class="cite Gill:00:HOOD"/>


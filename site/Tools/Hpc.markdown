Haskell Program Coverage (Hpc) is a high-fidelity code coverage tool for
Haskell, now in widespread use throughout the Haskell community. Hpc
includes tools that instrument Haskell programs to record program
coverage, run instrumented programs, and display the coverage
information obtained. It is included with the standard GHC distribution.

What is Haskell Program Coverage?
---------------------------------

Hpc is a tool-kit to record and display the code coverage of a Haskell
Program. Typically coverage tools are used to determine if test suites
is actually innovating parts of a program. Hpc is included with all
versions of GHC since 6.8. so no extra downloading or installing is
needed!

Quick Start and FAQ
-------------------

### I just want to start using Hpc. How do I do this?

You need a Haskell program, for example:

    % cat Main.hs
    main = print ("Hello, World " ++ show (const 1 2))

To compile use the ghc-6.8.1 (or later) command line option -fhpc:

    % ghc -fhpc Main.hs 
    [1 of 1] Compiling Main             ( Main.hs, Main.o )
    Linking Main ...
    % ./a.out
    "Hello, World 1"
    % hpc report a.out
     85% expressions used (6/7)
    100% boolean coverage (0/0)
         100% guards (0/0)
         100% 'if' conditions (0/0)
         100% qualifiers (0/0)
    100% alternatives used (0/0)
    100% local declarations used (0/0)

### How do I see my program marked up?

You run an instrumented binary, and then run hpc markup after execution.
(If you have have just run the example about, there is no need to
re-compile and re-run the binary; the coverage information is preserved
in the file Main.tix)

    % ./a.out
    Writing: Main.hs.html
    Writing: hpc_index.html
    Writing: hpc_index_fun.html
    Writing: hpc_index_alt.html
    Writing: hpc_index_exp.html

Now we have a file, Main.hs.html, which looks like this

~~~~ {.orig style="background: white; padding: 0px; margin: 0px"}
    1 main = print ("Hello, World " ++ show (const 1 2))
~~~~

The number 1 at the start of the line is the line number, and we can see
that '2' is highlighted; it was never evaluated during the execution of
this program. We have also generated some index files, that list all the
modules in our executable. `hpc_index.html`{.haskell
.geshifilter-haskell} looks like this.

![](http://www.ittc.ku.edusites/default/files/HpcMarkup.png)

### What files are you using to collect coverage?

  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  File   Summary                                                                               Location\         Contents\
                                                                                               Example           Example
  ------ ------------------------------------------------------------------------------------- ----------------- ----------------------------------------------------------------------------------------------------
  .tix   array of ticks, on for each location in the program, one per program                  Main.tix,\        `Tix [TixModule "Main" 123 9 [1,2,3,2,2,1,2,4], ...]`{.haskell .geshifilter-haskell}
                                                                                               a.out.tix         

  .mix   module specific information, mapping tick number to source location, one per module   .hpc/Main.mix,\   `Mix Main.hs" 1182484948 51696526 8 [(1:15-1:29,ExpBox False),...]`{.haskell .geshifilter-haskell}
                                                                                               .hpc/Foo.mix      
  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



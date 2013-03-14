Sunroof
=======

<div class="teaser">

Sunroof is a Domain Specific Language (DSL) for dynamically generating JavaScript.
Sunroof is build on top of the JS-monad, which, like the IO-monad, allows 
read and write access to external resources, but specifically JavaScript
resources. As such, Sunroof is primarily a feature-rich foreign
function API to the browser's JavaScript engine, and all the browser-specific
functionality, like HTML-based rendering, event handling, and even
drawing to the HTML5 canvas. 

</div>

Sunroof is a Haskell-hosted DSL. This
makes it easy to use Haskell abstractions for larger Javascript
applications without obscuring the produced Javascript on the Haskell
level. 
Furthermore, Sunroof offers two threading models for 
building on top Javascript, atomic and blocking threads.
This allows full access to Javascript APIs, but
using Haskell concurrency patterns, like MVars and Channels.
In combination with a small web services package, like Scotty,
Sunroof offers a great platform to build interactive web applications,
giving the ability to interleave Haskell and Javascript computations
with each other as needed.

### Key Links

 *  <http://hackage.haskell.org/package/sunroof>
 
### Sunroof Papers

 * <div class="cite Farmer:12:WebDSLs"/>


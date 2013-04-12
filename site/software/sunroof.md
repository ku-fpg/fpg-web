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

### Example

The sunroof compiler compiles a Sunroof effect that returns a
value into a JavaScript program. An example invocation is

~~~
GHCi> import Language.Sunroof
GHCi> import Language.Sunroof.JS.Browser
GHCi> import Data.Default
GHCi> txt <- sunroofCompileJS def "main" $ do 
                alert (js "Hello");
GHCi> putStrLn txt
var main = (function() {
  alert("Hello");
})();
~~~

The extra function and application are intentional and are a common JavaScript
trick to circumvent scoping issues.

To generate a function, not just an effect, you can use the `function` combinator.

~~~
GHCi> txt <- sunroofCompileJS def "square" $ do
               function $ \ (n :: JSNumber) -> do
                   return (n * n)
GHCi> putStrLn txt
var square = (function() {
  var v1 = function(v0) {
    return v0 * v0;
  };
  return v1;
})();
~~~

Now `square` in JavaScript is bound to the square function. Note that for 
having type annotations on function parameters you need to activate the 
language extension `ScopedTypeVariables`. [Look here for further examples][examples].

### Key Links

 *  [Tutorial](https://github.com/ku-fpg/sunroof-compiler/wiki/Tutorial)
 *  [Examples][examples]
 *  [Project Wiki & Development Resources](https://github.com/ku-fpg/sunroof-compiler/wiki)
 *  Hackage
     + [sunroof-compiler](http://hackage.haskell.org/package/sunroof-compiler)
     + [sunroof-server](http://hackage.haskell.org/package/sunroof-server)
     + [sunroof-examples](http://hackage.haskell.org/package/sunroof-examples)
 *  Github
     + [sunroof-compiler](https://github.com/ku-fpg/sunroof-compiler)
     + [sunroof-server](https://github.com/ku-fpg/sunroof-server)
     + [sunroof-examples](https://github.com/ku-fpg/sunroof-examples)
 
### Publications

 * <div class="cite Bracker:13:Draft-Sunroof"/>
 * <div class="cite Farmer:12:WebDSLs"/>
 * <div class="cite Sculthorpe:13:ConstrainedMonad"/>



[examples]: https://github.com/ku-fpg/sunroof-compiler/wiki/Examples

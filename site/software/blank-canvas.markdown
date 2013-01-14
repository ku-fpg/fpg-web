<div class="teaser">
blank-canvas is a Haskell port of the HTML5 Canvas API. Tutorials and
examples for the HTML5 Canvas should be trivial to port to this library.
blank-canvas works by providing a web service that displays the users'
Haskell commands inside a browser.
</div>

Example
-------

Compile the program on the left, access `http://localhost:3000/`, see
the picture on the right.

~~~~ {width="50%"}
import Graphics.Blank

main = blankCanvas 3000 $ \ context -> do
        send context $ do
                moveTo(50,50)
                lineTo(200,100)
                lineWidth 10
                strokeStyle "red"
                stroke()
~~~~

![](http://ittc.ku.edu/csdl/fpg/sites/default/files/red-line.png)

Downloading
-----------

    cabal install blank-canvas

Documentation
-------------

Haddock:
[http://hackage.haskell.org/package/blank-canvas](http://hackage.haskell.org/package/blank-canvas)
\
 There is also a [slide
set](http://ittc.ku.edu/csdl/fpg/sites/default/files/class17.pdf) about
blank-canvas.

Examples
--------

### Squares

![](http://ittc.ku.edu/csdl/fpg/sites/default/files/squares.png)

### Tic Tac Toe

![](http://ittc.ku.edu/csdl/fpg/sites/default/files/xox.png)

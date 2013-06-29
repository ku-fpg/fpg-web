This is our group website.

Directories in FPG
------------------

 * _make - all the generated content.
   At any point, _make can be removed, and everything in it rebuild.
    * _make/html - the root of the generated website
    * _make/autogen - pages that contain generated content, like the bib files
    * _make/contents - pages that are generated from markdown files
    * _make/bibtex - tmp dir for bibtex runs

 * data - data sources, like fpg.bib
 * css - the css files of the website
 * files - files to be copies raw (like pdf files, images on pages, pictures of us, etc)
 * img - images that are used by the webpages, including the twitter bootstrap ones
 * js - the javascript files

 * site - all content text, in markdown format.
    * site/projects - (typically-funded) initiatives and efforts that span many years. 
      Examples include hermit, hfec, kansas lava, and sunroof. These are written from
      the point of interested researchers.
    * site/software - short pages about our software, from the software users' point
      of view. Approx. one for each hackage packages, sometimes shared (example - kansas-lava
      and kansas-lava cores are on the kansas-lava page)
    * site/users - our "home" pages. Not required, but quite useful.
 * template - our templates for the page border, including nav bars.

Use of markdown
---------------

We use markdown inside the directory site for all our text.
 * Local links start with /, so my homepage is /users/andygill 
 * Local links are given without suffixes.

Testing
-------

Here, we [link](README).






--- sitegen
random: sample
category: hello
---
# A file with an almost empty header

It should work, the 'random' is just some extra which is ignored. And a bit
more.

<!--more-->

# Links

* A local link to [[test1]] should work
* A local .md link to [[test1.md]] should work
* Relative link to [[site1/home]] shouldn't work
* Relative .md link to [[site1/home.md]] shouldn't work
* Absolute link to [[/site1/home]] should work
* Absolute .md link to [[/site1/home.md]] should work
* Relative link to [[../site1/home]] should work
* Relative .md link to [[../site1/home.md]] should work.

And static files:

* a link to a [[/favicon.png|favicon]]
* a ling to a js file [[/js/highlight.pack.js|highlighting file]]
* this link won't work: [[js/favicon.png|favicon]]

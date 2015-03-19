Rstackdeque
========================================================

Persistent stacks, deques, and queues for R
---------------------------------------------

Have you ever wanted to use a stack or a queue for R, but just pulled your hair out
trying to use lists or vectors instead? Enter rstackdeque.

The most important feature of the stacks and queues (and double-ended-queues, or 
"deques") in this package are not only are they reasonably fast (amortized or 
worst-case O(1) depending on which you're using), inserting and removal are 
implemented as "side-effect-free" functions operating similar to other R 
structures. 

This is largely possible due to the fantastic work of
Chris Okasaki: see [Purely Functional Data Structures](http://www.amazon.com/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504) 
and [Simple and Efficient Purely Functional Queues and Deques](http://www.westpoint.edu/eecs/SiteAssets/SitePages/Faculty%20Publication%20Documents/Okasaki/jfp95queue.pdf).

For example:

```
> s <- rstack()
> s <- insert_top(s, "A")
> s <- insert_top(s, "B")
> s <- insert_top(s, "C")
> s2 <- without_top(s)
> print(s)
An rstack with  3  elements. 
 : chr "C"
 : chr "B"
 : chr "A"
> print(s2)
An rstack with  2  elements. 
 : chr "B"
 : chr "A"
```

Install
---------

You can install this package via CRAN:

```
> install.packages("rstackdeque") 
```

Then load it up and check the help and examples:

```
> require(rstackdeque)
> help(package = "rstackdeque")
> help(rstack)
> help(rdeque)
> help(rpqueue)
```

Vignette coming soon.

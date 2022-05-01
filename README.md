Rstackdeque
========================================================

Persistent stacks, deques, and queues for R
---------------------------------------------

A full description can be found in the R journal pub: [https://journal.r-project.org/archive/2015-1/oneil.pdf](https://journal.r-project.org/archive/2015-1/oneil.pdf)

...

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


Install
---------

You can install this package via CRAN:

```r
> install.packages("rstackdeque") 
```

Then load it up and check the help and examples:

```r
> require(rstackdeque)
> help(package = "rstackdeque")
> help(rstack)
> help(rdeque)
> help(rpqueue)
```

Quick Start
---------

An `rstack()` is a stack-like structure, that supports adding elements to the "top":

```r
library(rstackdeque)
library(magrittr)


suits <- rstack() %>%
         insert_top("Spades") %>%
         insert_top("Hearts") %>%
         insert_top("Clubs") %>%
         insert_top("Diamonds")

print(suits)
```

```
An rstack with  4  elements. 
 : chr "Diamonds"
 : chr "Clubs"
 : chr "Hearts"
 : chr "Spades"
```
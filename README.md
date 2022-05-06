---
title: "README"
author: "Shawn T O'Neil"
date: "5/1/2022"
output: 
  html_document:
    keep_md: true
---



## `rstackdeque` Persistent stacks, deques, and queues for R


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


## Install


You can install this package via CRAN:


```r
install.packages("rstackdeque") 
```

Then load it up and check the help and examples:


```r
require(rstackdeque)
help(package = "rstackdeque")
help(rstack)
help(rdeque)
help(rpqueue)
```

## Quick Start


### Stacks 

An `rstack()` is a stack-like structure, that supports adding elements to the "top":


```r
library(rstackdeque)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
stack <- rstack() %>%
           insert_top("A") %>%
           insert_top("B") %>%
           insert_top("C") %>%
           insert_top("D")

print(stack)
```

```
## An rstack with  4  elements. 
##  : chr "D"
##  : chr "C"
##  : chr "B"
##  : chr "A"
```



They also allow "peeking at" (returning a copy of) the top element. This does not change the contents of the stack.


```r
top_element <- peek_top(stack)
print(top_element)
```

```
## [1] "D"
```


Lastly, they support dropping the top element:


```r
stack <- without_top(stack)
print(stack)
```

```
## An rstack with  3  elements. 
##  : chr "C"
##  : chr "B"
##  : chr "A"
```


### Deques (double-ended queues)

Deques, or double-ended queues, support adding and removing elements to either the "front" or "back" of the queue. 



```r
deque <- rdeque() %>%
           insert_front("A") %>%
           insert_front("B") %>%
           insert_front("C") %>%
           insert_back("X") %>%
           insert_back("Y") %>%
           insert_back("Z")

print(deque)
```

```
## A deque with  6  elements.
## Front to back: 
##  $: chr "C"
##  $: chr "B"
##  $: chr "A"
##  $: chr "X"
##  $: chr "Y"
##  $: chr "Z"
```

```r
deque <- without_front(deque)
deque <- without_back(deque)
print(deque)
```

```
## A deque with  4  elements.
## Front to back: 
##  $: chr "B"
##  $: chr "A"
##  $: chr "X"
##  $: chr "Y"
```
The `peek_front()` and `peek_back()` functions return the first and last elements:


```r
first_el <- peek_front(deque)
last_el <- peek_back(deque)

print(first_el)
```

```
## [1] "B"
```

```r
print(last_el)
```

```
## [1] "Y"
```

While insertions and removals from `rstack`s are always fast, certain patterns of repeated insertions and removals from an `rdeque` will cause periodic 're-balancing' operations where a full copy of the deque is made with more efficient internal organization. Thus, although `rdeque`s provide more functionality than `rstack`s, `rstack`s are preferred unless deque operations are required. 

### Queues

Queues support a subset of functionality of dequeus - queues only allow inserting elements at the back of the queue, and removing elements from the front of the queue. Peeking is also only supported at the front of the queue.


```r
queue <- rpqueue() %>%
           insert_back("A") %>%
           insert_back("B") %>%
           insert_back("C")

print(queue)
```

```
## A queue with  3  elements.
## Front: 
##  $: chr "A"
##  $: chr "B"
##  $: chr "C"
```

```r
queue <- without_front(queue)
print(queue)
```

```
## A queue with  2  elements.
## Front: 
##  $: chr "B"
##  $: chr "C"
```

```r
first_el <- peek_front(queue)
print(first_el)
```

```
## [1] "B"
```
Queues provide a subset of functionality of deques, but this implementation comes with an efficiency improvement: while `rdeques` may experience
periodic re-balancing described above, `rpqueus` do not, such that insertions and removals always take approximately the same time. The complexity of supporting this for queues, however, makes the implementation slower in practice that `rstack`s. These queues are thus a good choice when queue functionality is needed in real-time applications (e.g. user interfaces). 


### Common Functionality

All three types support `empty()` (`TRUE` or `FALSE`), `length()`, and `head()` (where the head is the front of deques and queues, 
and the top of stacks), and `rstacks` also support `rev()` (but this operations makes a full copy).

We demonstrate features below for `rstack`s, but they also apply to `rdeque`s
and `rpqueue`s (discussed below).


#### Element types

Almost any datatype can be stored - vectors, lists, data.frames, models, etc.


```r
stuff <- rstack() %>%
         insert_top(mtcars) %>%
         insert_top(t.test(rnorm(25), rnorm(25)))

print(stuff)
```

```
## An rstack with  2  elements. 
##  :List of 10
##   ..$ statistic  : Named num 1.99
##   .. ..- attr(*, "names")= chr "t"
##   ..$ parameter  : Named num 48
##   .. ..- attr(*, "names")= chr "df"
##   ..$ p.value    : num 0.0527
##   ..$ conf.int   : num [1:2] -0.00614 1.01316
##   .. ..- attr(*, "conf.level")= num 0.95
##   ..$ estimate   : Named num [1:2] 0.137 -0.367
##   .. ..- attr(*, "names")= chr [1:2] "mean of x" "mean of y"
##   ..$ null.value : Named num 0
##   .. ..- attr(*, "names")= chr "difference in means"
##   ..$ stderr     : num 0.253
##   ..$ alternative: chr "two.sided"
##   ..$ method     : chr "Welch Two Sample t-test"
##   ..$ data.name  : chr "rnorm(25) and rnorm(25)"
##   ..- attr(*, "class")= chr "htest"
##  :'data.frame':	32 obs. of  11 variables:
##   ..$ mpg : num [1:32] 21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
##   ..$ cyl : num [1:32] 6 6 4 6 8 6 8 4 4 6 ...
##   ..$ disp: num [1:32] 160 160 108 258 360 ...
##   ..$ hp  : num [1:32] 110 110 93 110 175 105 245 62 95 123 ...
##   ..$ drat: num [1:32] 3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
##   ..$ wt  : num [1:32] 2.62 2.88 2.32 3.21 3.44 ...
##   ..$ qsec: num [1:32] 16.5 17 18.6 19.4 17 ...
##   ..$ vs  : num [1:32] 0 0 1 1 0 1 0 1 1 1 ...
##   ..$ am  : num [1:32] 1 1 1 0 0 0 0 0 0 0 ...
##   ..$ gear: num [1:32] 4 4 4 3 3 3 3 4 4 4 ...
##   ..$ carb: num [1:32] 4 4 1 1 2 1 4 2 2 4 ...
```



#### Conversion to or from lists or vectors


```r
letters_stack <- as.rstack(letters)
print(letters_stack)
```

```
## An rstack with  26  elements. 
## Top of the stack:
##   : chr "a"
##   : chr "b"
##   : chr "c"
##   : chr "d"
##   : chr "e"
##   : chr "f"
##     ...
```


... and converted to a list. If the list is simple, the R-base `unlist()` 
function is handy:


```r
letters_again <- as.list(letters) %>% unlist()
print(letters_again)
```

```
##  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
## [20] "t" "u" "v" "w" "x" "y" "z"
```


#### Conversion to data.frames

If the elements are lists that have the same element names in the same order, or dataframes with the
same column names in the same order, they can be converted to a `data.frame`. If elements are data frames, the rows
will be concatenated.


```r
people_stack <- rstack() %>%
                insert_top(list(name = "Joe", age = 26)) %>%
                insert_top(list(name = "Kim", age = 30))

people_df <- as.data.frame(people_stack)
print(people_df)
```

```
##   name age
## 1  Kim  30
## 2  Joe  26
```

#### Usage with loops

As a result, these structures pair nicely with loops. Here's a loop 
that computes a [Collatz sequence](https://en.wikipedia.org/wiki/Collatz_conjecture) from a given starting number.


```r
value <- 17
step <- 1

collatz_stack <- rstack()

while(value != 1) {
  if(value %% 2 == 0) {
    value <- value / 2
  } else {
    value <- value * 3 + 1
  }
  
  collatz_stack <- collatz_stack %>% insert_top(list(value = value, step = step))
  step <- step + 1
}

collatz_df <- as.data.frame(collatz_stack) %>% arrange(step)
print(collatz_df)
```

```
##    value step
## 1     52    1
## 2     26    2
## 3     13    3
## 4     40    4
## 5     20    5
## 6     10    6
## 7      5    7
## 8     16    8
## 9      8    9
## 10     4   10
## 11     2   11
## 12     1   12
```


Note: Although the structures in `rstackdeque` are generally designed to be fast, 
for performance-critical applications where the size of the result is known 
a-priori, using a [pre-allocation strategy](https://www.r-bloggers.com/2018/08/growing-objects-and-loop-memory-pre-allocation/) will be faster. 
Computing the Collatz sequence as shown above is an example where pre-allocation is difficult, and stacks, queues, and deques (double-ended queues) are important components of many algorithms. See Timothy Barry's [Collections in R: Review and Proposal](https://journal.r-project.org/archive/2018/RJ-2018-037/RJ-2018-037.pdf) (R Journal, 10(1), 2018) for a comparison of different R implementations of these and similar data structures. 

#### Persistence

All of the structures described are *persistent*, meaning that additions and 
removals don't alter the original; practically, a modified copy is returned.
(A full copy is not made however, the structures handle efficient organization
of the data in memory.)



```r
stack <- as.rstack(c("A", "B", "C"))
stack2 <- insert_top(stack, "G")
stack3 <- without_top(stack)

print(stack)
```

```
## An rstack with  3  elements. 
##  : chr "A"
##  : chr "B"
##  : chr "C"
```

```r
print(stack2)
```

```
## An rstack with  4  elements. 
##  : chr "G"
##  : chr "A"
##  : chr "B"
##  : chr "C"
```

```r
print(stack3)
```

```
## An rstack with  2  elements. 
##  : chr "B"
##  : chr "C"
```
This property makes functions that use the structures "side-effect-free" like most R types, supporting pure functions and parallelization. 



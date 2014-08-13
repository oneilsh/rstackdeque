\name{rstackdeque-package}
\alias{rstackdeque-package}
\alias{rstackdeque}
\docType{package}
\title{
Persistent, fast amortized stack and deque (double-ended queue) data structures for R.
}
\description{
Provides stacks and deques for R. Insertion/removal are functional (insertion and
removals are side-effect free and returned modified versions) and generally 
O(1) amortized. Includes some convenient helpers such as \code{as.data.frame}
and \code{as.list}.
}
\details{
\tabular{ll}{
Package: \tab rstackdeque\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-07-27\cr
License: \tab What license is it under?\cr
}
Stack, deque, and queue data structures for R: rstack(),
rdeque(), and rpqueue(). For efficiency, these structures support limited access; for
stacks: insert_top(), peek_top(), and without_top() (the latter returning a
version of the stack without the top element); for queues: insert_front(),
peek_front(), and without_front() (the latter returning a version of the
deque without the front element), and for deques additional corresponding methods
insert_back(), peek_back(), and without_back(). For convenience all types also support
O(N) conversion to lists with as.list(), and data frames with
as.data.frame(), which requires that all element have the same length and
non-contradictory names. 

Stacks are implemented as linked-lists via environments
for O(1) worst-case time. Deques are implemented as a pair of stacks, as in [1], with the 
modification that occasional O(N) rebalancing creates a copy to preserve old versions
at the cost of using O(N) memory for the new version. When used non- or partially-persistently, 
(e.g. \code{x <- without_front(x)}) all operations are O(1) amortized time. However, when used
fully persistently, occasionally inefficient operations may be repeated systematically. 
For example, if \code{y <- without_front(x)}
results in a rebalance, then subsequent similar calls will as well (e.g. \code{z <- without_front(x)})
and O(1) amortized time cannot be gauranteed. 

The rpqueue() structure is a queue based on a pair of lazy lists as described by [1] and [2], and does provide
O(1) amortized time for insert_back() and without_front() even when used fully persistently. However,
in practice rpqueues are about twice as slow as the rdeques described above, and should
only be used when earlier versions of the structure will be frequently removed from.
}
\author{
Shawn T. O'Neil <shawn.oneil@cgrb.oregonstate.edu>
}
\references{

[1] \emph{Purely Functional Data Structures}, Okasaki, C. 1999. Cambridge University Press.

[2] Simple and Efficient Purely Functional Queues and Deques, Okasaki, C. 1995. \emph{J. Functional Programming} 5(4): 583--592


}

\keyword{ package }
\seealso{
The \code{hash} package on CRAN, which provides hash-tables for R.

\code{http://cran.r-project.org/web/packages/hash/hash.pdf}
}
\examples{



## Basic stack operations
s <- rstack()
s <- insert_top(s, "A")
s <- insert_top(s, "B")
print(s)               ## B and A

s2 <- without_top(s)
print(s2)              ## just A
print(s)               ## still B and A
print(peek_top(s))     ## B

## Basic deque operations
d <- rdeque()
d <- insert_front(d, "A")
d <- insert_front(d, "B")
d <- insert_back(d, "C")
d <- insert_back(d, "D")
print(d)               ## B A C and D

d2 <- without_back(d)
print(d2)              ## B A C
print(d)               ## still B A C and D

## To and from lists:
e <- as.rstack(1:2000)
print(e)               ## stack of 2000
elist <- as.list(e)    ## list of 2000


## Converting a stack or deque to dataframe, built row-by-row
s <- rstack()
for(i in 1:1000) {
 if(runif(1,0,1) < 0.5) {
   s <- insert_top(s, data.frame(i = i, type = "sqrt", val = sqrt(i)))
 } else {
   s <- insert_top(s, data.frame(i = i, type = "log", val = log(i)))
 }
 if(i \%\% 10 == 0) {
   print(i/1000)
 }
}
print(head(as.data.frame(s)))





} 


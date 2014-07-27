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
Stack and deque data structures for R: rstack() and
rdeque(). For efficiency, these structures support limited access; for
stacks: insert_top(), peek_top(), and without_top() (the latter returning a
version of the stack without the top element); for deques: insert_front(),
peek_front(), and without_front() (the latter returning a version of the
deque without the front element), as well as corresponding methods
insert_back(), peek_back(), and without_back(). Both types also support
O(N) conversion to lists with, as.list(), and for convenience
as.data.frame(), which requires that all element have the same length() and
non-contradictory names().

Stacks are implemented as linked-lists via environments, with references behind-the-scenes
for O(1) time. Deques are implemented as a pair of stacks, as in the first chapter of [1], with the 
modification that rebalancing creates a copy to preserve efficiency for old versions
at the cost of using O(N) memory for the new version.
}
\author{
Shawn T. O'Neil <shawn.oneil@cgrb.oregonstate.edu>
}
\references{

[1] \emph{Purely Functional Data Structures}, Okasaki, C. 1999. Cambridge University Press.

}

\keyword{ package }
\seealso{
The \code{hash} package on CRAN, which provides hash-tables for R.

\code{http://cran.r-project.org/web/packages/hash/hash.pdf}
}
\examples{
s <- rstack()
s <- insert_top(s, "a")
}

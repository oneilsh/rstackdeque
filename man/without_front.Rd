% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/without_front.R
\name{without_front}
\alias{without_front}
\title{Return a version of an rdeque or rpqueue without the front element}
\usage{
without_front(x, ...)
}
\arguments{
\item{x}{rdeque or rpqueue to remove elements from.}

\item{...}{additional arguments to be passed to or from methods (ignored).}
}
\value{
a version of the rdeque or rpqueue with the front element removed.
}
\description{
Return a version of an rdeque or rpqueue without the front element
}
\details{
Simply returns a version of the given structure without the front element.
The original is left alone.
}
\examples{
d <- rdeque()
d <- insert_front(d, "a")
d <- insert_front(d, "b")
d <- insert_back(d, "c")

d2 <- without_front(d)
print(d2)

d3 <- without_front(d2)
print(d3)

print(d)
}
\references{
Okasaki, Chris. Purely Functional Data Structures. Cambridge University Press, 1999.
}


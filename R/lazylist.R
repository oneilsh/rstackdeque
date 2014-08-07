

## Uh, we don't use lazy lists for the new rpqueue :-P

#' @title Create a new lazy list
#' 
#' @description An internal datastructure to support rpqueues. Use instead of rstack at your own risk.
#' 
#' @details Implements lazy lists as tuple: a $head storing the data element, and a $tail storing a function
#' that will return the rest of the list.
lazylist <- function() {
  newlist <- new.env(parent = emptyenv())
  newlist$memtail <- NULL
  newlist$head <- NULL
  newlist$len <- 0
  newlist$tail <- function() {return(NULL)}
  class(newlist) <- "lazylist"
  return(newlist)
}






length.lazylist <- function(lazylist) {
  return(lazylist$len)
}






insert_top.lazylist <- function(lazylist, element) {
  if(is.null(element)) {
    stop("Sorry, you can't insert NULL into a lazylist.")
  }
  newlist <- lazylist()
  newlist$len <- length(lazylist) + 1
  newlist$head <- element
  # I'm actually not sure if delayedAssign is necessary here, but it doesn't hurt :-P
  #newlist$tail <- function() {return(lazylist)}
  delayedAssign("tail", function() {return(lazylist)}, assign.env = newlist)
  return(newlist)
}
insert_top <- function(lazylist, element) {UseMethod("insert_top", lazylist)}





as.list.lazylist <- function(x) {
  # let's not use recursion, so we don't blow the stack
  #return(c(x$head, as.list(x$tail())))
  prealloc <- vector("list", x$len)
  if(x$len < 1) {return(prealloc)}
  for(i in seq(1, length(x))) {
    prealloc[[i]] <- x$head
    x <- x$tail()
  }
  
  return(prealloc)
}





without_top.lazylist <- function(lazylist) {
  ## memoized!
  if(!is.null(lazylist$memtail)) {return(lazylist$memtail)}
  if(length(lazylist) < 1) {
    stop("Sorry, you can't get a version of an empty list without the top. Try checking empty() first.")
  }
  lazylist$memtail <- lazylist$tail()
  return(lazylist$memtail)
}
without_top <- function(lazylist) {UseMethod("without_top", lazylist)}






peek_top.lazylist <- function(lazylist) {
  if(length(lazylist) < 1) {
    stop("Sorry, you can't peek at the top of an empy list. Try checking empty() first.")
  }
  return(lazylist$head)
}
peek_top <- function(lazylist) {UseMethod("peek_top", lazylist)}







empty.lazylist <- function(lazylist) {
  if(length(lazylist) < 1) {return(TRUE)}
  return(FALSE)
}
empty <- function(lazylist) {UseMethod("empty", lazylist)}



## Concat would have to be written non-recursively and recreate the structure behind the scenes,
## and so would be O(n) and not incremental. This version would result in stack overflow.
#concat.lazylist <- function(lazylista, lazylistb) {
#  if(lazylista$len == 0) {
#    return(listb)
#  }
#  else {
#    newlist <- lazylist()
#    newlist$head <- lazylista$head
#    delayedAssign("tail", function() {tl(concat(lazylista, lazylistb))}, assign.env = newlist)
#    newlist$len <- lazylista$len + lazylistb$len
#    return(newlist)
#  }
#}
#concat <- function(lazylista, lazylistb) {UseMethod("concat", lazylista)}

#ll <- lazylist()
#ll <- insert_top(ll, "A")
#ll <- insert_top(ll, "B")
#ll <- insert_top(ll, "C")
#print(length(ll))
#ll <- without_top(ll)
#print(length(ll))




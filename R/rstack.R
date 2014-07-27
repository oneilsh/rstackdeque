## TODO: check for warnings on trying to remove from an empty queue or stack

#' @title
#' Create a new rstacknode with a given object
#' 
#' @description
#' For internal use by \code{rstack}s and \code{rdeque}s. An environment with no parent,
#' reference for the data and the next node.
#' 
#' @param data Data to reference with this node
#' @return Environment
#' 
rstacknode <- function(data) {
  newnode <- new.env(parent = emptyenv())
  newnode$data <- data
  newnode$nextnode <- NULL
  class(newnode) <- "rstacknode"
  return(newnode)
}


#' @title Create a new rstack
#' 
#' @description Creates a new, empty, \code{rstack} ready for use.
#' 
#' @details An rstack supports efficient insert into the top with \code{\link{insert_top}} (returning a version of
#' the stack with the new element), \code{\link{peek_top}} (returing the data stored at the top of the stack), and
#' \code{\link{without_top}} (returning a version with the top element removed). 
#' 
#' Other handy functions
#' insclude \code{\link{as.list}} and \code{\link{as.data.frame}} (the latter of which requires that
#' all elements can be appended to become rows of a data frame in a reasonable manner). Operations
#' are amortized O(1).
#' 
#' The \code{rstack} class also supports \code{rev} - this operation is O(N), and results in a copy. This 
#' means previous versions will retain their O(1) amortized nature (if we assume the cost of the reverse is charged
#' to the newly created stack), at the cost of memory usage. However, this means that if stacks
#' are used in a non-persistent way, e.g. \code{s <- rev(s)}, then the garbage collector is free to clean
#' up old versions of the data.
#' 
#' @export
#' @examples
#' s <- rstack()
#' s <- insert_top(s, "a")
#' s <- insert_top(s, "b")
#' print(s)
#' 
#' sl <- without_top(s)
#' print(sl)
#' print(s)
#' 
#' b <- peek_top(s)
#' print(b)
rstack <- function() {
  s <- new.env(parent = emptyenv())
  s$head <- NULL
  s$len <- 0
  class(s) <- "rstack"
  return(s)
}


## O(n) in the size of the input, converts to list
as.rstack.default <- function(input) {
  lastnode <- NULL
  listin <- rev(as.list(input))
  for(el in listin) {
    newnode <- rstacknode(el)
    newnode$nextnode <- lastnode
    lastnode <- newnode
  }
  newstack <- rstack()
  newstack$head <- lastnode
  newstack$len <- length(listin)
  return(newstack)
}

# O(1)
is_empty.rstack <- function(d) {
  if(length(d) < 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

## Default print. Awww yes.
print.rstack <- function(s) {
  cat(paste("An rstack with ", length(s), " elements. \n"))
  if(length(s) > 6 | T) {
    cat("Top of the stack:\n")
    str(as.list(head(s, 1)), comp.str = " ", no.list = T)
  } else {
    str(as.list(head(s, length(s))), comp.str = "", no.list = T)
  }
}

## O(n). Requires all elements to be the same length, and if any are named to have the same
## names. Stops otherwise.
as.data.frame.rstack <- function(d) {
  dlist <- as.list(d)
  uniquelens <- unique(lapply(dlist, length))
  if(length(uniquelens) > 1) {
    stop("Sorry, can't convert an rstack to a data frame unless all elements have the same length().")
  }
  uniquenamesets <- unique(lapply(dlist, names))
  if(length(uniquenamesets) > 1) {
    stop("Sorry, can't convert an rstack to a data frame when elements have contradictory names().")
  }
  return(do.call(rbind, dlist))
}

## insert_tops an element to the front of the given stack, returns modified stack (original left alone)
## O(1)
insert_top.rstack <- function(s, newdata) {
  newnode <- rstacknode(newdata)
  newstack <- rstack()
  newstack$len <- s$len + 1
  newnode$nextnode <- s$head
  newstack$head <- newnode
  return(newstack)
}

## Returns the data element at the top of the given stack, leaving the given stack alone.
## O(1)
peek_top.rstack <- function(s) {
  if(is.null(s$head)) {
    return(NA)
  } else {
    return(s$head$data)
  }
}

## Returns the length of the given stack.
## O(1)
length.rstack <- function(s) {
  return(s$len)
}


## Converts the stack to a list, with [[1]] being the top.
## Leaves the original stack alone
## O(n); with pre-allocation of the returned list
as.list.rstack <- function(s) {
  retlist <- vector("list", s$len)
  node <- s$head
  index <- 1
  while(!is.null(node)) {
    retlist[[index]] <- node$data
    node <- node$nextnode
    index <- index + 1
  }
  return(retlist)
}

## Reverses a stack. 
## O(n), but uses lists as an itermediary for whatever we can eek out from R's lists
rev.rstack <- function(s) {
  saslist <- as.list(s)
  lastnode <- NULL
  for(el in saslist) {
    newnode <- rstacknode(el)
    newnode$nextnode <- lastnode
    lastnode <- newnode
  }
  newstack <- rstack()
  newstack$head <- lastnode
  newstack$len <- length(saslist)
  return(newstack)
}



## Returns a version of the stack without the top element
## O(1)
without_top.rstack <- function(s, n = 1) {
  newstack <- rstack()
  node <- s$head
  for(i in seq(1,n)) {
    node <- node$nextnode
  }
  newstack$len <- s$len - n
  newstack$head <- node
  return(newstack)
}



## Returns the first few elements of an rstack as a new rstack.
## O(n) (n is the size of the head requested),
## not optimized with as.list or anything really 
head.rstack <- function(x, n = 6L) {
  newstack <- rstack()
  if(n == 0) {
    return(newstack)
  }
  node <- x$head
  for(i in seq(1,n)) {
    if(!is.null(node)) {
      newstack <- insert_top(newstack, node$data)
      node <- node$nextnode
    }
  }
  return(rev(newstack))
}


#' @title Create an \code{\link{rstack}} from a given input
#' 
#' @description Creates a new \code{\link{rstack}} from a given input. Coerces input to a 
#' list first using \code{as.list}, the elements of which become elements of the stack, and the
#' first element becoming the top of the stack.
#'
#' @details O(N) in the size of the input. Note that because data frames return a list of 
#' columns when run through \code{as.list}, running \code{as.rstack} results in a stack of
#' columns, rather than a stack of rows.
#'  
#' @return A new rstack
#' 
#' @export
#' @examples
#' s <- as.rstack(1:20)
#' print(s)
#' 
#' s <- as.rstack(1:200000)
#' print(s)
#' 
#' ## A stack with only 5 elements, one for each column
#' oops <- as.rstack(iris)
#' print(oops)
as.rstack <- function(x) {UseMethod("as.rstack", x)}


#' @title Insert an element into the top of an \code{rstack}
#' 
#' @description Returns a version of the stack with the new element in the top position.
#' 
#' @details O(1) time worst-case. Does not modify the original stack.
#' @param s The \code{rstack} to insert onto.
#' @param e The element to insert.
#' @return Modified version of the stack.
#' @export
#' @examples
#' s <- rstack()
#' s <- insert_top(s, "a")
#' s <- insert_top(s, "b")
#' print(s)
#' 
#' s2 <- insert_top(s, "c")
#' print(s2)
#' print(s)
insert_top <- function(s, e) { UseMethod("insert_top", s) }

#' @title Check if a stack or deque is empty
#' 
#' @description Returns \code{TRUE} if the stack or deque has length 0, FALSE otherwise.
#' 
#' @param d The \code{rstack} or \code{rdeque} to check
#' @return 1-element logical vector
#' @export
#' @examples
#' s <- rstack()
#' print(is_empty(s))        ## TRUE
#' s <- insert_top(s, "a")
#' print(is_empty(s))        ## FALSE
is_empty <- function(d) {UseMethod("is_empty", d)}

#' @title Return the data element at the top of the stack.
#' 
#' @description Simply returns the data element sitting at the top of the stack,
#' leaving the stack alone.
#' 
#' @details O(1) worst-case time.
#' @param s The stack to look at
#' @return The data element.
#' @examples
#' s <- rstack()
#' s <- insert_top(s, "a")
#' s <- insert_top(s, "b")
#' e <- peek_top(s)
#' print(e)
#' print(s)
peek_top <- function(s) { UseMethod("peek_top", s) }

#' @title Return version of the stack without the top
#' 
#' @details Simply returns a version of the given stack without the top \code{n} elements
#' (n = 1 by default). The original stack is left alone.
#' 
#' @details O(n) time worst case (in the number of elements removed). If the
#' number of elements removed would exceed the size of the stack, an empty stack is returned.
#' 
#' @param s The stack to remove elements from
#' @param n The number of elements to remove
#' @return A version of the fstack with \code{n} elements removed.
#' @export
#' @examples
#' s <- rstack()
#' s <- insert_top(s, "a")
#' s <- insert_top(s, "b")
#' s <- insert_top(s, "c")
#' 
#' s2 <- without_top(s)
#' print(s2)
#' 
#' s3 <- without_top(s, n = 2)
#' print(s3)
#' 
#' print(s)
without_top <- function(s, n) { UseMethod("without_top", s) }

## Also:
## print.rstack()
## as.list.rstack()
## length.rstack()
## head.rstack()
## as.data.frame.rstack()




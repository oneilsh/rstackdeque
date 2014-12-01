#' An rpqueue is an O(1) amortized queue even when used non-persistently (that is, earlier versions
#' of the datastructure may be fully used). For applications that don't require such full persistence,
#' the rdeque will likely be faster by a constant amount. 
#' 
#' Exported functions:
#' rpqueue()
#' length(q)
#' insert_back(q, element)
#' without_front(q)
#' as.list(q) 
#' as.rpqueue(input) 
#' as.data.frame(q) 
#' peek_front(q) 
#' empty(q) 
#' head(q) 
#' print(q) 







#' @title Create a new rpqueue
#' 
#' @description Creates a new, empty, \code{rpqueue} ready for use.
#' 
#' @details An rpqueue supports efficient insert into the back with \code{insert_back} and 
#' (returning a version of
#' the queue with the new element), peek into the front with \code{peek_front}
#' (returing the data stored at the front element), and removal with
#' \code{without_front} (returning a version with the front
#'  element removed). 
#' 
#' Other handy functions
#' include \code{as.list} and \code{as.data.frame} (the latter of which requires that
#' all elements can be appended to become rows of a data frame in a reasonable manner). All operations
#' are amortized O(1), but in practice rpqueues are a constant-factor slower than rdeques, and
#' so rdeques are recommended unless rpqueues are necessary (this can happen if many versions of the
#' same queue are continuously inserted into and removed from).
#' 
#' 
#' @examples
#' q <- rpqueue()
#' q <- insert_back(q, "a")
#' q <- insert_back(q, "b")
#' print(q)
#' 
#' q2 <- without_front(q)
#' print(q2)
#' print(q)
#' 
#' b <- peek_front(q)
#' print(b)
#' @export
rpqueue <- function() {
  newq <- new.env(parent = emptyenv())
  newq$lhat <- rstack()
  newq$l <- rstack()
  newq$r <- rstack()
  class(newq) <- "rpqueue"
  return(newq)
}

#' @export
print.rpqueue <- function(x, ...) {
  cat(paste("A queue with ", length(x), " elements.\n"))
  if(length(x) > 0) {
    cat(paste("Front: \n"))
  }
  if(length(x) > 0) {
    str(as.list(head(x, 6)), comp.str = "$", no.list = T)
  }
}

#' @title Return the first n elements of a queue as a queue
#' 
#' @description Returns the first n elements of a queue as a queue, or all of the elements if its length is less than n.
#' 
#' @details Runs in O(n) time (in the size of the number of elements requested); not particularly optimized.
#' @param x The rpqueue to get the head of
#' @param ... Arguments to be passed to or from other methods (ignored)
#' @param n The number of elements to get
#' @examples 
#' q <- rpqueue()
#' q <- insert_back(q, "a")
#' q <- insert_back(q, "b")
#' q <- insert_back(q, "c")
#' 
#' qt <- head(q, n = 2)
#' print(qt)
#' @export
head.rpqueue <- function(x, n = 6L, ...) {
  newrpqueue <- rpqueue()
  if(n < 0) {
    n = max(n, -1*length(x))
    n = length(x) + n
  } 
  if(n > length(x)) {
    n = length(x)
  } 
  if(n == 0) {
    return(newrpqueue)
  }
  for(i in seq(1,n)) {
    el <- peek_front(x)
    newrpqueue <- insert_back(newrpqueue, el)
    x <- without_front(x)
  }
  return(newrpqueue)
}


#' @export
as.rpqueue.default <- function(x) {
  retq <- rpqueue()
  for(el in as.list(x)) {
    retq <- insert_back(retq, el)
  }
  return(retq)  
}

#' @title Create an \code{rpqueue} from a given input
#' 
#' @description Creates a new \code{rpqueue} from a given input. Coerces input to a 
#' list first using \code{as.list}, the elements of which become elements of the queue, and the
#' first element becoming the front of the queue
#'
#' @details O(N) in the size of the input. Note that because data frames return a list of 
#' columns when run through \code{as.list}, running \code{as.rpqueue} results in a queue of
#' columns, rather than a queue of rows.
#' @param x Input to convert to a queue
#' @return A new rpqueue
#' @examples
#' d <- as.rpqueue(1:20)
#' print(d)
#' 
#' ## A queue with only 5 elements, one for each column
#' oops <- as.rdeque(iris)
#' print(oops)
#' @export
as.rpqueue <- function(x) {UseMethod("as.rpqueue", x)}


#' @title Conversion of an \code{rpqueue} to a \code{list}
#' 
#' @description Converts an \code{rpqueue} to a list, where the front of the queue becomes
#' the first element of the list, the back the last. 
#' 
#' @details O(N), but the generated list is pre-allocated for efficiency.
#' @param x The queue to convert
#' @param ... Additional parameters sent to as.list after initial conversion.
#' @return A list
#' @examples
#' q <- rpqueue()
#' q <- insert_back(q, "a")
#' q <- insert_back(q, "b")
#' 
#' qlist <- as.list(q)
#' print(qlist)
#' @export
as.list.rpqueue <- function(x, ...) {
  return(as.list(c(as.list(x$l), rev(as.list(x$r))), ...))
}

#' @title Convert an \code{rpqueue} to a \code{data.frame}.
#' 
#' @description Converts the elements of an \code{rpqueue} into rows of a dataframe, if this is reasonable.
#' 
#' @details This method runs in O(N) time, and will only work if all elements of the deque have the
#' same length() (e.g., same number of columns), and if any of the elements have names, then those
#' names do not conflict (e.g., same column names where used). This is accomplished by a call to
#' \code{do.call("rbind", as.list(d))}, where \code{as.list(d)} converts the deque \code{d} to a list
#' where the front element becomes the first element of the list and the back element becomes the last of the list.
#' @param x The deque to convert
#' @param row.names Passed on to as.data.frame before final conversion
#' @param optional Passed onto as.data.frame before final conversion
#' @param ... Passed onto as.data.frame before final conversion
#' @return A data frame with the first row(s) the previous front of the deque, last row(s) previous back of the deque.
#' @examples 
#' q <- rpqueue()
#' q <- insert_back(q, data.frame(names = c("Bob", "Joe"), ages = c(25, 18)))
#' q <- insert_back(q, data.frame(names = c("Mary", "Kate", "Ashley"), ages = c(27, 26, 21)))
#' print(q)
#' 
#' ddf <- as.data.frame(q)
#' print(ddf)
#' 
#' 
#' ## Building a queue in a loop, converting to a dataframe after the fact;
#' q <- rpqueue()
#' for(i in 1:1000) {
#'  if(runif(1,0,1) < 0.5) {
#'    q <- insert_back(q, data.frame(i = i, type = "sqrt", val = sqrt(i)))
#'  } else {
#'    q <- insert_back(q, data.frame(i = i, type = "log", val = log(i)))
#'  }
#'  if(i %% 100 == 0) {
#'    print(i/1000)
#'  }
#' }
#' print(head(as.data.frame(q)))
#' print(tail(as.data.frame(q)))
#' @export
as.data.frame.rpqueue <- function(x, row.names = NULL, optional = FALSE, ...) {
  dlist <- lapply(as.list(x), as.data.frame, ...)
  uniquelens <- unique(lapply(dlist, length))
  if(length(uniquelens) > 1) {
    stop("Sorry, can't convert an rdeque to a data frame unless all elements have the same length().")
  }
  uniquenamesets <- unique(lapply(dlist, names))
  if(length(uniquenamesets) > 1) {
    stop("Sorry, can't convert an rpqueue to a data frame when elements have contradictory names().")
  }
  return(as.data.frame(do.call(rbind, dlist), row.names, optional, ...))
}


#' @export
empty.rpqueue <- function(x) {
  if(length(x) > 0) {return(FALSE)}
  return(TRUE)
}
empty <- function(x) {UseMethod("empty", x)}


#' @export
peek_front.rpqueue <- function(x) {
  if(length(x) < 1) {
    stop("Sorry, can't peek_front into a queue that is empty. Try checking with empty() first.")
  }
  if(length(x$l) > 0) {
    return(peek_top(x$l))
    # invariant: if l is empty but the deque is not, r has only one element
  } else {
    return(peek_top(x$r))
  }
}
peek_front <- function(x) {UseMethod("peek_front", x)}


#' @title Default method for \code{length} on an rpqueue
#' 
#' @description Returns the number of elements in an \code{rpqueue}.
#' 
#' @details O(1) time, as this information is stored seperately and updated on insert/remove.
#' @param x The rpqueue to get the length of
#' @return A vector of length 1, which the number of elements of the queue
#' @examples
#' q <- rpqueue()
#' q <- insert_back(q, "a")
#' print(length(q))         # 1
#' q <- insert_back(q, "b")
#' print(length(q))         # 2
#' @export
length.rpqueue <- function(x) {
  return(length(x$l) + length(x$r))
}



#' @export
rotate.rpqueue <- function(rpqueue, acclazylist) {
  ## safety check :-P
  if(length(rpqueue) == 0) {
    return(rpqueue)
  }
  if(length(rpqueue$l) == 0) {
    newq <- rpqueue()
    newq$l <- rstack()

    newq$l$head <- rstacknode(peek_top(rpqueue$r))
    newq$l$head$nextnode <- acclazylist$head  #ie, tail is the lazylist
    newq$l$len <- length(rpqueue) + length(acclazylist)
    newq$r <- rstack()
    return(newq)
  } else {
    newq <- rpqueue()
    newq$l <- rstack()
    newq$l$head <- rstacknode(peek_top(rpqueue$l))
    newq$l$len <- length(rpqueue) + length(acclazylist)
    
    without_heads <- rpqueue()
    without_heads$l <- without_top(rpqueue$l)
    without_heads$r <- without_top(rpqueue$r)
    
    acc <- insert_top(acclazylist, peek_top(rpqueue$r))
    delayedAssign("nextnode", rotate(without_heads, acc)$l$head, assign.env = newq$l$head)
    newq$r <- rstack()
    return(newq)
  }
}

#' @title Incremental rotation method for rpqueue objects
#' 
#' @description An internal method for keeping the pair of lazy lists approximately equal
#' in length in an incremental fashion. 
#' 
#' @details See \emph{Simple and Efficient Purely Functional Queues and Deques}, 
#' Okasaki 1995, J. Functional Programming, 5(4) 583 to 592
#' 
#' @param rpqueue The rpqueue to rotate
#' @param acclazylist A lazy list that serves as the incremental accumulator
#' @return A fully rotated queue, but with the l list delayed in evaluation
#' @export
rotate <- function(rpqueue, acclazylist) {UseMethod("rotate", rpqueue)}



#' @export
makeequal.rpqueue <- function(rpqueue) {
  #print(paste(length(rpqueue$lhat, length(rpqueue$l), length(rpqueue$r))))
  if(length(rpqueue$lhat) > 0) {
    # maybe here we can avoid creating a whole new object?
    newq <- rpqueue()
    newq$l <- rpqueue$l
    newq$r <- rpqueue$r
    newq$lhat <- without_top(rpqueue$lhat)
    return(newq)
  } else {
    newq <- rpqueue()
    acc <- rstack()
    resq <- rotate(rpqueue, acc)
    newq$l <- resq$l
    newq$lhat <- resq$l
    newq$r <- rstack()
    return(newq)
  }
}
#' @title Make the two sides equal if needed.
#' @details A wrapper around rotate.
#' @param rpqueue The queue to make equal
#' @return A made-equal queue (if it needed it)
#' @export
makeequal <- function(rpqueue) {UseMethod("makeequal", rpqueue)}


#' @export
insert_back.rpqueue <- function(x, e) {
  newq <- rpqueue()
  newq$l <- x$l
  newq$r <- insert_top(x$r, e)
  newq$lhat <- x$lhat
  return(makeequal(newq))
}
insert_back <- function(x, e) {UseMethod("insert_back", x)}


#' @export
without_front.rpqueue <- function(x) {
  if(length(x) < 1) {
    stop("Sorry, you can't get a version of an empty queue without the front. Try checking with empty() head.")
  }
  newq <- rpqueue()
  newq$l <- without_top(x$l)
  newq$r <- x$r
  newq$lhat <- x$lhat
  return(makeequal(newq))
}
without_front <- function(x) {UseMethod("without_front", x)}


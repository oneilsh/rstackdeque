#' O(1) amortized deque, but only when used semi-persistnetly. Exported functions:
#' 
#' rdeque()
#' empty(d)
#' as.rdeque(input)
#' length(d)
#' as.list(d)
#' print(d)
#' as.data.frame(d)
#' without_front(d)
#' without_back(d)
#' insert_front(d, element)
#' insert_back(d, element)
#' peek_front(d)
#' peek_back(d)
#' head() 




### Invariant: if there is at least 2 elements, then both sides are nonempty.
### Rebalancing happens when there are more than 2 elements, and one of the sides
### falls below 6 elements (so very small deques might be rebalanced often, but this
### means we can always peek at the top and bottom 6 elements, or whatever is there)

############# METHODS




#' @title Create a new rdeque
#' 
#' @description Creates a new, empty, \code{rdeque} ready for use.
#' 
#' @details An rdeque supports efficient insert into the front and back with \code{insert_front} and 
#' \code{insert_back} (returning a version of
#' the deque with the new element), peek into the front and back with \code{peek_front} and \code{peek_back} 
#' (returing the data stored at the front or back), and removal with
#' \code{without_front} and \code{without_back} (returning a version with the front or back
#'  element removed). 
#' 
#' Other handy functions
#' include \code{as.list} and \code{as.data.frame} (the latter of which requires that
#' all elements can be appended to become rows of a data frame in a reasonable manner). Most operations
#' are amortized O(1), UNLESS previous versions of the structure are used heavily. In these cases, if you notice
#' significant slowdown, consider using an rpqueue.
#' 
#' @examples
#' d <- rdeque()
#' d <- insert_front(d, "a")
#' d <- insert_front(d, "b")
#' d <- insert_back(d, "c")
#' d <- insert_back(d, "d")
#' print(d)
#' 
#' d2 <- without_back(d)
#' print(d2)
#' print(d)
#' 
#' b <- peek_front(d)
#' print(b)
#' @export
rdeque <- function() {
  d <- new.env(parent = emptyenv())
  d$l <- rstack()
  d$r <- rstack()
  class(d) <- "rdeque"
  return(d)
}


#' @export
empty.rdeque <- function(x) {
  if(length(x) < 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}



#' @title Return the top n elements of a deque as a deque
#' 
#' @description Returns the top n elements of a deque as a deque, or all of the elements if its length is less than n.
#' 
#' @details Runs in O(n) time (in the size of the number of elements requested); not particularly optimized.
#' @param x The deque to get the head of
#' @param ... Arguments to be passed to or from other methods (ignored)
#' @param n The number of elements to get
#' @examples 
#' d <- rdeque()
#' d <- insert_back(d, "a")
#' d <- insert_back(d, "b")
#' d <- insert_back(d, "c")
#' 
#' dt <- head(d, n = 2)
#' print(dt)
#' @export
head.rdeque <- function(x, n = 6L, ...) {
  newdeque <- rdeque()
  if(n < 0) {
    n = max(n, -1*length(x))
    n = length(x) + n
  } 
  if(n > length(x)) {
    n = length(x)
  } 
  if(n == 0) {
    return(newdeque)
  }
  for(i in seq(1,n)) {
    newdeque <- insert_back(newdeque, peek_front(x))
    x <- without_front(x)
  }
  return(newdeque)
}


#' @export
as.rdeque.default <- function(x) {
  input <- x
  newd <- rdeque()
  if(length(input) == 0) {
    return(newd)
  } else if(length(input) == 1) {
    newd$r <- insert_top(newd$r, as.list(input)[[1]])
    return(newd)
  } else {
    alllist <- as.list(input)
    mid <- as.integer(length(alllist)/2)
    left <- alllist[1:mid]
    right <- rev(alllist[(mid+1):length(alllist)])
    
    newd <- rdeque()
    newd$l <- as.rstack(left)
    newd$r <- as.rstack(right)
    return(newd)
  }
}


#' @title Default method for \code{length} on an rdeque
#' 
#' @description Returns the number of elements in an \code{rdeque}.
#' 
#' @details O(1) time, as this information is stored seperately and updated on insert/remove.
#' @param x The deque to get the length of
#' @return A vector of length 1, which the number of elements of the deque
#' @examples
#' d <- rdeque()
#' d <- insert_front(d, "a")
#' print(length(d))         # 1
#' d <- insert_back(d, "b")
#' print(length(d))         # 2
#' @export
length.rdeque <- function(x) {
  return(x$l$len + x$r$len)
}


#' @title Conversion of an \code{rdeque} to a \code{list}
#' 
#' @description Converts an \code{rdeque} to a list, where the front of the deque becomes
#' the first element of the list, the back the last. 
#' 
#' @details O(N), but the generated list is pre-allocated for efficiency.
#' @param x The stack to convert
#' @param ... Additional parameters sent to as.list after initial conversion.
#' @return A list
#' @examples
#' d <- rdeque()
#' d <- insert_front(d, "a")
#' d <- insert_front(d, "b")
#' 
#' dlist <- as.list(d)
#' print(dlist)
#' @export
as.list.rdeque <- function(x, ...) {
  return(as.list(c(as.list(x$l), rev(as.list(x$r))), ...))
}

#' @export
print.rdeque <- function(x, ...) {
  d <- x
  cat(paste("A deque with ", length(d), " elements.\n"))
  if(length(d) > 0) {
    cat(paste("Front to back: \n"))
  }
  if(length(d$l) > 0) {
    str(as.list(head(d$l, 6)), comp.str = "$", no.list = T)
  }
  if(length(d) > 12) {
    cat("    ...\n")
  }
  if(length(d$r) > 0) {
    str(rev(as.list(head(d$r, 6))), comp.str = "$", no.list = T)
  }
}

#' @title Convert an \code{rdeque} to a \code{data.frame}.
#' 
#' @description Converts the elements of an \code{rdeque} into rows of a dataframe, if this is reasonable.
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
#' d <- rdeque()
#' d <- insert_front(d, data.frame(names = c("Bob", "Joe"), ages = c(25, 18)))
#' d <- insert_back(d, data.frame(names = c("Mary", "Kate", "Ashley"), ages = c(27, 26, 21)))
#' print(d)
#' 
#' ddf <- as.data.frame(d)
#' print(ddf)
#' 
#' 
#' ## Building a deque in a loop, converting to a dataframe after the fact;
#' ## sometimes adding to the front, sometimes to the back
#' d <- rdeque()
#' for(i in 1:1000) {
#'  if(runif(1,0,1) < 0.5) {
#'    d <- insert_front(d, data.frame(i = i, type = "sqrt", val = sqrt(i)))
#'  } else {
#'    d <- insert_back(d, data.frame(i = i, type = "log", val = log(i)))
#'  }
#'  if(i %% 100 == 0) {
#'    print(i/1000)
#'  }
#' }
#' print(head(as.data.frame(d)))
#' print(tail(as.data.frame(d)))
#' @export
as.data.frame.rdeque <- function(x, row.names = NULL, optional = FALSE, ...) {
  dlist <- as.list(x)
  uniquelens <- unique(lapply(dlist, length))
  if(length(uniquelens) > 1) {
    stop("Sorry, can't convert an rdeque to a data frame unless all elements have the same length().")
  }
  uniquenamesets <- unique(lapply(dlist, names))
  if(length(uniquenamesets) > 1) {
    stop("Sorry, can't convert an rdeque to a data frame when elements have contradictory names().")
  }
  return(as.data.frame(do.call(rbind, dlist), row.names, optional, ...))
}



#' @export
fix.rdeque <- function(d) {
  if(length(d) < 2 | (length(d$l) > 6 & length(d$r) > 6)) {
    return(d)
  } else {
    alllist <- c(as.list(d$l), rev(as.list(d$r)))
    mid <- as.integer(length(alllist)/2)
    left <- alllist[1:mid]
    right <- rev(alllist[(mid+1):length(alllist)])
    
    newd <- rdeque()
    newd$l <- as.rstack(left)
    newd$r <- as.rstack(right)
    return(newd)
  }
}


#' @export
without_front.rdeque <- function(x) {
    ## if the length of l is 0, then r has only one element (invariant), so we can return an empty deque
  if(length(x$l) == 0) {
    return(rdeque())
  } 
  newd <- rdeque()
  newd$l <- x$l
  newd$r <- x$r
  newd$l <- without_top(newd$l)
  newd <- fix(newd)
  return(newd)
}


#' @export
without_back.rdeque <- function(d) {
    ## if the length of r is 0, then l has only one element (invariant), so we can return an empty deque
  if(length(d$r) == 0) {
    return(rdeque())
  } 
  newd <- rdeque()
  newd$l <- d$l
  newd$r <- d$r
  newd$r <- without_top(newd$r)
  newd <- fix(newd)
  return(newd)
}

#' @export
peek_front.rdeque <- function(x) {
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


#' @export
peek_back.rdeque <- function(d) {
  if(length(d) < 1) {
    stop("Sorry, can't peek_front into a queue that is empty. Try checking with empty() first.")
  }
  if(length(d$r) > 0) {
    return(peek_top(d$r))
    # invariant: if r is empty but the deque is not, l has only one element
  } else {
    return(peek_top(d$l))
  }
}



#' @export
insert_front.rdeque <- function(d, e) {
  newd <- rdeque()
  newd$l <- insert_top(d$l, e)
  newd$r <- d$r
  newd <- fix(newd)
  return(newd)
}


#' @export
insert_back.rdeque <- function(x, e) {
  newd <- rdeque()
  newd$r <- insert_top(x$r, e)
  newd$l <- x$l
  newd <- fix(newd)
  return(newd)
}










#' @title Internal fix method for deques
#' 
#' @description Maintains the invariant that there is always something in $l and $r
#' so long as there is 2 more elements in the queue.
#' 
#' @details In fact, fix will be called whenever there are fewer than 6 elements in both
#' the front and end of the deque. Generally this method is O(N), and so a full copy is returned.
#' @param d The deque to fix
#' @return The fixed, "balanced" deque
#' @export
fix <- function(d) {UseMethod("fix", d)}


#' @title Create an \code{rdeque} from a given input
#' 
#' @description Creates a new \code{rdeque} from a given input. Coerces input to a 
#' list first using \code{as.list}, the elements of which become elements of the deque, and the
#' first element becoming the front of the deque.
#'
#' @details O(N) in the size of the input. Note that because data frames return a list of 
#' columns when run through \code{as.list}, running \code{as.rdeque} results in a deque of
#' columns, rather than a deque of rows.
#' @param x Input to convert to a deque
#' @return A new rdeque
#' @examples
#' d <- as.rdeque(1:20)
#' print(d)
#' 
#' d <- as.rdeque(1:200000)
#' print(d)
#' 
#' ## A deque with only 5 elements, one for each column
#' oops <- as.rdeque(iris)
#' print(oops)
#' @export
as.rdeque <- function(x) {UseMethod("as.rdeque", x)}






#' @title Return version of a deque/queue without the front
#' 
#' @details Simply returns a version of the given structure without the front element.
#' The original is left alone.
#' 
#' @details O(n) time worst case (in the number of elements removed) for rdeque, O(log(n)) for prqueue. Will
#' throw an error if the structure is empty to begin with.
#' 
#' @param x The deque/queue to remove elements from
#' @return A version of the deque/queue with the front element removed.
#' @examples
#' d <- rdeque()
#' d <- insert_front(d, "a")
#' d <- insert_front(d, "b")
#' d <- insert_back(d, "c")
#' 
#' d2 <- without_front(d)
#' print(d2)
#' 
#' d3 <- without_front(d2)
#' print(d3)
#' 
#' print(d)
#' @export
without_front <- function(x) {UseMethod("without_front", x)}


#' @title Return version of the deque without the back
#' 
#' @details Simply returns a version of the given deque without the back \code{n} elements
#' (n = 1 by default). The original deque is left alone.
#' 
#' @details O(n) time worst case (in the number of elements removed). If the
#' number of elements removed would exceed the size of the deque, an empty deque is returned.
#' 
#' @param d The deque to remove elements from
#' @return A version of the deque with \code{n} elements removed.
#' @examples
#' d <- rdeque()
#' d <- insert_front(d, "a")
#' d <- insert_front(d, "b")
#' d <- insert_back(d, "c")
#' 
#' d2 <- without_back(d)
#' print(d2)
#' 
#' d3 <- without_back(d2)
#' print(d3)
#' 
#' print(d)
#' @export
without_back <- function(d) {UseMethod("without_back", d)}


#' @title Return the data element at the front of the deque.
#' 
#' @description Simply returns the data element sitting at the front of the deque,
#' leaving the deque alone.
#' 
#' @details O(1) worst-case time.
#' @param x The deque to look at.
#' @return The data element.
#' @examples
#' d <- rdeque()
#' d <- insert_front(d, "a")
#' d <- insert_back(d, "b")
#' e <- peek_front(d)
#' print(e)
#' print(d)
#' @export
peek_front <- function(x) {UseMethod("peek_front", x)}






#' @title Return the data element at the back of the deque.
#' 
#' @description Simply returns the data element sitting at the back of the deque,
#' leaving the deque alone.
#' 
#' @details O(1) worst-case time.
#' @param d The deque to look at.
#' @return The data element.
#' @examples
#' d <- rdeque()
#' d <- insert_front(d, "a")
#' d <- insert_back(d, "b")
#' e <- peek_back(d)
#' print(e)
#' print(d)
#' @export
peek_back <- function(d) {UseMethod("peek_back", d)}



#' @title Insert an element into the front of an \code{rdeque}
#' 
#' @description Returns a version of the deque with the new element in the front position.
#' 
#' @details O(1) time worst-case. Does not modify the original deque. 
#' @param d The \code{rdeque} to insert onto.
#' @param e The element to insert.
#' @return Modified version of the deque
#' @examples
#' d <- rdeque()
#' d <- insert_front(d, "a")
#' d <- insert_front(d, "b")
#' print(d)
#' 
#' d2 <- insert_front(d, "c")
#' print(d2)
#' print(d)
#' @export
insert_front <- function(d, e) {UseMethod("insert_front", d)}



#' @title Insert an element into the back of an \code{rdeque} or \code{rpqueue}
#' 
#' @description Returns a version of the deque/queue with the new element in the back position.
#' 
#' @details O(1) time worst-case. Does not modify the original. 
#' @param x The deque or queue to insert onto.
#' @param e The element to insert.
#' @return Modified version of the deque/queue.
#' @examples
#' d <- rdeque()
#' d <- insert_back(d, "a")
#' d <- insert_back(d, "b")
#' print(d)
#' 
#' d2 <- insert_back(d, "c")
#' print(d2)
#' print(d)
#' @export
insert_back <- function(x, e) {UseMethod("insert_back", x)}







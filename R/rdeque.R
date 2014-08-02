
### Invariant: if there is at least 2 elements, then both sides are nonempty.
### Rebalancing happens when there are more than 2 elements, and one of the sides
### falls below 6 elements (so very small deques might be rebalanced often, but this
### means we can always peek at the top and bottom 6 elements, or whatever is there)

## Todo: peek_front and peek_back need to work even if there are fewer than 2 elements
## Github test...





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
#' are amortized O(1) (so long as we assume the cost of removal is charged to the new variable).
#' 
#' 
#' @name rdeque
#' @export
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
rdeque <- function() {
  d <- new.env(parent = emptyenv())
  d$l <- rstack()
  d$r <- rstack()
  class(d) <- "rdeque"
  return(d)
}


#' @title Default method for checking emptyness of an rdeque
#' @seealso \code{is_empty}
#' @rdname is_empty
#' @name is_empty
is_empty.rdeque <- function(d) {
  if(length(d) < 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#' @title Default method for converting to an rdeque
#' @seealso \code{as.rdeque}
#' @rdname as.rdeque
#' @name as.rdeque
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
#' @export
#' @name length
#' @examples
#' d <- rdeque()
#' d <- insert_front(d, "a")
#' print(length(d))         # 1
#' d <- insert_back(d, "b")
#' print(length(d))         # 2
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
#' @export
#' @name as.list
#' @examples
#' d <- rdeque()
#' d <- insert_front(d, "a")
#' d <- insert_front(d, "b")
#' 
#' dlist <- as.list(d)
#' print(dlist)
as.list.rdeque <- function(x, ...) {
  return(as.list(c(as.list(x$l), rev(as.list(x$r))), ...))
}

#' @title Default print method for an rdeque
#' 
#' @description Prints the front and back of an rdeque, up to the first 6 elements each.
#' 
#' @param x The deque to print
#' @param ... Arguments passed to or from other functions (unused)
#' @export
#' @name print
#' 
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
#' @export
#' @name as.data.frame
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
#' require("dplyr")
#' d <- rdeque()
#' for(i in 1:10000) {
#'  if(runif(1,0,1) < 0.5) {
#'    d <- d %>% insert_front(data.frame(i = i, type = "sqrt", val = sqrt(i)))
#'  } else {
#'    d <- d %>% insert_back(data.frame(i = i, type = "log", val = log(i)))
#'  }
#'  if(i %% 100 == 0) {
#'    print(i/10000)
#'  }
#' }
#' print(head(as.data.frame(d)))
#' print(tail(as.data.frame(d)))
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



#' @title Internal method for maintaining deque-i-ness
#' @rdname fix
#' @name fix
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


#' @title Default method for \code{rdeque} \code{without_front}
#' @seealso \code{without_front}
#' @rdname without_front
#' @name without_front
without_front.rdeque <- function(d, n = 1) {
  if(length(d) < 1) {
    stop("Sorry, cannot run without_front() on an rdeque that is empty. Check with is_empty() first.")
  }
  if(n < 1) {
    return(d)
    ## if the length of l is 0, then r has only one element (invariant)
  } else if(length(d$l) == 0) {
    return(rdeque())
  } 
  newd <- rdeque()
  newd$l <- d$l
  newd$r <- d$r
  for(i in seq(1,n)) {
    newd$l <- without_top(newd$l)
    newd <- fix(newd)
  }
  return(newd)
}


#' @title Default method for \code{rdeque} \code{without_front}
#' @seealso \code{without_front}
#' @rdname without_back
#' @name without_back
without_back.rdeque <- function(d, n = 1) {
  if(length(d) < 1) {
    stop("Sorry, cannot run without_back() on an rdeque that is empty. Check with is_empty() first.")
  }
  if(n < 1) {
    return(d)
    ## if the length of r is 0, then l has only one element (invariant)
  } else if(length(d$r) == 0) {
    return(rdeque())
  } 
  newd <- rdeque()
  newd$l <- d$l
  newd$r <- d$r
  for(i in seq(1,n)) {
    newd$r <- without_top(newd$r)
    newd <- fix(newd)
  }
  return(newd)
}

#' @title Default method for \code{rdeque} \code{peek_front}
#' @seealso \code{peek_front}
#' @rdname peek_front
#' @name peek_front
peek_front.rdeque <- function(d) {
  if(length(d) < 1) {return(NULL)}
  if(length(d$l) > 0) {
    return(peek_top(d$l))
    # invariant: if l is empty but the deque is not, r has only one element
  } else {
    return(peek_top(d$r))
  }
}


#' @title Default method for \code{rdeque} \code{peek_back}
#' @seealso \code{peek_back}
#' @rdname peek_back
#' @name peek_back
peek_back.rdeque <- function(d) {
  if(length(d) < 1) {return(NULL)}
  if(length(d$r) > 0) {
    return(peek_top(d$r))
    # invariant: if r is empty but the deque is not, l has only one element
  } else {
    return(peek_top(d$l))
  }
}



#' @title Default method for \code{rdeque} \code{insert_front}
#' @seealso \code{insert_front}
#' @rdname insert_front
#' @name insert_front
insert_front.rdeque <- function(d, e) {
  newd <- rdeque()
  newd$l <- insert_top(d$l, e)
  newd$r <- d$r
  newd <- fix(newd)
  return(newd)
}


#' @title Default method for \code{rdeque} \code{insert_back}
#' @seealso \code{insert_back}
#' @rdname insert_back
#' @name insert_back
insert_back.rdeque <- function(d, e) {
  newd <- rdeque()
  newd$r <- insert_top(d$r, e)
  newd$l <- d$l
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
#' @name as.rdeque
#' @export
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
as.rdeque <- function(x) {UseMethod("as.rdeque", x)}






#' @title Return version of the deque without the front
#' 
#' @details Simply returns a version of the given deque without the front \code{n} elements
#' (n = 1 by default). The original deque is left alone.
#' 
#' @details O(n) time worst case (in the number of elements removed). If the
#' number of elements removed would exceed the size of the deque, an empty deque is returned.
#' 
#' @param d The deque to remove elements from
#' @param n The number of elements to remove
#' @return A version of the deque with \code{n} elements removed.
#' @export
#' @name without_front
#' @examples
#' d <- rdeque()
#' d <- insert_front(d, "a")
#' d <- insert_front(d, "b")
#' d <- insert_back(d, "c")
#' 
#' d2 <- without_front(d)
#' print(d2)
#' 
#' d3 <- without_front(d, n = 2)
#' print(d3)
#' 
#' print(d)
without_front <- function(d) {UseMethod("without_front", d)}


#' @title Return version of the deque without the back
#' 
#' @details Simply returns a version of the given deque without the back \code{n} elements
#' (n = 1 by default). The original deque is left alone.
#' 
#' @details O(n) time worst case (in the number of elements removed). If the
#' number of elements removed would exceed the size of the deque, an empty deque is returned.
#' 
#' @param d The deque to remove elements from
#' @param n The number of elements to remove
#' @return A version of the deque with \code{n} elements removed.
#' @export
#' @name without_back
#' @examples
#' d <- rdeque()
#' d <- insert_front(d, "a")
#' d <- insert_front(d, "b")
#' d <- insert_back(d, "c")
#' 
#' d2 <- without_back(d)
#' print(d2)
#' 
#' d3 <- without_back(d, n = 2)
#' print(d3)
#' 
#' print(d)
without_back <- function(d) {UseMethod("without_back", d)}


#' @title Return the data element at the front of the deque.
#' 
#' @description Simply returns the data element sitting at the front of the deque,
#' leaving the deque alone.
#' 
#' @details O(1) worst-case time.
#' @param d The deque to look at.
#' @return The data element.
#' @name peek_front
#' @examples
#' d <- rstack()
#' d <- insert_front(d, "a")
#' d <- insert_back(d, "b")
#' e <- peek_front(d)
#' print(e)
#' print(d)
peek_front <- function(d) {UseMethod("peek_front", d)}






#' @title Return the data element at the back of the deque.
#' 
#' @description Simply returns the data element sitting at the back of the deque,
#' leaving the deque alone.
#' 
#' @details O(1) worst-case time.
#' @param d The deque to look at.
#' @return The data element.
#' @name peek_back
#' @examples
#' d <- rstack()
#' d <- insert_front(d, "a")
#' d <- insert_back(d, "b")
#' e <- peek_back(d)
#' print(e)
#' print(d)
peek_back <- function(d) {UseMethod("peek_back", d)}



#' @title Insert an element into the front of an \code{rdeque}
#' 
#' @description Returns a version of the deque with the new element in the front position.
#' 
#' @details O(1) time worst-case. Does not modify the original deque. 
#' @param d The \code{rdeque} to insert onto.
#' @param e The element to insert.
#' @return Modified version of the deque
#' @name insert_front
#' @export
#' @examples
#' d <- rdeque()
#' d <- insert_front(d, "a")
#' d <- insert_front(d, "b")
#' print(d)
#' 
#' d2 <- insert_front(d, "c")
#' print(d2)
#' print(d)
insert_front <- function(d, e) {UseMethod("insert_front", d)}



#' @title Insert an element into the back of an \code{rdeque}
#' 
#' @description Returns a version of the deque with the new element in the back position.
#' 
#' @details O(1) time worst-case. Does not modify the original deque. 
#' @param d The \code{rdeque} to insert onto.
#' @param e The element to insert.
#' @return Modified version of the deque
#' @name insert_back
#' @export
#' @examples
#' d <- rdeque()
#' d <- insert_back(d, "a")
#' d <- insert_back(d, "b")
#' print(d)
#' 
#' d2 <- insert_back(d, "c")
#' print(d2)
#' print(d)
insert_back <- function(d, e) {UseMethod("insert_back", d)}







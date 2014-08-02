#' @title
#' Create a new rstacknode with a given object
#' 
#' @description
#' For internal use by \code{rstack}s and \code{rdeque}s. An environment with no parent,
#' reference for the data and the next node.
#' @name rstacknode
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
#' @details An rstack supports efficient insert into the top with \code{insert_top} (returning a version of
#' the stack with the new element), \code{peek_top} (returing the data stored at the top of the stack), and
#' \code{without_top} (returning a version with the top element removed). 
#' 
#' Other handy functions
#' insclude \code{as.list} and \code{as.data.frame} (the latter of which requires that
#' all elements can be appended to become rows of a data frame in a reasonable manner). Operations
#' are amortized O(1).
#' 
#' The \code{rstack} class also supports \code{rev} - this operation is O(N), and results in a copy. This 
#' means previous versions will retain their O(1) amortized nature (if we assume the cost of the reverse is charged
#' to the newly created stack), at the cost of memory usage. However, this means that if stacks
#' are used in a non-persistent way, e.g. \code{s <- rev(s)}, then the garbage collector is free to clean
#' up old versions of the data.
#' @name rstack
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


#' @title Default method for converting to an rstack
#' @seealso \code{as.rstack}
#' @rdname as.rstack
#' @name as.rstack
as.rstack.default <- function(x) {
  input <- x
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

#' @title Default method for checking emptyness of an rstack
#' @seealso \code{is_empty}
#' @rdname is_empty
#' @name is_empty
is_empty.rstack <- function(d) {
  if(length(d) < 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @title Default print method for an rstack
#' 
#' @description Prints the top of an rstack, up to the first 6 elements.
#' 
#' @name print
#' @param x The stack to print
#' @param ... Arguments to be passed to or from other methods (ignored)
#' @export
#' 
print.rstack <- function(x, ...) {
  s <- x
  cat(paste("An rstack with ", length(s), " elements. \n"))
  if(length(s) > 0) {
    if(length(s) > 6) {
      cat("Top of the stack:\n")
      str(as.list(head(s, 6)), comp.str = " ", no.list = T)
      cat("    ...")
    } else {
      str(as.list(head(s, length(s))), comp.str = "", no.list = T)
    }
  }
}


#' @title Convert an \code{rstack} to a \code{data.frame}.
#' 
#' @description Converts the elements of an \code{rstack} into rows of a dataframe, if this is reasonable.
#' 
#' @details This method runs in O(N) time, and will only work if all elements of the stack have the
#' same length() (e.g., same number of columns), and if any of the elements have names, then those
#' names do not conflict (e.g., same column names where used). This is accomplished by a call to
#' \code{do.call("rbind", as.list(s))}, where \code{as.list(s)} converts the stack \code{s} to a list
#' where the top element becomes the first element of the list.
#' @param x The stack to convert
#' @param row.names Passed on to as.data.frame before final conversion
#' @param optional Passed onto as.data.frame before final conversion
#' @param ... Passed onto as.data.frame before final conversion
#' @return A data frame with the first row the previous top of the stack.
#' @export
#' @name as.data.frame
#' @examples 
#' s <- rstack()
#' s <- insert_top(s, data.frame(names = c("Bob", "Joe"), ages = c(25, 18)))
#' s <- insert_top(s, data.frame(names = c("Mary", "Kate", "Ashley"), ages = c(27, 26, 21)))
#' print(s)
#' 
#' sd <- as.data.frame(s)
#' print(sd)
#' 
#' 
#' ## Building a stack in a loop, converting to a dataframe after the fact:
#' require("dplyr")
#' s <- rstack()
#' for(i in 1:10000) {
#'  if(runif(1,0,1) < 0.5) {
#'    s <- s %>% insert_top(data.frame(i = i, type = "sqrt", val = sqrt(i)))
#'  } else {
#'    s <- s %>% insert_top(data.frame(i = i, type = "log", val = log(i)))
#'  }
#'  if(i %% 100 == 0) {
#'    print(i/10000)
#'  }
#' }
#' print(head(as.data.frame(s)))
as.data.frame.rstack <- function(x, row.names = NULL, optional = FALSE, ...) {
  dlist <- as.list(x)
  uniquelens <- unique(lapply(dlist, length))
  if(length(uniquelens) > 1) {
    stop("Sorry, can't convert an rstack to a data frame unless all elements have the same length().")
  }
  uniquenamesets <- unique(lapply(dlist, names))
  if(length(uniquenamesets) > 1) {
    stop("Sorry, can't convert an rstack to a data frame when elements have contradictory names().")
  }
  return(as.data.frame(do.call(rbind, dlist), row.names, optional, ...))
}

#' @title Default method for \code{insert_top} for rstack.
#' @seealso \code{insert_top}
#' @rdname insert_top
#' @name insert_top
insert_top.rstack <- function(s, e) {
  newnode <- rstacknode(e)
  newstack <- rstack()
  newstack$len <- s$len + 1
  newnode$nextnode <- s$head
  newstack$head <- newnode
  return(newstack)
}


#' @title Default method for \code{peek_top} for rstacks.
#' @seealso \code{peek_top}
#' @rdname peek_top
#' @name peek_top
peek_top.rstack <- function(s) {
  if(is.null(s$head)) {
    stop("Sorry, you can't peek at the top of an empty stack. Try checking with empty() first.")
  } else {
    return(s$head$data)
  }
}

#' @title Default method for modifying the top of an rstack.
#' @seealso \code{peek_top<-}
#' @rdname peek_top<-
#' @name peek_top<-
`peek_top<-.rstack` <- function(s, value) {
  if(length(s) < 1) {
    stop("Sorry, you can't assign to the top of an empty stack. Try checking with empty() first.")
  }
  s$head$data <- value
  return(s)
}

#' @title Default method for \code{length} on an rstack.
#' 
#' @description Returns the number of elements in an \code{rstack}.
#' 
#' @details O(1) time, as this information is stored seperately and updated on insert/remove.
#' @param x The stack to get the length of
#' @return A vector of length 1, which the number of elements of the stack.
#' @export
#' @name length
#' @examples
#' s <- rstack()
#' s <- insert_top(s, "a")
#' print(length(s))         # 1
#' s <- insert_top(s, "b")
#' print(length(s))         # 2
length.rstack <- function(x) {
  return(x$len)
}


#' @title Conversion of an \code{rstack} to a \code{list}
#' 
#' @description Converts an \code{rstack} to a list, where the top of the stack becomes
#' the first element of the list, the second-from-top the second, and so on. 
#' 
#' @details O(N), but the generated list is pre-allocated for efficiency.
#' @param x The stack to convert
#' @param ... Additional arguments passed to as.list after initial conversion to list.
#' @return A list
#' @name as.list
#' @export
#' @examples
#' s <- rstack()
#' s <- insert_top(s, "a")
#' s <- insert_top(s, "b")
#' 
#' slist <- as.list(s)
#' print(slist)
as.list.rstack <- function(x, ...) {
  retlist <- vector("list", x$len)
  node <- x$head
  index <- 1
  while(!is.null(node)) {
    retlist[[index]] <- node$data
    node <- node$nextnode
    index <- index + 1
  }
  return(as.list(retlist, ...))
}

#' @title Reverse an \code{rstack}
#' 
#' @description Returns a reversed version of an \code{rstack}, where the old last element (generally
#' inaccessible) is now the top.
#' 
#' @details This method is O(N), though it works behind-the-scenes by converting the input stack
#' to a list, reversing the list, and building the result as a new rstack. The original is thus
#' left alone, preserving O(1) amortized time for the original (assuming the "cost" of reversing
#' is charged to the newly created stack) at the cost of additional memory usage. Of course, 
#' if the stack is not being used in a preserved manner, e.g. \code{s <- rev(s)}, the garbage collector
#' will be free to clean up the original data if it is no longer usable.
#' @param x The stack to reverse
#' @return A reversed version fo the stack
#' @name rev
#' @export
#' @examples
#' s <- rstack()
#' s <- insert_top(s, "a")
#' s <- insert_top(s, "b")
#' s <- insert_top(s, "c")
#' 
#' r <- rev(s)
#' print(r)
#' print(s)
rev.rstack <- function(x) {
  saslist <- as.list(x)
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



#' @title Default method for \code{rstack} \code{without_top}
#' @seealso \code{without_top}
#' @rdname without_top
#' @name without_top
without_top.rstack <- function(s, n = 1) {
  if(length(s) < 1) {
    stop("Cannot run without_top() on an empty rstack. Check with is_empty() first.")
  } 
  newstack <- rstack()
  node <- s$head
  for(i in seq(1,n)) {
    node <- node$nextnode
  }
  newstack$len <- s$len - n
  newstack$head <- node
  return(newstack)
}



#' @title Return the top n elements of a stack as a stack
#' 
#' @description Returns the top n elements of a stack as a stack, or all of the elements if its length is less than n.
#' 
#' @details Runs in O(n) time (in the size of the number of elements requested)
#' @param x The stack to get the head of
#' @param ... Arguments to be passed to or from other methods (ignored)
#' @param n The number of elements to get
#' @export
#' @name head
#' @examples 
#' s <- rstack()
#' s <- insert_top(s, "a")
#' s <- insert_top(s, "b")
#' s <- insert_top(s, "c")
#' 
#' st <- head(s, n = 2)
#' print(st)
head.rstack <- function(x, n = 6L, ...) {
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


#' @title Create an \code{rstack} from a given input
#' 
#' @description Creates a new \code{rstack} from a given input. Coerces input to a 
#' list first using \code{as.list}, the elements of which become elements of the stack, and the
#' first element becoming the top of the stack.
#'
#' @details O(N) in the size of the input. Note that because data frames return a list of 
#' columns when run through \code{as.list}, running \code{as.rstack} results in a stack of
#' columns, rather than a stack of rows.
#' @param x Input to convert to a stack
#' @return A new rstack
#' @name as.rstack
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
#' @name insert_top
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
#' @name is_empty
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
#' @name peek_top
#' @examples
#' s <- rstack()
#' s <- insert_top(s, "a")
#' s <- insert_top(s, "b")
#' e <- peek_top(s)
#' print(e)
#' print(s)
#' 
#' ## Assigning to the top data element with peek_top:
#' s <- rstack()
#' s <- insert_top(data.frame(a = 1, b = 1))
#' s <- insert_top(data.frame(a = 1, b = 1))
#' 
#' peek_top(s)$a <- 100
#' print(s)
#' 
#' peek_top(s) <- data.frame(a = 100, b = 100)
peek_top <- function(s) { UseMethod("peek_top", s) }

#' @title Assign to/modify the top of a stack.
#' 
#' @description Allows modification access to the top of a stack.
#' 
#' @details O(1) worst case time. Throws an error if the stack is empty.
#' @param s The stack to modify the first element of
#' @param value The value to assign to the top data element.
#' @name peek_top<-
#' @examples
#' s <- rstack()
#' s <- insert_top(data.frame(a = 1, b = 1))
#' s <- insert_top(data.frame(a = 1, b = 1))
#' 
#' peek_top(s)$a <- 100
#' print(s)
#' 
#' peek_top(s) <- data.frame(a = 100, b = 100)
`peek_top<-` <- function(s, value) { UseMethod("peek_top<-", s) }


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
#' @return A version of the stack with \code{n} elements removed.
#' @name without_top
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




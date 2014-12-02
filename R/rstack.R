#' A class providing an O(1) worst-case time for stacks. Exported functions:
#' 
#' rstack()
#' as.rstack(input)
#' empty(s)
#' print(s)
#' as.data.frame(s)
#' peek_top(s)
#' length(s)
#' insert_top(s, element)
#' without_top(s)
#' as.list(s)
#' rev(s)
#' head(s, n = 6)



#' @title
#' Create a new rstacknode from a given object.
#' 
#' @description
#' For use by rstacks and rdeques. Simply an environment with no parent,
#' reference for the data and the next node.
#' @param data data to reference with this node.
#' @return an environment.
#' 
rstacknode <- function(data) {
  newnode <- new.env(parent = emptyenv())
  newnode$data <- data
  newnode$nextnode <- NULL
  class(newnode) <- "rstacknode"
  return(newnode)
}


#' @title Create a new empty rstack.
#' 
#' @description Creates a new, empty, rstack ready for use.
#' 
#' @details An rstack supports efficient insert into the top with insert_top (returning a version of
#' the stack with the new element), peek_top (returing the data stored at the top of the stack), and
#' without_top (returning a version with the top element removed). 
#' 
#' Other handy functions
#' insclude \code{as.list} and \code{as.data.frame} (the latter of which requires that
#' all elements can be appended to become rows of a data frame in a reasonable manner). Operations
#' are amortized O(1).
#' 
#' The rstack class also supports rev - this operation is O(N), and results in a copy. This 
#' means previous versions will retain their O(1) amortized nature (if we assume the cost of the reverse is charged
#' to the newly created stack), at the cost of memory usage. However, this means that if stacks
#' are used in a non-persistent way, e.g. \code{s <- rev(s)}, then the garbage collector is free to clean
#' up old versions of the data.
#' @param ... additional arguments to be passed to or from methods.
#' @return an empty rstack.
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
#' @export
rstack <- function(...) {
  s <- new.env(parent = emptyenv())
  s$head <- NULL
  s$len <- 0
  class(s) <- "rstack"
  return(s)
}


#' @export
as.rstack.default <- function(x, ...) {
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

#' @export
empty.rstack <- function(x, ...) {
  if(length(x) < 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @export
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


#' @title Convert an rstack to a data.frame.
#' 
#' @description Converts the elements of an rstack into rows of a dataframe, if this is reasonable.
#' 
#' @details This method runs in O(N) time, and will only work if all elements of the stack have the
#' same length() (e.g., same number of columns), and if any of the elements have names, then those
#' names do not conflict (e.g., same column names where used). This is accomplished by a call to
#' \code{do.call("rbind", as.list(s))}, where \code{as.list(s)} converts the stack s to a list
#' where the top element becomes the first element of the list.
#' @param x rstack to convert.
#' @param row.names passed on to as.data.frame before final conversion.
#' @param optional passed onto as.data.frame before final conversion.
#' @param ... Passed onto as.data.frame before final conversion.
#' @return a data frame with the first row the previous top of the stack.
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
#' s <- rstack()
#' for(i in 1:1000) {
#'  if(runif(1,0,1) < 0.5) {
#'    s <- insert_top(s, data.frame(i = i, type = "sqrt", val = sqrt(i)))
#'  } else {
#'    s <- insert_top(s, data.frame(i = i, type = "log", val = log(i)))
#'  }
#'  if(i %% 100 == 0) {
#'    print(i/1000)
#'  }
#' }
#' print(head(as.data.frame(s)))
#' @export
as.data.frame.rstack <- function(x, row.names = NULL, optional = FALSE, ...) {
  dlist <- lapply(as.list(x), as.data.frame, optional = optional, ...)
  uniquelens <- unique(lapply(dlist, length))
  if(length(uniquelens) > 1) {
    stop("cannot convert an rstack to a data.frame unless all elements have the same length()")
  }
  uniquenamesets <- unique(lapply(dlist, names))
  if(length(uniquenamesets) > 1) {
    stop("cannot convert an rstack to a data.frame when elements have contradictory names()")
  }
  return(as.data.frame(do.call(rbind, dlist), row.names, optional, ...))
}



#' @export
insert_top.rstack <- function(s, e, ...) {
  newnode <- rstacknode(e)
  newstack <- rstack()
  newstack$len <- s$len + 1
  newnode$nextnode <- s$head
  newstack$head <- newnode
  return(newstack)
}


#' @export
peek_top.rstack <- function(s, ...) {
  if(is.null(s$head)) {
    stop("cannot peek() at the top of an empty stack, try checking with empty() first")
  } else {
    return(s$head$data)
  }
}

#' @export
`peek_top<-.rstack` <- function(s, ..., value) {
  if(length(s) < 1) {
    stop("cannot assign to the top of an empty stack, try checking with empty() first")
  }
  s$head$data <- value
  return(s)
}

#' @title Default method for length on an rstack.
#' 
#' @description Returns the number of elements in an rstack.
#' 
#' @details O(1) time, as this information is stored seperately and updated on insert/remove.
#' @param x rstack to get the length of.
#' @return a vector of length 1, which the number of elements of the stack.
#' @examples
#' s <- rstack()
#' s <- insert_top(s, "a")
#' print(length(s))         # 1
#' s <- insert_top(s, "b")
#' print(length(s))         # 2
#' @export
length.rstack <- function(x) {
  return(x$len)
}


#' @title Convert an rstack to a list.
#' 
#' @description Converts an rstack to a list, where the top of the stack becomes
#' the first element of the list, the second-from-top the second, and so on. 
#' 
#' @details O(N), but the generated list is pre-allocated for efficiency.
#' @param x rstack to convert.
#' @param ... additional arguments passed to as.list after initial conversion to list.
#' @return a list.
#' @examples
#' s <- rstack()
#' s <- insert_top(s, "a")
#' s <- insert_top(s, "b")
#' 
#' slist <- as.list(s)
#' print(slist)
#' @export
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

#' @title Reverse an rstack.
#' 
#' @description Returns a reversed version of an rstack, where the old last element (generally
#' inaccessible) is now the top.
#' 
#' @details This method is O(N), though it works behind-the-scenes by converting the input stack
#' to a list, reversing the list, and building the result as a new rstack. The original is thus
#' left alone, preserving O(1) amortized time for the original (assuming the "cost" of reversing
#' is charged to the newly created stack) at the cost of additional memory usage. Of course, 
#' if the stack is not being used in a preserved manner, e.g. \code{s <- rev(s)}, the garbage collector
#' will be free to clean up the original data if it is no longer usable.
#' @param x rstack to reverse.
#' @return a reversed version of the stack.
#' @examples
#' s <- rstack()
#' s <- insert_top(s, "a")
#' s <- insert_top(s, "b")
#' s <- insert_top(s, "c")
#' 
#' r <- rev(s)
#' print(r)
#' print(s)
#' @export
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



#' @export
without_top.rstack <- function(s, n = 1, ...) {
  if(length(s) < 1) {
    stop("cannot run without_top() on an empty rstack, check with empty() first")
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



#' @title Return the top n elements of an rstack as an rstack.
#' 
#' @description Returns the top n elements of a stack as a stack, or all of the elements if its length is less than n.
#' 
#' @details Runs in O(n) time (in the size of the number of elements requested)
#' @param x rstack to get the head of.
#' @param ... arguments to be passed to or from other methods (ignored).
#' @param n number of elements to get.
#' @return a smaller rstack, with only the top elements kept.
#' @examples 
#' s <- rstack()
#' s <- insert_top(s, "a")
#' s <- insert_top(s, "b")
#' s <- insert_top(s, "c")
#' 
#' st <- head(s, n = 2)
#' print(st)
#' @export
head.rstack <- function(x, n = 6L, ...) {
  newstack <- rstack()
  if(n < 0) {
    n = max(n, -1*length(x))
    n = length(x) + n
  } 
  if(n > length(x)) {
    n = length(x)
  } 
  if(n == 0 | n < -1*length(x)) {
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


#' @title Create an rstack pre-filled from a given input.
#' 
#' @description Creates a new rstack from a given input. Coerces input to a 
#' list first using \code{as.list}, the elements of which become elements of the stack, and the
#' first element becoming the top of the stack.
#'
#' @details O(N) in the size of the input. Note that because data frames return a list of 
#' columns when run through \code{as.list}, running \code{as.rstack} results in a stack of
#' columns, rather than a stack of rows.
#' @param x input to convert to a stack.
#' @param ... additional arguments to be passed to or from methods.
#' @return a new rstack.
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
#' @export
as.rstack <- function(x, ...) {UseMethod("as.rstack", x)}


#' @title Insert an element into the top of an rstack.
#' 
#' @description Returns a version of the stack with the new element in the top position.
#' 
#' @details O(1) time worst-case. Does not modify the original stack.
#' @param s rstack to insert onto.
#' @param e element to insert.
#' @param ... additional arguments to be passed to or from methods.
#' @return modified version of the stack.
#' @examples
#' s <- rstack()
#' s <- insert_top(s, "a")
#' s <- insert_top(s, "b")
#' print(s)
#' 
#' s2 <- insert_top(s, "c")
#' print(s2)
#' print(s)
#' @export
insert_top <- function(s, e, ...) { UseMethod("insert_top", s) }







#' @title Check if an rstack, rdeque, or rpqueue is empty.
#' 
#' @description Returns TRUE if the structure has length 0, FALSE otherwise.
#' 
#' @param x rstack, rdeque, or rpqueue to check.
#' @param ... additional arguments to be passed to or from methods.
#' @return 1-element logical vector.
#' @examples
#' s <- rstack()
#' print(empty(s))        ## TRUE
#' s <- insert_top(s, "a")
#' print(empty(s))        ## FALSE
#' @export
empty <- function(x, ...) {UseMethod("empty", x)}





#' @title Return the data element at the top of an rstack.
#' 
#' @description Simply returns the data element sitting at the top of the stack,
#' leaving the stack alone.
#' 
#' @details O(1) worst-case time.
#' @param s stack to look at.
#' @param ... additional arguments to be passed to or from methods.
#' @return data element existing at the top of the given stack.
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
#' s <- insert_top(s, data.frame(a = 1, b = 1))
#' s <- insert_top(s, data.frame(a = 1, b = 1))
#' 
#' peek_top(s)$a <- 100
#' print(s)
#' 
#' peek_top(s) <- data.frame(a = 100, b = 100)
#' @export
peek_top <- function(s, ...) { UseMethod("peek_top", s) }



#' @title Assign to/modify the top of an rstack.
#' 
#' @description Allows modification access to the top of a stack.
#' 
#' @details O(1) worst case time. Throws an error if the stack is empty.
#' @param s rstack to modify the first element of.
#' @param value value to assign to the top data element.
#' @param ... additional arguments to be passed to or from methods.
#' @return modified rstack.
#' @examples
#' s <- rstack()
#' s <- insert_top(s, data.frame(a = 1, b = 1))
#' s <- insert_top(s, data.frame(a = 1, b = 1))
#' 
#' peek_top(s)$a <- 100
#' print(s)
#' 
#' peek_top(s) <- data.frame(a = 100, b = 100)
#' @export
`peek_top<-` <- function(s, ..., value) { UseMethod("peek_top<-", s) }


#' @title Return a version of an rstack without the top element.
#' 
#' @details Simply returns a version of the given stack without the top n elements
#' (n = 1 by default). The original stack is left alone.
#' 
#' @details O(n) time worst case (in the number of elements removed). If the
#' number of elements removed would exceed the size of the stack, an empty stack is returned.
#' 
#' @param s rstack to remove elements from.
#' @param n number of elements to remove.
#' @param ... additional arguments to be passed to or from methods.
#' @return version of the stack with n elements removed.
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
#' @export
without_top <- function(s, n, ...) { UseMethod("without_top", s) }






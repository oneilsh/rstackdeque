
### Invariant: if there is at least 2 elements, then both sides are nonempty.
### Rebalancing happens when there are more than 2 elements, and one of the sides
### falls below 6 elements (so very small deques might be rebalanced often, but this
### means we can always peek at the top and bottom 6 elements, or whatever is there)

## Todo: peek_front and peek_back need to work even if there are fewer than 2 elements

## Returns a new, empty fdeqeue object
## O(1)
rdeque <- function() {
  d <- new.env(parent = emptyenv())
  d$l <- rstack()
  d$r <- rstack()
  class(d) <- "rdeque"
  return(d)
}

## O(1)
is_empty.rdeque <- function(d) {
  if(length(d) < 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

## converts it's input into a list, then into a deque with the first element at the front. 
## O(n)
as.rdeque.default <- function(input) {
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

## O(1)
length.rdeque <- function(d) {
  return(d$l$len + d$r$len)
}


## O(N)
as.list.rdeque <- function(d) {
  return(c(as.list(d$l), rev(as.list(d$r))))
}

## Currently O(1), maybe O(N) in the future if we want to see more than 
## the first 5 elements.
print.rdeque <- function(d) {
  cat(paste("A deque with ", length(d), " elements.\n"))
  cat(paste("Front to back: \n"))
  str(as.list(head(d$l, 6)), comp.str = "$", no.list = T)
  cat("     ...\n")
  str(rev(as.list(head(d$r, 6))), comp.str = "$", no.list = T)
}

## O(n). Requires all elements to be the same length, and if any are named to have the same
## names. Stops otherwise.
as.data.frame.rdeque <- function(d) {
  dlist <- as.list(d)
  uniquelens <- unique(lapply(dlist, length))
  if(length(uniquelens) > 1) {
    stop("Sorry, can't convert an rdeque to a data frame unless all elements have the same length().")
  }
  uniquenamesets <- unique(lapply(dlist, names))
  if(length(uniquenamesets) > 1) {
    stop("Sorry, can't convert an rdeque to a data frame when elements have contradictory names().")
  }
  return(do.call(rbind, dlist))
}

## Maintains the invariant: that both l and r are nonempty whenever the deque is nonempty.
## If the size of d is 2 or greater, and one of the sides is empty returns a balanced deque. 
## O(n), "optimized" using R lists. 
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


## O(n), amortized. Occasionally O(N) if fixing required.
without_front.rdeque <- function(d, n = 1) {
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

## O(n), amortized. Occasionally O(N) if fixing required.
without_back.rdeque <- function(d, n = 1) {
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

## O(1)
peek_front.rdeque <- function(d) {
  return(peek_top(d$l))
}

## O(1)
peek_back.rdeque <- function(d) {
  return(peek_top(d$r))
}


## O(1)
insert_front.rdeque <- function(d, el) {
  newd <- rdeque()
  newd$l <- insert_top(d$l, el)
  newd$r <- d$r
  newd <- fix(newd)
  return(newd)
}


## O(1)
insert_back.rdeque <- function(d, el) {
  newd <- rdeque()
  newd$r <- insert_top(d$r, el)
  newd$l <- d$l
  newd <- fix(newd)
  return(newd)
}


fix <- function(d) {UseMethod("fix", d)}
as.rdeque <- function(x) {UseMethod("as.rdeque", d)}
is_empty <- function(d) {UseMethod("is_empty", d)}
without_front <- function(d) {UseMethod("without_front", d)}
without_back <- function(d) {UseMethod("without_back", d)}
peek_front <- function(d) {UseMethod("peek_front", d)}
peek_back <- function(d) {UseMethod("peek_back", d)}
insert_front <- function(d, x) {UseMethod("insert_front", d)}
insert_back <- function(d, x) {UseMethod("insert_back", d)}



if(F) {
library(rstackdeque)

mq <- rdeque()
fq <- rdeque()

for(i in seq(1,10)) {
  # someone arrives with 10% probability
  if(runif(1,0,1) < 0.1) {
    if(runif(1,0,1 < 0.5)) {
      ## ladies take at least 10 seconds, mean 79 seconds
      lady <- data.frame(arrivaltime = i, waittime = max(as.integer(rnorm(1,79,20)), 10), fronttime = 1)
      fq <- insert_back(fq, lady)
    } else {
      ## men take at least 10 seconds, mean 45 secons
      gentleman <- data.frame(arrivaltime = i, waittime = max(as.integer(rnorm(1,45,10)), 10), fronttime = 1)
      mq <- insert_back(mq, gentleman)
    }
  }
  
  ## remove anyone having waited at the front the queue for their waittime (they did their business)
  lady <- peek_front(fq)
  #if(i >= lady$fronttime + lady$waittime) {
  #  fq <- remove_front(fq)
  #  peek_front(fq)
  #}
}

library(rstackdeque)
test <- rstack()
test <- insert_top(test, data.frame(a = 1, b = 1))
test <- insert_top(test, data.frame(a = 1, b = 1))
print(test)
peek_top(test) <- 100
print(test)

}
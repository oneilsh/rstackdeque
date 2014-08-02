if(F) {
rm(list = ls())
library(rstackdeque)

library(ggplot2)


quicksort <- function(x, depth) {
  depth <- depth + 1
  if(length(x) < 2) {return(x)}  
  
  pivot <- x[1]
  lt <- x[x < pivot]
  eq <- x[x == pivot]
  gt <- x[x > pivot]
  iter <- length(history)
  if(length(lt) > 0) {history <<- insert_top(history, data.frame(iter = iter, depth = depth, pos = sort(lt), vals = lt, type = "presorted", side = "left"))}
  if(length(eq) > 0) {history <<- insert_top(history, data.frame(iter = iter, depth = depth, pos = sort(eq), vals = eq, type = "presorted", side = "equal")) }
  if(length(gt) > 0) {history <<- insert_top(history, data.frame(iter = iter, depth = depth, pos = sort(gt), vals = gt, type = "presorted", side = "right")) }
  
  lts <- quicksort(lt, depth)
  gts <- quicksort(gt, depth)
  
  ret <- c(lts, eq, gts)
  return(ret)
}


test <- sample(1:500, 500)
print(test)

history <- rstack()
history <- insert_top(history, data.frame(iter = 0, depth = 0, pos = sort(test), vals = test, type = "presorted", side = "equal"))

res <- quicksort(test, 0)
print(res)

history_df <- as.data.frame(history)
history_df$side <- factor(history_df$side, levels = c("left", "equal", "right"), labels = c("Left", "Presorted", "Right"))

p <- ggplot(history_df) +
  geom_tile(aes(x = pos, y = depth, fill = vals, color = side)) +
  theme_bw(18) +
  scale_fill_continuous(name = "Element Value") +
  scale_color_discrete(name = "Recursion Side") +
  scale_x_continuous(name = "Element Position") +
  scale_y_reverse(name = "Recursion Depth")
plot(p)

}
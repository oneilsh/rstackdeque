
test_that("insert and remove work as expected", {
  s <- rstack()
  s <- insert_top(s, "c")
  s <- insert_top(s, "b")
  s <- insert_top(s, "a")
  expect_that(length(s), equals(3))
  expect_that(as.list(s), is_identical_to(as.list(c("a", "b", "c"))) )
  
  ## drop one
  sp <- without_top(s)
  expect_that(length(sp), equals(2))
  expect_that(as.list(sp), is_identical_to(as.list(c("b", "c"))) )  
  
  ## s still ok?
  expect_that(length(s), equals(3))
  expect_that(as.list(s), is_identical_to(as.list(c("a", "b", "c"))) )
  
  ## add one
  se <- insert_top(s, "aa")
  expect_that(length(se), equals(4))
  expect_that(as.list(se), is_identical_to(as.list(c("aa", "a", "b", "c"))) )  
  
  ## s still ok?
  expect_that(length(s), equals(3))
  expect_that(as.list(s), is_identical_to(as.list(c("a", "b", "c"))) )
})
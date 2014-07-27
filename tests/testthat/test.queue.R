
test_that("insert_front and without_front work as expected", {
  s <- rdeque()
  s <- insert_front(s, "c")
  s <- insert_front(s, "b")
  s <- insert_front(s, "a")
  expect_that(length(s), equals(3))
  expect_that(as.list(s), is_identical_to(as.list(c("a", "b", "c"))) )
  
  ## drop one
  sp <- without_front(s)
  expect_that(length(sp), equals(2))
  expect_that(as.list(sp), is_identical_to(as.list(c("b", "c"))) )  
  
  ## s still ok?
  expect_that(length(s), equals(3))
  expect_that(as.list(s), is_identical_to(as.list(c("a", "b", "c"))) )
  
  ## add one
  se <- insert_front(s, "aa")
  expect_that(length(se), equals(4))
  expect_that(as.list(se), is_identical_to(as.list(c("aa", "a", "b", "c"))) )  
  
  ## s still ok?
  expect_that(length(s), equals(3))
  expect_that(as.list(s), is_identical_to(as.list(c("a", "b", "c"))) )
})



test_that("insert_back and without_back work as expected", {
  s <- rdeque()
  s <- insert_back(s, "c")
  s <- insert_back(s, "b")
  s <- insert_back(s, "a")
  expect_that(length(s), equals(3))
  expect_that(as.list(s), is_identical_to(as.list(c("c", "b", "a"))) )
  
  ## drop one
  sp <- without_back(s)
  expect_that(length(sp), equals(2))
  expect_that(as.list(sp), is_identical_to(as.list(c("c", "b"))) )  
  
  ## s still ok?
  expect_that(length(s), equals(3))
  expect_that(as.list(s), is_identical_to(as.list(c("c", "b", "a"))) )
  
  ## add one
  se <- insert_back(s, "aa")
  expect_that(length(se), equals(4))
  expect_that(as.list(se), is_identical_to(as.list(c("c", "b", "a", "aa"))) )  
  
  ## s still ok?
  expect_that(length(s), equals(3))
  expect_that(as.list(s), is_identical_to(as.list(c("c", "b", "a"))) )
})

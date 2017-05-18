####### First bit #######

library(testthat)

## use expect_output from testthat

fizzbuzz <- function(number) {
  return(number)
}

expect_equal(fizzbuzz(1), 1)
expect_equal(fizzbuzz(2), 2)
expect_match(fizzbuzz(3), "fizz") # failing test

fizzbuzz2 <- function(number) {
  test <- if (number%%3 == 0) "fizz" else number
  return(test)
}

expect_equal(fizzbuzz2(1), 1)
expect_equal(fizzbuzz2(2), 2)
expect_match(fizzbuzz2(3), "fizz")
expect_equal(fizzbuzz2(4), 4)
expect_match(fizzbuzz2(5), "buzz") # failing test

fizzbuzz3 <- function(number) {
  test <- if (number%%3 == 0) {
    "fizz"
  } else if (number%%5 == 0){
    "buzz"
  } else number
  
  return(test)
}

expect_equal(fizzbuzz3(1), 1)
expect_equal(fizzbuzz3(2), 2)
expect_match(fizzbuzz3(3), "fizz")
expect_equal(fizzbuzz3(4), 4)
expect_match(fizzbuzz3(5), "buzz")
expect_match(fizzbuzz3(15), "fizzbuzz") # failing test

fizzbuzz4 <- function(number) {
  test <- if (number%%3 == 0 && number%%5 == 0){
    "fizzbuzz"
    } else if (number%%3 == 0) {
    "fizz"
  } else if (number%%5 == 0){
    "buzz"
  } else number
  
  return(test)
}

expect_equal(fizzbuzz4(1), 1)
expect_equal(fizzbuzz4(2), 2)
expect_match(fizzbuzz4(3), "fizz")
expect_equal(fizzbuzz4(4), 4)
expect_match(fizzbuzz4(5), "buzz")
expect_match(fizzbuzz4(15), "fizzbuzz")

manyNumbersInARow <- c(1:20)

fizzRowResult <- sapply(manyNumbersInARow, fizzbuzz4)

print(fizzRowResult)


####### Second bit #######

# Pretend you have a data set with a bunch of 'random' numbers which
#  you want to do fizzbuzz on in a column:

fizzbuzz5 <- function(number) {
  test <- if (number%%3 == 0 && number%%5 == 0){
    "fizzbuzz"
  } else if (number%%3 == 0) {
    "fizz"
  } else if (number%%5 == 0){
    "buzz"
  } else "no buzz :("
  
  return(test)
}

data <- data.frame(numbers = c(sample.int(1000, 50)))

# apply fizzbuzz
any((data$numbers %% 3 == 0) & (data$numbers %% 5 == 0))

data$fizzbuzz <- NA

data$fizzbuzz <- sapply(data$numbers, fizzbuzz5)

table(data$fizzbuzz, useNA=c("always"))


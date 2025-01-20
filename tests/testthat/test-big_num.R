rmpfr_test_multiplication <- function(string_num_1, string_num_2) {
  # using Rmpfr to verify big_num's accuracy
  num <- Rmpfr::mpfr(string_num_1, 200) * Rmpfr::mpfr(string_num_2, 200)
  string_num_solution <- as(num, "character")

  expect_equal(big_num(string_num_1) * big_num(string_num_2), big_num(string_num_solution))
}


test_that("multiplication commutes", {
  expect_equal((big_num(2) * big_num(2)), big_num(4))

  expect_equal(big_num(2) * big_num(5), big_num(10))
  expect_equal(big_num(5) * big_num(2), big_num(10))

  expect_equal(big_num(148) * big_num(997), big_num(147556))
  expect_equal(big_num(997) * big_num(148), big_num(147556))

  rmpfr_test_multiplication("11111111", "122333444455555")
  rmpfr_test_multiplication("122333444455555", "11111111")
})

test_that("multiplication big", {
  string_1 <- "321474836474"
  string_2 <- "46548999646551237776"
  string_3 <- "21471047128412996458887978163"

  rmpfr_test_multiplication(string_1, string_2)
  rmpfr_test_multiplication(string_3, string_2)
})


test_that("multiplication zero", {
  string_1 <- "321474836474"
  string_2 <- "46548999646551237776"
  string_3 <- "21471047128412996458887978163"

  rmpfr_test_multiplication(string_1, "0")
  rmpfr_test_multiplication(string_2, "0")
  rmpfr_test_multiplication(string_3, "0")
})


test_that("multiplication one", {
  string_1 <- "321474836474"
  string_2 <- "46548999646551237776"
  string_3 <- "21471047128412996458887978163"

  rmpfr_test_multiplication(string_1, "1")
  rmpfr_test_multiplication(string_2, "1")
  rmpfr_test_multiplication(string_3, "1")
})


test_that("exponentiation works", {
  expect_equal(big_num(2)^0, big_num(1))
  expect_equal(big_num(2)^1, big_num(2))
  expect_equal(big_num(2)^2, big_num(4))

  expect_equal(big_num(11)^20, big_num("61159090448414546291"))
})

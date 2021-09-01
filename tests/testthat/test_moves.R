test_that("single_sow_CW_south", {
  expect_equal(
    single_sow(
      "S",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      1,
      5,
      1
    ),
    list(
      N = matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      S = matrix(
        c(0, 0, 0, 0, 0, 3, 3, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      end_pit_r = 1,
      end_pit_c = 7,
      should_end = FALSE
    )
  )
  # check breaking points
  expect_equal(
    single_sow(
      "S",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      1,
      7,
      1
    ),
    list(
      N = matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      S = matrix(
        c(0, 0, 0, 0, 2, 2, 0, 3,
          2, 2, 2, 2, 2, 2, 2, 3),
        nrow = 2,
        byrow = TRUE
      ),
      end_pit_r = 2,
      end_pit_c = 8,
      should_end = FALSE
    )
  )
  expect_equal(
    single_sow(
      "S",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      pit_r = 2,
      pit_c = 1,
      1
    ),
    list(
      N = matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      S = matrix(
        c(1, 1, 0, 0, 2, 2, 2, 2,
          0, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      end_pit_r = 1,
      end_pit_c = 2,
      should_end = TRUE
    )
  )

  expect_equal(
    single_sow(
      "S",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      pit_r = 2,
      pit_c = 2,
      1
    ),
    list(
      N = matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      S = matrix(
        c(1, 0, 0, 0, 2, 2, 2, 2,
          3, 0, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      end_pit_r = 1,
      end_pit_c = 1,
      should_end = TRUE
    )
  )

  expect_equal(
    single_sow(
      "S",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      pit_r = 2,
      pit_c = 8,
      1
    ),
    list(
      N = matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      S = matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 3, 3, 0),
        nrow = 2,
        byrow = TRUE
      ),
      end_pit_r = 2,
      end_pit_c = 6,
      should_end = FALSE
    )
  )

})

test_that("single_sow_CCW_south", {
  expect_equal(
    single_sow(
      "S",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      1,
      5,
      0
    ),
    list(
      N = matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      S = matrix(
        c(0, 0, 1, 1, 0, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      end_pit_r = 1,
      end_pit_c = 3,
      should_end = TRUE
    )
  )
  # check breaking points
  expect_equal(
    single_sow(
      "S",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      2,
      7,
      0
    ),
    list(
      N = matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      S = matrix(
        c(0, 0, 0, 0, 2, 2, 2, 3,
          2, 2, 2, 2, 2, 2, 0, 3),
        nrow = 2,
        byrow = TRUE
      ),
      end_pit_r = 1,
      end_pit_c = 8,
      should_end = FALSE
    )
  )
  expect_equal(
    single_sow(
      "S",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(2, 0, 0, 0, 0, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      pit_r = 1,
      pit_c = 1,
      0
    ),
    list(
      N = matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      S = matrix(
        c(0, 0, 0, 0, 0, 2, 2, 2,
          3, 3, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      end_pit_r = 2,
      end_pit_c = 2,
      should_end = FALSE
    )
  )

  expect_equal(
    single_sow(
      "S",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 2, 0, 0, 2, 2, 2, 2,
          2, 0, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      pit_r = 1,
      pit_c = 2,
      0
    ),
    list(
      N = matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      S = matrix(
        c(1, 0, 0, 0, 2, 2, 2, 2,
          3, 0, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      end_pit_r = 2,
      end_pit_c = 1,
      should_end = FALSE
    )
  )

  expect_equal(
    single_sow(
      "S",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      pit_r = 2,
      pit_c = 1,
      0
    ),
    list(
      N = matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      S = matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          0, 3, 3, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      end_pit_r = 2,
      end_pit_c = 3,
      should_end = FALSE
    )
  )

})


test_that("single_sow_CCW_north", {
  expect_equal(
    single_sow(
      "N",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      1,
      5,
      0
    ),
    list(
      N = matrix(
        c(2, 2, 3, 3, 0, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      S =  matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      end_pit_r = 1,
      end_pit_c = 3,
      should_end = FALSE
    )
  )

  expect_equal(
    single_sow(
      "N",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      1,
      1,
      0
    ),
    list(
      N = matrix(
        c(0, 2, 2, 2, 2, 2, 2, 2,
          3, 3, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      S =  matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      end_pit_r = 2,
      end_pit_c = 2,
      should_end = FALSE
    )
  )

  expect_equal(
    single_sow(
      "N",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      2,
      3,
      0
    ),
    list(
      N = matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 0, 3, 1, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      S =  matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      end_pit_r = 2,
      end_pit_c = 5,
      should_end = TRUE
    )
  )
})

test_that("single_sow long sowings", {
  expect_equal(
    single_sow(
      "N",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 16, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      2,
      3,
      0
    ),
    list(
      N = matrix(
        c(3, 3, 3, 3, 3, 3, 3, 3,
          3, 3, 1, 3, 1, 1, 1, 1),
        nrow = 2,
        byrow = TRUE
      ),
      S =  matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      end_pit_r = 2,
      end_pit_c = 3,
      should_end = TRUE
    )
  )

  expect_equal(
    single_sow(
      "N",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 17, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      2,
      3,
      0
    ),
    list(
      N = matrix(
        c(3, 3, 3, 3, 3, 3, 3, 3,
          3, 3, 1, 4, 1, 1, 1, 1),
        nrow = 2,
        byrow = TRUE
      ),
      S =  matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      end_pit_r = 2,
      end_pit_c = 4,
      should_end = FALSE
    )
  )
})

test_that("capture rule test", {
  expect_equal(
    capture_rule(
      "N",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      2,
      5
    ),
    list(captured = TRUE,
      N = matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 4, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      S =  matrix(
        c(0, 0, 0, 0, 0, 2, 2, 2,
          2, 2, 2, 2, 0, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      stones_won = 4
    )
  )


  expect_equal(
    capture_rule(
      "N",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      1,
      5
    ),
    FALSE

  )

  expect_equal(
    capture_rule(
      "N",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      2,
      3
    ),
    FALSE

  )

  expect_equal(
    capture_rule(
      "S",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      2,
      3
    ),
    FALSE

  )

  expect_equal(
    capture_rule(
      "S",
      matrix(
        c(2, 2, 2, 2, 2, 2, 2, 2,
          2, 2, 2, 2, 0, 0, 0, 0),
        nrow = 2,
        byrow = TRUE
      ),
      matrix(
        c(0, 0, 0, 0, 2, 2, 2, 2,
          2, 2, 2, 2, 2, 2, 2, 2),
        nrow = 2,
        byrow = TRUE
      ),
      1,
      8
    ),
    FALSE

  )


})

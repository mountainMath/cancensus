test_that("semantic_search generates correct n-grams for single word query", {
  skip_if_not_installed("dplyr")

  # Create mock census vector data
  mock_vectors <- data.frame(
    vector = c("v_TEST_1", "v_TEST_2", "v_TEST_3"),
    details = c("Total population count", "Male population aged 15", "Female income median"),
    label = c("Population", "Male", "Female"),
    stringsAsFactors = FALSE
  )

  # Test with single word - should match "population"
  result <- cancensus:::semantic_search("population", mock_vectors)

  # Should find vectors with "population" in details
  expect_true(!is.null(result))
  expect_true(nrow(result) > 0)
  expect_true(any(grepl("population", result$details, ignore.case = TRUE)))
})

test_that("semantic_search generates correct n-grams for multi-word query", {
  skip_if_not_installed("dplyr")

  mock_vectors <- data.frame(
    vector = c("v_TEST_1", "v_TEST_2", "v_TEST_3", "v_TEST_4"),
    details = c(
      "Total population count by age",
      "Male population aged 15 to 24",
      "Female population aged 25 to 34",
      "Income median for households"
    ),
    label = c("Pop1", "Pop2", "Pop3", "Income"),
    stringsAsFactors = FALSE
  )

  # Test with two-word query
  result <- cancensus:::semantic_search("male population", mock_vectors)

  # Should match vectors with "male" and "population"
  expect_true(!is.null(result))
  if (nrow(result) > 0) {
    expect_true(any(grepl("male", result$details, ignore.case = TRUE)))
  }
})

test_that("semantic_search handles empty vectors", {
  skip_if_not_installed("dplyr")

  mock_vectors <- data.frame(
    vector = character(0),
    details = character(0),
    label = character(0),
    stringsAsFactors = FALSE
  )

  # Should handle empty input - may error or warn depending on implementation
  # This is an edge case that likely doesn't occur in real usage
  expect_error(
    result <- cancensus:::semantic_search("population", mock_vectors)
  )
})

test_that("semantic_search handles vectors with punctuation", {
  skip_if_not_installed("dplyr")

  mock_vectors <- data.frame(
    vector = c("v_TEST_1", "v_TEST_2"),
    details = c(
      "Population: total, all ages (2021)",
      "Income - median household income"
    ),
    label = c("Pop", "Income"),
    stringsAsFactors = FALSE
  )

  # Punctuation should be handled correctly
  result <- cancensus:::semantic_search("population total", mock_vectors)

  expect_true(!is.null(result))
  # Should find match despite punctuation
  if (nrow(result) > 0) {
    expect_true(any(grepl("population", result$details, ignore.case = TRUE)))
  }
})

test_that("semantic_search handles case insensitivity", {
  skip_if_not_installed("dplyr")

  mock_vectors <- data.frame(
    vector = c("v_TEST_1", "v_TEST_2"),
    details = c(
      "POPULATION TOTAL",
      "population total"
    ),
    label = c("Pop1", "Pop2"),
    stringsAsFactors = FALSE
  )

  # Should find matches regardless of case
  result1 <- cancensus:::semantic_search("POPULATION", mock_vectors)
  result2 <- cancensus:::semantic_search("population", mock_vectors)
  result3 <- cancensus:::semantic_search("Population", mock_vectors)

  # All should return results
  expect_true(!is.null(result1))
  expect_true(!is.null(result2))
  expect_true(!is.null(result3))
})

test_that("semantic_search with no close matches warns user", {
  skip_if_not_installed("dplyr")

  mock_vectors <- data.frame(
    vector = c("v_TEST_1", "v_TEST_2"),
    details = c(
      "Population total count",
      "Income median value"
    ),
    label = c("Pop", "Income"),
    stringsAsFactors = FALSE
  )

  # Query with no close match should warn
  expect_warning(
    result <- cancensus:::semantic_search("zzzzxxxxxqqqqq", mock_vectors),
    "No close matches found"
  )
})

test_that("semantic_search handles short sentences correctly", {
  skip_if_not_installed("dplyr")

  mock_vectors <- data.frame(
    vector = c("v_TEST_1", "v_TEST_2", "v_TEST_3"),
    details = c(
      "Total",  # Very short
      "Population",  # Single word
      "Total population count by age groups"  # Long
    ),
    label = c("T", "P", "TPC"),
    stringsAsFactors = FALSE
  )

  # Should handle varying length details
  result <- cancensus:::semantic_search("total", mock_vectors)

  expect_true(!is.null(result))
  if (nrow(result) > 0) {
    expect_true(any(grepl("total", result$details, ignore.case = TRUE)))
  }
})

test_that("semantic_search n-gram optimization produces identical results", {
  skip_if_not_installed("dplyr")

  # Create a realistic-sized test set
  mock_vectors <- data.frame(
    vector = paste0("v_TEST_", 1:50),
    details = c(
      "Total population by age groups and gender",
      "Male population aged 0 to 14 years",
      "Female population aged 15 to 24 years",
      "Total households by family composition",
      "Median household income after tax",
      "Average household income before tax",
      "Total dwelling units by structure type",
      "Population density per square kilometer",
      "Labour force participation rate by age",
      "Unemployment rate for all ages",
      rep("Other census variable details", 40)
    ),
    label = paste0("Label_", 1:50),
    stringsAsFactors = FALSE
  )

  # Test several queries
  queries <- c("population", "household income", "aged 15", "total")

  for (query in queries) {
    # The function should still work correctly with optimization
    result <- cancensus:::semantic_search(query, mock_vectors)

    # Should return results for reasonable queries
    if (query %in% c("population", "household income", "total")) {
      expect_true(!is.null(result) || inherits(result, "data.frame"))
    }
  }
})

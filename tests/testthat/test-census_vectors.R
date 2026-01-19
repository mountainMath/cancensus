test_that("parent_census_vectors returns all parent vectors", {
  skip_if_not_installed("dplyr")

  # Create simple 3-level hierarchy: v_TEST_3 -> v_TEST_2 -> v_TEST_1
  mock_vectors <- data.frame(
    vector = c("v_TEST_1", "v_TEST_2", "v_TEST_3"),
    parent_vector = c(NA, "v_TEST_1", "v_TEST_2"),
    label = c("Root", "Child", "Grandchild"),
    details = c("", "", ""),
    aggregation = rep("Additive", 3),
    type = rep("Total", 3),
    stringsAsFactors = FALSE
  )
  attr(mock_vectors, "dataset") <- "TEST"

  # Mock the list_census_vectors function temporarily
  with_mocked_bindings(
    list_census_vectors = function(dataset, use_cache = TRUE, quiet = TRUE) {
      mock_vectors
    },
    {
      # Test with grandchild - should return both parent and grandparent
      grandchild <- mock_vectors[3, , drop = FALSE]
      result <- parent_census_vectors(grandchild)

      # Should include v_TEST_2 and v_TEST_1 (all parents up the chain)
      expect_true("v_TEST_2" %in% result$vector, info = "Should include direct parent v_TEST_2")
      expect_true("v_TEST_1" %in% result$vector, info = "Should include grandparent v_TEST_1")
      expect_false("v_TEST_3" %in% result$vector, info = "Should not include itself")
      expect_equal(nrow(result), 2, info = "Should return exactly 2 parents")

      # Test with child - should return only the root
      child <- mock_vectors[2, , drop = FALSE]
      result2 <- parent_census_vectors(child)

      expect_true("v_TEST_1" %in% result2$vector, info = "Should include parent v_TEST_1")
      expect_false("v_TEST_2" %in% result2$vector, info = "Should not include itself")
      expect_equal(nrow(result2), 1, info = "Should return exactly 1 parent")
    }
  )
})

test_that("child_census_vectors works with mock data", {
  skip_if_not_installed("dplyr")

  # Create mock vector data structure
  mock_vectors <- data.frame(
    vector = c("v_TEST_1", "v_TEST_2", "v_TEST_3", "v_TEST_4", "v_TEST_5"),
    parent_vector = c(NA, "v_TEST_1", "v_TEST_1", "v_TEST_2", "v_TEST_3"),
    label = c("Root", "Child1", "Child2", "Grandchild1", "GreatGrandchild1"),
    details = c("", "", "", "", ""),
    aggregation = rep("Additive", 5),
    type = rep("Total", 5),
    stringsAsFactors = FALSE
  )
  attr(mock_vectors, "dataset") <- "TEST"

  with_mocked_bindings(
    list_census_vectors = function(dataset, use_cache = TRUE, quiet = TRUE) {
      mock_vectors
    },
    {
      # Test with root node - should return all children
      root_vector <- mock_vectors[1, , drop = FALSE]
      result <- child_census_vectors(root_vector)

      # Should include all descendants
      expect_true("v_TEST_2" %in% result$vector)
      expect_true("v_TEST_3" %in% result$vector)
      expect_true("v_TEST_4" %in% result$vector)
      expect_true("v_TEST_5" %in% result$vector)
      expect_false("v_TEST_1" %in% result$vector) # Should not include itself by default

      # Test max_level parameter
      result_level1 <- child_census_vectors(root_vector, max_level = 1)
      expect_true("v_TEST_2" %in% result_level1$vector)
      expect_true("v_TEST_3" %in% result_level1$vector)
      expect_false("v_TEST_4" %in% result_level1$vector) # Should not go deeper

      # Test keep_parent parameter
      result_with_parent <- child_census_vectors(root_vector, keep_parent = TRUE)
      expect_true("v_TEST_1" %in% result_with_parent$vector)

      # Test leaves_only parameter
      result_leaves <- child_census_vectors(root_vector, leaves_only = TRUE)
      # Only v_TEST_5 has no children
      expect_true("v_TEST_5" %in% result_leaves$vector)
      expect_false("v_TEST_2" %in% result_leaves$vector) # Has children
    }
  )
})

test_that("parent_census_vectors handles empty results", {
  skip_if_not_installed("dplyr")

  mock_vectors <- data.frame(
    vector = c("v_TEST_1"),
    parent_vector = c(NA),
    label = c("Root"),
    details = c(""),
    aggregation = "Additive",
    type = "Total",
    stringsAsFactors = FALSE
  )
  attr(mock_vectors, "dataset") <- "TEST"

  with_mocked_bindings(
    list_census_vectors = function(dataset, use_cache = TRUE, quiet = TRUE) {
      mock_vectors
    },
    {
      # Root node has no parent
      root_vector <- mock_vectors[1, , drop = FALSE]
      result <- parent_census_vectors(root_vector)

      # Should return empty data frame with correct structure
      expect_equal(nrow(result), 0)
      expect_true("vector" %in% names(result))
      expect_true("parent_vector" %in% names(result))
    }
  )
})

test_that("child_census_vectors handles empty results", {
  skip_if_not_installed("dplyr")

  mock_vectors <- data.frame(
    vector = c("v_TEST_1"),
    parent_vector = c(NA),
    label = c("Leaf"),
    details = c(""),
    aggregation = "Additive",
    type = "Total",
    stringsAsFactors = FALSE
  )
  attr(mock_vectors, "dataset") <- "TEST"

  with_mocked_bindings(
    list_census_vectors = function(dataset, use_cache = TRUE, quiet = TRUE) {
      mock_vectors
    },
    {
      # Leaf node has no children
      leaf_vector <- mock_vectors[1, , drop = FALSE]
      result <- child_census_vectors(leaf_vector)

      # Should return empty data frame with correct structure
      expect_equal(nrow(result), 0)
      expect_true("vector" %in% names(result))
      expect_true("parent_vector" %in% names(result))
    }
  )
})

test_that("parent_census_vectors handles character vector input", {
  skip_if_not_installed("dplyr")

  mock_vectors <- data.frame(
    vector = c("v_TEST_1", "v_TEST_2"),
    parent_vector = c(NA, "v_TEST_1"),
    label = c("Root", "Child"),
    details = c("", ""),
    aggregation = rep("Additive", 2),
    type = rep("Total", 2),
    stringsAsFactors = FALSE
  )
  attr(mock_vectors, "dataset") <- "TEST"

  with_mocked_bindings(
    list_census_vectors = function(dataset, use_cache = TRUE, quiet = TRUE) {
      mock_vectors
    },
    dataset_from_vector_list = function(x) {
      "TEST"
    },
    clean_vector_list = function(x, dataset = NULL) {
      if (is.character(x)) {
        mock_vectors[mock_vectors$vector %in% x, , drop = FALSE]
      } else {
        x
      }
    },
    {
      # Test with character vector
      result <- parent_census_vectors("v_TEST_2")

      # Should include v_TEST_1
      expect_true("v_TEST_1" %in% result$vector)
    }
  )
})

test_that("child_census_vectors handles character vector input", {
  skip_if_not_installed("dplyr")

  mock_vectors <- data.frame(
    vector = c("v_TEST_1", "v_TEST_2"),
    parent_vector = c(NA, "v_TEST_1"),
    label = c("Root", "Child"),
    details = c("", ""),
    aggregation = rep("Additive", 2),
    type = rep("Total", 2),
    stringsAsFactors = FALSE
  )
  attr(mock_vectors, "dataset") <- "TEST"

  with_mocked_bindings(
    list_census_vectors = function(dataset, use_cache = TRUE, quiet = TRUE) {
      mock_vectors
    },
    dataset_from_vector_list = function(x) {
      "TEST"
    },
    clean_vector_list = function(x) {
      if (is.character(x)) {
        result <- mock_vectors[mock_vectors$vector %in% x, , drop = FALSE]
        attr(result, "dataset") <- "TEST"
        result
      } else {
        x
      }
    },
    {
      # Test with character vector
      result <- child_census_vectors("v_TEST_1")

      # Should include v_TEST_2
      expect_true("v_TEST_2" %in% result$vector)
    }
  )
})

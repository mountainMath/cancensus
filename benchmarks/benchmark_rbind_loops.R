# Benchmark script for rbind loop optimization
# This script tests the performance improvement from replacing rbind() in loops
# with list accumulation + bind_rows()

library(microbenchmark)
library(dplyr)

# Create synthetic census vector data that mimics the structure
# used by list_census_vectors()
create_mock_vectors <- function(n_vectors = 1000, max_depth = 5) {
  # Create a hierarchical structure of vectors
  vectors <- data.frame(
    vector = paste0("v_TEST_", 1:n_vectors),
    parent_vector = c(NA, sample(paste0("v_TEST_", 1:(n_vectors-1)), n_vectors-1, replace = TRUE)),
    label = paste("Label", 1:n_vectors),
    details = paste("Details", 1:n_vectors),
    aggregation = sample(c("Additive", "Average"), n_vectors, replace = TRUE),
    type = sample(c("Total", "Male", "Female"), n_vectors, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Add dataset attribute
  attr(vectors, "dataset") <- "TEST"

  return(vectors)
}

# Original implementation using rbind in loop
parent_census_vectors_old <- function(vector_list, all_vectors) {
  base_list <- vector_list
  n <- 0
  vector_list <- all_vectors %>%
    dplyr::filter(vector %in% base_list$parent_vector) %>%
    dplyr::distinct(vector, .keep_all = TRUE)

  while (n != nrow(vector_list)) {
    n <- nrow(vector_list)
    new_list <- all_vectors %>%
      dplyr::filter(vector %in% vector_list$parent_vector)
    vector_list <- vector_list %>% rbind(new_list) %>%
      dplyr::distinct(vector, .keep_all = TRUE)
  }

  return(vector_list)
}

# Optimized implementation using list accumulation
parent_census_vectors_new <- function(vector_list, all_vectors) {
  base_list <- vector_list
  n <- 0
  vector_list <- all_vectors %>%
    dplyr::filter(vector %in% base_list$parent_vector) %>%
    dplyr::distinct(vector, .keep_all = TRUE)

  # Collect results in a list
  results_list <- list(vector_list)

  while (n != nrow(vector_list)) {
    n <- nrow(vector_list)
    new_list <- all_vectors %>%
      dplyr::filter(vector %in% vector_list$parent_vector)

    if (nrow(new_list) > 0) {
      results_list <- c(results_list, list(new_list))
      vector_list <- dplyr::bind_rows(results_list) %>%
        dplyr::distinct(vector, .keep_all = TRUE)
    }
  }

  return(vector_list)
}

# Similar implementations for child_census_vectors
child_census_vectors_old <- function(vector_list, all_vectors, max_level = NA) {
  base_list <- vector_list
  n <- 0
  child_level <- 1

  vector_list <- all_vectors %>%
    dplyr::filter(.data$parent_vector %in% base_list$vector) %>%
    dplyr::distinct(vector, .keep_all = TRUE)

  while (n != nrow(vector_list) && (is.na(max_level) || child_level < max_level)) {
    child_level <- child_level + 1
    n <- nrow(vector_list)
    new_list <- all_vectors %>%
      dplyr::filter(.data$parent_vector %in% vector_list$vector)
    vector_list <- vector_list %>% rbind(new_list) %>%
      dplyr::distinct(vector, .keep_all = TRUE)
  }

  return(vector_list)
}

child_census_vectors_new <- function(vector_list, all_vectors, max_level = NA) {
  base_list <- vector_list
  n <- 0
  child_level <- 1

  vector_list <- all_vectors %>%
    dplyr::filter(.data$parent_vector %in% base_list$vector) %>%
    dplyr::distinct(vector, .keep_all = TRUE)

  # Collect results in a list
  results_list <- list(vector_list)

  while (n != nrow(vector_list) && (is.na(max_level) || child_level < max_level)) {
    child_level <- child_level + 1
    n <- nrow(vector_list)
    new_list <- all_vectors %>%
      dplyr::filter(.data$parent_vector %in% vector_list$vector)

    if (nrow(new_list) > 0) {
      results_list <- c(results_list, list(new_list))
      vector_list <- dplyr::bind_rows(results_list) %>%
        dplyr::distinct(vector, .keep_all = TRUE)
    }
  }

  return(vector_list)
}

# Run benchmarks
cat("Creating mock data...\n")
all_vectors <- create_mock_vectors(n_vectors = 500, max_depth = 5)

# Select a starting vector deep in the hierarchy
starting_vector <- all_vectors %>%
  filter(!is.na(parent_vector)) %>%
  slice(100)

cat("\n=== Benchmark: parent_census_vectors ===\n")
cat("Testing with starting vector:", starting_vector$vector, "\n\n")

result_parent <- microbenchmark(
  old = parent_census_vectors_old(starting_vector, all_vectors),
  new = parent_census_vectors_new(starting_vector, all_vectors),
  times = 100
)

print(result_parent)

cat("\n=== Benchmark: child_census_vectors ===\n")
# Select a parent vector with children
parent_vector <- all_vectors %>%
  filter(vector %in% all_vectors$parent_vector) %>%
  slice(1)

cat("Testing with parent vector:", parent_vector$vector, "\n\n")

result_child <- microbenchmark(
  old = child_census_vectors_old(parent_vector, all_vectors),
  new = child_census_vectors_new(parent_vector, all_vectors),
  times = 100
)

print(result_child)

# Verify results are identical
cat("\n=== Verification ===\n")
old_result <- parent_census_vectors_old(starting_vector, all_vectors)
new_result <- parent_census_vectors_new(starting_vector, all_vectors)
cat("parent_census_vectors results identical:",
    identical(arrange(old_result, vector), arrange(new_result, vector)), "\n")

old_result_child <- child_census_vectors_old(parent_vector, all_vectors)
new_result_child <- child_census_vectors_new(parent_vector, all_vectors)
cat("child_census_vectors results identical:",
    identical(arrange(old_result_child, vector), arrange(new_result_child, vector)), "\n")

# Calculate speedup
speedup_parent <- summary(result_parent)$median[1] / summary(result_parent)$median[2]
speedup_child <- summary(result_child)$median[1] / summary(result_child)$median[2]

cat("\n=== Summary ===\n")
cat(sprintf("parent_census_vectors speedup: %.2fx\n", speedup_parent))
cat(sprintf("child_census_vectors speedup: %.2fx\n", speedup_child))

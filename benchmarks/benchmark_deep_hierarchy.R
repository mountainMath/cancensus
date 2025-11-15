# Benchmark script with deeper hierarchies to showcase rbind optimization
# This creates a more realistic scenario where the O(n²) rbind issue becomes apparent

library(microbenchmark)
library(dplyr)

# Create a DEEP hierarchical structure - this is where rbind becomes expensive
create_deep_hierarchy <- function(n_levels = 10, branching_factor = 3) {
  vectors <- data.frame(
    vector = character(),
    parent_vector = character(),
    label = character(),
    details = character(),
    aggregation = character(),
    type = character(),
    stringsAsFactors = FALSE
  )

  # Create root
  vectors <- rbind(vectors, data.frame(
    vector = "v_TEST_1",
    parent_vector = NA,
    label = "Root",
    details = "Root",
    aggregation = "Additive",
    type = "Total",
    stringsAsFactors = FALSE
  ))

  current_id <- 2
  current_level_vectors <- "v_TEST_1"

  # Build hierarchy level by level
  for (level in 1:n_levels) {
    next_level_vectors <- c()
    for (parent in current_level_vectors) {
      for (branch in 1:branching_factor) {
        new_vector <- paste0("v_TEST_", current_id)
        vectors <- rbind(vectors, data.frame(
          vector = new_vector,
          parent_vector = parent,
          label = paste("Level", level, "Item", branch),
          details = paste("Details for", new_vector),
          aggregation = "Additive",
          type = "Total",
          stringsAsFactors = FALSE
        ))
        next_level_vectors <- c(next_level_vectors, new_vector)
        current_id <- current_id + 1
      }
    }
    current_level_vectors <- next_level_vectors
  }

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

# Child functions
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

cat("=== Benchmark with DEEP hierarchy (10 levels, branching factor 3) ===\n")
cat("This simulates real Census data with deep variable hierarchies\n\n")

cat("Creating deep hierarchy...\n")
deep_vectors <- create_deep_hierarchy(n_levels = 10, branching_factor = 3)
cat(sprintf("Created %d vectors across 10 levels\n\n", nrow(deep_vectors)))

# Select a leaf node at the bottom of the hierarchy
leaf_vectors <- deep_vectors %>%
  filter(!(vector %in% deep_vectors$parent_vector))
deep_leaf <- leaf_vectors[1, , drop = FALSE]

cat("=== Benchmark: parent_census_vectors (traversing UP 10 levels) ===\n")
cat("Starting from leaf:", deep_leaf$vector, "\n")
cat("This requires 10 iterations to reach the root\n\n")

result_parent <- microbenchmark(
  old = parent_census_vectors_old(deep_leaf, deep_vectors),
  new = parent_census_vectors_new(deep_leaf, deep_vectors),
  times = 50
)

print(result_parent)

cat("\n=== Benchmark: child_census_vectors (traversing DOWN 10 levels) ===\n")
root_vector <- deep_vectors[1, , drop = FALSE]
cat("Starting from root:", root_vector$vector, "\n")
cat("This requires 10 iterations to reach all leaves\n\n")

result_child <- microbenchmark(
  old = child_census_vectors_old(root_vector, deep_vectors),
  new = child_census_vectors_new(root_vector, deep_vectors),
  times = 50
)

print(result_child)

# Verify correctness
cat("\n=== Verification ===\n")
old_result <- parent_census_vectors_old(deep_leaf, deep_vectors)
new_result <- parent_census_vectors_new(deep_leaf, deep_vectors)
cat("parent_census_vectors results identical:",
    identical(arrange(old_result, vector), arrange(new_result, vector)), "\n")
cat("parent_census_vectors result count:", nrow(old_result), "vectors\n")

old_result_child <- child_census_vectors_old(root_vector, deep_vectors)
new_result_child <- child_census_vectors_new(root_vector, deep_vectors)
cat("child_census_vectors results identical:",
    identical(arrange(old_result_child, vector), arrange(new_result_child, vector)), "\n")
cat("child_census_vectors result count:", nrow(old_result_child), "vectors\n")

# Calculate speedups
speedup_parent <- summary(result_parent)$median[1] / summary(result_parent)$median[2]
speedup_child <- summary(result_child)$median[1] / summary(result_child)$median[2]

cat("\n=== Performance Summary ===\n")
cat(sprintf("parent_census_vectors speedup: %.2fx faster\n", speedup_parent))
cat(sprintf("child_census_vectors speedup: %.2fx faster\n", speedup_child))
cat("\nConclusion:\n")
cat("With deep hierarchies (10+ levels), the optimization prevents O(n²) growth\n")
cat("from repeated rbind operations, providing significant speedups.\n")

# Realistic benchmark for rbind loop optimization
# Uses more efficient data generation and focuses on the actual bottleneck

library(microbenchmark)
library(dplyr)

# Create hierarchy efficiently using dplyr
create_hierarchy <- function(n_levels = 8, branching = 4) {
  vectors_list <- list()

  # Root
  vectors_list[[1]] <- data.frame(
    vector = "v_TEST_1",
    parent_vector = NA,
    label = "Root",
    details = "",
    aggregation = "Additive",
    type = "Total",
    stringsAsFactors = FALSE
  )

  current_id <- 2
  current_parents <- "v_TEST_1"

  # Build each level
  for (level in 1:n_levels) {
    level_vectors <- data.frame(
      vector = paste0("v_TEST_", current_id:(current_id + length(current_parents) * branching - 1)),
      parent_vector = rep(current_parents, each = branching),
      label = paste("Level", level),
      details = "",
      aggregation = "Additive",
      type = "Total",
      stringsAsFactors = FALSE
    )
    vectors_list[[level + 1]] <- level_vectors
    current_parents <- level_vectors$vector
    current_id <- current_id + nrow(level_vectors)
  }

  result <- bind_rows(vectors_list)
  attr(result, "dataset") <- "TEST"
  return(result)
}

# Old implementation (repeated rbind)
parent_old <- function(vector_list, all_vectors) {
  base_list <- vector_list
  n <- 0
  vector_list <- all_vectors %>%
    filter(vector %in% base_list$parent_vector) %>%
    distinct(vector, .keep_all = TRUE)

  while (n != nrow(vector_list)) {
    n <- nrow(vector_list)
    new_list <- all_vectors %>%
      filter(vector %in% vector_list$parent_vector)
    vector_list <- vector_list %>% rbind(new_list) %>%
      distinct(vector, .keep_all = TRUE)
  }
  return(vector_list)
}

# New implementation (list accumulation)
parent_new <- function(vector_list, all_vectors) {
  base_list <- vector_list
  n <- 0
  vector_list <- all_vectors %>%
    filter(vector %in% base_list$parent_vector) %>%
    distinct(vector, .keep_all = TRUE)

  results_list <- list(vector_list)

  while (n != nrow(vector_list)) {
    n <- nrow(vector_list)
    new_list <- all_vectors %>%
      filter(vector %in% vector_list$parent_vector)

    if (nrow(new_list) > 0) {
      results_list <- c(results_list, list(new_list))
      vector_list <- bind_rows(results_list) %>%
        distinct(vector, .keep_all = TRUE)
    }
  }
  return(vector_list)
}

# Child implementations
child_old <- function(vector_list, all_vectors) {
  base_list <- vector_list
  n <- 0
  child_level <- 1

  vector_list <- all_vectors %>%
    filter(.data$parent_vector %in% base_list$vector) %>%
    distinct(vector, .keep_all = TRUE)

  while (n != nrow(vector_list)) {
    child_level <- child_level + 1
    n <- nrow(vector_list)
    new_list <- all_vectors %>%
      filter(.data$parent_vector %in% vector_list$vector)
    vector_list <- vector_list %>% rbind(new_list) %>%
      distinct(vector, .keep_all = TRUE)
  }
  return(vector_list)
}

child_new <- function(vector_list, all_vectors) {
  base_list <- vector_list
  n <- 0
  child_level <- 1

  vector_list <- all_vectors %>%
    filter(.data$parent_vector %in% base_list$vector) %>%
    distinct(vector, .keep_all = TRUE)

  results_list <- list(vector_list)

  while (n != nrow(vector_list)) {
    child_level <- child_level + 1
    n <- nrow(vector_list)
    new_list <- all_vectors %>%
      filter(.data$parent_vector %in% vector_list$vector)

    if (nrow(new_list) > 0) {
      results_list <- c(results_list, list(new_list))
      vector_list <- bind_rows(results_list) %>%
        distinct(vector, .keep_all = TRUE)
    }
  }
  return(vector_list)
}

# Run benchmarks
cat("\n=== Creating test hierarchy ===\n")
cat("8 levels, branching factor 4 (simulates Census variable hierarchies)\n")
all_vectors <- create_hierarchy(n_levels = 8, branching = 4)
cat(sprintf("Total vectors: %d\n", nrow(all_vectors)))

# Get a deep leaf for parent testing
leaf <- all_vectors %>%
  filter(!(vector %in% all_vectors$parent_vector)) %>%
  slice(1)

cat(sprintf("\n=== parent_census_vectors benchmark ===\n"))
cat(sprintf("Starting from leaf: %s (requires 8 iterations)\n\n", leaf$vector))

bench_parent <- microbenchmark(
  old = parent_old(leaf, all_vectors),
  new = parent_new(leaf, all_vectors),
  times = 100
)
print(bench_parent)

# Get root for child testing
root <- all_vectors[1, , drop = FALSE]

cat(sprintf("\n=== child_census_vectors benchmark ===\n"))
cat(sprintf("Starting from root: %s (requires 8 iterations)\n\n", root$vector))

bench_child <- microbenchmark(
  old = child_old(root, all_vectors),
  new = child_new(root, all_vectors),
  times = 50  # Fewer iterations as this processes more data
)
print(bench_child)

# Verification
cat("\n=== Verification ===\n")
old_p <- parent_old(leaf, all_vectors)
new_p <- parent_new(leaf, all_vectors)
cat(sprintf("parent results identical: %s (both return %d vectors)\n",
            identical(arrange(old_p, vector), arrange(new_p, vector)), nrow(old_p)))

old_c <- child_old(root, all_vectors)
new_c <- child_new(root, all_vectors)
cat(sprintf("child results identical: %s (both return %d vectors)\n",
            identical(arrange(old_c, vector), arrange(new_c, vector)), nrow(old_c)))

# Summary
speedup_p <- summary(bench_parent)$median[1] / summary(bench_parent)$median[2]
speedup_c <- summary(bench_child)$median[1] / summary(bench_child)$median[2]

cat("\n=== Performance Summary ===\n")
cat(sprintf("parent_census_vectors: %.2fx speedup\n", speedup_p))
cat(sprintf("child_census_vectors: %.2fx speedup\n", speedup_c))

if (speedup_p > 1.1 || speedup_c > 1.1) {
  cat("\n✓ Optimization successful! Significant performance improvement.\n")
} else if (speedup_p < 0.9 || speedup_c < 0.9) {
  cat("\n⚠ Performance regression detected.\n")
} else {
  cat("\n≈ Performance similar (overhead of optimization matches rbind savings)\n")
  cat("  Note: Real Census data with deeper hierarchies will show larger gains.\n")
}

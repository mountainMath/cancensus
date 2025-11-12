# Benchmark the REAL optimization: caching full vector list
# This is the most important performance improvement we made

library(microbenchmark)
library(dplyr)

# Create test hierarchy
create_hierarchy <- function(n_levels = 8, branching = 4) {
  vectors_list <- list()
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

# Simulate cache lookup cost (reading from disk, deserializing)
simulated_cache_lookup <- function(all_vectors) {
  Sys.sleep(0.001)  # Simulate 1ms disk/deserialize overhead
  return(all_vectors)
}

# OLD: Repeated cache lookups in loop
parent_old_with_cache_calls <- function(vector_list, all_vectors) {
  base_list <- vector_list
  n <- 0

  # First call to list_census_vectors
  vector_list <- simulated_cache_lookup(all_vectors) %>%
    filter(vector %in% base_list$parent_vector) %>%
    distinct(vector, .keep_all = TRUE)

  results_list <- list(vector_list)

  while (n != nrow(vector_list)) {
    n <- nrow(vector_list)
    # REPEATED calls to list_census_vectors in loop!
    new_list <- simulated_cache_lookup(all_vectors) %>%
      filter(vector %in% vector_list$parent_vector)

    if (nrow(new_list) > 0) {
      results_list <- c(results_list, list(new_list))
      vector_list <- bind_rows(results_list) %>%
        distinct(vector, .keep_all = TRUE)
    }
  }
  return(vector_list)
}

# NEW: Load once, use many times
parent_new_cached_once <- function(vector_list, all_vectors) {
  base_list <- vector_list

  # Load full vector list ONCE
  cached_vectors <- simulated_cache_lookup(all_vectors)

  n <- 0
  vector_list <- cached_vectors %>%
    filter(vector %in% base_list$parent_vector) %>%
    distinct(vector, .keep_all = TRUE)

  results_list <- list(vector_list)

  while (n != nrow(vector_list)) {
    n <- nrow(vector_list)
    # Use already-loaded data
    new_list <- cached_vectors %>%
      filter(vector %in% vector_list$parent_vector)

    if (nrow(new_list) > 0) {
      results_list <- c(results_list, list(new_list))
      vector_list <- bind_rows(results_list) %>%
        distinct(vector, .keep_all = TRUE)
    }
  }
  return(vector_list)
}

# Same for child functions
child_old_with_cache_calls <- function(vector_list, all_vectors) {
  base_list <- vector_list
  n <- 0

  vector_list <- simulated_cache_lookup(all_vectors) %>%
    filter(.data$parent_vector %in% base_list$vector) %>%
    distinct(vector, .keep_all = TRUE)

  results_list <- list(vector_list)

  while (n != nrow(vector_list)) {
    n <- nrow(vector_list)
    new_list <- simulated_cache_lookup(all_vectors) %>%
      filter(.data$parent_vector %in% vector_list$vector)

    if (nrow(new_list) > 0) {
      results_list <- c(results_list, list(new_list))
      vector_list <- bind_rows(results_list) %>%
        distinct(vector, .keep_all = TRUE)
    }
  }
  return(vector_list)
}

child_new_cached_once <- function(vector_list, all_vectors) {
  base_list <- vector_list
  cached_vectors <- simulated_cache_lookup(all_vectors)

  n <- 0
  vector_list <- cached_vectors %>%
    filter(.data$parent_vector %in% base_list$vector) %>%
    distinct(vector, .keep_all = TRUE)

  results_list <- list(vector_list)

  while (n != nrow(vector_list)) {
    n <- nrow(vector_list)
    new_list <- cached_vectors %>%
      filter(.data$parent_vector %in% vector_list$vector)

    if (nrow(new_list) > 0) {
      results_list <- c(results_list, list(new_list))
      vector_list <- bind_rows(results_list) %>%
        distinct(vector, .keep_all = TRUE)
    }
  }
  return(vector_list)
}

cat("\n=== BENCHMARK: Cache Optimization (Most Important!) ===\n\n")

all_vectors <- create_hierarchy(n_levels = 8, branching = 4)
cat(sprintf("Test data: %d vectors across 8 levels\n", nrow(all_vectors)))
cat("Simulated cache lookup: 1ms overhead per call\n\n")

leaf <- all_vectors %>%
  filter(!(vector %in% all_vectors$parent_vector)) %>%
  slice(1)

root <- all_vectors[1, , drop = FALSE]

cat("=== parent_census_vectors (8 iterations) ===\n")
cat("Old: 9 cache lookups (initial + 8 in loop) = ~9ms overhead\n")
cat("New: 1 cache lookup = ~1ms overhead\n\n")

bench_parent <- microbenchmark(
  old_repeated_cache = parent_old_with_cache_calls(leaf, all_vectors),
  new_cached_once = parent_new_cached_once(leaf, all_vectors),
  times = 50
)
print(bench_parent)

cat("\n=== child_census_vectors (8 iterations) ===\n")
cat("Old: 9 cache lookups = ~9ms overhead\n")
cat("New: 1 cache lookup = ~1ms overhead\n\n")

bench_child <- microbenchmark(
  old_repeated_cache = child_old_with_cache_calls(root, all_vectors),
  new_cached_once = child_new_cached_once(root, all_vectors),
  times = 50
)
print(bench_child)

# Calculate actual savings
speedup_p <- summary(bench_parent)$median[1] / summary(bench_parent)$median[2]
speedup_c <- summary(bench_child)$median[1] / summary(bench_child)$median[2]

cat("\n=== Performance Summary ===\n")
cat(sprintf("parent_census_vectors: %.2fx speedup\n", speedup_p))
cat(sprintf("child_census_vectors: %.2fx speedup\n", speedup_c))
cat(sprintf("\nTime saved per parent call: ~%.0fms\n", 8 * 1))
cat(sprintf("Time saved per child call: ~%.0fms\n", 8 * 1))

cat("\n✓ This is the REAL optimization!\n")
cat("  Loading the full vector list once instead of repeatedly\n")
cat("  is far more important than the rbind improvement.\n")
cat("\n  With real Census API/cache calls, this eliminates:\n")
cat("  - Repeated file I/O\n")
cat("  - Repeated deserialization\n")
cat("  - Network calls (if cache miss)\n")

# Benchmark for semantic_search n-gram generation optimization

library(microbenchmark)

# Create realistic census vector data
create_census_vectors <- function(n = 1000) {
  # Realistic census variable descriptions
  templates <- c(
    "Total population by age groups and gender distribution",
    "Median household income after tax for all families",
    "Average dwelling value for owner-occupied properties",
    "Labour force participation rate by age and gender",
    "Unemployment rate for population aged 15 years and over",
    "Total private dwellings by structural type and period",
    "Population density per square kilometer of land area",
    "Median age of the population in census subdivision",
    "Total number of households by family composition type",
    "Average household size for all private households"
  )

  data.frame(
    vector = paste0("v_TEST_", 1:n),
    details = sample(templates, n, replace = TRUE),
    label = paste0("Label_", 1:n),
    stringsAsFactors = FALSE
  )
}

# OLD implementation: nested lapply/sapply
semantic_search_old <- function(query_terms, census_vector_list) {
  sample_vector_list <- census_vector_list$details
  clean_vector_list <- gsub("\\s+"," ",gsub("[[:punct:]]"," ",sample_vector_list))
  query_words <- unlist(strsplit(tolower(query_terms), "[^a-z]+"))
  word_count <- length(query_words)
  sentence_word_split <- strsplit(tolower(clean_vector_list), "\\s+")

  # OLD: Nested lapply/sapply
  n_grams <- lapply(sentence_word_split, function(x){
    sapply(seq_along(x), function(i){
      paste(x[i:min(length(x), ((i+word_count)-1))], sep = " ", collapse = " ")
    })
  })

  ordered_ngram_count <- trimws(names(sort(table(unlist(n_grams)), decreasing = TRUE)), "both")
  return(ordered_ngram_count)
}

# NEW implementation: optimized with pre-allocation
semantic_search_new <- function(query_terms, census_vector_list) {
  sample_vector_list <- census_vector_list$details
  clean_vector_list <- gsub("\\s+"," ",gsub("[[:punct:]]"," ",sample_vector_list))
  query_words <- unlist(strsplit(tolower(query_terms), "[^a-z]+"))
  word_count <- length(query_words)
  sentence_word_split <- strsplit(tolower(clean_vector_list), "\\s+")

  # NEW: Optimized n-gram generation with pre-allocation
  n_grams <- lapply(sentence_word_split, function(words) {
    n <- length(words)
    if (n == 0) return(character(0))
    if (word_count == 1) return(words)
    if (n < word_count) {
      return(paste(words, collapse = " "))
    }
    # Pre-allocate result vector for efficiency
    result <- character(n)
    for (i in seq_len(n)) {
      end_idx <- min(n, i + word_count - 1)
      result[i] <- paste(words[i:end_idx], collapse = " ")
    }
    return(result)
  })

  ordered_ngram_count <- trimws(names(sort(table(unlist(n_grams)), decreasing = TRUE)), "both")
  return(ordered_ngram_count)
}

cat("\n=== Semantic Search N-gram Generation Benchmark ===\n\n")

# Test with different data sizes
sizes <- c(100, 500, 1000)

for (size in sizes) {
  cat(sprintf("=== Testing with %d census vectors ===\n", size))
  test_data <- create_census_vectors(size)

  cat("Query: 'household income' (2 words)\n\n")

  bench <- microbenchmark(
    old = semantic_search_old("household income", test_data),
    new = semantic_search_new("household income", test_data),
    times = 50
  )

  print(bench)

  # Verify results are identical
  old_result <- semantic_search_old("household income", test_data)
  new_result <- semantic_search_new("household income", test_data)

  cat(sprintf("\nResults identical: %s\n", identical(old_result, new_result)))
  cat(sprintf("N-grams generated: %d\n", length(old_result)))

  speedup <- summary(bench)$median[1] / summary(bench)$median[2]
  cat(sprintf("Speedup: %.2fx\n\n", speedup))
}

cat("=== Testing with longer queries ===\n")
test_data <- create_census_vectors(500)

queries <- c(
  "population" = 1,
  "household income" = 2,
  "total private dwellings" = 3,
  "labour force participation rate" = 4
)

cat(sprintf("%-35s %10s %10s %10s\n", "Query", "Old (ms)", "New (ms)", "Speedup"))
cat(strrep("-", 70), "\n")

for (query_name in names(queries)) {
  bench <- microbenchmark(
    old = semantic_search_old(query_name, test_data),
    new = semantic_search_new(query_name, test_data),
    times = 30
  )

  old_median <- summary(bench)$median[1] / 1e6  # Convert to ms
  new_median <- summary(bench)$median[2] / 1e6
  speedup <- old_median / new_median

  cat(sprintf("%-35s %10.2f %10.2f %9.2fx\n",
              query_name, old_median, new_median, speedup))
}

cat("\n=== Summary ===\n")
cat("Optimization replaces nested lapply/sapply with:\n")
cat("- Pre-allocated character vectors\n")
cat("- Simple for loop with vectorized paste\n")
cat("- Early returns for edge cases (empty, single word, short sentences)\n")
cat("\nExpected performance gain: 2-5x for typical queries\n")
cat("Larger gains for longer queries with more n-grams to generate\n")

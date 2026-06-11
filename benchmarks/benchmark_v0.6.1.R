# Benchmark for the 0.6.1 performance changes, using mock data only (no API).
#
# Compares hierarchy traversal (BFS rewrite), semantic/keyword search
# (adist pruning, vapply corpus), and the in-memory session cache for
# list_census_vectors().
#
# Run against a package tree:
#   Rscript benchmarks/benchmark_v0.6.1.R <path-to-package>
# To reproduce the before/after comparison, run once against a worktree at
# v0.6.0 and once against the 0.6.1 tree.
#
# Results (R 4.5.0, macOS, median of microbenchmark):
#
#                        v0.6.0      v0.6.1     speedup
#  child_full            4.59 ms     0.78 ms     ~5.9x
#  child_leaves          4.86 ms     1.20 ms     ~4.1x
#  parent_deep           3.13 ms     0.40 ms     ~7.8x
#  semantic search        291 ms      245 ms     ~1.2x
#  keyword search         131 ms      129 ms      ~1x
#  list_vectors (cached) 4.77 ms    0.003 ms   ~1600x
#
# The hierarchy benchmarks use a 6-level, branching-factor-4 tree (~5,500
# nodes); the search and cache benchmarks use a 7,700-row vector list sized
# like CA16.

args <- commandArgs(trailingOnly = TRUE)
pkg_path <- if (length(args) > 0) args[1] else "."
suppressMessages({
  library(dplyr)
  library(microbenchmark)
  devtools::load_all(pkg_path, quiet = TRUE)
})

set.seed(123)

## --- mock census vector list, sized like CA16 (~7700 vectors) -------------
templates <- c(
  "Total population by age groups and gender distribution in private households",
  "Median household income after tax for all economic families and persons",
  "Average dwelling value for owner occupied private dwellings by structural type",
  "Labour force participation rate by age and gender for population aged 15 years and over",
  "Total count of private dwellings by structural type of dwelling and period of construction",
  "Population density per square kilometer of land area in census subdivisions",
  "Median age of the population in census subdivision by gender and age cohort",
  "Total number of households by family composition type and household size",
  "Knowledge of official languages for the total population excluding institutional residents",
  "Ethnic origin for the population in private households 25 percent sample data"
)
n_vec <- 7700
mock_vectors <- tibble(
  vector = paste0("v_TEST_", 1:n_vec),
  type = "Total",
  label = paste0("Label_", 1:n_vec),
  details = paste(sample(templates, n_vec, replace = TRUE), 1:n_vec),
  parent_vector = NA_character_,
  aggregation = "Additive"
)

## --- mock deep hierarchy: 6 levels, branching 4 (~5500 nodes) --------------
build_hier <- function(n_levels = 6, branching = 4) {
  vecs <- "v_HIER_1"; parents <- NA_character_; current <- "v_HIER_1"; id <- 2
  for (l in 1:n_levels) {
    nxt <- character(0)
    for (p in current) {
      kids <- paste0("v_HIER_", id:(id + branching - 1)); id <- id + branching
      vecs <- c(vecs, kids); parents <- c(parents, rep(p, branching)); nxt <- c(nxt, kids)
    }
    current <- nxt
  }
  tibble(vector = vecs, parent_vector = parents, label = vecs,
         details = "", aggregation = "Additive", type = "Total")
}
mock_hier <- build_hier()
attr(mock_hier, "dataset") <- "TEST"
cat("hierarchy nodes:", nrow(mock_hier), "\n")

## --- fabricate the tempdir file cache used by list_census_vectors ---------
result <- mock_vectors
attr(result, "dataset") <- "ZZTEST"
save(result, file = file.path(tempdir(), "ZZTEST_vectors.rda"))

root <- mock_hier[1, ]

bm_hier <- testthat::with_mocked_bindings(
  list_census_vectors = function(dataset, use_cache = TRUE, quiet = TRUE) mock_hier,
  microbenchmark(
    child_full   = child_census_vectors(root, leaves_only = FALSE),
    child_leaves = child_census_vectors(root, leaves_only = TRUE),
    parent_deep  = parent_census_vectors(mock_hier[nrow(mock_hier), ]),
    times = 20
  ),
  .package = "cancensus"
)

bm_search <- microbenchmark(
  semantic = suppressWarnings(suppressMessages(
    cancensus:::semantic_search("after tax income", mock_vectors[, c("vector","type","label","details")]))),
  keyword  = suppressWarnings(suppressMessages(
    cancensus:::keyword_search("household income", mock_vectors[, c("vector","type","label","details")], interactive = FALSE))),
  times = 10
)

bm_cache <- microbenchmark(
  list_vectors_cached = list_census_vectors("ZZTEST"),
  times = 50
)

print(bm_hier, unit = "ms", signif = 3)
print(bm_search, unit = "ms", signif = 3)
print(bm_cache, unit = "ms", signif = 3)

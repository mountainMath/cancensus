# French labels (#229), labelled columns (#230), and labels containing colons

vector_info_csv <- function(with_french = TRUE) {
  rows <- data.frame(
    vector = c("v_FR_1", "v_FR_2", "v_FR_3"),
    label = c("Population", "Size of census family: 2 persons", "Age"),
    type = c("Total", "Total", "Female"),
    units = "1", add = "1", parent = c("", "v_FR_1", "v_FR_1"),
    details = c("CA 2021 Census; 100% data; Population",
                "CA 2021 Census; 100% data; Population; Size of census family: 2 persons",
                "CA 2021 Census; 100% data; Population; Age"),
    label_fr = c("Population", "Taille de la famille de recensement : 2 personnes", "Âge"),
    details_fr = c("Recensement CA 2021; Données intégrales (100 %); Population",
                   "Recensement CA 2021; Données intégrales (100 %); Population; Taille de la famille de recensement : 2 personnes",
                   "Recensement CA 2021; Données intégrales (100 %); Population; Âge"),
    stringsAsFactors = FALSE
  )
  if (!with_french) {
    rows$label_fr <- ""
    rows$details_fr <- ""
  }
  paste0(capture.output(utils::write.csv(rows, row.names = FALSE, na = "")), collapse = "\n")
}

mock_response <- function(body) {
  structure(list(status_code = 200L, content = charToRaw(enc2utf8(body)),
                 headers = httr::insensitive(list(`data-version` = "d.1", `content-type` = "text/plain"))),
            class = "response")
}

# Metadata caches and the fallback warning are per session, reset them so tests are repeatable
reset_dataset <- function(dataset) {
  keys <- c(paste0(dataset, "_vectors"), paste0("language_fallback_warning_", dataset))
  rm(list = intersect(keys, ls(.cancensus_session_cache)), envir = .cancensus_session_cache)
  unlink(file.path(tempdir(), paste0(dataset, "_vectors.rda")))
}

# Serve the vector metadata for a dataset unique to the test, to not hit session caches of other tests
local_vector_info <- function(dataset, with_french = TRUE, env = parent.frame()) {
  reset_dataset(dataset)
  counter <- new.env()
  counter$n <- 0
  local_mocked_bindings(
    cm_GET = function(...) {
      counter$n <- counter$n + 1
      mock_response(vector_info_csv(with_french))
    },
    handle_cm_status_code = function(...) invisible(NULL),
    .env = env
  )
  counter
}

data_csv <- paste0("GeoUID,Type,Region Name,Population,",
                   "v_FR_1: Population,v_FR_2: Size of census family: 2 persons,v_FR_3: Age\n",
                   "5915022,CSD,Vancouver,10,10,4,6\n")

# Run get_census against mocked endpoints with a fresh cache for the test
local_census_api <- function(env = parent.frame()) {
  cache_dir <- tempfile("cancensus_test_cache")
  dir.create(cache_dir)
  counter <- new.env()
  counter$n <- 0
  local_mocked_bindings(
    first_run_checks = function() invisible(NULL),
    cache_path = function(...) file.path(cache_dir, paste0(...)),
    warn_if_cache_not_persistent = function(...) invisible(NULL),
    cm_POST = function(...) {
      counter$n <- counter$n + 1
      mock_response(data_csv)
    },
    .env = env
  )
  counter
}

census <- function(dataset, ..., quiet = TRUE) {
  get_census(dataset, regions = list(CSD = "5915022"), level = "CSD",
             vectors = c("v_FR_1", "v_FR_2", "v_FR_3"), api_key = "CensusMapper_test", quiet = quiet, ...)
}

test_that("parse_vector_columns only splits on the first colon", {
  parsed <- parse_vector_columns(c("v_CA11_1: Population",
                                   "v_CA11_2: Size of census family: 2 persons",
                                   "v_CA11_3: Taille de la famille de recensement : 2 personnes",
                                   "v_CA11_4: Between 5 and 6:59 a.m.",
                                   "v_CA11_5: A: b: c"))
  expect_named(parsed, c("Vector", "Detail"))
  expect_equal(parsed$Vector, paste0("v_CA11_", 1:5))
  expect_equal(parsed$Detail, c("Population", "Size of census family: 2 persons",
                                "Taille de la famille de recensement : 2 personnes",
                                "Between 5 and 6:59 a.m.", "A: b: c"))
})

test_that("parse_vector_columns handles empty input and missing labels", {
  expect_equal(nrow(parse_vector_columns(character(0))), 0)
  expect_named(parse_vector_columns(character(0)), c("Vector", "Detail"))
  expect_equal(parse_vector_columns("v_CA11_1")$Detail, "")
})

test_that("get_census keeps full labels containing colons in label_vectors", {
  local_census_api()
  result <- census("COLON1")
  expect_named(label_vectors(result), c("Vector", "Detail"))
  expect_equal(label_vectors(result)$Detail,
               c("Population", "Size of census family: 2 persons", "Age"))
  expect_true("v_FR_2: Size of census family: 2 persons" %in% names(result))

  short <- census("COLON1", labels = "short")
  expect_true(all(c("v_FR_1", "v_FR_2", "v_FR_3") %in% names(short)))
  expect_equal(label_vectors(short)$Detail[2], "Size of census family: 2 persons")
})

test_that("validate_language accepts valid languages and rejects others", {
  expect_equal(validate_language("en"), "en")
  expect_equal(validate_language("FR"), "fr")
  expect_error(validate_language("de"), "Language must be one of en, fr")
  expect_error(validate_language(c("en", "fr")), "Language must be one of")
  expect_error(validate_language(NA), "Language must be one of")
  expect_error(validate_language(NULL), "Language must be one of")
})

test_that("list_census_vectors returns the same columns in both languages", {
  counter <- local_vector_info("VEC1")
  en <- list_census_vectors("VEC1", language = "en")
  fr <- list_census_vectors("VEC1", language = "fr")
  expect_named(en, c("vector", "type", "label", "units", "parent_vector", "aggregation", "details"))
  expect_named(fr, names(en))
  expect_equal(en$label[3], "Age")
  expect_equal(fr$label[3], "Âge")
  expect_match(fr$details[3], "^Recensement")
  expect_equal(attr(en, "language"), "en")
  expect_equal(attr(fr, "language"), "fr")
  expect_equal(attr(fr, "dataset"), "VEC1")
  # the type column stays usable as a filter key
  expect_equal(as.character(fr$type), as.character(en$type))
  # the metadata cache is independent of language
  expect_equal(counter$n, 1)
})

test_that("list_census_vectors uses the cancensus.language option", {
  local_vector_info("VEC2")
  rlang::local_options(cancensus.language = "fr")
  expect_equal(list_census_vectors("VEC2")$label[3], "Âge")
  expect_equal(list_census_vectors("VEC2", language = "en")$label[3], "Age")
})

test_that("list_census_vectors falls back to English with one warning per dataset", {
  local_vector_info("VEC3", with_french = FALSE)
  expect_warning(fr <- list_census_vectors("VEC3", language = "fr"),
                 "French labels are not available for dataset VEC3")
  expect_equal(fr$label[3], "Age")
  expect_false(anyNA(fr$details))
  expect_equal(attr(fr, "language"), "fr")
  expect_no_warning(list_census_vectors("VEC3", language = "fr"))
  # no warning asking for English
  expect_no_warning(list_census_vectors("VEC3", language = "en"))
})

test_that("localize_census_vectors falls back per vector", {
  info <- dplyr::tibble(vector = c("v_1", "v_2", "v_3"), label = c("a", "b", "c"), details = c("A", "B", "C"),
                        label_fr = c("x", NA, ""), details_fr = c("X", NA, ""))
  attr(info, "dataset") <- "VEC4"
  expect_warning(fr <- localize_census_vectors(info, "fr"), "for 2 of 3 vectors in dataset VEC4")
  expect_equal(fr$label, c("x", "b", "c"))
  expect_equal(fr$details, c("X", "B", "C"))
  expect_no_warning(localize_census_vectors(info[0, ], "fr", warn = TRUE))
})

test_that("localize_census_vectors falls back when the API sends no French columns", {
  info <- dplyr::tibble(vector = "v_1", label = "a", details = "A")
  attr(info, "dataset") <- "VEC5"
  expect_warning(fr <- localize_census_vectors(info, "fr"), "not available for dataset VEC5")
  expect_equal(fr$label, "a")
})

test_that("parent and child vectors keep the language of the vector list", {
  local_vector_info("VEC6")
  reset_dataset("FR") # character input derives the dataset from the vector code
  fr <- list_census_vectors("VEC6", language = "fr")
  children <- child_census_vectors(dplyr::filter(fr, vector == "v_FR_1"))
  expect_setequal(children$label, c("Taille de la famille de recensement : 2 personnes", "Âge"))
  expect_equal(attr(children, "language"), "fr")

  parents <- parent_census_vectors(dplyr::filter(fr, vector == "v_FR_3"))
  expect_equal(parents$vector, "v_FR_1")
  expect_equal(attr(parents, "language"), "fr")

  # an explicit language wins over the language of the list
  children_en <- child_census_vectors(dplyr::filter(fr, vector == "v_FR_1"), language = "en")
  expect_true("Age" %in% children_en$label)

  # character input takes the explicit language
  children_chr <- child_census_vectors("v_FR_1", language = "fr")
  expect_true("Âge" %in% children_chr$label)
  parents_chr <- parent_census_vectors("v_FR_3", language = "fr")
  expect_equal(attr(parents_chr, "language"), "fr")
})

test_that("visualize_vector_hierarchy prints labels in the requested language", {
  local_vector_info("FR")
  output <- capture.output(visualize_vector_hierarchy("v_FR_1", language = "fr", quiet = TRUE))
  expect_true(any(grepl("Âge", output, fixed = TRUE)))
  expect_false(any(grepl("Age", output, fixed = TRUE)))
})

test_that("find_census_vectors searches in the requested language", {
  local_vector_info("VEC7")
  en <- find_census_vectors("census family", "VEC7", language = "en")
  expect_equal(en$vector, "v_FR_2")
  expect_equal(en$details, "Population; Size of census family: 2 persons")

  fr <- find_census_vectors("famille de recensement", "VEC7", language = "fr")
  expect_equal(fr$vector, "v_FR_2")
  expect_equal(fr$details, "Population; Taille de la famille de recensement : 2 personnes")

  expect_warning(find_census_vectors("census family", "VEC7", language = "fr"), "No exact matches")
  # type filter takes English values in both languages
  expect_equal(find_census_vectors("Âge", "VEC7", type = "female", language = "fr")$vector, "v_FR_3")
})

test_that("find_census_vectors handles accented queries in keyword and semantic search", {
  local_vector_info("VEC8")
  keyword <- find_census_vectors("âge", "VEC8", query_type = "keyword", language = "fr", interactive = FALSE)
  expect_equal(keyword$vector, "v_FR_3")
  semantic <- find_census_vectors("âge", "VEC8", query_type = "semantic", language = "fr")
  expect_true("v_FR_3" %in% semantic$vector)
})

test_that("get_census translates column names and label_vectors", {
  local_census_api()
  local_vector_info("GET1")
  result <- census("GET1", language = "fr")
  expect_true(all(c("v_FR_1: Population",
                    "v_FR_2: Taille de la famille de recensement : 2 personnes",
                    "v_FR_3: Âge") %in% names(result)))
  expect_equal(result[["v_FR_3: Âge"]], 6)
  expect_equal(label_vectors(result)$Detail[3], "Âge")
  # other columns stay the same
  expect_true(all(c("GeoUID", "Region Name", "Population") %in% names(result)))
})

test_that("changing the language does not download data again and English needs no metadata", {
  downloads <- local_census_api()
  metadata <- local_vector_info("GET2")
  en <- census("GET2", language = "en")
  expect_equal(metadata$n, 0)
  fr <- census("GET2", language = "fr")
  expect_equal(downloads$n, 1)
  expect_equal(metadata$n, 1)
  en2 <- census("GET2", language = "en")
  expect_equal(names(en), names(en2))
  expect_equal(downloads$n, 1)
})

test_that("get_census with language = 'fr' returns codes for short labels and named vectors", {
  local_census_api()
  local_vector_info("GET3")
  short <- census("GET3", language = "fr", labels = "short")
  expect_true(all(c("v_FR_1", "v_FR_2", "v_FR_3") %in% names(short)))
  expect_equal(label_vectors(short)$Detail[3], "Âge")

  named <- get_census("GET3", regions = list(CSD = "5915022"), level = "CSD", api_key = "CensusMapper_test",
                      vectors = c(pop = "v_FR_1", fam = "v_FR_2", age = "v_FR_3"), quiet = TRUE, language = "fr")
  expect_true(all(c("pop", "fam", "age") %in% names(named)))
})

test_that("get_census falls back to English when French is unavailable", {
  local_census_api()
  local_vector_info("GET4", with_french = FALSE)
  expect_warning(
    result <- suppressMessages(census("GET4", language = "fr", quiet = FALSE)),
    "French labels are not available for dataset GET4"
  )
  expect_true("v_FR_3: Age" %in% names(result))
})

test_that("get_census quiet = TRUE suppresses the fallback warning without using it up", {
  local_census_api()
  local_vector_info("GET5", with_french = FALSE)
  expect_no_warning(census("GET5", language = "fr"))
  expect_warning(list_census_vectors("GET5", language = "fr"), "not available for dataset GET5")
})

test_that("get_census keeps English labels when the metadata request fails", {
  local_census_api()
  reset_dataset("GET6")
  local_mocked_bindings(cm_GET = function(...) stop("offline"))
  expect_no_warning(result <- census("GET6", language = "fr"))
  expect_true("v_FR_3: Age" %in% names(result))
  expect_warning(
    suppressMessages(census("GET6", language = "fr", quiet = FALSE)),
    "Could not retrieve French labels"
  )
})

test_that("labels = 'labelled' names columns by vector and sets label attributes", {
  local_census_api()
  result <- census("LAB1", labels = "labelled")
  expect_true(all(c("v_FR_1", "v_FR_2", "v_FR_3") %in% names(result)))
  expect_equal(attr(result$v_FR_1, "label"), "Population")
  expect_equal(attr(result$v_FR_2, "label"), "Size of census family: 2 persons")
  expect_equal(as.numeric(result$v_FR_3), 6)
  expect_equal(label_vectors(result)$Vector, c("v_FR_1", "v_FR_2", "v_FR_3"))
  # non-vector columns remain unlabelled
  expect_null(attr(result$Population, "label"))
})

test_that("labels = 'labelled' follows the language and needs no metadata in English", {
  local_census_api()
  metadata <- local_vector_info("LAB2")
  census("LAB2", labels = "labelled")
  expect_equal(metadata$n, 0)
  fr <- census("LAB2", labels = "labelled", language = "fr")
  expect_equal(attr(fr$v_FR_3, "label"), "Âge")
  expect_equal(attr(fr$v_FR_2, "label"), "Taille de la famille de recensement : 2 personnes")
})

test_that("labels = 'labelled' retains labels through renaming and dplyr verbs", {
  local_census_api()
  named <- get_census("LAB3", regions = list(CSD = "5915022"), level = "CSD", api_key = "CensusMapper_test",
                      vectors = c(pop = "v_FR_1", fam = "v_FR_2", age = "v_FR_3"), quiet = TRUE, labels = "labelled")
  expect_equal(attr(named$age, "label"), "Age")
  expect_equal(attr(named$fam, "label"), "Size of census family: 2 persons")
  filtered <- dplyr::filter(named, .data$age > 0)
  expect_equal(attr(filtered$age, "label"), "Age")
})

test_that("labels = 'short' keeps the text before the first colon, in any language", {
  local_census_api()
  local_vector_info("SHORT1")
  expected <- c("GeoUID", "Type", "Region Name", "Population", "v_FR_1", "v_FR_2", "v_FR_3")
  expect_named(census("SHORT1", labels = "short"), expected)
  expect_named(census("SHORT1", labels = "short", language = "fr"), expected)
})

test_that("get_census handles partly named vectors", {
  local_census_api()
  partly_named <- function(...) {
    get_census("PART1", regions = list(CSD = "5915022"), level = "CSD", api_key = "CensusMapper_test",
               vectors = c(pop = "v_FR_1", "v_FR_2", age = "v_FR_3"), quiet = TRUE, ...)
  }
  expected <- c("GeoUID", "Type", "Region Name", "Population", "pop", "v_FR_2", "age")
  expect_named(partly_named(), expected)
  expect_named(partly_named(labels = "short"), expected)

  labelled <- partly_named(labels = "labelled")
  expect_named(labelled, expected)
  expect_equal(attr(labelled$age, "label"), "Age")
  expect_equal(attr(labelled$v_FR_2, "label"), "Size of census family: 2 persons")
})

test_that("get_census rejects invalid labels and languages before any request", {
  downloads <- local_census_api()
  expect_error(census("VAL1", labels = "labeled"), "`labels` parameter must be one of")
  expect_error(census("VAL1", language = "de"), "Language must be one of")
  expect_equal(downloads$n, 0)
})

test_that("dataset_attribution uses the requested language", {
  local_mocked_bindings(list_census_datasets = function(...) {
    dplyr::tibble(dataset = c("CA16", "CA21", "TX1"),
                  attribution = c("StatCan 2016 Census", "StatCan 2021 Census", "Tax 2017"),
                  attribution_fr = c("StatCan, Recensement de 2016", "StatCan, Recensement de 2021", ""))
  })
  expect_equal(dataset_attribution(c("CA16", "CA21"), language = "en"), "StatCan 2016, 2021 Census")
  expect_equal(dataset_attribution(c("CA16", "CA21"), language = "fr"), "StatCan, Recensement de 2016, 2021")
  expect_equal(dataset_attribution("TX1", language = "fr"), "Tax 2017")
})

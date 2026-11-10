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
    list_census_datasets = function(...) dplyr::tibble(dataset = c("TX2018", "BROKEN"), geo_dataset = c("CA16", "other")),
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
                 "VEC3", class = "cancensus_french_labels_unavailable_dataset")
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
  expect_warning(fr <- localize_census_vectors(info, "fr"), "2 des 3 vecteurs .* VEC4",
                 class = "cancensus_french_labels_unavailable_vectors")
  expect_warning(localize_census_vectors(info, "fr", warn = FALSE), NA)
  rm(list = "language_fallback_warning_VEC4", envir = .cancensus_session_cache)
  rlang::local_options(cancensus.language = "fr")
  expect_warning(cancensus:::warn_language_fallback("VEC4", 2, 3, language = "en"), "for 2 of 3 vectors in dataset VEC4")
  expect_equal(fr$label, c("x", "b", "c"))
  expect_equal(fr$details, c("X", "B", "C"))
  expect_no_warning(localize_census_vectors(info[0, ], "fr", warn = TRUE))
})

test_that("localize_census_vectors falls back when the API sends no French columns", {
  info <- dplyr::tibble(vector = "v_1", label = "a", details = "A")
  attr(info, "dataset") <- "VEC5"
  expect_warning(fr <- localize_census_vectors(info, "fr"), "VEC5",
                 class = "cancensus_french_labels_unavailable_dataset")
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

test_that("visualize_vector_hierarchy marks leaves and types in the requested language", {
  local_vector_info("FR")
  fr <- capture.output(visualize_vector_hierarchy("v_FR_1", language = "fr", show_type = TRUE, quiet = TRUE))
  en <- capture.output(visualize_vector_hierarchy("v_FR_1", language = "en", show_type = TRUE, quiet = TRUE))
  expect_false(any(grepl("(leaf)", fr, fixed = TRUE)))
  expect_identical(grepl("(feuille)", fr, fixed = TRUE), grepl("(leaf)", en, fixed = TRUE))
  expect_true(any(grepl("(feuille)", fr, fixed = TRUE)))
  expect_true(any(grepl("[Femmes]", fr, fixed = TRUE)))
  expect_true(any(grepl("[Female]", en, fixed = TRUE)))
  expect_false(any(grepl("[Female]", fr, fixed = TRUE)))
  expect_equal(vector_type_label(c("Total", "Male", "Female", NA), "fr"), c("Total", "Hommes", "Femmes", NA))
})

test_that("find_census_vectors searches in the requested language", {
  local_vector_info("VEC7")
  en <- find_census_vectors("census family", "VEC7", language = "en")
  expect_equal(en$vector, "v_FR_2")
  expect_equal(en$details, "Population; Size of census family: 2 persons")

  fr <- find_census_vectors("famille de recensement", "VEC7", language = "fr")
  expect_equal(fr$vector, "v_FR_2")
  expect_equal(fr$details, "Population; Taille de la famille de recensement : 2 personnes")

  expect_warning(find_census_vectors("census family", "VEC7", language = "fr"),
                 "Aucune correspondance exacte", class = "cancensus_no_exact_matches")
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
    "GET4", class = "cancensus_french_labels_unavailable_dataset"
  )
  expect_true("v_FR_3: Age" %in% names(result))
})

test_that("get_census quiet = TRUE suppresses the fallback warning without using it up", {
  local_census_api()
  local_vector_info("GET5", with_french = FALSE)
  expect_no_warning(census("GET5", language = "fr"))
  expect_warning(list_census_vectors("GET5", language = "fr"), "GET5",
                 class = "cancensus_french_labels_unavailable_dataset")
})

test_that("get_census keeps English labels when the metadata request fails", {
  local_census_api()
  reset_dataset("GET6")
  local_mocked_bindings(cm_GET = function(...) stop("offline"))
  expect_no_warning(result <- census("GET6", language = "fr"))
  expect_true("v_FR_3: Age" %in% names(result))
  expect_warning(
    suppressMessages(census("GET6", language = "fr", quiet = FALSE)),
    "offline", class = "cancensus_french_labels_fetch_failed"
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
})

test_that("labels = 'labelled' labels the other columns with the census year", {
  local_census_api()
  result <- census("CA21", labels = "labelled")
  expect_equal(attr(result$Population, "label"), "Population 2021")
  expect_equal(attr(result$GeoUID, "label"), "Geographic identifier")
  expect_equal(attr(result$Name, "label"), "Region name")
  expect_named(result, c("GeoUID", "Type", "Name", "Population", "v_FR_1", "v_FR_2", "v_FR_3"))
  # the data does not get changed
  expect_equal(as.numeric(result$Population), 10)
  expect_equal(as.character(result$Name), "Vancouver")
})

test_that("labels = 'labelled' labels the other columns when no vectors are requested", {
  local_census_api()
  result <- get_census("CA16", regions = list(CSD = "5915022"), level = "CSD",
                       api_key = "CensusMapper_test", quiet = TRUE, labels = "labelled")
  expect_equal(attr(result$Population, "label"), "Population 2016")
})

test_that("other columns are only labelled for labels = 'labelled'", {
  local_census_api()
  expect_null(attr(census("CA21", labels = "short")$Population, "label"))
  expect_null(attr(census("CA21")$Population, "label"))
})

test_that("names chosen for vectors are kept with the label of the vector", {
  local_census_api()
  result <- get_census("CA21", regions = list(CSD = "5915022"), level = "CSD",
                       vectors = c(pop = "v_FR_1", name = "v_FR_3"),
                       api_key = "CensusMapper_test", quiet = TRUE, labels = "labelled")
  expect_true(all(c("pop", "name", "Population") %in% names(result)))
  expect_equal(attr(result$pop, "label"), "Population")
  # not the label for the name column of spatial data
  expect_equal(attr(result$name, "label"), "Age")
  expect_equal(attr(result$Population, "label"), "Population 2021")
})

test_that("census_year_for_dataset uses the census the data refers to", {
  local_census_api()
  expect_equal(census_year_for_dataset("CA1996"), 1996L)
  expect_equal(census_year_for_dataset("CA01"), 2001L)
  expect_equal(census_year_for_dataset("CA21"), 2021L)
  expect_equal(census_year_for_dataset("TX2018"), 2016L)
  expect_true(is.na(census_year_for_dataset("UNKNOWN")))
  expect_true(is.na(census_year_for_dataset("BROKEN")))
})

test_that("census_year_for_dataset needs no dataset list for census datasets and survives its failure", {
  local_mocked_bindings(list_census_datasets = function(...) stop("no connection"))
  expect_equal(census_year_for_dataset("CA21"), 2021L)
  expect_true(is.na(census_year_for_dataset("TX2018")))
})

test_that("label_census_columns fills in years and language", {
  local_census_api()
  d <- dplyr::tibble(Population = 1, Dwellings = 2, Households = 3,
                     `Adjusted Population (previous Census)` = 5, `NHS Non-Return Rate` = 0.1, v_1 = 1)
  labels_of <- function(x) unname(unlist(lapply(x, attr, "label")))
  expect_equal(labels_of(label_census_columns(d, "CA11", "en")),
               c("Population 2011", "Dwellings 2011", "Households 2011", "Population 2006", "NHS non-response rate 2011"))
  expect_equal(labels_of(label_census_columns(d, "TX2018", "fr")),
               c("Population 2016", "Logements 2016", "Ménages 2016", "Population 2011", "Taux de non-réponse de l'ENM 2016"))
  # no dangling placeholders without a year
  expect_equal(attr(label_census_columns(d, "UNKNOWN", "en")$Population, "label"), "Population")
  expect_null(attr(label_census_columns(d, "CA21", "en")$v_1, "label"))
})

test_that("label_census_columns gives data and geometry columns the same names, types and order", {
  local_census_api()
  data <- dplyr::tibble(`v_1: A` = 1, GeoUID = "1", Type = factor("CSD"), `Region Name` = factor("Vancouver"),
                        `Area (sq km)` = 2.5, Population = 10, Dwellings = 5, Households = 4, CD_UID = "59")
  geo <- dplyr::tibble(`Shape Area` = 2.5, Type = factor("CSD"), Households = 4L, `Quality Flags` = "00000",
                       `Dwellings 2016` = 3L, name = "Vancouver", GeoUID = "1", Population = 10L,
                       `Households 2016` = 3L, Dwellings = 5L, CD_UID = "59", `Population 2016` = 9L,
                       geometry = sf::st_sfc(sf::st_point(c(0, 0)))) %>% sf::st_sf()
  both <- dplyr::bind_cols(geo, dplyr::tibble(`Region Name` = factor("Vancouver"), `Area (sq km)` = 2.5, `v_1: A` = 1))
  common <- c("GeoUID", "Type", "Name", "Area", "Population", "Dwellings", "Households")
  extra <- c("Population_Previous", "Dwellings_Previous", "Households_Previous", "Quality", "CD_UID")

  labelled_data <- label_census_columns(data, "CA21", "en")
  labelled_geo <- label_census_columns(geo, "CA21", "en")
  labelled_both <- label_census_columns(both, "CA21", "en")
  expect_named(labelled_data, c(common, "CD_UID", "v_1: A"))
  expect_named(labelled_geo, c(common, extra, "geometry"))
  expect_named(labelled_both, c(common, extra, "v_1: A", "geometry"))
  expect_s3_class(labelled_both, "sf")

  types <- function(x) vapply(sf::st_drop_geometry(x)[common], function(c) class(c)[1], "")
  plain_types <- vapply(labelled_data[common], function(c) class(c)[1], "")
  expect_equal(types(labelled_geo), plain_types)
  expect_equal(types(labelled_both), plain_types)
  expect_equal(plain_types[["Name"]], "character")
  expect_equal(plain_types[["Population"]], "numeric")

  expect_equal(attr(labelled_geo$Population_Previous, "label"), "Population 2016")
  expect_equal(attr(labelled_geo$Households_Previous, "label"), "Households 2016")
  expect_equal(attr(labelled_both$Name, "label"), "Region name")
  expect_equal(attr(labelled_both$Area, "label"), "Area (sq km)")
})

test_that("with labels = 'labelled' names chosen for vectors win over other columns, which get an underscore", {
  local_census_api()
  clashing <- function(...) {
    get_census("CA21", regions = list(CSD = "5915022"), level = "CSD", vectors = c(Population = "v_FR_1", Age = "v_FR_3"),
               api_key = "CensusMapper_test", quiet = TRUE, ...)
  }
  # other label options keep their behaviour
  expect_error(clashing(labels = "detailed"))
  expect_error(clashing(labels = "short"))
  labelled <- clashing(labels = "labelled")
  expect_true(all(c("Population", "Population_", "Age") %in% names(labelled)))
  expect_lt(match("Population_", names(labelled)), match("Population", names(labelled)))
  expect_equal(attr(labelled$Population, "label"), "Population")
  expect_equal(attr(labelled$Population_, "label"), "Population 2021")

  named_like_short_name <- get_census("CA21", regions = list(CSD = "5915022"), level = "CSD", vectors = c(Name = "v_FR_3"),
                                      api_key = "CensusMapper_test", quiet = TRUE, labels = "labelled")
  expect_equal(attr(named_like_short_name$Name, "label"), "Age")
  expect_equal(as.character(named_like_short_name$Name_), "Vancouver")
  expect_equal(attr(named_like_short_name$Name_, "label"), "Region name")
})

test_that("add_underscore_to_clashes keeps names unique and leaves other columns alone", {
  d <- dplyr::tibble(a = 1, b = 2, a_ = 3, v_1 = 4)
  expect_named(add_underscore_to_clashes(d, "b"), c("a", "b_", "a_", "v_1"))
  d2 <- dplyr::tibble(a = 1, b = 2, v_1 = 3)
  expect_named(add_underscore_to_clashes(d2, c("a", "b")), c("a_", "b_", "v_1"))
  # the underscored name is taken, by another column or by a chosen name
  expect_error(add_underscore_to_clashes(d, "a"), "`a_`")
  expect_error(add_underscore_to_clashes(d, c("b", "b_")), "`b_`")
  expect_named(add_underscore_to_clashes(d, "other"), names(d))
  expect_named(add_underscore_to_clashes(d, character(0)), names(d))
  # vector columns are not moved out of the way
  expect_named(add_underscore_to_clashes(d, "v_1"), names(d))
  g <- sf::st_sf(dplyr::tibble(a = 1, geometry = sf::st_sfc(sf::st_point(c(0, 0)))))
  renamed <- add_underscore_to_clashes(g, "geometry")
  expect_s3_class(renamed, "sf")
  expect_equal(attr(renamed, "sf_column"), "geometry_")
})

test_that("the labels default message shows once per session, only when relying on the default", {
  local_census_api()
  rlang::local_options(cancensus.labels = NULL)
  reset_message <- function() {
    rm(list = intersect("labels_default_message", ls(.cancensus_session_cache)), envir = .cancensus_session_cache)
  }
  call_census <- function(...) {
    get_census("CA21", regions = list(CSD = "5915022"), level = "CSD", vectors = "v_FR_1",
               api_key = "CensusMapper_test", ...)
  }
  labels_message <- "cancensus.labels"
  reset_message()

  # setting the parameter or quiet = TRUE neither shows the message nor uses it up
  expect_no_message(call_census(quiet = FALSE, labels = "detailed"), message = labels_message)
  expect_no_message(call_census(quiet = TRUE))
  expect_message(call_census(quiet = FALSE), labels_message)
  expect_no_message(call_census(quiet = FALSE), message = labels_message)

  reset_message()
  rlang::local_options(cancensus.labels = "detailed")
  expect_no_message(call_census(quiet = FALSE), message = labels_message)
  reset_message()
})

test_that("the cancensus.labels option sets the default for labels", {
  local_census_api()
  rlang::local_options(cancensus.labels = NULL)
  expect_true("v_FR_2: Size of census family: 2 persons" %in% names(census("CA21")))
  rlang::local_options(cancensus.labels = "labelled")
  expect_true("Name" %in% names(census("CA21")))
  # the parameter takes priority over the option
  expect_true("Region Name" %in% names(census("CA21", labels = "short")))
  rlang::local_options(cancensus.labels = "other")
  expect_error(census("CA21"), "labels")
})

test_that("column names only change for labels = 'labelled'", {
  local_census_api()
  rlang::local_options(cancensus.labels = NULL)
  expect_true("Region Name" %in% names(census("CA21", labels = "short")))
  expect_true("Region Name" %in% names(census("CA21")))
})

test_that("census_column_labels covers all columns renamed by the package", {
  renamed <- unlist(lapply(c("DB", "DA", "CT", "CSD", "CD", "CMA", "PR"), function(l) names(name_change_for_level(l))))
  expect_true(all(c(names(base_name_change), renamed, "Region Name", "Area (sq km)", "name", "key") %in%
                    census_column_labels$column))
  expect_false(any(duplicated(census_column_labels$column)))
  expect_false(any(grepl("[[:space:]]", census_column_labels$short_name)))
  expect_false(any(is.na(census_column_labels$en) | is.na(census_column_labels$fr)))
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
  expect_named(labelled, sub("Region Name", "Name", expected, fixed = TRUE))
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

# Run get_census against mocked endpoints that also serve geometry
local_census_geo_api <- function(env = parent.frame()) {
  geo_json <- paste0('{"type":"FeatureCollection","features":[{"type":"Feature",',
                     '"properties":{"id":"5915022","t":"CSD","a":1.5},',
                     '"geometry":{"type":"Polygon","coordinates":[[[0,0],[1,0],[1,1],[0,0]]]}}]}')
  local_census_api(env = env)
  local_mocked_bindings(
    cm_POST = function(url, ...) mock_response(if (grepl("geo.geojson", url, fixed = TRUE)) geo_json else data_csv),
    .env = env
  )
}

test_that("label_vectors works for geographic data in all label modes", {
  local_census_geo_api()
  expected <- c("Population", "Size of census family: 2 persons", "Age")
  for (labels in c("detailed", "short", "labelled")) {
    result <- census("GEOLV", geo_format = "sf", labels = labels)
    expect_s3_class(result, "sf")
    expect_equal(label_vectors(result)$Detail, expected)

    named <- get_census("GEOLV", regions = list(CSD = "5915022"), level = "CSD", geo_format = "sf",
                        vectors = c(pop = "v_FR_1", "v_FR_2", "v_FR_3"), labels = labels,
                        api_key = "CensusMapper_test", quiet = TRUE)
    expect_true("pop" %in% names(named))
    expect_equal(label_vectors(named)$Vector, c("v_FR_1", "v_FR_2", "v_FR_3"))
  }

  skip_if_not_installed("sp")
  expect_equal(label_vectors(census("GEOLV", geo_format = "sp", labels = "short"))$Detail, expected)
})

test_that("label_vectors and column labels are translated for geographic data", {
  local_vector_info("GEOFR")
  local_census_geo_api()
  french <- c("Population", "Taille de la famille de recensement : 2 personnes", "\u00c2ge")
  for (labels in c("detailed", "short", "labelled")) {
    result <- census("GEOFR", geo_format = "sf", labels = labels, language = "fr")
    expect_s3_class(result, "sf")
    expect_equal(label_vectors(result)$Detail, french)
  }

  detailed <- census("GEOFR", geo_format = "sf", labels = "detailed", language = "fr")
  expect_true("v_FR_3: \u00c2ge" %in% names(detailed))

  named <- get_census("GEOFR", regions = list(CSD = "5915022"), level = "CSD", geo_format = "sf",
                      vectors = c(age = "v_FR_3"), labels = "labelled", language = "fr",
                      api_key = "CensusMapper_test", quiet = TRUE)
  expect_equal(attr(named$age, "label"), "\u00c2ge")
  expect_true("v_FR_3" %in% label_vectors(named)$Vector)
})

test_that("label_vectors warns for geographic data without vectors", {
  local_census_geo_api()
  result <- get_census("GEONOV", regions = list(CSD = "5915022"), level = "CSD", geo_format = "sf",
                       labels = "labelled", api_key = "CensusMapper_test", quiet = TRUE)
  expect_s3_class(result, "sf")
  expect_warning(label_vectors(result), "does not have variables")
})

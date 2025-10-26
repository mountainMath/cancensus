#' Query the CensusMapper API for vectors with descriptions matching a search term or phrase (deprecated)
#'
#' @param searchterm The term or phrase to search for e.g. \code{"Ojibway"}.
#' Search terms are case insensitive. If unable to find a given string,
#' this function will suggest similarly named objects.
#' @param dataset The dataset to query for available vectors, e.g.
#'   \code{"CA16"}.
#' @param type One of \code{NA}, \code{'Total'}, \code{'Male'} or \code{'Female'}.
#' If specified, only return variables of specified `type`.
#' @param ... Further arguments passed on to \code{\link{list_census_vectors}}.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' search_census_vectors('Ojibway', 'CA16')
#'
#' # This will return a warning that no match was found, but will suggest similar terms.
#' search_census_vectors('Ojibwe', 'CA16', 'Total')
#' }
search_census_vectors <- function(searchterm, dataset, type=NA, ...) {
  warning("search_census_vectors(). See ?find_census_vectors() for more robust search functions.")
  veclist <- list_census_vectors(dataset, ...)
  result <- veclist[grep(searchterm, veclist$label, ignore.case = TRUE),]

  # filter by type if needed
  if (!is.na(type) && length(rownames(result)) > 0) {
    result <- result[result$type==type,]
  }

  # Check if searchterm returned anything
  if (length(rownames(result)) > 0 ) {
    attr(result, "dataset") <- dataset
    return(result)
  }
  # If nothing matches, throw a warning and suggested alternatives.
  # If no suggested alternatives because the typo is too egregious, throw an error.
  else {
    # Check for similarly named terms. Uses base function agrep which is based on the Levenshtein edit distance for string similarity.
    # Default is set to 0.1 - can expand this to be more tolerant still.
    hintlist <- dplyr::as_tibble(unique(agrep(searchterm, veclist$label, ignore.case = TRUE, value = TRUE)))
    names(hintlist) <- "Similarly named objects"
    #
    if (length(hintlist) > 0) {
      warning("No results found. Please use accurate spelling. See above for list of variables with similar named terms.")
      print(hintlist)
    } else {
      stop("No results found.")
    }
  }
}

#' Query the CensusMapper API for vectors using exact, semantic, or keyword search
#'
#' @description Query the available list of Census vectors based on their label and return
#' details including vector code. Default search behaviour expects an exact match, but
#' keyword or semantic searches can be used instead by setting \code{query_type='keyword'} or
#' \code{query_type = 'semantic'} instead. Keyword search is useful when looking to explore
#' Census vectors based on broad themes like "income" or "language". Keyword search separates
#' the query into unigrams and returns Census vectors with matching words, ranked by incidence
#' of matches. Semantic search is designed for more precise searches while allowing room for error
#' for spelling or phrasing, as well as for finding closely related vector matches. Semantic search
#' separates the query into n-grams and relies on string distance measurement using a generalized
#' Levenshtein distance approach.
#'
#' Some census vectors return population counts segmented by \code{Female} and \code{Male} populations, in
#' addition to a total aggregate. By default, query matches will return matches for the \code{Total}
#' aggregation, but can optionally return only the \code{Female} or \code{Male} aggregations by adding
#' \code{type = 'female'} or \code{type = 'male'} as a parameter.
#'
#' @param query The term or phrase to search for e.g. \code{'Oji-cree'}.
#' Search queries are case insensitive.
#' @param dataset The dataset to query for available vectors, e.g. \code{'CA16'}.
#' To see a list of available datasets: \code{list_census_datasets()}
#' @param type One of \code{'all'}, \code{'total'}, \code{'male'} or \code{'female'}.
#' If specified, only return aggregations of specified `type`. By default, only
#' the \code{'total'} aggregation will be returned.
#' @param query_type One of \code{exact}, \code{'semantic'} or \code{'keyword'}.
#' By default, assumes exact string matching, but the alternatives may be better
#' options in some cases. See description section for more details on query types.
#' @param ... Other arguments passed to internal functions.
#'
#' @export
#'
#' @examples
#'\dontrun{
#' find_census_vectors('Oji-cree', dataset = 'CA16', type = 'total', query_type = 'exact')
#'
#' find_census_vectors('commuting duration', dataset = 'CA11', type = 'female', query_type = 'keyword')
#'
#' find_census_vectors('after tax income', dataset = 'CA16', type = 'total', query_type = 'semantic')
#'
#' # This incorrect spelling will return a warning that no match was found,
#' # but will suggest trying semantic or keyword search.
#' find_census_vectors('Ojibwey', dataset = 'CA16', type = 'total')
#'
#' # This will find near matches as well
#' find_census_vectors('Ojibwey', dataset = 'CA16', type = 'total', query_type = "semantic")
#'
#' find_census_vectors('commute duration', dataset = 'CA16', type = 'female', query_type = 'keyword')
#'
#' find_census_vectors('commute duration', dataset = 'CA11', type = 'all', query_type = 'keyword')
#'
#' find_census_vectors('ukrainian origin', dataset = 'CA16', type = 'total', query_type = 'keyword')
#'
#' }
find_census_vectors <- function(query, dataset, type = "all", query_type = "exact", ...) {
  type = tolower(type)
  query_type = tolower(query_type)

  if(!(any(type %in% c("total","male","female","all")))) {
    stop("Type must be one of 'total','female', or 'male'. See ?find_census_vectors() for more details.",
         call. = FALSE)
  }

  if(!(any(query_type %in% c("exact","semantic","keyword")))) {
    stop("Query type must be one of 'exact','semantic', or 'keyword'. See ?find_census_vectors() for more details.",
    call. = FALSE)
  }
  type = tools::toTitleCase(type)
  census_vector_list <- list_census_vectors(dataset)[, c("vector", "type", "label","details")]
  census_vector_list$details <- gsub("^(.*)Census; |100% data; ","",census_vector_list$details)
  # filter by type if provided
  if (any(type %in% c("Total", "Male", "Female")))
  {
    census_vector_list <- census_vector_list[census_vector_list$type %in% type, ]
  }
  if (query_type == "exact") {
    result <- census_vector_list[grep(query, census_vector_list$details, ignore.case = TRUE), ]
    if(length(result$vector)>=1) result else {
      warning("No exact matches found. Please check spelling and try again or consider using semantic or keyword search.\nSee ?find_census_vectors() for more details.\n\nAlternatively, you can launch the Censusmapper web API in a browser by calling explore_census_vectors(dataset)",
              call. = FALSE)
    }
  } else if (query_type == "semantic") {
    semantic_search(query, census_vector_list = census_vector_list)

  } else if (query_type == "keyword") {
    keyword_search(query, census_vector_list = census_vector_list, ...)
  }
}

#' Internal function for powering semantic searches in find_census_vectors
#'
#' @description Default search behaviour in \code{find_census_vectors()} expects an exact match, but
#' keyword or semantic searches can be used instead by setting \code{query_type="keyword"} or
#' \code{query_type = "semantic"} instead. Keyword search is useful when looking to explore
#' Census vectors based on broad themes like "income" or "language". Keyword search seperates
#' the query into unigrams and returns Census vectors with matching words, ranked by incidence
#' of matches. Semantic search is designed for more precise searches while allowing room for error
#' for spelling or phrasing, as well as for finding closely related vector matches. Semantic search
#' separates the query into n-grams and relies on string distance measurement using a generalized
#' Levenshtein distance approach.
#'
#' @keywords Internal
#'
#' @noRd
#'
#' @noMd
semantic_search <- function(query_terms, census_vector_list) {
  sample_vector_list <- census_vector_list$details
  clean_vector_list <- gsub("\\s+"," ",gsub("[[:punct:]]"," ",sample_vector_list))
  query_words <- unlist(strsplit(tolower(query_terms), "[^a-z]+"))
  word_count <- length(query_words)
  sentence_word_split <- strsplit(tolower(clean_vector_list), "\\s+")

  n_grams <- lapply(sentence_word_split, function(x){
    sapply(seq_along(x), function(i){
      paste(x[i:min(length(x), ((i+word_count)-1))], sep = " ", collapse = " ")
    })})

  ordered_ngram_count <- trimws(names(sort(table(unlist(n_grams)), decreasing = TRUE)), "both")
  revised_query <- c(query_terms, unlist(strsplit(query_terms, "\\s+")))
  lev_dist_df <- setNames(data.frame(sapply(seq_along(revised_query),
                                            function(i){
                                              utils::adist(revised_query[i], ordered_ngram_count, ignore.case = TRUE)
                                            }
  )), gsub("\\s+", "_", revised_query))

  if(min(lev_dist_df) > 2 | is.infinite(min(lev_dist_df))) {
    warning(
      "No close matches found. Please check spelling and try again or consider using keyword search instead.\nSee ?find_census_vectors() for more details.\n\nAlternatively, you can launch the Censusmapper web API in a browser by calling explore_census_vectors(dataset)",
      call. = FALSE
    )} else {
      res <- sample_vector_list[grep(ordered_ngram_count[sapply(seq_along(ncol(lev_dist_df)),
                                                                function(i) {
                                                                  which.min(lev_dist_df[, i])
                                                                })], clean_vector_list, value = FALSE, ignore.case = TRUE)]

      if(length(res) == 1) {census_vector_list[which(census_vector_list$details %in% res),]} else if(length(res) >1) {
        message("Multiple possible matches. Results ordered by closeness.")
        census_vector_list[which(census_vector_list$details %in% res),]
      }
    }
}

#' Internal function for powering keyword searches in find_census_vectors
#'
#' @description Default search behaviour in \code{find_census_vectors()} expects an exact match, but
#' keyword or semantic searches can be used instead by setting \code{query_type="keyword"} or
#' \code{query_type = "semantic"} instead. Keyword search is useful when looking to explore
#' Census vectors based on broad themes like "income" or "language". Keyword search seperates
#' the query into unigrams and returns Census vectors with matching words, ranked by incidence
#' of matches. Semantic search is designed for more precise searches while allowing room for error
#' for spelling or phrasing, as well as for finding closely related vector matches. Semantic search
#' separates the query into n-grams and relies on string distance measurement using a generalized
#' Levenshtein distance approach.
#'
#' @param interactive Set to \code{TRUE} by default, determines how keyword search works.
#' Keyword search will return the best matching results; however, if there are additional
#' matching keywords it will prompt the user with an option to show the additional results.
#' This can be manually overridden by setting \code{interactive=FALSE}, which will result in
#' only the top matched results showing up, which might be useful if this function is used in
#' automated scripts.
#'
#' @keywords Internal
#' @noRd
#' @noMd
keyword_search <- function(query_terms, census_vector_list, interactive = TRUE) {
  sample_vector_list <- census_vector_list$details
  vector_words <- strsplit(gsub("\\s+"," ",gsub("[[:punct:]]"," ",tolower(sample_vector_list))), split = " ")
  clean_vector_list <- lapply(vector_words, function(x) paste(unique(x), collapse = " "))

  query_words <- paste(unlist(strsplit(tolower(query_terms), "[^a-z]+")), collapse = "|")
  index_matches <- grep(query_words, clean_vector_list, ignore.case = TRUE)

  ret_matches <- clean_vector_list[index_matches]

  if (length(ret_matches) == 0) {
    warning(
      "No matches found. Please check spelling and try again or consider using semantic search instead.\nSee ?find_census_vectors() for more details.\n\nAlternatively, you can launch the Censusmapper web API in a browser by calling explore_census_vectors(dataset)",
      call. = FALSE
    )
  } else {
    res <- regmatches(ret_matches, gregexpr(paste0("\\<",gsub("\\|","\\\\>|\\\\<",query_words),"\\>"), ret_matches, ignore.case = TRUE))
    res_df <-
      data.frame(results = sample_vector_list[index_matches], n_match = lengths(res))

    top_res <-
      census_vector_list[which(census_vector_list$details %in% res_df[which(res_df$n_match == max(res_df$n_match)), ]$results), ]
    other_res <-
      census_vector_list[which(census_vector_list$details %in% res_df[which(res_df$n_match != max(res_df$n_match)), ]$results), ]
    if (length(res_df$results) == length(top_res$vector))
      return(top_res)
    else if (!interactive) {return(top_res)}
    else if (length(other_res$vector) > 0 & interactive){
      print(top_res)
      show_more <-
        utils::menu(
          c("Yes", "No"),
          title = paste0(
            "\nThere are ",
            length(other_res$vector),
            " additional keyword matches with less precision. Show more?"
          )
        )
      if (show_more == 1) {return(other_res)} else {
        message(paste0("Showing top ",length(top_res$vector)," results only"))
        return(top_res)}
    }
  }
}

#' Interactively browse Census variables and regions on Censusmapper.ca in a new browser window
#'
#' @description Finding the right Census variables or regions can be complicated.
#' \code{explore_census_vectors(dataset)} and \code{explore_census_regions(dataset)} will open a
#' new browser page or tab to an interactive Census variable and region exploration and selection
#' tool on the \href{https://censusmapper.ca/api}{Censusmapper.ca website}. Interactive
#' tools available for the CA16, CA11, CA06, and CA01 Census datasets and geographies.
#'
#' @param dataset The dataset to query for available vectors, e.g. \code{'CA16'}.
#' Interactive tools available for the CA16, CA11, CA06, and CA01 Census datasets and
#' geographies.
#'
#' @export
#'
#' @examples
#'\dontrun{
#'
#' explore_census_vectors(dataset = "CA16")
#'
#' explore_census_regions(dataset = "CA11")
#'
#' }
explore_census_vectors <- function(dataset = "CA16") {
  dataset <- translate_dataset(dataset)
  message("Opening interactive census variable explorer at censusmapper.ca/api in the browser")
  utils::browseURL(paste0("https://censusmapper.ca/api/",dataset,"#api_variable"))
}

#' Interactively browse Census variables and regions on Censusmapper.ca in a new browser window
#'
#' @description Finding the right Census variables or regions can be complicated.
#' \code{explore_census_vectors(dataset)} and \code{explore_census_regions(dataset)} will open a
#' new browser page or tab to an interactive Census variable and region exploration and selection
#' tool on the \href{https://censusmapper.ca/api}{Censusmapper.ca website}. Interactive
#' tools available for the CA16, CA11, CA06, and CA01 Census datasets and geographies.
#'
#' @param dataset The dataset to query for available vectors, e.g. \code{'CA16'}.
#' Interactive tools available for the CA16, CA11, CA06, and CA01 Census datasets and
#' geographies.
#'
#' @export
#'
#' @examples
#'\dontrun{
#'
#' explore_census_vectors(dataset = "CA16")
#'
#' explore_census_regions(dataset = "CA11")
#'
#' }
explore_census_regions <- function(dataset = "CA16") {
  dataset <- translate_dataset(dataset)
  message("Opening interactive census region explorer at censusmapper.ca/api in the browser")
  utils::browseURL(paste0("https://censusmapper.ca/api/",dataset,"#api_region"))
}

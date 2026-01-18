#' Visualize Census vector hierarchies as ASCII tree
#'
#' @description Displays an ASCII tree representation of the hierarchical structure
#' of Census vectors. This helps users understand the relationship between parent
#' and child vectors when selecting variables for their analysis.
#'
#' @param vector A Census vector code (e.g., "v_CA16_2510") or a filtered tibble
#'   as returned from \code{list_census_vectors}.
#' @param dataset The dataset to query for vector information, e.g. \code{"CA16"}.
#'   Only required if \code{vector} is a character string.
#' @param max_depth Maximum depth of the tree to display. Default is \code{NA}
#'   which shows the entire hierarchy.
#' @param show_type Logical. If \code{TRUE}, shows the type (Total/Male/Female)
#'   next to each vector. Default is \code{FALSE}.
#' @param quiet When \code{TRUE}, suppress messages. Default is \code{FALSE}.
#'
#' @return Invisibly returns a tibble of the vectors displayed in the tree.
#'   The tree is printed to the console as a side effect.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Visualize the age hierarchy for 2016 Census
#' visualize_vector_hierarchy("v_CA16_2510", dataset = "CA16")
#'
#' # Show only first two levels with type information
#' visualize_vector_hierarchy("v_CA16_2510", dataset = "CA16",
#'                            max_depth = 2, show_type = TRUE)
#'
#' # Using a vector tibble from list_census_vectors
#' library(dplyr)
#' list_census_vectors("CA16") %>%
#'   filter(vector == "v_CA16_2510") %>%
#'   visualize_vector_hierarchy()
#' }
visualize_vector_hierarchy <- function(vector, dataset = NULL, max_depth = NA,
                                       show_type = FALSE, quiet = FALSE) {

  # Handle input - either character vector code or tibble

  if (is.character(vector)) {
    if (is.null(dataset)) {
      # Try to extract dataset from vector code
      dataset <- tryCatch({
        dataset_from_vector_list(vector)
      }, error = function(e) {
        stop("Cannot determine dataset from vector code. Please provide the 'dataset' parameter.",
             call. = FALSE)
      })
    }
    dataset <- translate_dataset(dataset)

    # Get the root vector info
    all_vectors <- list_census_vectors(dataset, quiet = TRUE)
    root_info <- all_vectors %>% dplyr::filter(.data$vector == !!vector)

    if (nrow(root_info) == 0) {
      stop(paste0("Vector '", vector, "' not found in dataset '", dataset, "'."),
           call. = FALSE)
    }
  } else if (inherits(vector, "data.frame")) {
    # It's a tibble from list_census_vectors
    if (nrow(vector) == 0) {
      stop("Empty vector tibble provided.", call. = FALSE)
    }
    dataset <- attr(vector, "dataset")
    if (is.null(dataset)) {
      dataset <- dataset_from_vector_list(vector)
    }
    dataset <- translate_dataset(dataset)
    root_info <- vector
    all_vectors <- list_census_vectors(dataset, quiet = TRUE)
  } else {
    stop("'vector' must be a character string or a tibble from list_census_vectors().",
         call. = FALSE)
  }

  # Use first row if multiple vectors provided
  if (nrow(root_info) > 1) {
    if (!quiet) message("Multiple vectors provided. Using first vector: ", root_info$vector[1])
    root_info <- root_info[1, ]
  }

  root_vector <- root_info$vector

  # Get all children
  children <- child_census_vectors(root_vector, leaves_only = FALSE, max_level = max_depth)

  # Combine root with children
  tree_vectors <- dplyr::bind_rows(root_info, children)

  # Build the tree structure
  if (!quiet) {
    message("Vector hierarchy for ", root_vector, " (", dataset, "):\n")
  }

  # Print root
  root_label <- root_info$label
  if (show_type) {
    root_label <- paste0(root_label, " [", root_info$type, "]")
  }
  cat(paste0(root_vector, ": ", root_label, "\n"))

  # Recursively print children
  print_children(root_vector, all_vectors, tree_vectors, prefix = "",
                 show_type = show_type, current_depth = 1, max_depth = max_depth)

  invisible(tree_vectors)
}

#' Internal function to recursively print tree children
#' @noRd
print_children <- function(parent_vector, all_vectors, tree_vectors, prefix,
                           show_type, current_depth, max_depth) {

  # Check depth limit

if (!is.na(max_depth) && current_depth > max_depth) {
    return(invisible(NULL))
  }

  # Find direct children of this parent that are in our tree
  direct_children <- tree_vectors %>%
    dplyr::filter(.data$parent_vector == !!parent_vector)

  if (nrow(direct_children) == 0) return(invisible(NULL))

  n_children <- nrow(direct_children)

  for (i in seq_len(n_children)) {
    child <- direct_children[i, ]
    is_last <- (i == n_children)

    # Determine connectors
    connector <- if (is_last) "\u2514\u2500\u2500 " else "\u251C\u2500\u2500 "
    child_prefix <- if (is_last) "    " else "\u2502   "

    # Build label
    label <- child$label
    if (show_type) {
      label <- paste0(label, " [", child$type, "]")
    }

    # Check if this is a leaf (no children in tree)
    has_children_in_tree <- any(tree_vectors$parent_vector == child$vector, na.rm = TRUE)
    leaf_indicator <- if (!has_children_in_tree) " (leaf)" else ""

    # Print this child
    cat(paste0(prefix, connector, child$vector, ": ", label, leaf_indicator, "\n"))

    # Recursively print grandchildren
    print_children(child$vector, all_vectors, tree_vectors,
                   prefix = paste0(prefix, child_prefix),
                   show_type = show_type,
                   current_depth = current_depth + 1,
                   max_depth = max_depth)
  }

  invisible(NULL)
}

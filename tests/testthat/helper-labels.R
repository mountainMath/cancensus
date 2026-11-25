# The default of `labels` in get_census() is planned to change from "detailed" to "labelled". Tests that check the
# output of a particular labelling scheme set `labels` explicitly, tests of the default compare against this,
# read from the signature of get_census() so that the tests keep working when the default changes.
default_labels <- function() {
  rlang::with_options(cancensus.labels = NULL, eval(formals(get_census)$labels))
}

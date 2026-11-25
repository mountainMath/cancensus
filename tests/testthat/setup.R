# The tests expect the package defaults, so options set e.g. in a user's .Rprofile shouldn't change the results
rlang::local_options(cancensus.labels = NULL, cancensus.language = NULL, .frame = teardown_env())

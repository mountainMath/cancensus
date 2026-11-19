# Run code with a temporary home directory, so that tests don't touch the real .Renviron
with_test_home <- function(code) {
  home <- tempfile("cancensus_test_home")
  dir.create(home)
  old <- Sys.getenv(c("HOME", "CM_API_KEY", "CM_CACHE_PATH"), unset = NA)
  Sys.setenv(HOME = home)
  on.exit({
    for (name in names(old)) {
      if (is.na(old[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, as.list(old[name]))
    }
    unlink(home, recursive = TRUE)
  })
  force(code)
}

renviron <- function() readLines(file.path(Sys.getenv("HOME"), ".Renviron"))

test_that("installing the API key creates the .Renviron file", {
  with_test_home({
    suppressMessages(set_cancensus_api_key("CensusMapper_test", install = TRUE))
    expect_equal(renviron(), "CM_API_KEY='CensusMapper_test'")
    expect_equal(Sys.getenv("CM_API_KEY"), "CensusMapper_test")
    expect_false(file.exists(file.path(Sys.getenv("HOME"), ".Renviron_backup")))
  })
})

test_that("an installed setting only gets replaced when asked to", {
  with_test_home({
    suppressMessages(set_cancensus_api_key("CensusMapper_old", install = TRUE))
    expect_error(suppressMessages(set_cancensus_api_key("CensusMapper_new", install = TRUE)),
                 class = "cancensus_api_key_exists")
    expect_equal(renviron(), "CM_API_KEY='CensusMapper_old'")
    suppressMessages(set_cancensus_api_key("CensusMapper_new", install = TRUE, overwrite = TRUE))
    expect_equal(renviron(), "CM_API_KEY='CensusMapper_new'")

    suppressMessages(set_cancensus_cache_path("/tmp/old", install = TRUE))
    expect_error(suppressMessages(set_cancensus_cache_path("/tmp/new", install = TRUE)),
                 class = "cancensus_cache_path_exists")
    suppressMessages(set_cancensus_cache_path("/tmp/new", install = TRUE, overwrite = TRUE))
    expect_equal(renviron(), c("CM_API_KEY='CensusMapper_new'", "CM_CACHE_PATH='/tmp/new'"))
  })
})

test_that("installing a setting keeps the other lines of the .Renviron file", {
  with_test_home({
    path <- file.path(Sys.getenv("HOME"), ".Renviron")
    # no newline at the end of the file, and lines that mention the variable without setting it
    cat("# CM_API_KEY goes below\nOTHER_CM_API_KEY=abc\nCM_API_KEY='CensusMapper_old'\nLAST=1", file = path)
    suppressMessages(set_cancensus_api_key("CensusMapper_new", install = TRUE, overwrite = TRUE))
    expect_equal(renviron(), c("# CM_API_KEY goes below", "OTHER_CM_API_KEY=abc", "LAST=1",
                               "CM_API_KEY='CensusMapper_new'"))
  })
})

test_that("the backup of the .Renviron file is the version before the latest change", {
  with_test_home({
    backup <- file.path(Sys.getenv("HOME"), ".Renviron_backup")
    writeLines("FIRST=1", file.path(Sys.getenv("HOME"), ".Renviron"))
    suppressMessages(set_cancensus_api_key("CensusMapper_test", install = TRUE))
    expect_equal(readLines(backup), "FIRST=1")
    suppressMessages(set_cancensus_cache_path("/tmp/cache", install = TRUE))
    expect_equal(readLines(backup), c("FIRST=1", "CM_API_KEY='CensusMapper_test'"))
  })
})

test_that("settings for the session don't touch the .Renviron file", {
  with_test_home({
    suppressMessages(set_cancensus_api_key("CensusMapper_test"))
    suppressMessages(set_cancensus_cache_path("/tmp/cache"))
    expect_equal(Sys.getenv("CM_API_KEY"), "CensusMapper_test")
    expect_equal(Sys.getenv("CM_CACHE_PATH"), "/tmp/cache")
    expect_false(file.exists(file.path(Sys.getenv("HOME"), ".Renviron")))
  })
})

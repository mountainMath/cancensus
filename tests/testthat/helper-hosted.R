# A small parquet file like the hosted ones, with footer metadata
write_attributes_file <- function(path, uid = "1001", population = 5L) {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  columns <- paste0('{"DBUID":{"label_en":"Block ID","label_fr":"IDU de l\'\'îlot"},',
                    '"DBPOP2021":{"label_en":"Block population"}}')
  DBI::dbExecute(con, paste0(
    "COPY (SELECT '", uid, "' AS DBUID, ", population, "::INTEGER AS DBPOP2021, 'x' AS NOTE) TO ",
    DBI::dbQuoteString(con, path), " (FORMAT parquet, KV_METADATA {",
    "attribution_en: 'Statistics Canada, a test file', licence_en: 'Test licence', ",
    "title_fr: 'Fichier des attributs géographiques', primary_key: '[\"DBUID\"]', ",
    "columns: '", columns, "'})"))
  path
}

# Serve the file at `source` in place of the hosted file, and fail on any other request
mock_hosted_file <- function(source, env = parent.frame()) {
  local_mocked_bindings(
    fetch_hosted_file = function(url, path, quiet = FALSE) {
      file.copy(source, path, overwrite = TRUE)
      fake_response(200)
    },
    .env = env
  )
}

# Report a failure on any download attempt
mock_no_download <- function(env = parent.frame()) {
  local_mocked_bindings(
    fetch_hosted_file = function(url, path, quiet = FALSE) stop("Unexpected download of ", url),
    .env = env
  )
}

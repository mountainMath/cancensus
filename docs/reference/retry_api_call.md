# Retry an API call with exponential backoff

Retry an API call with exponential backoff

## Usage

``` r
retry_api_call(call_fn, max_retries = 3, quiet = FALSE)
```

## Arguments

- call_fn:

  A function that performs the API call and returns an httr response

- max_retries:

  Maximum number of retry attempts (default: 3)

- quiet:

  If TRUE, suppress retry messages

## Value

The httr response object

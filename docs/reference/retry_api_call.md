# Make an API call, retrying on transient failures

Retries with exponential backoff, honouring a \`Retry-After\` header of
up to 60 seconds. Responses saying the server is busy (HTTP 503 or 429)
get retried up to \`max(retries, busy_retries)\` times, other transient
HTTP errors (408, 502, 504) and network errors up to \`retries\` times.
Other responses, including other server errors, are returned as they
are.

## Usage

``` r
retry_api_call(
  call_fn,
  retries = 0,
  busy_retries = 2,
  quiet = FALSE,
  language = NULL
)
```

## Arguments

- call_fn:

  Function making the request and returning an httr response

- retries:

  Number of further attempts on transient failures

- busy_retries:

  Minimum number of further attempts when the server is busy

- quiet:

  Whether to suppress messages about retries. Waits of more than 2
  seconds for a busy server are always reported.

- language:

  Language of the messages, see `message_language`

## Value

The last httr response

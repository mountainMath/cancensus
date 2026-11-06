# User agent sent with CensusMapper API requests

Identifies the package and its version to the CensusMapper API, in the
form \`cancensus/\<version\> httr/\<version\> R/\<version\>\`. Packages
or tools building on \*\*cancensus\*\* can identify themselves by
setting the \`cancensus.user_agent_prefix\` option, e.g.
\`options(cancensus.user_agent_prefix = "mypackage/1.2.3")\`, which gets
prepended to the user agent string.

## Usage

``` r
cancensus_user_agent()
```

## Value

A character string

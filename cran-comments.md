## Submission

First submission for this package. This is the first CRAN package I've developed so I apologize in advance if I have made any oversight. Thanks for your time. 

## Test environments
* local OS X install, R 3.4.3
* ubuntu 14.04 (on travis-ci), R 3.4.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs and two NOTES:

 - A NOTE came up for "Maintainer:..." which I understand can be ignored
 - A NOTE came up specifying that 'README.md' or 'NEWS.md' cannot be checked without 'pandoc' being installed; however, this issue is not clear to me as 'pandoc' is definitely installed and those documents have been built. I have [documented this issue](https://stackoverflow.com/questions/47900037/cran-notes-that-files-cannot-be-checked-without-pandoc-being-installed) and hoping someone can clarify, but it does not come up when building on other environments.

## Reverse dependencies
This is a new release and there are no dependencies.

## Vignettes
Vignettes require API authentification and code chunks are not evaluated when system variables are not enabled. Vignettes are fully built and evaluated locally and on Travis with [log results](https://travis-ci.org/mountainMath/cancensus).

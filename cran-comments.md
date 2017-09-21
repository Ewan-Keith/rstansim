## Resubmission
This is a resubmission. In this version I have:

* Shortened the package title to remove redundancy.

* Put software package names in the package description inside single quotes.

* Added the test environment 'Windows Server 2012 (on ci.appveyor), R devel'.

* Not removed the dependency on the deprecated 'doSNOW' package. The previous reviewer requested that the 'parallel' package be used instead if possible. The 'parallel' and 'doParallel' options do not support printed progress bars whilst the 'doSNOW' package does. Given the long runtimes that the 'fit_models()' function is likely to have, the presence of a progress bar to inform users of progress, and to reassure that the function has not stuck, is considered worth the use of the deprecated package in this case over the available alternatives.

## Test environments
* local ubuntu 16.04, R 3.4.1
* ubuntu 14.04 (on travis-ci), R 3.4.1
* ubuntu 14.04 (on travis-ci), R devel
* Windows Server 2012 (on ci.appveyor), R 3.4.1
* Windows Server 2012 (on ci.appveyor), R devel
* win-builder, R devel

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.

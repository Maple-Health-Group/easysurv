## Resubmission
I confirm there are no references describing the methods in our package.

This is a resubmission. In this version I have:

* Removed wrapping of examples in if(FALSE){}. In write_to_xl.Rd,
we now use \\dontrun{} to prevent an Excel file from being created, saved and 
launched.

* Updated the Description field in DESCRIPTION, correcting the erroneous 
reference to 'flexsurvspline' as a package; it is a function. The same error was
addressed in the README file and in fit_models() documentation.

* Removed dependence on the 'fs' package. fs::path_package() was replaced with
system.file().

* Wrapped external functions in square brackets in roxygen2 documentation. This 
is to support auto-linking to external documentation.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

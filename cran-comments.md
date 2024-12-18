## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* Installed size is > 1MB.  The reason for this, is that the package is meant to ingest a specific JSON
    file in the Clinical Data Standards process (CDISC ARS metadata), which has a specific structure
    and is a large JSON file (2.2MB).  I provide this as part of the package, as it is used as an           example. There are also 2xcsv datasets in the inst/extdata folder, which are not directly used in 
    the package, but are usefule data to test on for the user.  There was initially a 3rd of 8.5MB, 
    which I removed for now, but would want to understand if it can be added in future.  
    
* Unable to verify current time - I'm not sure how to resolve this.  Please let me know if I missed
    something.
    
* No visible binding for global variable - I'm not sure how to resolve this.  I've tried using .data,
    but if I understand correctly it would need to be done on every variable, and I wasn't sure this is     correct approach.  Please let me know if I missed something.

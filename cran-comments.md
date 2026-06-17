## R CMD check results

0 errors | 0 warnings | 2 notes

The 2 notes are:

* `checking for future file timestamps`: unable to verify current time — a known
  environment artefact on this machine; not reproducible on CRAN infrastructure.

* `checking top-level files`: Non-standard file/directory found at top level:
  'CLAUDE.md' — this is an AI-assistant context file excluded from the package
  build via `.Rbuildignore`. It will not appear in the installed package.

## Downstream dependencies

None.

## Test environments

* Local Windows 11, R 4.4.1
* GitHub Actions: ubuntu (release, oldrel-1, devel), macOS (release), Windows (release)

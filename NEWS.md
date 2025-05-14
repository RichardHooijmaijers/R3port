# R3port 0.1.1

First version of the package

# R3port 0.2.3

- Improvements in compilation of latex files
- More responsive plots within html files
- Added tabular environment for latex tables and listings
- Added possibility to include labels for latex output
- Added possibility to define the location of the caption in latex output
- Added possibility to rotate figures in latex output
- Added possibility to delete figures in case they were already created under the same name

# R3port 0.2.4

- Added possibility to use a subtext in figure captions
- Added possibility to define a floating environment for latex tables
- For longtable changed titles to 'continued' in case it spans more than 1 page
- Changed location of a tablenote when longtable is used, to after end longtable (ensures that table is not resized with long notes)

# R3port 0.2.5

- Made necessary changes for CRAN following [this issue](https://github.com/r-lib/roxygen2/issues/1491)

# R3port 0.3.0

- Made adaptations for captions in latex (continued captions and sub caption) 
- Updated documentation, used md for roxygen2
- Included tests to improve coverage
- Minor improvements and bug fixes

# R3port 0.3.1

- Update for testthat test due to CRAN check

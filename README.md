# Survey statistics in a database

Multistage surveys can give rise to moderately large data sets (tens of millions of
rows). Most current software for survey analysis reads the data into memory, the
survey package in R provides fairly comprehensive analysis features for complex
surveys which are small enough to fit into memory easily, however, most of the
computations can actually be expressed as database operations. There is already a
similar approach with the sqlsurvey package in R which performs substantial computation
in SQL in the database, importing only small summary tables into R, this
approach scales to very large surveys such as the American Community Survey and
the Nationwide Emergency Department Sample, but this approach causes compatible
issues with different types of databases. Therefore, in this project I will work on
implementing R functions and testing some survey computations using the dplyr
and dbplyr R package as a database interface.

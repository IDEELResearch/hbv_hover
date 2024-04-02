# renv helper

#install.packages("renv")
# renv::init()
# after updating R, restore the environment using the previously written lockfile. packages will necessarily be out of date
renv::restore()

# same as restore, but doesn't respect the lockfile and takes the latest versions from CRAN
renv::install()
# records current state of the R environment. if packages are not installed (eg after an R version update) or not updated, a set of options will be returned with options to just give the status, update the packages and try again, or cancel
renv::snapshot()

# other functions 
renv::rebuild()
renv::dependencies()
renv::status()
# if packages have trouble, might be best to install individually, like:
install.packages("xfun")

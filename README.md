# https://rstudio.github.io/renv/index.html

# Project uses renv:
```
The packages (and all the metadata needed to reinstall them) are recorded into a lockfile, renv.lock, and a .Rprofile ensures that the library is used every time you open that project.

As you continue to work on your project, you will install and upgrade packages, either using install.packages() and update.packages or renv::install() and renv::update(). After you’ve confirmed your code works as expected, use renv::snapshot() to record the packages and their sources in the lockfile.

Later, if you need to share your code with someone else or run your code on new machine, your collaborator (or you) can call renv::restore() to reinstall the specific package versions recorded in the lockfile.
```


# hbv_hover
Analysis of households and individuals enrolled in HOVER-HBV study in Kinshasa, DRC


localCRAN <- path.expand("~/R/local-cran")

oldRepos <- getOption("repos")
cranURI <- paste("file://", normalizePath(localCRAN, winslash = "/"), sep = "")
options(repos = c(oldRepos, LocalCRAN= cranURI))




#Rscript -e "packrat::bundle(project='trunk', file='haplostats.tar.gz', include.vcs.history = FALSE, overwrite = TRUE)"
export ICUDT_DIR=${PWD}/data
Rscript setup.R --bootstrap-packrat

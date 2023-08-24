tar -xzf R_packages.tar.gz
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R
export R_LIBS=$PWD/packages
Rscript -e 'install.packages(c("ecp","tidyr","gSeg"), repos="https://cloud.r-project.org")'
tar -xzf R413.tar.gz
# (optional) if you have a set of packages (created in Part 1), untar them also
tar -xzf R_packages.tar.gz
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R
export R_LIBS=$PWD/packages
Rscript -e 'install.packages(c("ecp","tidyr","gSeg"), repos="https://cloud.r-project.org")'
Rscript energy_test.R 0 1
ls
exit

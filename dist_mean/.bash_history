ls
tar -xzf R_packages.tar.gz
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R
export R_LIBS=$PWD/packages
Rscript distributional_data.R 1 1
cat distributional_data.R 
R
tar -xzf R413.tar.gz
# (optional) if you have a set of packages (created in Part 1), untar them also
tar -xzf R_packages.tar.gz
export PATH=$PWD/R/bin:$PATH
export RHOME=$PWD/R
export R_LIBS=$PWD/packages
R 
Rscript distributional_data.R 1 1
nano distributional_data.R 
Rscript distributional_data.R 1 1
s
R
exit

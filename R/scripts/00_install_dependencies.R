

# The easiest way to install all dependencies is to open the dependencies.R file with RStudio and accept it's suggestion to install missing packages.

# I had issues with installing randomForest, and I used this work around:

urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-12.tar.gz"

install.packages(urlPackage, repos=NULL, type="source")
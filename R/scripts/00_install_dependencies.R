

# Selective Openness Code Repository --------------------------------------
# Code Repository for the "Selective Openness" paper
# Copyright (C) 2022 by Pedro Nascimento de Lima
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# Read the README.md file for usage instructions.
# -------------------------------------------------------------------------


# install packages --------------------------------------------------------

# I had issues with installing randomForest, and I used this work around:

urlPackage <- "https://cran.r-project.org/src/contrib/Archive/randomForest/randomForest_4.6-12.tar.gz"

install.packages(urlPackage, repos=NULL, type="source")
#===============================================================================
# LOAD LIBRARIES
# This script sets up a user-specific library path, ensures the required 
# packages are installed and loaded, and provides comments for each package to
# explain its purpose.
#===============================================================================

# Define the user-specific library path
user_lib <- "C:/Users/rchasmar/R/win-library/4.4"

# Create the directory if it doesn't exist
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE)
}

# Add the user-specific library path to the library paths
.libPaths(user_lib)

# Named list of required packages with comments
packages <- list(
  TrenchR = "TrenchR: Tools for Microclimate and Biophysical Ecology",
  tidyverse = "tidyverse: Easily Install and Load the 'Tidyverse'",
  StreamMetabolism = paste0("StreamMetabolism: Calculate Single Station ", 
                            "Metabolism from Diurnal Oxygen Curves"),
  LakeMetabolizer = paste0("LakeMetabolizer: Tools for the Analysis of ", 
                           "Ecosystem Metabolism"),
  imputeTS = "imputeTS: Time Series Missing Value Imputation",
  padr = "padr: Quickly Get Datetime Data Ready for Analysis"
)

# Function to check if a package is installed, and install it if not
install_if_missing <- function(pkg, comment) {
  cat("#", comment, "\n")
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, lib = user_lib, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Loop through the named list and apply the function
for (pkg in names(packages)) {
  install_if_missing(pkg, packages[[pkg]])
}


#===============================================================================
# LOAD LIBRARIES
# This script sets up a user-specific library path, ensures the required 
# packages are installed and loaded, and provides comments for each package to
# explain its purpose.
#===============================================================================

# Define the user-specific library path
user_lib <- "C:/Users/rchasmar/R/win-library/4.4"

# Create the directory if it doesn't exist
if (!dir.exists(
       paths = user_lib
     )) {
  dir.create(
         path = user_lib,
    recursive = TRUE
  )
}

# Add the user-specific library path to the library paths
.libPaths(
  new = user_lib
)

# Named list of required packages with comments
packages <- list(
                       TrenchR = paste0(
                                   "TrenchR: Tools for Microclimate and ",
                                   "Biophysical Ecology"
                                 ),
                     tidyverse = paste0(
                                   "tidyverse: Easily Install and Load the ",
                                   "'Tidyverse'"
                                 ),
              StreamMetabolism = paste0(
                                   "StreamMetabolism: Calculate Single Station", 
                                   " Metabolism from Diurnal Oxygen Curves"
                                 ),
               LakeMetabolizer = paste0(
                                   "LakeMetabolizer: Tools for the Analysis of", 
                                   " Ecosystem Metabolism"
                                 ),
                      imputeTS = paste0(
                                   "imputeTS: Time Series Missing Value ",
                                   "Imputation"
                                 ),
                          padr = paste0(
                                   "padr: Quickly Get Datetime Data Ready for ",
                                   "Analysis"
                                 ),
                          plyr = paste0(
                                   "plyr: Tools for Splitting, Applying and ",
                                   "Combining Data"
                                 )
            )

# Function to check if a package is installed, and install it if not
install_if_missing <- function(
  pkg,
  comment
) {
  cat(
    "#",
    comment,
    "\n"
  )
  if (!require(
                package = pkg,
         character.only = TRUE
       )) {
    install.packages(
              pkgs = pkg,
               lib = user_lib,
      dependencies = TRUE
    )
    library(
             package = pkg,
      character.only = TRUE
    )
  }
}

# Loop through the named list and apply the function
for (pkg in names(
              x = packages
            )) {
  install_if_missing(
        pkg = pkg,
    comment = packages[[pkg]]
  )
}


# Set working directory
setwd(paste0("C:/Users/rchasmar/Documents/Brownlee Reservoir ecosystem ",
             "metabolism analysis/USGS-GitHub-Brownlee-Reservoir-Metabolism-",
             "Directory"))

# Source the library loading script
source("code/load_libraries.R")

# Source the functions
source("code/functions.R")

#===============================================================================
# LOAD DATA
# This section loads and processes ecosystem metabolism data for Brownlee 
# Reservoir. It handles file paths, reads data from CSV and RData files, and 
# dynamically assigns variables based on river miles and depths.
#===============================================================================

# Define the common part of the file path
base_path <- paste0("C:/Users/rchasmar/Documents/Brownlee Reservoir ",
                    "ecosystem metabolism analysis/data/")

# Define the specific file names
file_names <- c(
  "PPR286_2019-2023.RData",
  "PPR300_2019-2023_all raw data.RData",
  "PPR318_2019-2023_all raw data.RData",
  "Profile_WithPARAndTime_ToUSGS_08082024.csv",
  "PPRBuoys_BrownleeDam_Weather.RData",
  "windspeed.ppr_weather@snake_river_318_mc.20210101.csv"
)

# Combine the base path with the file names to create full file paths
file_paths <- file.path(base_path, file_names)

# Define the river miles corresponding to the first three RData files
river_miles <- c("286", "300", "318")

# Define the depths for which data is available
depths <- c("01", "03", "06", "10", "15", "21", "30", "40")

# Read the profiles data from the CSV file
profiles <- read.csv(file_paths[4], header = TRUE)

# Loop through each river mile and load the corresponding RData file
for (i in seq_along(river_miles)) {
  # Load the RData file for the current river mile
  load(file_paths[i])
  
  # Split the profiles data by river mile and assign to a variable
  # Split function divides profiles into subsets based on River.Mile column
  assign(
    paste0("ppr", river_miles[i], "_", "prf"), 
    split(profiles, profiles$River.Mile)[[river_miles[i]]]
  )

  # Loop through each depth and assign the corresponding data
  for (depth in depths) {
    # Check if the data frame for the current depth exists in the environment
    if (exists(paste0("PPR", as.numeric(depth)))) {
      # Assign dataframe to new variable with name based on river mile and depth
      assign(
        paste0("ppr", river_miles[i], "_", depth), 
        get(paste0("PPR", as.numeric(depth)))
      )
    }
  }
  
  # Remove previous objects that match the pattern "^PPR" to avoid conflicts
  rm(list = ls(pattern = "^PPR"))
}

# Load the weather data from the specified RData file
# The load function loads the data into the environment
weather <- load(file_paths[5])
buoy_weather <- PPRWeather
  # Split the buoy_weather dataframe by 'Variable'
  buoy_wnd <- split(buoy_weather, buoy_weather$Variable)[["WindSpeed"]]
    # Further split the wind speed data by 'Location'
    ppr286_wnd <- split(buoy_wnd, buoy_wnd$Location)[["286"]]
    ppr300_wnd <- split(buoy_wnd, buoy_wnd$Location)[["300"]]
    ppr318_wnd <- split(buoy_wnd, buoy_wnd$Location)[["318"]]

  buoy_solrad <- split(buoy_weather, buoy_weather$Variable)[["SolRad"]]
    # Further split the solar radiation data by 'Location'
    ppr286_solrad <- split(buoy_solrad, buoy_solrad$Location)[["286"]]
    ppr300_solrad <- split(buoy_solrad, buoy_solrad$Location)[["300"]]
    ppr318_solrad <- split(buoy_solrad, buoy_solrad$Location)[["318"]]

  buoy_par <- split(buoy_weather, buoy_weather$Variable)[["PARLumin"]]
    # Further split the solar radiation data by 'Location'
    ppr286_par <- split(buoy_par, buoy_par$Location)[["286"]]
    ppr300_par <- split(buoy_par, buoy_par$Location)[["300"]]
    ppr318_par <- split(buoy_par, buoy_par$Location)[["318"]]

dam_sol_rad <- BrownleeDamSolRad
dam_wind <- BrownleeDamWindV

# Read the wind speed data from the CSV file, skipping the first 14 rows
# Select only the relevant columns: Timestamp and Value
ppr318_wnd_2021 <- read.csv(
  file = file_paths[6],
  header = TRUE, skip = 14
)[c("Timestamp..UTC.07.00.", "Value")]

# Remove all dataframes with names starting with a capital letter
rm(list = ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame) & 
               grepl("^[A-Z]", ls())])

#===============================================================================
# PROCESS TIME SERIES AND PROFILE DATAFRAMES
# Organize columns, pad datetime, interpolate gaps and subset to 10-min interval
#===============================================================================

# New datetime values to append
new_dates <- as.POSIXct(
  c("2017-01-01 00:00:00", "2023-12-31 23:50:00"), 
  format = "%Y-%m-%d %H:%M:%S"
)

# Define the patterns
pattern_ts <- "^ppr[0-9]+_[0-9]+$"
pattern_prf <- "^ppr[0-9]+_prf$"

# Rename ppr318_wnd_2021 dataframe columns and format datetime
colnames(ppr318_wnd_2021) <- c("DateTime", "Value")
ppr318_wnd_2021$DateTime <- as.POSIXct(
  ppr318_wnd_2021$DateTime, 
  format = "%Y-%m-%d %H:%M:%S"
)

# Create lists of dataframes that match patterns
dataframes_ts <- mget(ls(pattern = pattern_ts), envir = .GlobalEnv)
dataframes_prf <- mget(ls(pattern = pattern_prf), envir = .GlobalEnv)
dataframes_wthr <- list(
  ppr286_wnd    = ppr286_wnd, 
  ppr300_wnd    = ppr300_wnd, 
  ppr318_wnd    = ppr318_wnd,
  ppr318_wnd_2021 = ppr318_wnd_2021,
  ppr286_solrad = ppr286_solrad, 
  ppr300_solrad = ppr300_solrad, 
  ppr318_solrad = ppr318_solrad, 
  ppr286_par    = ppr286_par, 
  ppr300_par    = ppr300_par, 
  ppr318_par    = ppr318_par
)

# Loop through each dataframe in dataframes_ts
for (name in names(dataframes_ts)) {
  df <- dataframes_ts[[name]]
  
  # 1. Select the specified columns
  df <- df[, c('DateTime', 'DO', 'WTemp')]
  
  # 2. Rename the columns
  colnames(df) <- c("datetime", "do.obs", "wtr")
  
  # 3. Apply the pad function
  df <- pad(df, by = 'datetime', interval = 'min', break_above = 5)
  
  # 4. Remove duplicated rows based on the datetime column
  df <- df[!duplicated(df$datetime), ]
  
  # 5. Interpolate gaps
  df <- interpolate_gaps_spline(df, "do.obs", 10)
  df <- interpolate_gaps_spline(df, "wtr", 10)
  
  # 6. Subset minutes
  df <- subset_minutes(df)
 
  # 7. Create a new dataframe with the same column names and NA values
  new_rows <- data.frame(matrix(NA, nrow = 2, ncol = ncol(df)))
  names(new_rows) <- names(df)

  # 8. Assign the new datetime values to the DateTime column
  new_rows$datetime <- new_dates

  # 9. Append new rows to the beginning and end of the dataframe
  df <- rbind(new_rows[1, ], df, new_rows[2, ])

  # 10. Pad the dataframe to fill in missing datetime values
  df <- pad(df, by = 'datetime', interval = '10 min')

  # 11. Set row names
  rownames(df) <- seq(1, nrow(df), 1)

  # Update the dataframe in the list
  dataframes_ts[[name]] <- df
}

# Loop through each dataframe in dataframes_prf
for (name in names(dataframes_prf)) {
  df <- dataframes_prf[[name]]
  
  # 1. Select the specified columns
  df <- df[, c('DateTime', 'Date', 'DO', 'WTemp', 'PAR', 'Depth')]
  
  # 2. Rename the columns
  colnames(df) <- c("datetime", "date", "do.obs", "wtr", "irr", "depth")
  
  # 3. Set row names
  rownames(df) <- seq(1, nrow(df), 1)
  
  # 4. Format datetime
  df$datetime <- as.POSIXct(df$datetime, format = "%m/%d/%Y %H:%M")
  
  # 5. Convert the 'date' column to Date objects
  df$date <- as.Date(df$date, format = "%m/%d/%Y")

  # Update the dataframe in the list
  dataframes_prf[[name]] <- df
}

# Loop through each dataframe in dataframes_wthr
for (name in names(dataframes_wthr)) {
  df <- dataframes_wthr[[name]]

  # 1. Select the specified columns
  df <- df[, c('DateTime', 'Value')]

  # 2. Rename the columns
  colnames(df) <- c("datetime", "value")

  # 3. Round datetime to the nearest minute
  df <- mutate(df, datetime = round_date(datetime, "1 minute"))

  # 4. Pad the dataframe to fill in missing datetime values
  df <- pad(df, by = 'datetime', interval = 'min', break_above = 5)
  
  # 5. Remove duplicate datetime entries
  df <- df[!duplicated(df$datetime), ]

  # 6. Interpolate gaps in data using linear interpolation
  df <- interpolate_gaps_linear(df, "value", 10)
  
  # 7. Subset the dataframe to include only the necessary minutes
  df <- subset_minutes(df)

  # Skip steps 8-11 for "ppr318_wnd_2021"
  if (name != "ppr318_wnd_2021") {
    # 8. Create a new dataframe with the same column names and NA values
    new_rows <- data.frame(matrix(NA, nrow = 2, ncol = ncol(df)))
    names(new_rows) <- names(df)

    # 9. Assign the new datetime values to the DateTime column
    new_rows$datetime <- new_dates

    # 10. Append new rows to the beginning and end of the dataframe
    df <- rbind(new_rows[1, ], df, new_rows[2, ])

    # 11. Pad the dataframe to fill in missing datetime values
    df <- pad(df, by = 'datetime', interval = '10 min')
  }

  # 12. Set row names
  rownames(df) <- seq(1, nrow(df), 1)

  # Update the dataframe in the list
  dataframes_wthr[[name]] <- df
}

# Reassign the modified dataframes back to the global environment
list2env(dataframes_ts, envir = .GlobalEnv)
list2env(dataframes_prf, envir = .GlobalEnv)
list2env(dataframes_wthr, envir = .GlobalEnv)

# Find matching datetimes and update wind values in ppr318_wnd from 2021 data
matching_indices <- which(ppr318_wnd$datetime %in% ppr318_wnd_2021$datetime)
ppr318_wnd[matching_indices, "value"] <- ppr318_wnd_2021$value
dataframes_wthr[["ppr318_wnd"]] <- ppr318_wnd

#===============================================================================
# DO, DO SATURATION CONCENTRATION AND DO % SATURATION
#===============================================================================

ass


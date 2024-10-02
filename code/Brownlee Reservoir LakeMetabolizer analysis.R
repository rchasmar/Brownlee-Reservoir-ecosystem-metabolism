# Set working directory
setwd(
  dir = paste0(
          "C:/Users/rchasmar/Documents/Brownlee Reservoir ecosystem metabolism",
          " analysis/USGS-GitHub-Brownlee-Reservoir-Metabolism-Directory"
        )
)

# Source the library loading script
source(
  file = "code/load_libraries.R"
)

# Source the functions
source(
  file = "code/functions.R"
)

#===============================================================================
# LOAD DATA
# This section loads and processes ecosystem metabolism data for Brownlee 
# Reservoir. It handles file paths, reads data from CSV and RData files, and 
# dynamically assigns variables based on river miles and depths.
#===============================================================================

# Define the common part of the file path
base_path <- paste0(
               "C:/Users/rchasmar/Documents/Brownlee Reservoir ecosystem ",
               "metabolism analysis/data/"
             )

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
file_paths <- file.path(
                base_path,
                file_names
              )

# Define the river miles corresponding to the first three RData files
river_miles <- c(
                 "286",
                 "300",
                 "318"
               )

# Define the depths for which data is available
depths <- c(
            "01",
            "03",
            "06",
            "10",
            "15",
            "21",
            "30",
            "40"
          )

# Define variables for weather data
weather_vars <- c(
                  "WindSpeed",
                  "SolRad",
                  "PARLumin"
                )

# Read the profiles data from the CSV file
profiles <- read.csv(
                file = file_paths[4],
              header = TRUE
            )

# Loop through each river mile and load the corresponding RData file
for (i in seq_along(
            along.with = river_miles
          )) {
  # Load the RData file for the current river mile
  load(
    file = file_paths[i]
  )
  # Split the profiles data by river mile and assign to a variable
  # Split function divides profiles into subsets based on River.Mile column
  assign(
        x = paste0(
              "ppr",
              river_miles[i],
              "_",
              "prf"
            ), 
    value = split(
              x = profiles,
              f = profiles$River.Mile
            )[[river_miles[i]]]
  )

  # Loop through each depth and assign the corresponding data
  for (depth in depths) {
    # Check if the data frame for the current depth exists in the environment
    if (exists(
          x = paste0(
                "PPR",
                as.numeric(
                  x = depth
                )
              )
        )) {
      # Assign dataframe to new variable with name based on river mile and depth
      assign(
            x = paste0(
                  "ppr",
                  river_miles[i],
                  "_",
                  depth
                ), 
        value = get(
                  x = paste0(
                        "PPR",
                        as.numeric(depth)
                      )
                )
      )
    }
  }
  
  # Remove previous objects that match the pattern "^PPR" to avoid conflicts
  rm(
    list = ls(
             pattern = "^PPR"
           )
  )
}

# Load the weather data from the specified RData file
# The load function loads the data into the environment
weather <- load(
             file = file_paths[5]
           )
buoy_weather <- PPRWeather

# Apply the function to each weather variable
split_data <- setNames(
                object = lapply(
                             X = weather_vars,
                           FUN = function(var) split_by_var_and_rm(
                                                 var = var, 
                                                  rm = river_miles
                                               )
                         ), 
                    nm = weather_vars
              )

# Access the results by name
ppr286_wnd <- split_data$WindSpeed$`286`
ppr300_wnd <- split_data$WindSpeed$`300`
ppr318_wnd <- split_data$WindSpeed$`318`

ppr286_solrad <- split_data$SolRad$`286`
ppr300_solrad <- split_data$SolRad$`300`
ppr318_solrad <- split_data$SolRad$`318`

ppr286_par <- split_data$PARLumin$`286`
ppr300_par <- split_data$PARLumin$`300`
ppr318_par <- split_data$PARLumin$`318`

dam_sol_rad <- BrownleeDamSolRad
dam_wind <- BrownleeDamWindV

# Read the wind speed data from the CSV file, skipping the first 14 rows
# Select only the relevant columns: Timestamp and Value
ppr318_wnd_2021 <- read.csv(
                       file = file_paths[6],
                     header = TRUE,
                       skip = 14
                   )[c(
                       "Timestamp..UTC.07.00.",
                       "Value"
                     )]

# Remove all dataframes with names starting with a capital letter
rm(
  list = ls()[sapply(
                  X = mget(
                            x = ls(), 
                        envir = .GlobalEnv
                      ), 
                FUN = is.data.frame
              ) & grepl(
                    pattern = "^[A-Z]", 
                          x = ls()
                  )]
)

#===============================================================================
# PROCESS TIME SERIES AND PROFILE DATAFRAMES
# Organize columns, pad datetime, interpolate gaps and subset to 10-min interval
#===============================================================================

# New datetime values to append
new_dates <- as.POSIXct(
                    x = c(
                          "2017-01-01 00:00:00",
                          "2023-12-31 23:50:00"
                        ),
                   tz = "America/Denver", 
               format = "%Y-%m-%d %H:%M:%S"
             )

# Define the patterns
pattern_ts <- "^ppr[0-9]+_[0-9]+$"
pattern_prf <- "^ppr[0-9]+_prf$"

# Rename ppr318_wnd_2021 dataframe columns and format datetime
colnames(
  x = ppr318_wnd_2021
) <- c(
       "DateTime",
       "Value"
     )
ppr318_wnd_2021[ , "DateTime"] <- as.POSIXct(
                                         x = ppr318_wnd_2021[ , "DateTime"],
                                        tz = "America/Denver",
                                    format = "%Y-%m-%d %H:%M:%S"
                                  )

# Create lists of dataframes that match patterns
dataframes_ts <- mget(
                       x = ls(
                             pattern = pattern_ts
                           ),
                   envir = .GlobalEnv
                 )
dataframes_prf <- mget(
                        x = ls(
                              pattern = pattern_prf
                            ),
                    envir = .GlobalEnv
                  )
dataframes_wthr <- list(
                          ppr286_wnd = ppr286_wnd, 
                          ppr300_wnd = ppr300_wnd, 
                          ppr318_wnd = ppr318_wnd,
                     ppr318_wnd_2021 = ppr318_wnd_2021,
                       ppr286_solrad = ppr286_solrad, 
                       ppr300_solrad = ppr300_solrad, 
                       ppr318_solrad = ppr318_solrad, 
                          ppr286_par = ppr286_par, 
                          ppr300_par = ppr300_par, 
                          ppr318_par = ppr318_par
                   )

# Loop through each dataframe in dataframes_ts
for (name in names(
               x = dataframes_ts
             )) {
  
  df <- dataframes_ts[[name]]
  
  # 1. Select the specified columns
  df <- df[ , c(
                "DateTime",
                "DO",
                "WTemp"
              )]
  
  # 2. Rename the columns
  colnames(
    x = df
  ) <- c(
         "datetime",
         "do.obs",
         "wtr"
       )
  
  # 3. Apply the pad function
  df <- pad(
                    x = df,
                   by = 'datetime',
             interval = 'min',
          break_above = 5
        )
  
  # 4. Remove duplicated rows based on the datetime column
  df <- df[!duplicated(
              x = df[ , "datetime"]
            ), ]
  
  # 5. Interpolate gaps
  df <- interpolate_gaps_spline(
             data = df,
           column = "do.obs",
          max_gap = 10
        )
  df <- interpolate_gaps_spline(
             data = df,
           column = "wtr",
          max_gap = 10
        )
  
  # 6. Subset minutes
  df <- subset_minutes(
          df = df
        )
 
  # 7. Create a new dataframe with the same column names and NA values
  new_rows <- data.frame(
                matrix(
                  data = NA,
                  nrow = 2,
                  ncol = ncol(
                           x = df
                         )
                )
              )
  names(
    x = new_rows
  ) <- names(
         x = df
       )

  # 8. Assign the new datetime values to the DateTime column
  new_rows[ , "datetime"] <- new_dates

  # 9. Append new rows to the beginning and end of the dataframe
  df <- rbind(
          new_rows[1, ],
          df,
          new_rows[2, ]
        )

  # 10. Pad the dataframe to fill in missing datetime values
  df <- pad(
                 x = df,
                by = 'datetime',
          interval = '10 min'
        )

  # 11. Add a DOY column
  df[ , "doy"] <- as.numeric(
                    x = format(
                               x = df[ , "datetime"], 
                          format = "%j"
                        )
                  )

  # 12. Set row names
  rownames(
    x = df
  ) <- seq(
         from = 1,
           to = nrow(df),
           by = 1
       )

  # Update the dataframe in the list
  dataframes_ts[[name]] <- df
}

# Loop through each dataframe in dataframes_prf
for (name in names(
               x = dataframes_prf
             )) {
  df <- dataframes_prf[[name]]
  
  # 1. Select the specified columns
  df <- df[, c(
               "DateTime",
               "Date",
               "DO",
               "WTemp",
               "PAR",
               "SpecCond",
               "Depth"
             )]
  
  # 2. Rename the columns
  colnames(
    x = df
  ) <- c(
         "datetime",
         "date",
         "do.obs",
         "wtr",
         "irr",
         "cond",
         "depth"
       )
  
  # 3. Set row names
  rownames(
    x = df
  ) <- seq(
         from = 1,
           to = nrow(
                  x = df
                ),
           by = 1
       )
  
  # 4. Format datetime
  df[ , "datetime"] <- as.POSIXct(
                              x = df[ , "datetime"], 
                         format = "%m/%d/%Y %H:%M"
                       )
  
  # 5. Convert the 'date' column to Date objects
  df[ , "date"] <- as.Date(
                          x = df[ , "date"],
                     format = "%m/%d/%Y"
                   )

  # 6. Convert conductivity from mS/cm to µS/m
  df[ , "cond"] <- df[ , "cond"] * 1000

  # 7. Calculate salinity (PSU) based on conductivity
  df[ , "salinity"] <- 6e-04 * df[ , "cond"]

  # 8. Calculate water density using water.density function
  df[ , "wtr_density"] <- water.density(
                            wtr = df[ , "wtr"],
                            sal = df[ , "salinity"]
                          )

  # Update the dataframe in the list
  dataframes_prf[[name]] <- df
}

# Loop through each dataframe in dataframes_wthr
for (name in names(
               x = dataframes_wthr
             )) {
  df <- dataframes_wthr[[name]]

  # 1. Select the specified columns
  df <- df[, c(
               "DateTime",
               "Value"
             )]

  # 2. Rename the columns
  colnames(
    x = df
  ) <- c(
         "datetime",
         "value"
       )

  # 3. Round datetime to the nearest minute
  df <- mutate(
             .data = df, 
          datetime = round_date(
                          x = datetime,
                       unit = "minute"
                     )
        )

  # 4. Pad the dataframe to fill in missing datetime values
  df <- pad(
                    x = df,
                   by = 'datetime',
             interval = 'min',
          break_above = 5
        )
  
  # 5. Remove duplicate datetime entries
  df <- df[!duplicated(
              x = df[ , "datetime"]
            ), ]

  # 6. Interpolate gaps in data using linear interpolation
  df <- interpolate_gaps_linear(
             data = df,
           column = "value",
          max_gap = 10
        )
  
  # 7. Subset the dataframe to include only the necessary minutes
  df <- subset_minutes(
          df = df
        )

  # Skip steps 8-11 for "ppr318_wnd_2021"
  if (name != "ppr318_wnd_2021") {
    # 8. Create a new dataframe with the same column names and NA values
    new_rows <- data.frame(
                  matrix(
                    data = NA,
                    nrow = 2,
                    ncol = ncol(
                             x = df
                           )
                  )
                )
    names(
      x = new_rows
    ) <- names(
           x = df
         )

    # 9. Assign the new datetime values to the DateTime column
    new_rows[ , "datetime"] <- new_dates

    # 10. Append new rows to the beginning and end of the dataframe
    df <- rbind(
            new_rows[1, ],
            df,
            new_rows[2, ]
          )

    # 11. Pad the dataframe to fill in missing datetime values
    df <- pad(
                   x = df,
                  by = 'datetime',
            interval = '10 min'
          )
  }

  # 12. Set row names
  rownames(
    x = df
  ) <- seq(
         from = 1,
           to = nrow(
                  x = df
                ),
           by = 1
       )

  # Update the dataframe in the list
  dataframes_wthr[[name]] <- df
}

# List of data frames
dataframes_list <- list(
                     dataframes_ts,
                     dataframes_prf,
                     dataframes_wthr
                   )

# Loop through the list and use list2env
for (df in dataframes_list) {
  list2env(
        x = df,
    envir = .GlobalEnv
  )
}

# Find matching datetimes and update wind values in ppr318_wnd from 2021 data
matching_indices <- which(
                      ppr318_wnd$datetime %in%
                      ppr318_wnd_2021$datetime
                    )
ppr318_wnd[matching_indices, "value"] <- ppr318_wnd_2021[ , "value"]
dataframes_wthr[["ppr318_wnd"]] <- ppr318_wnd

#===============================================================================
# DO, DO SATURATION CONCENTRATION AND DO % SATURATION
#===============================================================================

# Loop through each dataframe in dataframes_ts
for (name in names(
               x = dataframes_ts
             )) {
  df <- dataframes_ts[[name]]
  
  # Compute do.sat
  df[ , "do.sat"] <- compute_oxygen_saturation(
                       x = df[ , "wtr"] + 273.15
                     )
  
  # Compute do.percent
  df[ , "do.percent"] <- df[ , "do.obs"] / df[ , "do.sat"] * 100
  
  # Set do.obs and do.sat to NA where do.percent > 300
  idx <- which(
           df[ , "do.percent"] > 300
         )
  df[idx, c(
            "do.obs",
            "do.percent"
          )] <- NA  

  # Apply the remove_do_anomalies function
  df <- remove_do_anomalies(
                 df = df,
          threshold = 2
        )
  
  # Update the dataframe in the list
  dataframes_ts[[name]] <- df
}

# Reassign the modified dataframe back to the global environment
list2env(
      x = dataframes_ts,
  envir = .GlobalEnv
)

#===============================================================================
# Z.MIX
#===============================================================================

# Extract the datetime column from one of the dataframes
datetime <- dataframes_ts[[1]]$datetime

# Apply the function to each prefix with the specified date ranges
ppr318_meta <- process_data(
                        prefix = "ppr318",
                 dataframes_ts = dataframes_ts,
                      datetime = datetime,
                    start_date = NULL,
                      end_date = NULL,
                         slope = 0.2,
                          spar = 0.3,
                   subset_data = FALSE
               )
ppr300_meta <- process_data(
                        prefix = "ppr300",
                 dataframes_ts = dataframes_ts,
                      datetime = datetime,
                    start_date = "2020-01-01",
                      end_date = "2022-12-31",
                         slope = 0.2,
                          spar = 0.3,
                   subset_data = TRUE
               )
ppr286_meta <- process_data(
                        prefix = "ppr286",
                 dataframes_ts = dataframes_ts,
                      datetime = datetime,
                    start_date = "2019-01-01",
                      end_date = "2023-12-31",
                         slope = 0.2,
                          spar = 0.3,
                   subset_data = TRUE
               )

# Set up the plotting area for 1x3 layout
par(
  mfrow = c(1, 3)
)

# Create plots for each dataframe
create_plot(
     df = ppr286_meta,
  title = "PPR286 Meta Data with Fitted and Predicted Values"
)
create_plot(
     df = ppr300_meta,
  title = "PPR300 Meta Data with Fitted and Predicted Values"
)
create_plot(
     df = ppr318_meta,
  title = "PPR318 Meta Data with Fitted and Predicted Values"
)

# Define the date sequence for plotting
date <- seq(
          from = as.POSIXct(
                    x = "2017-01-01",
                   tz = "America/Denver"
                 ),
            to = as.POSIXct(
                    x = "2024-01-01",
                   tz = "America/Denver"
                 ),
            by = "year"
        )

meta_dfs <- list(
              ppr318_meta,
              ppr300_meta,
              ppr286_meta
            )

# Plot the results for each dataset
par(
  mfrow = c(
            3, 1
          )
)
for (i in 1:length(
              x = meta_dfs
            )) {
  plot(
    meta ~ datetime,
    data = meta_dfs[[i]],
    xlim = range(date),
    ylim = c(40, 0),
    xlab = "",
    ylab = "Top of Metalimnion (m)",
    main = names(meta_dfs)[i]
  )
  axis.POSIXct(
      side = 1,     
        at = date,
    format = "%Y"
  )
  abline(
      v = date,
    lty = 'dashed'
  )
}
# Plot the results for each dataset
par(
  mfrow = c(1, 1)
)

# Names for the fit models
fit_names <- c(
               "fit_318",
               "fit_300",
               "fit_286"
             )

# Fit models for each meta dataframe using lapply and assign names
fit_models <- setNames(
                object = lapply(
                             X = meta_dfs,
                           FUN = fit_model
                         ),
                    nm = fit_names
              )

# Assigning fit models to the global environment
list2env(
      x = fit_models,
  envir = .GlobalEnv
)

# Loop through each dataframe in dataframes_ts
for (name in names(
               x = dataframes_ts
             )) {
  df <- dataframes_ts[[name]]

  if (grepl(
        pattern = "^ppr286", 
              x = name
      )) {
    df <- add_predictions(
               df = df,
            model = fit_286
          )
  } else if (grepl(
               pattern = "^ppr300",
                     x = name
             )) {
      df <- add_predictions(
                 df = df,
              model = fit_300
            )
    } else if (grepl(
                 pattern = "^ppr318",
                       x = name
               )) {
        df <- add_predictions(
                   df = df,
                model = fit_318
              )
      }
  
# Update the dataframe in the list
  dataframes_ts[[name]] <- df
}

list2env(
      x = dataframes_ts,
  envir = .GlobalEnv
)

#===============================================================================
# K.GAS
#===============================================================================

# Iterate over the list and update the time series dataframes
for (name in names(
               x = dataframes_ts
             )) {
  df <- dataframes_ts[[name]]  # Extract the dataframe by name
  
  # Add 'wnd' column based on the dataframe name pattern
  if (grepl(
        pattern = "^ppr286_",
              x = name
      )) {
    df <- add_wnd_column(
                df = df,
            wnd_df = ppr286_wnd
          )
  } else if (grepl(
               pattern = "^ppr300_",
                     x = name
             )) {
      df <- add_wnd_column(
                  df = df,
              wnd_df = ppr300_wnd
            )
    } else if (grepl(
                 pattern = "^ppr318_",
                       x = name
               )) {
        df <- add_wnd_column(
                    df = df,
                wnd_df = ppr318_wnd
              )
      }
  
  # Calculate k.gas and add it to the dataframe
  df <- calculate_kGAS(
          df = df
        )
  
  # Extract the numeric value after the underscore in the dataframe name
  value_after_underscore <- as.numeric(
                              x = sub(
                                        pattern = ".*_(\\d+)$",
                                    replacement = "\\1",
                                              x = name
                                  )
                            )
  
  # Find rows where extracted value is greater than value in z.mix column
  rows_to_update <- which(
                      value_after_underscore > df[ , "z.mix"]
                    )
  
  # Set k.gas to 0 for those rows
  df[ , "k.gas"][rows_to_update] <- 0
  
  # Update the dataframe in the list
  dataframes_ts[[name]] <- df
}

# Update the global environment with the modified dataframes
list2env(
      x = dataframes_ts,
  envir = .GlobalEnv
)

#===============================================================================
# IRRADIANCE
#===============================================================================

# Create lists of your dataframes and their names
solrad_list <- list(
                 ppr286_solrad,
                 ppr300_solrad,
                 ppr318_solrad
               )
par_list <- list(
              ppr286_par,
              ppr300_par,
              ppr318_par
            )
prf_list <- list( 
              ppr286_prf,
              ppr300_prf,
              ppr318_prf
            )
names_list <- c(
                "ppr286",
                "ppr300",
                "ppr318"
              )

# Apply fill_na_with_solrad function to each pair of solrad and par dataframes
par_list <- mapply(
                    FUN = fill_na_with_solrad,
              solrad_df = solrad_list,
                 par_df = par_list,
               SIMPLIFY = FALSE
            )

# Restore original names
names(
  x = par_list
) <- c(
       "ppr286_par",
       "ppr300_par",
       "ppr318_par"
     )

# Update the global environment with the modified dataframes
list2env(
      x = par_list,
  envir = .GlobalEnv
)

# Process each dataframe
results_list <- mapply(
                       FUN = process_dataframe,
                    prf_df = prf_list,
                    par_df = par_list,
                  SIMPLIFY = FALSE
                )

# Combine all results into one dataframe with source column
combined_results <- bind_rows(
                      lapply(
                          X = seq_along(
                                along.with = results_list
                              ),
                        FUN = function(i) {
                                mutate(
                                   .data = results_list[[i]],
                                  source = names_list[i]
                                )
                              }
                      )
                    )

# Define the first day of each month
first_of_month <- c(
                    1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335
                  )

# Plot all data on one plot
plot(
    Kd ~ day_of_year,
  data = combined_results,
  xlab = "Month", 
  ylab = expression(
           "Light Extinction Coefficient (m"^-1*")"
         ), 
  main = "Kd vs Day of Year",
   pch = 19,
  xlim = c(1, 366),
  ylim = c(0, 1.5), 
  xaxt = "n",
   col = as.factor(
            x = combined_results[ , "source"]
          )
)
axis(
    side = 1,
      at = first_of_month,
  labels = day_of_year_to_month(
             day_of_year = first_of_month
           )
)

# Fit a linear model with cosine and sine transformations
Time <- combined_results[ , "day_of_year"]
xc <- cos(2 * pi * Time / 365.25)
xs <- sin(2 * pi * Time / 365.25)
lm_fit <- lm(
            formula = Kd ~ xc + xs,
               data = combined_results
          )

# Generate a sequence of day_of_year values for prediction from 1 to 366
day_of_year_seq <- seq(
                           from = 1,
                             to = 366,
                     length.out = 366
                   )

# Predict Kd values using the linear model
xc_seq <- cos(
            x = 2 * pi * day_of_year_seq / 365.25
          )
xs_seq <- sin(
            x = 2 * pi * day_of_year_seq / 365.25
          )
fitted_values <- predict(
                    object = lm_fit, 
                   newdata = data.frame(
                               xc = xc_seq,
                               xs = xs_seq
                             )
                 )

# Add the fitted curve to the plot
lines(
    x = day_of_year_seq,
    y = fitted_values,
  col = "black",
  lwd = 2
)

# Add legend with matching colors
legend(
       x = "topright", 
  legend = unique(
             x = combined_results$source
           ), 
     col = unique(
             x = as.factor(
                   x = combined_results[ , "source"]
                 )
           ), 
     pch = 19
)

# Calculate R-squared for the Gaussian fit
harmonic_regression_r_squared <- calculate_harmonic_regression_r_squared(
                                   model = lm_fit, 
                                    data = combined_results
                                 )

# Add R-squared and p-value to the plot
text(
       x = 10,
       y = 1.4, 
  labels = paste(
             "R-squared:",
             round(
                    x = harmonic_regression_r_squared,
               digits = 3
             ),
             "\nP-value: < 0.005",
             sep = " "
           ), 
     pos = 4
)

# Iterate over the list and update the time series dataframes
for (name in names(
               x = dataframes_ts
             )) {
  df <- dataframes_ts[[name]]  # Extract the dataframe by name

  # Add 'irr_surface' column based on the dataframe name pattern
  if (grepl(
        pattern = "^ppr286_",
              x = name
      )) {
    df <- add_irr_surface_column(
                df = df,
            irr_df = ppr286_par
          )
  } else if (grepl(
               pattern = "^ppr300_",
                     x = name
             )) {
      df <- add_irr_surface_column(
                  df = df,
              irr_df = ppr300_par
            )
    } else if (grepl(
                 pattern = "^ppr318_",
                       x = name
               )) {
        df <- add_irr_surface_column(
                    df = df,
                irr_df = ppr318_par
              )
      }

  # Add harmonic regression predictions using the combined Gaussian fit model
  df <- add_harmonic_regression_predictions(
             df = df,
          model = lm_fit
        )

  # Extract the depth from the dataframe name
  depth <- as.numeric(
             x = sub(
                       pattern = ".*_(\\d+)$",
                   replacement = "\\1",
                             x = name
                 )
           )
  
  # Add PAR at different depths using the extracted depth
  df <- add_par_at_depths(
             df = df,
          depth = depth
        )

  # Set values less than 1 to zero in the 'irr' column
  df[ , "irr"][df[ , "irr"] < 1] <- 0
  
  # Update the dataframe in the list
  dataframes_ts[[name]] <- df
}

# Update the global environment with the modified dataframes
list2env(
      x = dataframes_ts,
  envir = .GlobalEnv
)

#===============================================================================
# Metabolism
#===============================================================================

# Iterate over the list and update the time series dataframes
for (name in names(dataframes_ts)) {
  df <- dataframes_ts[[name]]  # Extract the dataframe by name

  # Select the specified columns
  df <- df[ , c(
                "datetime",
                "do.obs",
                "do.sat",
                "k.gas",
                "z.mix",
                "irr",
                "wtr"
              )]

  # Update the dataframe in the list
  dataframes_ts[[name]] <- df
}

# Update the global environment with the modified dataframes
list2env(
      x = dataframes_ts,
  envir = .GlobalEnv
)

# List of dataframe names
dataframe_names <- c(
                     "ppr286_01",
                     "ppr286_03",
                     "ppr286_06",
                     "ppr286_10",
                     "ppr286_15",
                     "ppr286_21",
                     "ppr286_30",
                     "ppr286_40",
                     "ppr300_01",
                     "ppr300_03",
                     "ppr300_06",
                     "ppr300_10",
                     "ppr300_15",
                     "ppr300_21",
                     "ppr300_30",
                     "ppr300_40",
                     "ppr318_01",
                     "ppr318_03",
                     "ppr318_06",
                     "ppr318_10"
                   )

# Initialize a list to store the results
results_list <- list()

# Loop through each dataframe and apply metab function and additional processing
for (df_name in dataframe_names) {
  # Get the dataframe from the list
  df <- dataframes_ts[[df_name]]
  
  # Apply the metabb function
  mle.res <- metabb(
                      data = df,
                    method = "mle",
                  wtr.name = "wtr",
                  irr.name = "irr",
               do.obs.name = "do.obs"
             )
  
  # Set negative GPP values to NA
  mle.res$GPP[mle.res$GPP < 0] <- NA
  
  # Set positive R values to NA
  mle.res$R[mle.res$R > 0] <- NA
  
  # Calculate NEP as the sum of GPP and R
  mle.res$NEP <- mle.res$GPP + mle.res$R
  
  # Create a Date object based on year and day of year (doy)
  mle.res$date <- as.Date(
                         x = mle.res$doy - 1,
                    origin = paste0(
                               mle.res$year,
                               "-01-01"
                             )
                  )
  
  # Store the result in the results_list
  results_list[[df_name]] <- mle.res
}

#===============================================================================
# Plot
#===============================================================================

# Create a sequence of dates
Date <- seq(
          from = as.Date(
                   x = "2017-01-01"
                 ),
            to = as.Date(
                   x = "2024-01-01"
                 ),
            by = "year"
        )

# Loop through each dataframe in the results_list and create plots
for (df_name in names(
                  x = results_list
                )) {
  # Get the processed dataframe from the results_list
  mle.res <- results_list[[df_name]]
  
  # Calculate the cumulative sum reset for each year
  mle.res[ , "cum_nep"] <- ave(
                               x = ifelse(
                                     test = is.na(
                                              x = mle.res[ , "NEP"]
                                            ),
                                      yes = 0,
                                       no = mle.res[ , "NEP"]
                                   ),
                             format(
                                    x = mle.res[ , "date"],
                               format = "%Y"
                             ),
                             FUN = cumsum
                           )
  # Create the PNG file with larger dimensions
  png(
    filename = paste0(
                 df_name,
                 ".png"
               ),
       width = 1600,
      height = 1200
  )
  
  par(
    mfrow = c(2, 1),  # Change to 2 rows and 1 column
      oma = c(0, 2, 4, 0),  # Adjust oma to make space for the main title
      mar = c(5, 6, 4, 2) + 0.1,
      mgp = c(4, 1, 0)  # Adjust mgp to move the y-axis label
  )
  
  # First plot
  plot(
         GPP ~ date,
        data = mle.res,
        type = 'h',
         lwd = 2,
         col = "darkgreen",
        xlim = range(Date),
        ylim = c(-12, 12),
        xlab = "",
        ylab = expression(
                 Metabolism ~ (mg ~ O[2] ~ L^{-1} ~ d^{-1})
               ),
        xaxt = 'n',
     cex.lab = 1.6,
    cex.axis = 1.4,
         las = 1
  )
  lines(
       R ~ date,
    data = mle.res,
    type = 'h',
     lwd = 2,
     col = "firebrick3"
  )
  abline(
    h = 0
  )
  axis.Date(
        side = 1,
          at = Date,
      format = "%Y",
    cex.axis = 1.4
  )
  abline(
      v = Date,
    lty = 'dashed'
  )
  
  # Second plot
  plot(
     cum_nep ~ date,
        data = mle.res,
         pch = 19,
        xlim = range(Date),
        ylim = c(-200, 200),
        xlab = "",
        ylab = expression(
                 Cumulative ~ NEP ~ (mg ~ O[2] ~ L^{-1})
               ),
        xaxt = 'n',
     cex.lab = 1.6,
    cex.axis = 1.4,
         las = 1
  )
  abline(
      h = 0,
    lty = 'dashed'
  )
  axis.Date(
        side = 1,
          at = Date,
      format = "%Y",
    cex.axis = 1.4
  )
  abline(
      v = Date,
    lty = 'dashed'
  )
  
  # Add main title
  mtext(
     text = df_name,
    outer = TRUE,
      cex = 1.8
  )
  
  # Close the PNG device
  dev.off()
}

#===============================================================================
# PUSH TO GITHUB
#===============================================================================

automate_git()


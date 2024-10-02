#===============================================================================
# Function Name: split_by_var_and_rm
# Description: This function splits the buoy_weather dataframe by a specified 
#              variable and further splits the resulting data by river mile.
# Parameters: 
#   - var: The name of the variable to split by (as a string).
#   - rm: A vector of river miles to split the data by.
# Returns: A named list where each element corresponds to the data for a specific 
#          river mile.
#===============================================================================

# Function to split data by variable and river mile
split_by_var_and_rm <- function(
                         var,
                         rm
                       ) {
                         var_data <- split(
                                       x = buoy_weather,
                                       f = buoy_weather$Variable
                                     )[[var]]
                         split_list <- split(
                                         x = var_data,
                                         f = var_data$Location
                                       )
                         setNames(
                           object = lapply(
                                      X = rm,
                                      FUN = function(mile) split_list[[mile]]
                                    ),
                           nm = rm
                         )
                       }

#===============================================================================
# Function Name: interpolate_gaps_linear
# Description: This function interpolates gaps of 10 or fewer NAs in a specified 
#              column of a dataframe using linear interpolation.
# Parameters: 
#   - data: A dataframe containing the data.
#   - column: The name of the column to interpolate (as a string).
#   - max_gap: The maximum number of consecutive NAs to interpolate.
# Returns: A dataframe with the specified column's NAs interpolated where the 
#          gap is 10 or fewer.
#===============================================================================

interpolate_gaps_linear <- function(data, column, max_gap) {
  # Check if the column exists in the data
  if (!column %in% names(data)) {
    stop("Column not found in the data.")
  }
  
  # Check if max_gap is a positive integer
  if (!is.numeric(max_gap) || max_gap <= 0) {
    stop("max_gap should be a positive integer.")
  }
  
  # Identify the start and end of NA gaps
  na_runs <- rle(is.na(data[[column]]))
  na_lengths <- na_runs$lengths[na_runs$values]
  
  # Create a logical vector to mark gaps to interpolate
  interpolate <- rep(FALSE, length(data[[column]]))
  
  # Initialize the start index
  start_idx <- 1
  
  # Loop through each segment of the data
  for (i in seq_along(na_runs$lengths)) {
    end_idx <- start_idx + na_runs$lengths[i] - 1
    if (na_runs$values[i] && na_runs$lengths[i] <= max_gap) {
      # Check if the gap is bounded by non-NA values
      if (start_idx > 1 && end_idx < length(data[[column]])) {
        if (!is.na(data[[column]][start_idx - 1]) && 
            !is.na(data[[column]][end_idx + 1])) {
          interpolate[start_idx:end_idx] <- TRUE
        }
      }
    }
    start_idx <- end_idx + 1
  }
  
  # Interpolate only the marked gaps using linear interpolation
  interpolated_values <- approx(seq_along(data[[column]]), 
                                data[[column]], 
                                xout = seq_along(data[[column]]))$y
  data[[column]][interpolate] <- interpolated_values[interpolate]
  
  return(data)
}

#===============================================================================
# Function Name: interpolate_gaps_spline
# Description: This function interpolates gaps of 10 or fewer NAs in a specified 
#              column of a dataframe using spline interpolation.
# Parameters: 
#   - data: A dataframe containing the data.
#   - column: The name of the column to interpolate (as a string).
#   - max_gap: The maximum number of consecutive NAs to interpolate.
# Returns: A dataframe with the specified column's NAs interpolated where the 
#          gap is 10 or fewer.
#===============================================================================

interpolate_gaps_spline <- function(data, column, max_gap) {
  # Check if the column exists in the data
  if (!column %in% names(data)) {
    stop("Column not found in the data.")
  }
  
  # Check if max_gap is a positive integer
  if (!is.numeric(max_gap) || max_gap <= 0) {
    stop("max_gap should be a positive integer.")
  }
  
  # Identify the start and end of NA gaps
  na_runs <- rle(is.na(data[[column]]))
  na_lengths <- na_runs$lengths[na_runs$values]
  
  # Create a logical vector to mark gaps to interpolate
  interpolate <- rep(FALSE, length(data[[column]]))
  
  # Initialize the start index
  start_idx <- 1
  
  # Loop through each segment of the data
  for (i in seq_along(na_runs$lengths)) {
    end_idx <- start_idx + na_runs$lengths[i] - 1
    if (na_runs$values[i] && na_runs$lengths[i] <= max_gap) {
      # Check if the gap is bounded by non-NA values
      if (start_idx > 1 && end_idx < length(data[[column]])) {
        if (!is.na(data[[column]][start_idx - 1]) && 
            !is.na(data[[column]][end_idx + 1])) {
          interpolate[start_idx:end_idx] <- TRUE
        }
      }
    }
    start_idx <- end_idx + 1
  }
  
  # Interpolate only the marked gaps using spline interpolation
  interpolated_values <- spline(seq_along(data[[column]]), 
                                data[[column]], 
                                xout = seq_along(data[[column]]))$y
  data[[column]][interpolate] <- interpolated_values[interpolate]
  
  return(data)
}

#===============================================================================
# Function Name: subset_minutes
# Description: This function subsets a dataframe to include only rows where the 
#              'datetime' column's minute value is a multiple of 10.
# Parameters: 
#   - df: A dataframe containing a 'datetime' column.
# Returns: A dataframe with rows where the 'datetime' column's minute value is 
#          a multiple of 10.
#===============================================================================

subset_minutes <- function(df) {
  df[format(df$'datetime', "%M") %in% sprintf("%02d", seq(0, 50, by = 10)), ]
}

#===============================================================================
# Function Name: compute_oxygen_saturation
# Description: This function computes the 100% saturation oxygen concentration 
#              at nonstandard pressure from temperature (in Kelvin).
# Parameters: 
#   - x: Temperature in Kelvin.
# Returns: The 100% saturation oxygen concentration at nonstandard pressure.
#===============================================================================

compute_oxygen_saturation <- function(x) {
  C <- exp(-139.34411 + (1.575701*10^5/x) - (6.642308*10^7/x^2) + 
           (1.2438*10^10/x^3) - (8.621949*10^11/x^4) - 
           (0.13*((1.7674*10^-2) - (1.0754*10/x) + (2.1407*10^3/x^2))))
  P <- exp(5.25*log(1 - 626/44300))
  Pwv <- exp(11.8571 - (3840.7/x) - (216961/x^2))
  theta <- (9.672*10^-3) - (4.942*10^-5*x) + (6.436*10^-8*x^2)
  Cp <- C * (P*((1-Pwv/P)/(1-Pwv))) * ((1-theta*P)/(1-theta))
  return(Cp)
}

#===============================================================================
# Function Name: remove_do_anomalies
# Description: This function removes anomalies in dissolved oxygen (DO) 
#              observations by setting values to NA if the difference between 
#              consecutive observations exceeds a specified threshold.
# Parameters: 
#   - df: A dataframe containing the DO observations.
#   - threshold: The maximum allowed difference between consecutive DO 
#                observations. Defaults to 2.
# Returns: A dataframe with anomalies in DO observations removed.
#===============================================================================

remove_do_anomalies <- function(df, threshold) {
  df %>%
    mutate(do_diff = c(NA, diff(do.obs)), 
           do.obs = ifelse(abs(do_diff) > threshold, NA, do.obs)) %>% 
    select(-do_diff)
}

#===============================================================================
# Function Name: process_data
# Description: This function processes time series data by extracting and 
#              combining 'wtr' columns, subsetting based on date range, and 
#              applying various transformations.
# Parameters: 
#   - prefix: A string to match the names of dataframes in the list.
#   - dataframes_ts: A list of dataframes containing time series data.
#   - datetime: A vector of datetime values.
#   - start_date: The start date for subsetting the data (optional).
#   - end_date: The end date for subsetting the data (optional).
#   - slope: A parameter for the ts.meta.depths function.
#   - spar: A smoothing parameter for the smooth.spline function.
#   - subset_data: A logical value indicating whether to subset the data.
# Returns: A dataframe with processed metadata including the metalimnion top 
#          and day of year (DOY).
#===============================================================================

process_data <- function(
                  prefix,
                  dataframes_ts,
                  datetime,
                  start_date,
                  end_date,
                  slope,
                  spar,
                  subset_data
                ) {
  
                  # Extract and combine 'wtr' columns for the given prefix
                  dfs <- dataframes_ts[grep(
                                         pattern = prefix,
                                         x = names(dataframes_ts)
                                       )]
                  wtr <- data.frame(
                           datetime,
                           extract_wtr(
                             df_list = dfs
                           )
                         )
  
                  # Subset the data based on the provided date range (Optional)
                  if (subset_data &&
                      !is.null(x = start_date) &&
                      !is.null(x = end_date)) {
                    wtr <- wtr[wtr$datetime >= as.POSIXct(
                                                 x = start_date,
                                                 tz = "America/Denver"
                                               ) & 
                               wtr$datetime <= as.POSIXct(
                                                 x = end_date,
                                                 tz = "America/Denver"
                                               ), ]
                  }
  
                  # Apply ts.meta.depths with na.rm = TRUE
                  meta <- ts.meta.depths(
                            wtr = wtr,
                            slope = slope,
                            na.rm = TRUE
                          )
  
                  # Apply function to remove non-real values for metalimnion top
                  meta <- remove_non_finite_top(
                            df = meta
                          )
  
                  # Apply smooth.spline and get the fitted values
                  meta$meta <- smooth.spline(
                                 x = as.numeric(meta$datetime),
                                 y = meta$top,
                                 spar = spar
                               )$y
  
                  # Add DOY (Day of Year) column
                  meta$doy <- as.numeric(
                                format(
                                  x = meta$datetime,
                                  format = "%j"
                                )
                              )
  
                  return(meta)
                }

#===============================================================================
# Function Name: extract_wtr
# Description: This function extracts and renames 'wtr' columns from a list of 
#              dataframes.
# Parameters: 
#   - df_list: A list of dataframes containing the 'wtr' columns.
# Returns: A dataframe with the combined 'wtr' columns, renamed based on the 
#          original dataframe names.
#===============================================================================

extract_wtr <- function(df_list) {
  wtr_list <- lapply(df_list, function(df) df$wtr)  # Extract 'wtr' columns
  names(wtr_list) <- sapply(names(df_list), function(name) {  # Rename columns
    parts <- unlist(strsplit(name, "_"))
    paste0("wtr_", as.numeric(parts[2]))
  })
  wtr_df <- do.call(cbind, wtr_list)  # Combine columns into a dataframe
  return(wtr_df)  # Return the combined dataframe
}

#===============================================================================
# Function Name: remove_non_finite_top
# Description: This function removes rows from a dataframe where the 'top' 
#              column contains non-finite values (e.g., NA, NaN, Inf).
# Parameters: 
#   - df: A dataframe containing the 'top' column.
# Returns: A dataframe with rows removed where the 'top' column is not a real 
#          number.
#===============================================================================

remove_non_finite_top <- function(df) {
  df <- df[is.finite(df$top), ]
  return(df)
}

#===============================================================================
# Function Name: create_plot
# Description: This function performs analysis and creates a plot of the top of 
#              the metalimnion over the day of the year, including predictions 
#              based on a linear model with cosine and sine terms.
# Parameters: 
#   - df: A dataframe containing the data.
#   - title: A title for the plot.
# Returns: A plot showing the original data and the predicted values.
#===============================================================================

create_plot <- function(df, title) {
  # Convert datetime to numeric DOY
  Time <- df$doy
  
  # Create cosine and sine terms for the model
  xc <- cos(2 * pi * Time / 365.25)
  xs <- sin(2 * pi * Time / 365.25)
  
  # Fit the linear model
  fit.lm <- lm(df$meta ~ xc + xs)
  
  # Generate a sequence of time points for prediction (1 to 366)
  new_time <- seq(from = 1, to = 366, by = 1)
  
  # Create a new dataframe for prediction
  newdata <- data.frame(Time = new_time,
                        xc = cos(2 * pi * new_time / 365.25),
                        xs = sin(2 * pi * new_time / 365.25))
  
  # Find predictions for the new time series
  pred <- predict(fit.lm, newdata = newdata)
  
  # Plot the original data
  plot(df$meta ~ Time, main = title, xlab = "Day of Year", 
       ylab = "Top of Metalimnion (m)", ylim = c(40, 0), xlim = c(0, 366))
  
  # Add dashed lines for different sensor depths
  abline(h = c(1, 3, 6, 10, 15, 21, 30, 40), lty = 'dashed')

  # Add the predicted values to the plot
  lines(new_time, pred, col = "red", lwd = 2)
}

#===============================================================================
# Function Name: fit_model
# Description: This function fits a linear model to the 'meta' column of a 
#              dataframe using cosine and sine terms based on the day of the
#              year.
# Parameters: 
#   - df: A dataframe containing the 'meta' column and 'doy' (day of year)
#         column.
# Returns: A linear model fitted to the 'meta' column.
#===============================================================================

fit_model <- function(df) {
  Time <- df$doy
  xc <- cos(2 * pi * Time / 365.25)
  xs <- sin(2 * pi * Time / 365.25)
  lm(df$meta ~ xc + xs)
}

#===============================================================================
# Function Name: add_predictions
# Description: This function predicts and adds a 'z.mix' column to a dataframe 
#              using a provided linear model.
# Parameters: 
#   - df: A dataframe containing the 'doy' (day of year) column.
#   - model: A linear model used for prediction.
# Returns: A dataframe with an added 'z.mix' column containing the predicted
#          values.
#===============================================================================

add_predictions <- function(df, model) {
  Time <- df$doy
  newdata <- data.frame(Time = Time,
                        xc = cos(2 * pi * Time / 365.25),
                        xs = sin(2 * pi * Time / 365.25))
  df$z.mix <- predict(model, newdata = newdata)
  return(df)
}

#===============================================================================
# Function Name: add_wnd_column
# Description: This function adds a 'wnd' column from a wind speed dataframe to 
#              the corresponding time series dataframe.
# Parameters: 
#   - df: A dataframe containing the time series data.
#   - wnd_df: A dataframe containing the wind speed data.
# Returns: The updated time series dataframe with the 'wnd' column added.
#===============================================================================

add_wnd_column <- function(df, wnd_df) {
  df$wnd <- wnd_df$value  # Add 'wnd' column from wnd_df to df
  return(df)  # Return the updated dataframe
}

#===============================================================================
# Function Name: calculate_kGAS
# Description: This function calculates k600 and k.gas and adds them to the 
#              dataframe.
# Parameters: 
#   - df: A dataframe containing the time series data.
# Returns: The updated dataframe with the 'k.gas' column added.
#===============================================================================

calculate_kGAS <- function(df) {
  k600_df <- k.cole(df)  # Calculate k600 using the k.cole function
  
  kGAS_df <- k600.2.kGAS(
    data.frame(k600_df, wtr = df$wtr)
  )  # Convert k600 to k.gas using water temperature
  
  df$k.gas <- kGAS_df[, 2]  # Add 'k.gas' column to df
  
  return(df)  # Return the updated dataframe
}

#===============================================================================
# Function Name: fill_na_with_solrad
# Description: This function fills NA values in the 'par' dataframe using values 
#              from the 'solrad' dataframe, multiplied by a factor of 1.93.
# Parameters: 
#   - solrad_df: A dataframe containing the 'solrad' values.
#   - par_df: A dataframe containing the 'par' values.
# Returns: The updated 'par' dataframe with NA values filled using 'solrad'
#          values.
#===============================================================================

fill_na_with_solrad <- function(solrad_df, par_df) {
  # Identify the rows where 'solrad' has a value and 'par' is NA
  condition <- !is.na(solrad_df$value) & is.na(par_df$value)
  
  # Fill the NA values in 'par' with 'solrad' values multiplied by 1.93
  par_df$value[condition] <- solrad_df$value[condition] * 1.93
  
  return(par_df)
}

#===============================================================================
# Function Name: add_irr_surface_column
# Description: This function adds 'irr_surface' values from irradiance 
#              dataframes to corresponding time series dataframes.
# Parameters: 
#   - df: A dataframe containing the time series data.
#   - irr_df: A dataframe containing the irradiance data.
# Returns: The updated time series dataframe with the 'irr_surface' column
#          added.
#===============================================================================

add_irr_surface_column <- function(df, irr_df) {
  df$irr_surface <- irr_df$value  # Add 'irr_surface' column from irr_df to df
  return(df)  # Return the updated dataframe
}

#===============================================================================
# Function Name: find_closest_datetime
# Description: This function finds the closest datetime to a target datetime 
#              from a vector of datetimes.
# Parameters: 
#   - target_datetime: The target datetime to compare against.
#   - datetime_vector: A vector of datetimes to search through.
# Returns: The datetime from the vector that is closest to the target datetime.
#===============================================================================

find_closest_datetime <- function(target_datetime, datetime_vector) {
  diffs <- abs(difftime(datetime_vector, target_datetime, units = "secs"))
  closest_index <- which.min(diffs)
  return(datetime_vector[closest_index])
}

#===============================================================================
# Function Name: calculate_r_squared
# Description: This function calculates the R-squared value for a given model 
#              and dataset.
# Parameters: 
#   - model: The linear model for which to calculate R-squared.
#   - data: A dataframe containing the observed values.
# Returns: The R-squared value indicating the proportion of variance explained 
#          by the model.
#===============================================================================

calculate_r_squared <- function(model, data) {
  observed <- data$irr
  fitted <- predict(model)
  tss <- sum((observed - mean(observed))^2)
  rss <- sum(residuals(model)^2)
  r_squared <- 1 - (rss / tss)
  return(r_squared)
}

#===============================================================================
# Function Name: process_dataframe
# Description: This function processes dataframes by fitting a nonlinear model 
#              to irradiance data and calculating Kd and R-squared values.
# Parameters: 
#   - prf_df: A dataframe containing profile data.
#   - par_df: A dataframe containing PAR data.
# Returns: A dataframe with Kd and R-squared values for each date.
#===============================================================================

process_dataframe <- function(prf_df, par_df) {
  split_data <- split(prf_df, prf_df$date)
  results_list <- list()
  
  for (date in names(split_data)) {
    df <- split_data[[date]]
    first_datetime <- df$datetime[1]
    closest_datetime <- find_closest_datetime(first_datetime, 
                                              par_df$datetime)
    I0 <- par_df$value[par_df$datetime == closest_datetime]
    
    if (is.na(I0) || is.infinite(I0)) {
      next
    }
    
    fit <- tryCatch({
      nls(irr ~ I0 * exp(-Kd * depth), data = df, start = list(Kd = 0.1))
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(fit)) {
      next
    }
    
    Kd_value <- coef(fit)["Kd"]
    r_squared <- calculate_r_squared(fit, df)
    results_list[[date]] <- data.frame(date = as.Date(date), 
                                       Kd = Kd_value, 
                                       R_squared = r_squared)
  }
  
  results_df <- do.call(rbind, results_list)
  results_df$R_squared <- as.numeric(format(results_df$R_squared, 
                                            scientific = FALSE))
  results_df$day_of_year <- yday(results_df$date)
  results_df <- results_df %>%
    mutate(Kd = ifelse(R_squared < 0.9 | Kd > 1.5, NA, Kd),
           R_squared = ifelse(R_squared < 0 | R_squared > 1, NA, R_squared))
  rownames(results_df) <- seq_len(nrow(results_df))
  
  return(results_df)
}

#===============================================================================
# Function Name: day_of_year_to_month
# Description: This function converts a day of the year to a month format 
#              without the year, suitable for axis labels.
# Parameters: 
#   - day_of_year: An integer representing the day of the year.
# Returns: The month corresponding to the given day of the year.
#===============================================================================

day_of_year_to_month <- function(day_of_year) {
  month(as.Date(day_of_year - 1, origin = "2023-01-01"), label = TRUE)
}

#===============================================================================
# Function Name: calculate_harmonic_regression_r_squared
# Description: This function calculates the R-squared value for a harmonic 
#              regression model and dataset.
# Parameters: 
#   - model: The harmonic regression model for which to calculate R-squared.
#   - data: A dataframe containing the observed Kd values.
# Returns: The R-squared value indicating the proportion of variance explained 
#          by the model.
#===============================================================================

calculate_harmonic_regression_r_squared <- function(model, data) {
  observed <- data$Kd
  fitted <- predict(model, newdata = data)
  tss <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
  rss <- sum((observed - fitted)^2, na.rm = TRUE)
  r_squared <- 1 - (rss / tss)
  return(r_squared)
}

#===============================================================================
# Function Name: add_harmonic_regression_predictions
# Description: This function adds harmonic regression predictions to a dataframe 
#              using a provided model.
# Parameters: 
#   - df: A dataframe containing the day of year ('doy') column.
#   - model: A harmonic regression model used for prediction.
# Returns: The updated dataframe with the 'Kd' column containing the predicted 
#          values.
#===============================================================================

add_harmonic_regression_predictions <- function(df, model) {
  day_of_year <- df$doy
  xc <- cos(2 * pi * day_of_year / 365.25)
  xs <- sin(2 * pi * day_of_year / 365.25)
  newdata <- data.frame(xc = xc, xs = xs)
  df$Kd <- predict(model, newdata = newdata)
  return(df)
}

#===============================================================================
# Function Name: add_par_at_depths
# Description: This function calculates and adds PAR (Photosynthetically Active 
#              Radiation) at specified depths to a dataframe.
# Parameters: 
#   - df: A dataframe containing 'irr_surface' and 'Kd' columns.
#   - depth: The depth at which to calculate PAR.
# Returns: The updated dataframe with the 'irr' column containing PAR values at 
#          the specified depth.
#===============================================================================

add_par_at_depths <- function(df, depth) {
  # Calculate PAR at the given depth
  df$irr <- df$irr_surface * exp(-df$Kd * depth)
  return(df)
}

#===============================================================================
# Function Name: metabb
# Description: This function processes water quality data by matching column 
#              names, applying a specified metabolic method, and returning the 
#              results.
# Parameters: 
#   - data: A dataframe containing the water quality data.
#   - method: A character string specifying the metabolic method to use. 
#             Options are "bookkeep", "bayesian", "kalman", "ols", "mle".
#   - wtr.name: A character string for the water temperature column name.
#   - irr.name: A character string for the irradiance column name.
#   - do.obs.name: A character string for the observed dissolved oxygen column name.
#   - ...: Additional arguments passed to the metabolic method function.
# Returns: The processed dataframe with metabolic calculations applied.
#===============================================================================

metabb <- function(
  data,
  method,
  wtr.name,
  irr.name, 
  do.obs.name,
  ...
) {
    
  # Capture additional arguments
  m.args <- list(...)
    
  # Rename columns if necessary
  if (wtr.name != "wtr") {
    if (!"wtr" %in% names(
                      x = data
                    )) {
      names(
        x = data
      )[names(
          x = data
        ) == wtr.name] <- "wtr"
    } else {
        data[ , "wtr"] <- data[ , wtr.name]
      }
  }
  if (irr.name != "irr") {
    if (!"irr" %in% names(
                      x = data
                    )) {
      names(
        x = data
      )[names(
          x = data
        ) == irr.name] <- "irr"
    } else {
        data[ , "irr"] <- data[ , irr.name]
      }
  }   
  if (do.obs.name != "do.obs") {
    if (!"do.obs" %in% names(
                         x = data
                       )) {
      names(
        x = data
      )[names(
          x = data
        ) == do.obs.name] <- "do.obs"
    } else {
        data[ , "do.obs"] <- data[ , do.obs.name]
      }
  }
    
  # Define possible methods and match the provided method
  possibleMethods <- c(
                       "bookkeep",
                       "bayesian",
                       "kalman",
                       "ols",
                       "mle"
                     )
  mtd <- match.arg(
               arg = method,
           choices = possibleMethods
         )
  mtdCall <- paste(
               "metab",
               mtd,
               sep = "."
             )
    
  # Prepare data by removing rows with NAs
  data1 <- addNAs(
                       x = data[complete.cases(data), ],
             percentReqd = 1
           )
  data2 <- data1[complete.cases(data1), ]
    
  # Generate unique IDs based on year and day of year (doy)
  ids <- id(
           list(
             data2[ , "year"],
             trunc(
               x = data2[ , "doy"]
             )
           )
         )
  ids <- as.integer(
           x = ids - (min(ids) - 1)
         )
  nid <- length(
           x = unique(
                 x = ids
               )
         )
    
  # Initialize results list
  results <- vector(
                 mode = "list",
               length = nid
             )
  names(
    x = results
  ) <- unique(
         x = ids
       )
    
  # Loop through unique IDs and apply the selected method
  for (i in unique(
              x = ids
            )) {
    poss.args <- c(
                   "do.obs",
                   "do.sat",
                   "k.gas",
                   "z.mix",
                   "irr",
                   "wtr",
                   "datetime"
                 )
    used.args <- poss.args[poss.args %in% names(
                                            x = data2
                                          )]
    largs0 <- as.list(
                x = data2[i == ids, used.args]
              )
    largs <- c(
               largs0,
               m.args[!names(
                         x = m.args
                       ) %in% names(
                                x = largs0
                              )]
             )
    results[[as.character(
               x = i
             )]] <- do.call(
                      what = mtdCall,
                      args = largs
                    )
  }
    
    # Combine results into a single data frame
    answer0 <- conquerList(
                      x = results,
                 naming = data.frame(
                            year = data2[!duplicated(
                                            x = ids
                                          ), "year"],
                             doy = trunc(
                                     x = data2[!duplicated(
                                                  x = ids
                                                ), "doy"]
                                   )
                          )
               )
    a0.names <- names(
                  x = results[[1]]
                )
    
    # Process the combined results
    if (length(
          x = a0.names
        ) > 1 & is.list(
                  x = answer0
                ) & !is.data.frame(
                       x = answer0
                     )) {
      names(
        x = answer0
      ) <- a0.names
      answer <- answer0$"metab"

      for (i in 1:length(
                    x = a0.names
                  )) {
        if (a0.names[i] == "metab") {
          next
        }
        if (a0.names[i] == "smoothDO") {
          t.sDO <- answer0[[a0.names[i]]]
          t.sDO <- t.sDO[ , !names(
                               x = t.sDO
                             ) %in% c(
                                      "doy",
                                      "year"
                                    )]
          attr(
                x = answer,
            which = "smoothDO.vec"
          ) <- c(
                 t(
                   x = t.sDO
                 )
               )
        }
        attr(
              x = answer,
          which = a0.names[i]
        ) <- answer0[[a0.names[i]]]
      }
    } else {
      answer <- answer0
      }
    
    return(answer)
}

#===============================================================================
# Function Name: addNAs
# Description: This function adds missing datetime entries to a dataframe, 
#              ensuring continuous time series data by filling in gaps with NA 
#              values.
# Parameters: 
#   - x: A dataframe containing the original time series data.
#   - ...: Additional arguments passed to the byeShort function.
# Returns: The updated dataframe with missing datetime entries filled in and 
#          corresponding 'doy' and 'year' columns added if they were missing.
#===============================================================================

addNAs <- function(
  x,
  ...
) {
  # Identify columns related to date, day of year (doy), and year
  dateL <- grepl(
                 pattern = ".?date?.",
                       x = names(x),
             ignore.case = TRUE
           ) # matches anything with "date" in it
  dL <- grepl(
          pattern = "^[dD][oO][yY]$",
                x = names(x)
        ) # matches doy, regardless of case
  yL <- grepl(
          pattern = "^[yY]ear4?$",
                x = names(x)
        ) # matches Year, year, year4, Year4

  # The datetime checks result in error if conditions not met
  if (any(
        dateL
      )) { # look for the date column
      names(
        x = x
      )[dateL] <- "datetime"
  } else {
      stop(
        "No 'datetime' column found"
      )
  }
  if (!"POSIXct" %in% class(
                        x = x[ , "datetime"]
                      )) { # ensure the date column is POSIXct
      stop(
        "date column must be POSIXct"
      )
  }

  # If doy/year aren't found, they'll be added
  # Because we are requiring POSIXct datetime, these values can be generated if they're missing
  if (any(
        dL
      )) { # look for "day of year" column
      names(
        x = x
      )[dL] <- "doy"
  } else {
      x[ , "doy"] <- date2doy(
                       x = x[ , "datetime"]
                     )
  }
  if (any(
        yL
      )) { # look for "year" column
      names(
        x = x
      )[yL] <- "year"
  } else {
      x[ , "year"] <- as.integer(
                        x = format.Date(
                                   x = x[ , "datetime"],
                              format = "%Y"
                            )
                      )
  }

  # Define rounding digits and mode function
  rdig <- 4
  Mode <- function(x) { # function to find the mode
      ux <- unique(
              x = x
            )
      ux[which.max(
           x = tabulate(
                 bin = match(
                             x = x,
                         table = ux
                       )
               )
         )]
  }
  
  # Calculate expected frequency and minutes between samples
  ex <- round(
          x = Mode(
                x = 1 / diff(
                          x = x[, "doy"]
                        )
              )
        ) # frequency calculated in metab.xx()
  mins <- 1 / ex * 24 * 60
  is.wholenumber <- function(
    x,
    tol = .Machine[["double.eps"]]^0.5
  ) {
    abs(
      x = x - round(
                x = x
              )
    ) < tol
  }
  if (!is.wholenumber(
         x = mins
       )) {
    warning(
      "Time between samples not whole number"
    ) 
  }

  # Adjust data using byeShort function
  x1 <- byeShort(
                     X = x,
              Expected = ex,
               ToCount = "doy",
          TruncToCount = TRUE,
          ...
        )
  if (nrow(
        x = x1
      ) == 0) {
    return(x1)
  }

  # Generate ideal datetime sequence and merge with existing data
  Range <- range(
             x1[ , "datetime"]
           )
  Ideal <- data.frame(
             "datetime" = seq(
                            from = Range[1],
                              to = Range[2],
                              by = paste(
                                     mins,
                                     "mins"
                                   )
                          )
           )

  print(
    paste(
      "NA's added to fill in time series:",
      dim(
        x = Ideal
      )[1] - dim(
               x = x1
             )[1],
      sep = " "
    )
  )
  flush.console()
  x2 <- merge(
              x = x1,
              y = Ideal,
          all.x = TRUE,
          all.y = TRUE
        )
  
  # Interpolate year and doy if they were originally present
  if (any(
        yL
      )) {
    x2[ , "year"] <- approx(
                          x = x2[ , "datetime"],
                          y = x2[ , "year"],
                       xout = Ideal[, 1]
                     )$y
  }
  if (any(
        dL
      )) {
    x2[ , "doy"] <- approx(
                         x = x2[ , "datetime"],
                         y = x2[ , "doy"],
                      xout = Ideal[, 1]
                    )$y
  }

  return(x2)
}

#===============================================================================
# Function Name: date2doy
# Description: This function converts POSIX date-time objects to the day of the 
#              year, including fractional days.
# Parameters: 
#   - x: A POSIX date-time object.
# Returns: A numeric value representing the day of the year with fractional 
#          parts.
#===============================================================================

date2doy <- function(x) {
  # Ensure the input is a POSIX date-time object
  stopifnot(
    any(
      grepl(
        pattern = "^POSIX",
              x = class(
                    x = x[1]
                  )
      )
    )
  )
  
  # Extract the day of the year as a numeric value
  day <- as.numeric(
           x = format(
                      x = x,
                 format = "%j"
               )
         )
  
  # Pattern to match the time part of the date-time string
  pat <- quote(
           expr = "([0-9]{2}:){2}[0-9]{2}"
         )
  
  # Convert the time part to midnight (00:00:00) in America/Denver timezone
  midnight <- as.POSIXct(
                 x = gsub(
                           pattern = pat,
                       replacement = "00:00:00",
                                 x = x
                     ),
                tz = "America/Denver"
              )
  
  # Calculate the fractional day difference from midnight
  frac <- as.numeric(
            x = difftime(
                  time1 = x,
                  time2 = midnight,
                  units = "days"
                )
          )
  
  # Return the day of the year with the fractional part
  day + frac
}

#===============================================================================
# Function Name: byeShort
# Description: This function removes duplicates and incomplete records from a 
#              dataframe based on specified criteria, ensuring data integrity 
#              for time series analysis.
# Parameters: 
#   - X: A dataframe containing the original data.
#   - percentReqd: A numeric value specifying the required percentage of expected 
#                  records for a group to be retained.
#   - Expected: A numeric value specifying the expected number of records per group.
#   - ToCount: A character string specifying the column name used for counting 
#              records.
#   - TruncToCount: A logical value indicating whether to truncate the count.
#   - By: A character vector specifying the columns used to define groups.
# Returns: The updated dataframe with duplicates and incomplete records removed.
#===============================================================================

byeShort <- function(
  X,
  percentReqd = 0.80,
  Expected = 288,
  ToCount = "doy",
  TruncToCount = TRUE,
  By = c(
         "year",
         "doy"
       )
) {
  # Function to remove duplicates and incomplete records based on specified criteria
  
  # Helper function to remove duplicates based on the ToCount column
  dups <- function(x) {
    x[!duplicated(
         x = round(
                    x = x[ , ToCount],
               digits = 9
             )
       ), ]
  }
  
  # Remove duplicates within each group defined by By, excluding ToCount
  X <- ddply(
              .data = X,
         .variables = setdiff(
                        x = By,
                        y = ToCount
                      ),
               .fun = dups
       )
  
  # Create an index column for tracking row positions
  ByInd <- data.frame(
             X[ , By],
             "IND" = 1:nrow(
                         x = X
                       )
           )
  
  # Helper function to calculate the size and range of each group
  which_nrow <- function(x) {
    c(
       "Size" = nrow(
                  x = x
                ),
      "Start" = min(
                  x[ , "IND"]
                ),
       "Stop" = max(
                  x[ , "IND"]
                )
    )
  }
  
  # Apply the which_nrow function to each group defined by By
  Sizes <- ddply(
                  .data = trunc(
                            x = ByInd
                          ),
             .variables = By,
                   .fun = which_nrow
           )
  
  # Identify groups that are too short based on the expected size and required percentage
  TooShort <- Sizes[which(
                      Sizes[ , "Size"] < Expected * percentReqd
                    ), c(
                         "Start",
                         "Stop"
                       )]
  
  # Helper function to generate a sequence from Start to Stop
  Start2Stop <- function(x) {
    x[1]:x[2]
  }
  
  # Generate a vector of row indices to be removed
  WaveTo <- unlist(
                      x = apply(
                                 X = TooShort,
                            MARGIN = 1,
                               FUN = Start2Stop
                          ),
              use.names = FALSE
            )
  
  # Print the number of points removed
  print(
    paste(
      "Points removed due to incomplete day or duplicated time step:",
      length(
        x = WaveTo
      ),
      sep = " "
    )
  )
  flush.console()
  
  # Remove the identified rows from the dataset
  if (length(
        x = WaveTo
      ) != 0) {
    Xr <- X[-WaveTo, ]
  } else {
      Xr <- X
    }
  
  return(Xr)
}

#===============================================================================
# Function Name: conquerList
# Description: This function processes a list of data frames or lists, combining 
#              them into a single data frame or list based on specified criteria.
# Parameters: 
#   - x: A list of data frames or lists to be combined.
#   - naming: An optional data frame to be used for naming the combined results.
# Returns: A combined data frame or list based on the input structure.
#===============================================================================

conquerList <- function(
  x,
  naming = NULL
) {
  # If x is not a list or is a data frame, return x as is
  if (!is.list(
         x = x
       ) | is.data.frame(
             x = x
           )) {
    return(x)
  }
  
  s1 <- length(
          x = x
        )
  s2 <- length(
          x = x[[1]]
        )
  u1 <- unlist(
                  x = x,
          recursive = FALSE
        )
  
  # Ensure the length of the unlisted elements matches the expected size
  stopifnot(
    length(
      x = u1
    ) == s1 * s2
  )
  
  # Check if the first element of x is a data frame with a single row
  if (is.data.frame(
        x = x[[1]]
      )) {
    single.row <- nrow(
                    x = x[[1]]
                  ) == 1L
  } else {
      single.row <- FALSE
    }
  
  # If each element of the list x contains a row of a data frame, return combined data frame
  if (single.row & is.list(
                     x = x
                   )) {
    return(
      cbind(
        naming,
        ldply(
          .data = x
        )
      )
    )
  }
  
  # Initialize a list to store the results
  cqd <- vector(
             mode = "list",
           length = s2
         )
  
  # Loop through each column of the first element of x
  for (i in 1:s2) {
    # Generate a sequence of indices for the i-th column across all elements of x
    ti <- seq(
            from = i,
              to = s1 * s2,
              by = s2
          )
    tl <- vector(
              mode = "list",
            length = s1
          )
    
    # Extract the i-th column from each element of x
    for (j in 1:s1) {
      tl[[j]] <- u1[[ti[j]]]
    }
    
    # Combine the extracted columns into a data frame or list
    if (is.data.frame(
          x = tl[[1]]
        ) | !is.list(
               x = tl[[1]]
             )) {
      if (!is.null(
             x = naming
           )) {
        cqd[[i]] <- cbind(
                      naming,
                      ldply(
                        .data = tl
                      )
                    )
      } else {
        cqd[[i]] <- ldply(
                      .data = tl
                    )
        }
    } else {
      cqd[[i]] <- llply(
                    .data = tl
                  )
      }
  }
  
  return(cqd)
}

#===============================================================================
# Function Name: automate_git
# Description: This function stages, commits, and pushes changes to a GitHub 
#              repository using system commands.
# Parameters: 
#   - commit_message: A message for the commit. Defaults to "Automated commit 
#                     from R script".
# Returns: None.
#===============================================================================

automate_git <- function(commit_message = "Automated commit from R script") {
  # Stage all changes
  system("git add .")
  
  # Commit the changes
  system(paste('git commit -m', shQuote(commit_message)))
  
  # Push the changes
  system("git push -u origin main")
}



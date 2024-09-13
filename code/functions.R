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

remove_do_anomalies <- function(df, threshold = 2) {
  df %>%
    mutate(do_diff = c(NA, diff(do.obs)), 
           do.obs = ifelse(abs(do_diff) > threshold, NA, do.obs)) %>% 
    select(-do_diff)
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
# Function Name: calculate_gaussian_r_squared
# Description: This function calculates the R-squared value for a Gaussian fit 
#              model and dataset.
# Parameters: 
#   - model: The Gaussian model for which to calculate R-squared.
#   - data: A dataframe containing the observed Kd values.
# Returns: The R-squared value indicating the proportion of variance explained 
#          by the model.
#===============================================================================

calculate_gaussian_r_squared <- function(model, data) {
  observed <- data$Kd
  fitted <- predict(model, newdata = data)
  tss <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
  rss <- sum((observed - fitted)^2, na.rm = TRUE)
  r_squared <- 1 - (rss / tss)
  return(r_squared)
}

#===============================================================================
# Function Name: add_gaussian_predictions
# Description: This function adds Gaussian predictions to a dataframe using a 
#              provided model.
# Parameters: 
#   - df: A dataframe containing the day of year ('doy') column.
#   - model: A Gaussian model used for prediction.
# Returns: The updated dataframe with the 'Kd' column containing the predicted 
#          values.
#===============================================================================

add_gaussian_predictions <- function(df, model) {
  day_of_year <- df$doy
  newdata <- data.frame(day_of_year = day_of_year)
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



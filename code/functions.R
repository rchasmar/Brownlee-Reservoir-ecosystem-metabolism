#===============================================================================
# Function to interpolate gaps of 10 or fewer NAs using linear interpolation
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
# Function to interpolate gaps of 10 or fewer NAs using spline interpolation
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
  
  # Interpolate only the marked gaps using spline
  interpolated_values <- spline(seq_along(data[[column]]), 
                                data[[column]], 
                                xout = seq_along(data[[column]]))$y
  data[[column]][interpolate] <- interpolated_values[interpolate]
  
  return(data)
}

#===============================================================================
# Function to subset dataframe
#===============================================================================

subset_minutes <- function(df) {
  df[format(df$'datetime', "%M") %in% sprintf("%02d", seq(0, 50, by = 10)), ]
}

#===============================================================================
# Function to compute 100% saturation oxygen concentration at nonstandard 
# pressure from temperature (K)
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
#
#===============================================================================

remove_do_anomalies <- function(df, threshold = 2) {
  df %>%
    mutate(do_diff = c(NA, diff(do.obs)), 
           do.obs = ifelse(abs(do_diff) > threshold, NA, do.obs)) %>% 
    select(-do_diff)
}

#===============================================================================
# Function to remove rows where 'top' is not a real number
#===============================================================================

remove_non_finite_top <- function(df) {
  df <- df[is.finite(df$top), ]
  return(df)
}

#===============================================================================
# Function to perform the analysis and create the plot
# Parameters:
#   df: Dataframe containing the data
#   title: Title for the plot
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
  new_time <- seq(1, 366, by = 1)
  
  # Create a new dataframe for prediction
  newdata <- data.frame(Time = new_time,
                        xc = cos(2 * pi * new_time / 365.25),
                        xs = sin(2 * pi * new_time / 365.25))
  
  # Find predictions for the new time series
  pred <- predict(fit.lm, newdata = newdata)
  
  # Plot the original data
  plot(df$meta ~ Time, main = title, xlab = "Time", ylab = "Meta", 
       ylim = c(0, 40), xlim = c(0, 366))
  
  # Add the predicted values to the plot
  lines(new_time, pred, col = "red", lwd = 2)
}

#===============================================================================
# Fit models for the meta dataframes
#===============================================================================

fit_model <- function(df) {
  Time <- df$doy
  xc <- cos(2 * pi * Time / 365.25)
  xs <- sin(2 * pi * Time / 365.25)
  lm(df$meta ~ xc + xs)
}

#===============================================================================
# Function to predict and add z.mix column
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
# Function to stage, commit, and push changes to GitHub
#===============================================================================

automate_git <- function(commit_message = "Automated commit from R script") {
  # Stage all changes
  system("git add .")
  
  # Commit the changes
  system(paste('git commit -m', shQuote(commit_message)))
  
  # Push the changes
  system("git push -u origin main")
}



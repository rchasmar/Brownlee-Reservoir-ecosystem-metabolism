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




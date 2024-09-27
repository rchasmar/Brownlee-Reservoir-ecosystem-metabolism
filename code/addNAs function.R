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

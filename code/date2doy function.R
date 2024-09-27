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
  
  # Convert the time part to midnight (00:00:00) in GMT timezone
  midnight <- as.POSIXct(
                 x = gsub(
                           pattern = pat,
                       replacement = "00:00:00",
                                 x = x
                     ),
                tz = "GMT"
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

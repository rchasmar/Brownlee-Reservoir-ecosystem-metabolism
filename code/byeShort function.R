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

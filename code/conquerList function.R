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

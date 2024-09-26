

# Create a sequence of dates
Date <- seq(as.POSIXct("2017-01-01"), as.POSIXct("2024-01-01"), by = "year")


par(
  mfrow = c(6, 1)
)
plot(do.obs ~ datetime, data = ppr286_01, xlab = "")  
axis.POSIXct(1, at = Date, format = "%Y")
abline(v = Date, lty = 'dashed')
plot(do.sat ~ datetime, data = ppr286_01, xlab = "")  
axis.POSIXct(1, at = Date, format = "%Y")
abline(v = Date, lty = 'dashed')
plot(wtr ~ datetime, data = ppr286_01, xlab = "")  
axis.POSIXct(1, at = Date, format = "%Y")
abline(v = Date, lty = 'dashed')
plot(k.gas ~ datetime, data = ppr286_01, xlab = "")  
axis.POSIXct(1, at = Date, format = "%Y")
abline(v = Date, lty = 'dashed')
plot(z.mix ~ datetime, data = ppr286_01, xlab = "")  
axis.POSIXct(1, at = Date, format = "%Y")
abline(v = Date, lty = 'dashed')
plot(irr ~ datetime, data = ppr286_01, xlab = "")  
axis.POSIXct(1, at = Date, format = "%Y")
abline(v = Date, lty = 'dashed')

plot(irr ~ datetime, data = ppr286_30, xlab = "", ylim = c(0, 0.1))  
axis.POSIXct(1, at = Date, format = "%Y")
abline(v = Date, lty = 'dashed')


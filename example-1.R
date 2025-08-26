# Examples from grDevices::plotmath helpfil
if (Sys.info()['sysname'] == "Darwin") os <- "macos"
if (Sys.info()['sysname'] == "Linux") os <- "linux"
if (Sys.info()['sysname'] == "Windows") os <- "windows"

library(ragg)
library(glue)

reso <- 300
scl <- reso/72

agg_png(glue('img/example-1-ragg-plot-{os}.png'), units = "px", res = reso, height = scl*480, width = scl*480)
par(mar = c(5, 5, 5, 5), cex.lab = 1.3)
plot(1:10, xlab = expression(hat(beta)), ylab = expression(hat(beta)))
title("Plot exported with agg_png()")
dev.off()

png(glue('img/example-1-png-plot-{os}.png'), units = "px", res = reso, height = scl*480, width = scl*480)
par(mar = c(5, 5, 5, 5), cex.lab = 1.3)
plot(1:10, xlab = expression(hat(beta)), ylab = expression(hat(beta)))
title("Plot exported with png()")
dev.off()

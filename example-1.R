# Examples from grDevices::plotmath helpfil
if (Sys.info()['sysname'] == "Darwin") os <- "macos"
if (Sys.info()['sysname'] == "Linux") os <- "linux"
if (Sys.info()['sysname'] == "Windows") os <- "windows"

library(ragg)
library(glue)

agg_png(glue('img/example-1-ragg-plot-{os}.png'))
par(mar = c(5, 5, 5, 5), cex.lab = 1.3)
plot(1:10, xlab = expression(hat(beta)), ylab = expression(hat(beta)))
title("Plot exported with agg_png()")
dev.off()

png(glue('img/example-1-png-plot-{os}.png'))
par(mar = c(5, 5, 5, 5), cex.lab = 1.3)
plot(1:10, xlab = expression(hat(beta)), ylab = expression(hat(beta)))
title("Plot exported with png()")
dev.off()

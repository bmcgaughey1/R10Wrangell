# compare metrics with and without class 1 points
#
library(terra)


# first set of files with no obvious incorrectly classified points --------
# Height P95
P95_wo_class1File <- "H:/R10Wrangell/AP/Products_WrangellMetricsTest_2024-03-08/FINAL_WrangellMetricsTest_2024-03-08/Metrics_30METERS/elev_P95_2plus_30METERS.asc"
P95_w_class1File <- "H:/R10Wrangell/AP1/Products_WrangellMetricsTest_2024-03-08/FINAL_WrangellMetricsTest_2024-03-08/Metrics_30METERS/elev_P95_2plus_30METERS.asc"

# read files
P95wc1 <- rast(P95_w_class1File)
P95woc1 <- rast(P95_wo_class1File)

hist(P95wc1, breaks = 20, main = "P95 height with class 1 points", xlab = "P95 height (m)")
hist(P95woc1, breaks = 20, main = "P95 height without class 1 points", xlab = "P95 height (m)")

P95diff <- P95wc1 - P95woc1
summary(P95diff)

hist(P95diff, breaks = 20, main = "Difference", xlab = "P95 height difference (with class 1 - without) (m)")

# Height max
Max_wo_class1File <- "H:/R10Wrangell/AP/Products_WrangellMetricsTest_2024-03-08/FINAL_WrangellMetricsTest_2024-03-08/Metrics_30METERS/elev_max_2plus_30METERS.asc"
Max_w_class1File <- "H:/R10Wrangell/AP1/Products_WrangellMetricsTest_2024-03-08/FINAL_WrangellMetricsTest_2024-03-08/Metrics_30METERS/elev_max_2plus_30METERS.asc"

Maxwc1 <- rast(Max_w_class1File)
Maxwoc1 <- rast(Max_wo_class1File)

hist(Maxwc1, breaks = 20, main = "Max height with class 1 points", xlab = "P95 height (m)")
hist(Maxwoc1, breaks = 20, main = "Max height without class 1 points", xlab = "P95 height (m)")

Maxdiff <- Maxwc1 - Maxwoc1
summary(Maxdiff)

hist(Maxdiff, breaks = 20, main = "Difference", xlab = "Max height difference (with class 1 - without) (m)")

# second set of points that has incorrectly classified points -------------
P95_wo_class1File <- "H:/R10Wrangell/AP683_noclass1/Products_WrangellMetricsTest_2024-03-08/FINAL_WrangellMetricsTest_2024-03-08/Metrics_30METERS/elev_P95_2plus_30METERS.asc"
P95_w_class1File <- "H:/R10Wrangell/AP683c1/Products_WrangellMetricsTest_2024-03-08/FINAL_WrangellMetricsTest_2024-03-08/Metrics_30METERS/elev_P95_2plus_30METERS.asc"

# read files
P95wc1 <- rast(P95_w_class1File)
P95woc1 <- rast(P95_wo_class1File)

hist(P95wc1, breaks = 20, main = "P95 height with class 1 points", xlab = "P95 height (m)")
hist(P95woc1, breaks = 20, main = "P95 height without class 1 points", xlab = "P95 height (m)")

P95diff <- P95wc1 - P95woc1
summary(P95diff)

hist(P95diff, breaks = 20, main = "Difference", xlab = "P95 height difference (with class 1 - without) (m)")
plot(P95diff, main = "P95 difference", xlab = "P95 height difference (with class 1 - without) (m)")

# Height max
Max_wo_class1File <- "H:/R10Wrangell/AP683_noclass1/Products_WrangellMetricsTest_2024-03-08/FINAL_WrangellMetricsTest_2024-03-08/Metrics_30METERS/elev_max_2plus_30METERS.asc"
Max_w_class1File <- "H:/R10Wrangell/AP683c1/Products_WrangellMetricsTest_2024-03-08/FINAL_WrangellMetricsTest_2024-03-08/Metrics_30METERS/elev_max_2plus_30METERS.asc"

Maxwc1 <- rast(Max_w_class1File)
Maxwoc1 <- rast(Max_wo_class1File)

hist(Maxwc1, breaks = 20, main = "Max height with class 1 points", xlab = "P95 height (m)")
hist(Maxwoc1, breaks = 20, main = "Max height without class 1 points", xlab = "P95 height (m)")

Maxdiff <- Maxwc1 - Maxwoc1
summary(Maxdiff)

hist(Maxdiff, breaks = 20, main = "Difference", xlab = "Max height difference (with class 1 - without) (m)")
plot(Maxdiff, main = "Max difference", xlab = "Max height difference (with class 1 - without) (m)")



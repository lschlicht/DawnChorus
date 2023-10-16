tmp = subset(ActivityDataPerPeriod, direction_reliable == 1 & overlap == 1 & DB == -36 & !is.na(f_type))
tmp[, any_bigger_zero := max(no_vocs, na.rm = TRUE), by = counter]
tmp[, f_type := factor(f_type, levels = c("before", "during", "after"))]
COL_dawn = colours()[419]
COL_dusk = colours()[430]
COL_day = colours()[86]

OUTLINE = TRUE
SAME_Y_RANGE = TRUE
CEX_TEXT = 1.5

jpeg("2023-04-06 Raw data.jpeg", width = 900, height = 600)

par(las = 1)
par(cex.axis = 1.5)
XLIM = c(0.5, 11.5)
if(OUTLINE == TRUE) YLIM = list(c(-0.5, 14), c(-0.5, 12), c(-0.5, 6))
if(OUTLINE == FALSE) YLIM = list(c(-0.5, 14), c(-0.5, 6), c(-0.5, 0.5))
if(SAME_Y_RANGE == TRUE) YLIM = list(c(-0.5, 15), c(-0.5, 15), c(-0.5, 15))
#YLIM = list(c(0, 14), c(0, 6), c(0, 5))
boxstats = boxplot(tmp[sleep == 1 & type == "during", vocs_per_time],
        tmp[sleep == 1 & type == "after", vocs_per_time],
        at = c(1, 2), xaxt = "n", xlim = XLIM, ylim = YLIM[[1]], yaxt = "n", outline = OUTLINE, notch = TRUE, col = COL_dawn)
axis(2)
points(1, mean(tmp[sleep == 1 & type == "during", vocs_per_time]), pch = 4, cex = 5)
points(2, mean(tmp[sleep == 1 & type == "after", vocs_per_time]), pch = 4, cex = 5)
text(c(1.5), -0.5, paste0("N = ", boxstats$n[1]), cex = CEX_TEXT)


par(new = TRUE)
boxstats = boxplot(tmp[sleep == 0 & type == "before", vocs_per_time],
        tmp[sleep == 0 & type == "during", vocs_per_time],
        tmp[sleep == 0 & type == "after", vocs_per_time],
        at = c(5, 6, 7), xaxt = "n", xlim = XLIM, ylim = YLIM[[2]], yaxt = "n", outline = OUTLINE, notch = TRUE, col = COL_day)
axis(2, line = -19)
points(5, mean(tmp[sleep == 0 & type == "before", vocs_per_time]), pch = 4, cex = 5)
points(6, mean(tmp[sleep == 0 & type == "during", vocs_per_time]), pch = 4, cex = 5)
points(7, mean(tmp[sleep == 0 & type == "after", vocs_per_time]), pch = 4, cex = 5)
text(c(6), -0.5, paste0("N = ", boxstats$n[1]), cex = CEX_TEXT)


par(new = TRUE)
boxstats = boxplot(tmp[sleep == -1 & type == "before", vocs_per_time],
        tmp[sleep == -1 & type == "during", vocs_per_time],
        at = c(10, 11), xaxt = "n", xlim = XLIM, ylim = YLIM[[3]], yaxt = "n", outline = OUTLINE, notch = TRUE, col = COL_dusk)
axis(2, line = -43)
points(10, mean(tmp[sleep == -1 & type == "before", vocs_per_time]), pch = 4, cex = 5)
points(11, mean(tmp[sleep == -1 & type == "during", vocs_per_time], na.rm = TRUE), pch = 4, cex = 5)
text(c(10.5), -0.5, paste0("N = ", boxstats$n[1]), cex = CEX_TEXT)

axis(1, at = c(1, 2, 5, 6, 7, 10, 11), labels = c("Inside", "After", "Before", "Inside", "After", "Before", "Inside"))
axis(1, at = 6, labels = "♀ in box", line = 2, lwd = 0)
axis(3, at = c(1.5, 6, 10.5), labels = c("Dawn", "Day", "Dusk"), lwd = 0, cex.axis = 2)

dev.off()

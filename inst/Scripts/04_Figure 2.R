#based on ActivityDataPerMinute and ActivityDataPerPeriod

require(scales)

#stats 3: slope while female inside nestbox #####
#overall test 2
DB_value = -36

#glmmTMB
m = glmmTMB(no_vocs ~ time_to_in + (1|box) +  (1|counter), data = subset(ActivityDataPerMinute, sleep == 0 & DB == -36 & f_type == "during" & direction_reliable == 1), ziformula = ~0, family = poisson)
#for some reason the confidence interval that is calculated when the randeom effects are included do not reflect well the model output. Removing the random intercepts from the model does not change the slope estimate or the statistical significance, but it does yield a figure where the numbers match the model output. I therefore use the model without random intercepts for the figure. The figure looks almost identical when using bootMer instead of se.fit 0> use se.fit which takes less computing time.
m0 = glmmTMB(no_vocs ~ time_to_in, data = subset(ActivityDataPerMinute, sleep == 0 & DB == -36 & f_type == "during" & direction_reliable == 1), ziformula = ~0, family = poisson)
summary(m0)$coef$cond
summary(m)$coef$cond



P = predict(m, subset(ActivityDataPerMinute, sleep == 0 & DB == -36 & f_type == "during" & direction_reliable == 1), type = "link", re.form = NA, se.fit = TRUE)
P = data.table(fit = P$fit, se.fit = P$se.fit, time_to_in = subset(ActivityDataPerMinute, sleep == 0 & DB == -36 & f_type == "during" & direction_reliable == 1)$time_to_in)
P[, CIlow := fit - 1.96*se.fit]
P[, CIup := fit + 1.96*se.fit]
setorder(P, time_to_in)
P[, fit := family(m)$linkinv(fit)]
P[, CIlow := family(m)$linkinv(CIlow)]
P[, CIup := family(m)$linkinv(CIup)]

#calculate song rates: I tried different models and methods. This way the stats are most correctly fit, and the absolute values are similar to the (raw) box plots.
ActivityDataPerPeriod_tmp = copy(ActivityDataPerPeriod)
ActivityDataPerPeriod_tmp[, time_inside_min2 := time_inside_min]
ActivityDataPerPeriod_tmp[sleep == 1, time_inside_min2 := 30]
ActivityDataPerPeriod_tmp[sleep == -1, time_inside_min2 := 10]
for(k in 1:nrow(MOD1)) {
  tmp = subset(ActivityDataPerPeriod_tmp, direction_reliable != 100 & overlap == 1 & sleep == MOD1[k,sleep])
  MOD1[k, YYt := YY/mean(tmp[, time_inside_min2])]
  MOD1[k, LSDlowt := LSDlow/mean(tmp[, time_inside_min2])]
  MOD1[k, LSDupt := LSDup/mean(tmp[, time_inside_min2])]
}


MOD3 <- P





#FIGURE#####
grDevices::cairo_pdf(paste0(getwd(), "/ResultsAndFigures/Figure 2.pdf"))
COL_dayFig2 = "black"
CEX_LAB = 2
CEX_AXIS = 1.5
par(mar = c(5, 5, 3, 0.5))
par(las = 1)
par(family = "serif")
plot(MOD3$fit~MOD3$time_to_in, type = "n", ylim = c(0, 3),ylab = "", xlab = "Time since ♀ nestbox entry (min)", cex.axis = CEX_AXIS, cex.lab = CEX_LAB, axes = FALSE)
axis(1, at = c(-1:5)*10, labels = c(-1:5)*10, cex.axis = CEX_AXIS)
axis(2, at = -10:10, labels = -10:10, cex.axis = CEX_AXIS)
axis(2, at = 1.5, labels = "Song rate (strophes/min) ± CI", tick = FALSE, line = 2, cex.axis = CEX_LAB, hadj = 0.5, las = 0)

polygon(c(MOD3[,time_to_in], rev(MOD3[,time_to_in])), c(MOD3[,CIlow],rev(MOD3[, CIup])), col = alpha(COL_dayFig2, alpha = 0.2), border = NA)
lines(MOD3[,time_to_in], MOD3[,fit], col = colorRampPalette(c(COL_dayFig2, "black"))(100)[50], lwd = 2)

dev.off()









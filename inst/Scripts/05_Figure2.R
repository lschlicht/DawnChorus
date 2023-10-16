#based on ActivityDataPerMinute and ActivityDataPerPeriod

require(scales)

#stats 3: slope while female inside nestbox #####
#overall test 2
DB_value = -36

#glmmTMB
m = glmmTMB(no_vocs ~ time_to_in + (1|box) +  (1|counter), data = subset(ActivityDataPerMinute, sleep == 0 & DB == -36 & f_type == "during" & direction_reliable == 1), ziformula = ~1, family = poisson)
#for some reason the confidence interval that is calculated when the random effects are included do not reflect well the model output. Removing the random intercepts from the model does not change the slope estimate or the statistical significance, but it does yield a figure where the numbers match the model output. I therefore use the model without random intercepts for the figure. The figure looks almost identical when using bootMer instead of se.fit 0> use se.fit which takes less computing time.
m0 = glmmTMB(no_vocs ~ time_to_in, data = subset(ActivityDataPerMinute, sleep == 0 & DB == -36 & f_type == "during" & direction_reliable == 1), ziformula = ~0, family = poisson)
summary(m0)$coef$cond
summary(m)$coef$cond


#predict original range
P = predict(m, subset(ActivityDataPerMinute, sleep == 0 & DB == -36 & f_type == "during" & direction_reliable == 1), type = "link", re.form = NA, se.fit = TRUE)
P = data.table(fit = P$fit, se.fit = P$se.fit, time_to_in = subset(ActivityDataPerMinute, sleep == 0 & DB == -36 & f_type == "during" & direction_reliable == 1)$time_to_in)

P[, CIlow := fit - 1.96*se.fit]
P[, CIup := fit + 1.96*se.fit]
setorder(P, time_to_in)
P[, fit := family(m)$linkinv(fit)]
P[, CIlow := family(m)$linkinv(CIlow)]
P[, CIup := family(m)$linkinv(CIup)]

#predict new range
Pn = predict(m, newdata = data.table(time_to_in = (1:1000)/10, box = NA, counter = NA), type = "link", re.form = NA, se.fit = TRUE)
Pn = data.table(fit = Pn$fit, se.fit = Pn$se.fit, time_to_in = (1:1000)/10, box = NA, counter = NA)


Pn[, CIlow := fit - 1.96*se.fit]
Pn[, CIup := fit + 1.96*se.fit]
setorder(Pn, time_to_in)
Pn[, fit := family(m)$linkinv(fit)]
Pn[, CIlow := family(m)$linkinv(CIlow)]
Pn[, CIup := family(m)$linkinv(CIup)]

#get average dawn song rate
source("inst/Scripts/03_Figure 1.R")
SongRateDawn = MOD1[sleep == 1 & var == "during",YYt]

#define original range of song rates
RangeTimeToIn = range(subset(ActivityDataPerMinute, sleep == 0 & DB == -36 & f_type == "during" & direction_reliable == 1)$time_to_in)[2]


#FIGURE A#####
MOD3 <- P
grDevices::cairo_pdf(paste0(getwd(), "/ResultsAndFigures/Figure 2A.pdf"))
COL_dayFig2 = "black"
CEX_LAB = 2
CEX_AXIS = 1.5
par(mar = c(5, 5, 3, 0.5))
par(las = 1)
#par(family = "serif")
plot(MOD3$fit~MOD3$time_to_in, type = "n", ylim = c(0, 6.1),ylab = "", xlab = "Time since ♀ nestbox entry (min)", cex.axis = CEX_AXIS, cex.lab = CEX_LAB, axes = FALSE)
axis(1, at = c(-1:5)*10, labels = c(-1:5)*10, cex.axis = CEX_AXIS)
axis(2, at = -10:10, labels = -10:10, cex.axis = CEX_AXIS)
axis(2, at = 3, labels = "Song rate (strophes/min) ± CI", tick = FALSE, line = 2, cex.axis = CEX_LAB, hadj = 0.5, las = 0)

polygon(c(MOD3[,time_to_in], rev(MOD3[,time_to_in])), c(MOD3[,CIlow],rev(MOD3[, CIup])), col = alpha(COL_dayFig2, alpha = 0.2), border = NA)
lines(MOD3[,time_to_in], MOD3[,fit], col = colorRampPalette(c(COL_dayFig2, "black"))(100)[50], lwd = 2)
dev.off()


#FIGURE B#####
MOD3 <- Pn
MOD3[, extrapolated := ifelse(time_to_in <= RangeTimeToIn, 0, 1)]
MOD3[, COL := ifelse(time_to_in <= RangeTimeToIn, "black", "red")]
grDevices::cairo_pdf(paste0(getwd(), "/ResultsAndFigures/Figure 2B.pdf"))
#COL_dayFig2 = "black"
  CEX_LAB = 2
  CEX_AXIS = 1.5
  YLIM = c(0, 15)
  par(mar = c(5, 5, 3, 0.5))
  par(las = 1)
  plot(c(0,0) ~ c(100, 100), xlim = range(MOD3$time_to_in), ylim = YLIM, type = "n", ylab = "", xlab = "Time since ♀ nestbox entry (min)", cex.axis = CEX_AXIS, cex.lab = CEX_LAB, axes = FALSE)
  lines(MOD3[extrapolated == 1,time_to_in], MOD3[extrapolated == 1, fit], col = MOD3[extrapolated == 1, COL][1], lwd = 2)
  lines(MOD3[extrapolated == 0,time_to_in], MOD3[extrapolated == 0, fit], col = MOD3[extrapolated == 0, COL][1], lwd = 2)

  axis(1, at = c(-1:50)*10, labels = c(-1:50)*10, cex.axis = CEX_AXIS)
  axis(2, at = -10:20, labels = -10:20, cex.axis = CEX_AXIS)
  axis(2, at = 4.5, labels = "Song rate (strophes/min) ± CI", tick = FALSE, line = 2, cex.axis = CEX_LAB, hadj = 0.5, las = 0)

  polygon(c(MOD3[extrapolated == 1,time_to_in], rev(MOD3[extrapolated == 1,time_to_in])), c(MOD3[extrapolated == 1,CIlow],rev(MOD3[extrapolated == 1, CIup])), col = alpha(MOD3[extrapolated == 1, COL][1], alpha = 0.2), border = NA)
  polygon(c(MOD3[extrapolated == 0,time_to_in], rev(MOD3[extrapolated == 0,time_to_in])), c(MOD3[extrapolated == 0,CIlow],rev(MOD3[extrapolated == 0, CIup])), col = alpha(MOD3[extrapolated == 0, COL][1], alpha = 0.2), border = NA)

  abline(h = SongRateDawn)
  dev.off()



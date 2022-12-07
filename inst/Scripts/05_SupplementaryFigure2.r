#Supplemental Figure 2: Plot observational data
#library(readr)

ObservationsData = data.table(read.table(paste0(getwd(), "/extdata/Observations.csv"), sep = ";"))

#warnings are ok...
setnames(ObservationsData, names(ObservationsData), c("V1", "path", "time", "V4", "V5", "V6", "d_class", "distance_org", "location", "dB"))
ObservationsData[, fd_class := factor(as.numeric(d_class))] #NAs are introduced on purpose
ObservationsData[, distance := parse_number(distance_org)]#NAs are introduced on purpose
ObservationsData[, dB := as.numeric(dB)]

ObservationsData = subset(ObservationsData, !is.na(fd_class)) #to remove calls whith can be a lot more quiet


#correct positive dB signs (typos)
ObservationsData[dB > 0, dB := -dB]

DELTA = mean(c(4, 4, 2, 1, 3, 7, 7, 5, 6, 3, 3, 3, 4)) #avg. db difference between Song Scope and Kaleidoscope (correct for software difference)
ObservationsData[, dB := dB + DELTA ]



#Figure S2
par(mfrow = c(3,1))
CEX_AXIS = 1.5
CEX_LAB = 2.5
par(mar = c(4.6, 5.6, 1.1, 4.6))
COL_CLASSES = c("red", "blue")

ObservationsData[, COL := "black"]
ObservationsData[fd_class == -2, COL := COL_CLASSES[1]]
ObservationsData[fd_class == 2, COL := COL_CLASSES[2]]


#panel 1
m = lmer(distance ~ dB + (1|location), data = ObservationsData) #model is singular, but results are the same as linear model.

m = lm(distance ~ dB, data = ObservationsData)
summary(m)$coefficients[,2]
#qqnorm(resid(m))
#hist(resid(m))
R2 = summary(m)$r.squared

ObservationsData_sub = subset(ObservationsData, !is.na(distance) & !is.na(dB))
plot(distance ~dB, data = ObservationsData_sub, pch = 16, col = ObservationsData_sub[,COL], cex = 1.3, cex.axis = CEX_AXIS, cex.lab = CEX_LAB, xlab = "Loudness (dB)", ylab = "Distance (m)")
abline(summary(m)$coefficients[,1], lwd = 2)
R2 #0.38
legend("topright", expression("R"^2*"=0.38"), bty = "n", cex = 2)
#m2 = lm(dB~distance, data = ObservationsData)
#summary(m2)$coefficients[,2]
#CO2 = summary(m2)$coefficients[,1]
#XX1 = -60
#XX2 = 30
#lines(y = c((XX1-CO2[1])/CO2[2], (XX2-CO2[1])/CO2[2]), x = c(XX1, XX2), lty = 2)

#panel 2
par(las = 1)
#par(cex.axis = 1.5, cex.lab = 2)
boxplot(dB ~ fd_class, data = ObservationsData, xlab = "", boxwex = 0.25, xlim = c(0.5, 5.5), at = (1:5)-0.15, xaxt = "n", cex.axis = CEX_AXIS, cex.lab = CEX_LAB, col = c(COL_CLASSES[1], rep("light grey", 3), COL_CLASSES[2]))
axis(side = 3, at = 1:5, labels = table(ObservationsData$fd_class), line = -4, cex.axis = CEX_AXIS, tick = FALSE)
par(new = TRUE)
plot(dB ~ jitter(as.numeric(d_class)+0.15, factor = 0.5), data = ObservationsData, xlab = "Identity class", xlim = c(-2.5, 2.5), col = ObservationsData[, COL], pch =  16, cex.axis = CEX_AXIS, cex.lab = CEX_LAB)
axis(4, at = DIST[, dB], labels = round(DIST[, distance], digits = 0), ylab = 2, cex.axis = CEX_AXIS)
axis(4, at = -35, labels = "Distance in m", line = 2.5, cex.axis = CEX_LAB, las = 0)

#panel 3
tmp = subset(ObservationsData, !is.na(dB) & (d_class == -2 | d_class == 2), select = c(dB, d_class))
tmp[, dB := round(dB)]
tmp = table(tmp[, d_class], tmp[, dB])
par(las = 1)
XLIM = c(-60, -10)
YLIM = c(-0.5, 10)
plot(c(0,1), c(0,1), type = "n", xlim = XLIM, ylim = YLIM, xlab = "Loudness (dB)", ylab = "Frequency", cex.lab = CEX_LAB, cex.axis = CEX_AXIS)
lines(as.numeric(colnames(tmp)), tmp[1,], col = COL_CLASSES[1], lwd = 2)
lines(as.numeric(colnames(tmp)), tmp[2,], col = COL_CLASSES[2], lwd = 2)
lines(x = c(-40, -32), y = c(-0.4, -0.4), lwd = 15, lend = 1)



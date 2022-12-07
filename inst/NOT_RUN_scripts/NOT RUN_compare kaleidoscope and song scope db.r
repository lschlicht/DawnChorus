library(readxl)
require(data.table)

Comp_SC_KS <- data.table(read_excel("/ds/grpkempenaers/Lotte/R Studio projects/Data for package dc/Observations/2021-12-22 Annotations Song Scope and Kaleidoscope.xlsx"))
setnames(Comp_SC_KS, names(Comp_SC_KS), make.names(names(Comp_SC_KS)))
Comp_SC_KS[, dB.songscope := as.numeric(gsub(',', '.', dB.songscope))]
Comp_SC_KS[, dB.kaleidoscope := as.numeric(gsub(',', '.', dB.kaleidoscope))]
Comp_SC_KS[, delta := dB.songscope - dB.kaleidoscope]
MED = median(Comp_SC_KS[, delta], na.rm = TRUE)
plot(Comp_SC_KS[, dB.songscope] ~ Comp_SC_KS[, dB.kaleidoscope])
abline(MED, 1)
abline(MED-2, 1, col = "red"); abline(MED+2, 1, col = "red")
abline(MED-4, 1, col = "blue"); abline(MED+4, 1, col = "blue")

plot(density(na.omit(Comp_SC_KS[, delta])))
hist(Comp_SC_KS[, delta], 30)

#subtract median
hist(Comp_SC_KS[, delta] - MED, 30)
summary(Comp_SC_KS[, delta] - MED)
#====> use song scope fpr analyses
MED #median is 4. Use
summary(Comp_SC_KS[,dB.songscope])
summary(Comp_SC_KS[,dB.kaleidoscope])

#1. SETUP DATA #####
#require package "dc" OR load ActivityDataPerMinute, ActivityDataPerPeriod, if the latter do not exist, run "load and combine activity and song data.r"  to create them.
#as a backup, these datasets are also saved as .csv files to the extdata folder. In this case, make sure to reformat the relevant columns to POSIXct, that is:
#for ActivityDataPerMinute: in_, out_, time_min, start, end; (the four time_to_... columns are difftime objects with the unit "mins", but that should not matter)
#for ActivityDataPerPeriod: in_, out_, start, end; (f_type is a three-level factor, base level is "during")
SongData = data.table(read.table(paste0(getwd(), "/extdata/SongData.csv"), sep = ","))

#2. SETUP COLOURS #####
COL_FEMALE = "red"
COL_MALE = colours()[131]
COL_dawn = colours()[419]
COL_dusk = colours()[430]
COL_day = colours()[86]
LWD2 = 3

#3. DEFINE FEMALE BEHAVIOUR FOR PANEL A and B #####
def_f = list(
  def_f1 = data.table(in_ = c(0, 10, 11.5, 17), out_ = c(6,10.1,12.3, 24)),
  def_f2 = data.table(in_ = c(0, 8, 16, 17.25), out_ = c(5.8,8.2,16.1, 24)),
  def_f3 = data.table(in_ = c(0, 10, 13, 17.75), out_ = c(6.2, 10.1, 13.2, 24)),
  def_f4 = data.table(in_ = c(0, 8.1, 9, 15, 17.15), out_ = c(6.1, 8.5, 9.1, 15.3, 24)),
  def_f5 = data.table(in_ = c(0, 7.5, 13.1, 17.35), out_ = c(5.9, 7.8, 13.3, 24)),
  def_f6 = data.table(in_ = c(0, 8.7, 14.2, 16.1, 17.55), out_ = c(6.15, 8.9, 14.4, 16.2, 24))

)


#4. CALCULATE MALE SONG AND CREATE RELEVANT DATA FOR PANEL A AND B (based on 3.)#####
#females#####
female = list()
for(i in 1 : length(def_f)) {
  female[[length(female)+1]] = data.table(XLEFT = def_f[[i]][, in_], YBOTTOM = 1.3, XRIGHT = def_f[[i]][, out_], YTOP = 1.7)
}
sFemales = rbindlist(female)
#make polygon definition

sFemales = rbind(data.table(x = sFemales[, XLEFT], y = 1), data.table(x = sFemales[, XRIGHT], y = -1))
setorder(sFemales, x)
sFemales[, y := cumsum(y)]
sFemales[, counter := 1:nrow(sFemales)]
sFemales = na.omit(rbind(cbind(sFemales, df = 2), data.table(x = sFemales[, x], y = shift(sFemales[, y]), counter = sFemales[, counter], df = 1)))
setorder(sFemales, x, counter, df)
sFemales[, ":=" (counter = NULL, df = NULL)]
sFemales = rbind(sFemales, data.table(x = 0, y = 0))

#males#####
male = list()
  for(i in 1 : length(def_f)) {
    def_m1 = data.table(x = sort(c(def_f[[i]][, in_], def_f[[i]][, out_], def_f[[i]][1,out_]-0.5, def_f[[i]][nrow(def_f[[i]]),in_]+(1/6))))
    def_m1 = rbind(def_m1, def_m1)
    setorder(def_m1, x)
    def_m1 = def_m1[c(-1, -nrow(def_m1))]
    def_m1[, rate := c(0, 0, rep(c(1, 1, 0, 0), nrow(def_m1-2)/4))]

    def_m1[, id := i]

    male[[length(male)+1]] = def_m1
  }
  #sum
  sMales = rbindlist(male)
  setorder(sMales, x)
  sMales[, delta := rate - shift(rate, fill = 0), by = id]
  sMales[!is.na(delta), S := round(cumsum(delta), digits = 2)]


#5. DATA FOR EXAMPLE DAY PANEL C #####
#plot main data
BOX = 136
DATE = "2019-04-24"
id = copy(ActivityDataPerMinute)
id[, date_ := as.IDate(start)]
id = subset(id, box == BOX & DB == -36 & date_ == DATE)
id[, time_in := as.numeric(as.ITime(in_))/60/60]
id[, time_out := as.numeric(as.ITime(out_)+out_duration)/60/60]
id[, score_start := as.numeric(as.ITime(start))/60/60]
id[, score_end := as.numeric(as.ITime(end))/60/60]
id[sleep == 1, time_in := 0]
id[sleep == 1, score_start := as.numeric(as.ITime(out_)+out_duration-30*60)/60/60]
id[sleep == 1, score_end := as.numeric(as.ITime(out_)+out_duration+30*60)/60/60]
#male song and calls
idm = subset(SongData, box == BOX & as.IDate(datetime_) == DATE & dB >= -36 & song > 0)
idm[, song_time := as.numeric(as.ITime(datetime_))/60/60]
idm[, YY := ifelse(song == 1, 1.75, 1.25)]

#plot manual analysis

#example one day analsis
library(readxl)
ex <- data.table(read_excel(paste0(getwd(), "/extdata/2022-09-23_full day example_box 136.xlsx"), col_names = FALSE)) #warnings are ok
setnames(ex, names(ex), c("path", "offset", "V1", "V2", "V3", "type", "dB", "ID", "comment"))
ex[, inside := 0]; ex[dB == "inside", inside := 1]
ex[, dB := as.numeric(dB)] #warning is ok
ex[dB > 0, dB := -dB]
ex[, ":=" (V1 = NULL, V2 = NULL, V3 = NULL)]
ex[, time_ := substring(path, nchar(path) - 9, nchar(path) - 4)]
ex[, time_ := as.ITime(paste0(substring(time_, 1, 2), ":", substring(time_, 3, 4), ":", substring(time_, 5, 6)))]
ex[, path := NULL]
ex[, time_ := as.ITime(offset) + time_]
ex[, offset := NULL]
ex[, time_ := as.numeric(time_)/60/60 + 1] #remember that the time is off by 1 hour in the sound data

#dataset of ins and outs
inout = ex[type %in% c("front", "entry", "exit", "left")]
inout = subset(inout, ID %in% c("female", "male"))
setorder(inout, time_)
#rearrange into wide dataset
inout[type == "front", type := "entry"]
inout[type == "left", type := "exit"]

#every "entry" after and "entry" gets a "1", and will be deleted, then events will be classified using cumsum
inout[, counter := ifelse(type == "entry" & shift(type, fill = "exit") == "entry", 1, 0), by = ID]
inout = inout[counter == 0,]
inout[, counter := cumsum(ifelse(type == "entry", 1, 0)), by = ID]
inout = merge(inout[type == "entry", .(ID, counter, time_)], inout[type == "exit", .(ID, counter, time_)], by = c("ID", "counter"), suffixes = c("in", "out"), all = TRUE)
inout[is.na(time_in), time_in := 0]
inout[is.na(time_out), time_out := 24]
inout[, COL := as.numeric(as.factor(ID))+1]

#add times of analysis to inout
inout[ID == "female", start_analysis := time_in - (time_out - time_in)]
inout[counter == max(counter) & ID == "female", start_analysis := time_in - (15/60)] #dusk use 15 minutes
inout[counter == 0 & ID == "female", start_analysis := time_out - 0.5] #dawn use 30 minutes

#remove those that were not analyzed
inout[(time_out - time_in)*60 < 2.5, start_analysis := NA]

inout[ID == "female", end_analysis := time_out + (time_out - time_in)]
inout[counter == max(counter) & ID == "female", end_analysis := time_in + (15/60)] #dusk use 15 minutes
inout[counter == 0 & ID == "female", end_analysis := time_out + 0.5] #dawn use 30 minutes
inout[(time_out - time_in)*60 < 2.5, end_analysis := NA]

inout[ID == "female", COL_rect := COL_day]
inout[ID == "female" & counter == 0, COL_rect := COL_dawn]
inout[ID == "female" & counter == max(counter), COL_rect := COL_dusk]

#sort inout such that day data points are plotted first
inout[, sorting := 0]
inout[counter == 0 | counter == max(counter), sorting := 1]

#define colours
inout[, COL_rect_dark := colorRampPalette(c(COL_rect, "black"))(100)[40], by = rownames(inout)]
inout[, time_in_analysis := ifelse(COL_rect == COL_dawn, start_analysis, time_in)]
inout[, time_out_analysis := ifelse(COL_rect == COL_dusk, end_analysis, time_out)]

setorder(inout, sorting)

#dataset of song and calls
vocs = subset(ex, type %in% c("calls", "talk", "song", "alarm", "courtship calls", "predator tsi-call", "scolding"))
vocs[, rough_type := 0.8] # song
#vocs[type %in% c("calls", "scolding", "talk"), rough_type := 0.6] # calls
#vocs[type %in% c("alarm", "predator tsi-call"), rough_type := 0.4] # alarms
vocs[type %in% c("courtship calls"), rough_type := 0.2] #courtship calls
table(vocs$rough_type)
vocs[, COL := "black"]
vocs[inside == 1, COL := "blue"]
vocs[, CEX := 0.5]
vocs[inside == 1, CEX := 1]
#select only comparable dB to main dataset
vocs = subset(vocs, inside == 1 | dB > -36)

vocs2 = subset(vocs, type == "song" | type == "calls")
interval = 1
vocs2[, time_min := ((time_*60)%/%interval)*interval/60]
vocs2[, rate_min := .N, by = time_min]
vocs2 = unique(subset(vocs2, select = c(COL, CEX, time_min, rate_min)))
tmp = range(vocs2[, time_min])*60/interval
tmp = data.table(time_min = (tmp[1] : tmp[2])*interval/60)
vocs2 = merge(vocs2, tmp, by = "time_min", all = TRUE)
vocs2[is.na(rate_min), ":=" (COL = "black", CEX = 0.5, rate_min = 0)]


#6. STATISTICAL DATA FOR PANEL D#####
#overall test
tmp = subset(ActivityDataPerPeriod, direction_reliable == 1 & overlap == 1)
DB_value = -36
MOD1 = list()
for(SLEEP in c(-1:1)) {
  print(SLEEP)
  if(SLEEP == -1) { m = glmmTMB(no_vocs ~ 0+f_type + (1|counter), data = subset(tmp, sleep == SLEEP & DB == DB_value), ziformula = ~1, family = poisson)
  } else {
  m = glmmTMB(no_vocs ~ 0+f_type + (1|counter), data = subset(tmp, sleep == SLEEP & DB == DB_value), ziformula = ~1, family = poisson)
  }
  tmp2 = data.table(summary(m)$coefficients$cond)
  tmp2[, sleep := SLEEP]
  tmp2[, var := rownames(summary(m)$coefficients$cond)]
  tmp2[, fam := "zero-inflated poisson"]
  #tmp2[, min_df := ifelse(SLEEP == -1, summary(m)$AICtab["df.resid"], min(summary(m)$ngrps$cond))]
  tmp2[, min_df := min(summary(m)$ngrps$cond)]

  #if(SLEEP == -1) {m = glmmTMB(no_vocs ~ f_type, data = subset(tmp, sleep == SLEEP & DB == DB_value), ziformula = ~1, family = poisson)} else {
  m = glmmTMB(no_vocs ~ f_type + (1|counter), data = subset(tmp, sleep == SLEEP & DB == DB_value), ziformula = ~1, family = poisson)
  #}
  tmp2[, SE_diff := mean(summary(m)$coefficients$cond[-1, 2])] #because the differences are quite similar, and this is only for the figure!!! It's the best way to plot it I could find and following Crawley's statbook

  MOD1[[length(MOD1)+1]] = tmp2
}
FAM = family(m)
MOD1 = rbindlist(MOD1)
setnames(MOD1, names(MOD1), c("Est", "SE", "z", "P", "sleep", "var", "fam", "min_df", "SE_diff"))
MOD1[var == "f_typebefore", var := "before"]
MOD1[var == "f_typeafter", var := "after"]
MOD1[var == "f_typeduring", var := "during"]
MOD1[, XX := c(9, 8, 5, 4, 6, 1, 2)]
#MOD1[, XX := c(5, 4, 6, 1, 2)]
MOD1[, YY := FAM$linkinv(Est)]
MOD1[, LSDlow := (FAM$linkinv(Est - (qt(0.975, min_df) * SE_diff)/2))] #LSD, Crawley Statbook, S166
MOD1[, LSDup := (FAM$linkinv(Est + (qt(0.975, min_df) * SE_diff)/2))] #LSD, Crawley Statbook, S166
MOD1[, COL := c(rep(COL_dusk, 2), rep(COL_day, 3), rep(COL_dawn, 2))]


#calculate song rates: I tried different models and methods. This way the stats are most correctly fit, and the absolute values are similar to the (raw) box plots.
ActivityDataPerPeriod_tmp = copy(ActivityDataPerPeriod)
ActivityDataPerPeriod_tmp[, time_inside_min2 := time_inside_min]
ActivityDataPerPeriod_tmp[sleep == 1, time_inside_min2 := 30]
ActivityDataPerPeriod_tmp[sleep == -1, time_inside_min2 := 10]
for(k in 1:nrow(MOD1)) {
  tmp = subset(ActivityDataPerPeriod_tmp, direction_reliable != 100 & overlap == 1 & sleep == MOD1[k,sleep])
  MOD1[k, YYt := YY/median(tmp[, time_inside_min2])]
  MOD1[k, LSDlowt := LSDlow/median(tmp[, time_inside_min2])]
  MOD1[k, LSDupt := LSDup/median(tmp[, time_inside_min2])]
}

#make darker colours
MOD1[, COL_dark := colorRampPalette(c(COL, "black"))(100)[40], by = list(rownames(MOD1))]
MOD1[, COL_analysis := ifelse(var == "during", COL_dark, COL), by = list(rownames(MOD1))]


#END OF DATA PREPARATIONS######



#7. MAKE FIGURE#####

grDevices::cairo_pdf(paste0(getwd(), "/ResultsAndFigures/Figure 1.pdf"), width = 14, height = 6)

#layout #####
layout(matrix(
    c(1, 2,
      3, 4, 5, 6, 7, 8,
      9, 9, 9, 9, 9, 9, 9, 9,
      10, 11,
      12, 12, 12, 12, 12, 12,
      13, 13, 13, 13, 13, 13, 13, 13,
      10, 11,
      12, 12, 12, 12, 12, 12,
      13, 13, 13, 13, 13, 13, 13, 13), ncol = 3),
  heights = rep(c(1.5, 2.5, rep(1, 6), 4),3), widths = c(1, 0.6, 0.4))
#layout.show(13)

par(las = 1)
XLIM = c(5, 18)

#figures #####
#left panels #####
par_mar_right = 2
#title #####
par(mar = c(0,0,0,0))
plot(1:10, 1:10, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
mtext("", side = 1, line = -1, cex = 2)

#plot brightness #####
  BOTTOM = 0
  TOP = 1
  iMAX = 150
  par(mar = c(0, 15, 3, par_mar_right))
  plot(c(4, 22), c(0,1), type = "n", xlab = "", xaxt = "n", bty = "n", ylab = "", yaxt = "n", xlim = XLIM)
  rect(4.5, BOTTOM, 5.5, TOP, col = "black", border = NA) # night: morning
  rect(17, BOTTOM, 22, TOP, col = "black", border = NA) # night: evening
  for(i in 1:iMAX) {
    rect(5+i/100, BOTTOM, 5+i/100+0.1, TOP, col = grey(i/iMAX), border = NA) #day
    rect(18-i/100, BOTTOM, 18-i/100-0.1, TOP, col = grey(i/iMAX), border = NA) #day
  }
  abline(h = c(BOTTOM, TOP))
  text(x = c(5.2, 12, 17.8), y = 0.5, labels = c("night", "day", "night"), col = c("white", "black", "white"), cex = 2)

#plot pairs#####
CEX_AXIS = 1.3

for(i in 1:length(female)) {

  line_of_label = -50
  #female
  par(mar = c(0.3, 15, 0.5, par_mar_right))
  plot(c(4, 21), c(0,1.7), type = "n", xlab = "", xaxt = "n", bty = "n", ylab = "", yaxt = "n", xlim = XLIM)
  rect(female[[i]][, XLEFT], female[[i]][, YBOTTOM], female[[i]][, XRIGHT], female[[i]][, YTOP], border = "black", density = 30)
  if(i == 1) axis(2, at = 1.5, label = c("♀ in box"), cex.axis = CEX_AXIS, line = 0, tick = TRUE)
  lines(male[[i]], lwd = 2)
  if(i == 1) axis(2, at = c(0), label = c("♂ song"), cex.axis = CEX_AXIS, line = 0)
  #axis(2, at = c(0,1), label = c("low", "high"))

  mtext(paste0("Pair ", i), side = 2, line = 10, cex = 1.2, adj = 0)
}




#plot sum of male song: sMales#####
par(mar = c(1, 15, 5, par_mar_right))
plot(c(4, 21), c(0,3), type = "n", xlab = "", xaxt = "n", bty = "n", ylab = "", yaxt = "n", xlim = XLIM, ylim = c(0, length(male) + 2))
polygon(sFemales[, x], sFemales[,y/length(male) + length(male)+ 1], density = 25)
lines(sMales[, x], sMales[, S], col = "black", lwd =  1.5)

axis(2, at = c(0, length(male)), label = c(0, length(male)), cex.axis = CEX_AXIS)
axis(2, at = c(0+length(male)+1, 1 + length(male)+1), label = c(0, length(male)), cex.axis = CEX_AXIS)

axis(2, at = length(male) + 1.5, label = "No. ♀♀ in box", tick = F, cex.axis = 1.8, line = 1)
axis(2, at = length(male)/2, label = "No. ♂♂ singing", tick = F, cex.axis = 1.8, line = 1)

#add right panels #####
par_left = 15
CEX_AXISr = 1.3 #1.8
CEX_LABr = 1.8
LINEr = 5
#title######
par(mar = c(0,0,0,0))
plot(1:10, 1:10, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
mtext("   ", side = 1, line = -1, cex = 2)

#plot brightness (same as above, apart from "par")#####
  BOTTOM = 0
  TOP = 1
  iMAX = 150
  par(mar = c(0, par_left, 3, 0.5))
  plot(c(4, 22), c(0,1), type = "n", xlab = "", xaxt = "n", bty = "n", ylab = "", yaxt = "n", xlim = XLIM)
  rect(4.5, BOTTOM, 5.5, TOP, col = "black", border = NA) # night: morning
  rect(17, BOTTOM, 18.5, TOP, col = "black", border = NA) # night: evening
  for(i in 1:iMAX) {
    rect(5+i/100, BOTTOM, 5+i/100+0.1, TOP, col = grey(i/iMAX), border = NA) #day
    rect(18-i/100, BOTTOM, 18-i/100-0.1, TOP, col = grey(i/iMAX), border = NA) #day
  }
  abline(h = c(BOTTOM, TOP))
  text(x = c(5.2, 12, 17.8), y = 0.5, labels = c("night", "day", "night"), col = c("white", "black", "white"), cex = 2)



##male and female#####

par(mar = c(0.3, par_left, 0.5, 0.5))
plot(c(min(vocs2[, time_min]), max(vocs2[, time_min])), c(0, 1.7), ylab = "", type = "n", yaxt = "n", xaxt = "n", axes = F)
#1-day example dataset
#dark analyses (before/after)

rect(inout[ID == "female", start_analysis], 0, inout[ID == "female", end_analysis], 1.2, col = inout[ID == "female", COL_rect], border = inout[ID == "female", COL_rect])
#light analyses (during)
rect(inout[ID == "female" & !is.na(start_analysis), time_in_analysis], 0, inout[ID == "female" & !is.na(start_analysis), time_out_analysis], 1.2, col = inout[ID == "female" & !is.na(start_analysis), COL_rect_dark], border = inout[ID == "female" & !is.na(start_analysis), COL_rect_dark])

#female
rect(inout[, time_in], 1.3, inout[, time_out], 1.7, border = "black", density = 30)
points(vocs2[, time_min], vocs2[, rate_min]/max(vocs2[, rate_min]), cex = vocs2[, CEX], pch = 16, col = vocs2[,COL], type = "l")

#mtext("Example\npair", side = 2, at = 1.7/2, line = 7, las = 2, cex = 1.5)

axis(2, at = 1.5, label = c("♀ in box"), cex.axis = CEX_LABr, line = LINEr, tick = FALSE, hadj = 0.5)
axis(2, at = c(0.5), label = c("Song rate\n(strophes/min)"), cex.axis = CEX_LABr, lwd = -1, line = LINEr+1, hadj = 0.5)
axis(2, at = c(0:ceiling(max(vocs2[, rate_min])/2)*2)/max(vocs2[, rate_min]), label = c(0:ceiling(max(vocs2[, rate_min])/2)*2), cex.axis = CEX_AXISr)



##stats#####
par(mar = c(5, par_left-1, 7, 0.5))
YLIM = c(0, 400) #no_song output
plot(c(1, 9.5), c(1,2), type = "n", xlab = "", ylab = "", xaxt = "n", cex.lab = CEX_LABr, cex.axis = CEX_AXISr, yaxt = "n", bty = "n")
axis(2, at = 1.5, labels = "Song rate\n(strophes/min)\n±LSD", line = LINEr, cex.axis = CEX_LABr, tick = FALSE, las = 2, hadj = 0.5)
axis(3, at = c(1.5, 5, 8.5), labels = c("Dawn", "Day", "Dusk"), tick =FALSE, cex.lab = CEX_LABr, cex.axis = 1.5, line = -1)
axis(1, at = c(1.5, 5, 8.5), labels = c("", "♀ in box", ""), tick =FALSE, cex.lab = CEX_LABr, cex.axis = 1.5, line = 1.5)
PLUS = 0.3
axis(1, at = c(1+PLUS,2+PLUS, 4+PLUS,5+PLUS,6+PLUS, 8+PLUS,9+PLUS), labels = c("Inside", "After", "Before", "Inside", "After", "Before", "Inside"), tick =FALSE, cex.lab = CEX_LABr, cex.axis = 1.3, line = 0)

#draw dawn
for(SLEEP in -1:1) {
    par(new = TRUE)
    par(mar = c(7, par_left-0.6, 7, 0.5))
    if(SLEEP == -1) { POS = 7.8; YLIM = c(0, 2)}#c(-0.2, 2)}
    if(SLEEP == 0) { POS = 3.8; YLIM = c(0, 0.9)}#c(0.2, 0.9)}
    if(SLEEP == 1) { POS = 0.8; YLIM = c(0, 8)}#c(1, 8)}
    plot(c(1, 9.5), c(5,200), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n", ylim = YLIM)
    arrows(MOD1[sleep == SLEEP, XX+0.3], MOD1[sleep == SLEEP,LSDlowt], MOD1[sleep == SLEEP,XX+0.3], MOD1[sleep == SLEEP, LSDupt], code = 3, angle = 90, length = 0.19, col = MOD1[sleep == SLEEP, COL_analysis], lwd = 3)
    points(MOD1[sleep == SLEEP, XX+0.3], MOD1[sleep == SLEEP, YYt], pch = 21, bg = MOD1[sleep == SLEEP, COL_analysis], cex = 2.5, lwd = 2, xpd = TRUE)
    lines(x = c(POS, POS), y = c(0, 100))
    axis(2, pos = POS, lwd = 0, lwd.ticks = 1,xpd = TRUE, cex.axis = CEX_AXISr)
}

text(1.5+0.3, -2.2, "P < 0.001", xpd = TRUE, cex = 1) #-0.6
text(4.5+0.3, -2.2, "P < 0.001", xpd = TRUE, cex = 1)
text(5.5+0.3, -2.2, "P < 0.001", xpd = TRUE, cex = 1)
text(8.5+0.3, -2.2, "P < 0.001", xpd = TRUE, cex = 1)
axis(side = 1, at = c(1+0.3, 2+0.3), labels = c("", ""), tck = 0.03, pos = -1.5) #-0.4
axis(side = 1, at = c(4+0.3, 4.95+0.3), labels = c("", ""), tck = 0.03, pos = -1.5)
axis(side = 1, at = c(5.05+0.3, 6+0.3), labels = c("", ""), tck = 0.03, pos = -1.5)
axis(side = 1, at = c(8+0.3, 9+0.3), labels = c("", ""), tck = 0.03, pos = -1.5)
text(1.5+0.3, -0.8, "N = 45", xpd = TRUE, cex = 1) #-0.6
text(5.0+0.3, -0.8, "N = 55", xpd = TRUE, cex = 1)
text(8.5+0.3, -0.8, "N = 119", xpd = TRUE, cex = 1)

#dev.off #####
dev.off()
#
#
#
#
#
#
#
#
#





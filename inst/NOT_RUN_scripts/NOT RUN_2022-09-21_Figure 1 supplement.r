###Suppl. Fig. S??: Description of female visits to the nestbox ######
#SETUP
###don't forget to save " sessionInfo()"!!!
load("/ds/grpkempenaers/Lotte/R Studio projects/Data for package dc/SNB data/2022-05-24_raw_data.RData")
#plot raw SNB data#####
############remove!!!

head(dat)
ddd = copy(dat)
ddd[, time_in := as.ITime(in_)]
ddd[, time_out := as.ITime(out_)+out_duration]
ddd[, hour_time_in := hour(time_in)]
ddd[, hour_time_out := hour(time_out)]
tbl = data.table(table(ddd[sleep == 0, hour_time_in], ddd[sleep == 0, rel_day]))
setnames(tbl, names(tbl), c("hour_", "rel_day", "N"))
tbl[, hour_ := as.numeric(hour_)]
tbl[, rel_day := as.numeric(rel_day)]
plot(c(min(tbl[,rel_day]), max(tbl[,rel_day])), c(max(tbl[,hour_]), min(tbl[,hour_])), type = "n", xlab = "Days to first egg", ylab = "Hour", ylim = c(max(tbl[,hour_]), min(tbl[,hour_])))
for(i in 1 : nrow(tbl)) {
  points(tbl[i,rel_day], tbl[i,hour_], cex = tbl[i,N])
}
#end plot#####


###Suppl. Fig S1: Number of females in box across day
df_stat = copy(hooray)

#add ID
require(sdb)
con = dbcon("lschlicht")
tr = data.table(rbind(
  dbq(con, "SELECT DISTINCT ID, transponder FROM BTatWESTERHOLZ.CHICKS where transponder is not NULL"),
  dbq(con, "SELECT DISTINCT ID, transponder FROM BTatWESTERHOLZ.ADULTS where transponder is not NULL")
))
df_stat = merge(df_stat, tr, by.x = "transp", by.y = "transponder")

#add sex and remove males
sex = dbq(con, "SELECT DISTINCT ID, sex from BTatWESTERHOLZ.SEX")
df_stat = merge(df_stat, sex, by = "ID")
df_stat = df_stat[sex == 2,]

#restrict to the time period of the MS, days -5 to 5 around the female's first egg
br = dbq(con, "SELECT DISTINCT DATE(firstEgg) as firstEgg, box, IDfemale as ID FROM BTatWESTERHOLZ.BREEDING where year_ = 2019")
#manually assign the "correct" breeding attempt from the database, or remove the entries. <- this is done identically to the raw data!
br = br[ID != "B4P8334" & #two breeding attempts within 4 days => remove
          !(ID == "B4P8714" & firstEgg == "2019-05-18") & #first breeding attempt lead full clutch => remove second attempt
          !(ID == "B4P9057" & firstEgg == "2019-05-03") & #same as above
          !(ID == "B4X1126" & firstEgg == "2019-05-14") & #same as above
          !ID == "B4X1250" & #something wrong with social pair assignment, remove both entries
          !(ID == "B4X1278" & firstEgg == "2019-05-09") & #first breeding attempt ... see above
          !(ID == "B4X1857" & firstEgg == "2019-05-05") & #see above
          !(ID == "B4X1859" & firstEgg == "2019-05-08") & #see above
          !(ID == "B4X1958" & firstEgg == "2019-04-19") #male of first attempt unknown => use second
        ,]

df_stat = merge(df_stat, br[, .(firstEgg, box)], by = "box")
df_stat[, rel_day_out := as.IDate(out_) - as.IDate(firstEgg)]
df_stat[, rel_day_in := as.IDate(in_) - as.IDate(firstEgg)]
df_stat[, d_rel_day := rel_day_out - rel_day_in]
df_stat = df_stat[d_rel_day == 0 | d_rel_day == 1,]
df_stat = df_stat[rel_day_in > -6 & rel_day_out < 6]
df_stat = unique(df_stat)
tmp_day = df_stat[d_rel_day == 0,]; tmp_day[, rel_day := rel_day_in]; tmp_day[, sleep := 0] #day data
tmp_dawn = df_stat[d_rel_day == 1,]; tmp_dawn[, rel_day := rel_day_out]; tmp_dawn[, sleep := 1] #dawn data
tmp_dusk = df_stat[d_rel_day == 1,]; tmp_dusk[, rel_day := rel_day_in]; tmp_dusk[, sleep := -1] #dusk data

df_stat = rbindlist(list(tmp_day, tmp_dawn, tmp_dusk))
df_stat = df_stat[abs(rel_day) < 6, ]

#reduce dataset again to avoid duplicates
df_stat = df_stat[sleep != -1,]

#add date
df_stat[, date_ := as.IDate(out_)]

#summarize to each date and remove dates with too few females (e.g. less than 20?)
ra = data.table(expand.grid(unique(df_stat[,date_]), (0*60):(23.9*60)))
setnames(ra, names(ra), c("date_", "min_"))
ra[, datetime_ := as.POSIXct(paste0(date_, " ", as.ITime(min_)*60))]
ra[, N := as.numeric(NA)]
for(i in 1 : nrow(ra)) {
  if((i %% 500) == 0) print(i / nrow(ra))
  #how many individuals were inside at the same time?
  ra[i,N := nrow(
    subset(df_stat, (in_ <= ra[i,datetime_] & out_+out_duration >= ra[i, datetime_]))
  )
  ]
}
#copy(ra) -> backup
#remove data with less than 20 females
N_fem = unique(df_stat[, .(ID, date_)])
N_fem[, N_max := .N, by = date_]
N_fem = unique(N_fem[,.(date_, N_max)])
N_fem[, date_ := as.IDate(date_)]
ra_sub = merge(ra, N_fem, by = "date_")
table(ra_sub$N_max)
ra_sub = subset(ra_sub, N_max >= 20)

#calculate means and 95% quantile polygon across all dates
ra_sub[, median_N := median(N/N_max), by = min_]
ra_sub[, low_qu_N := quantile(N/N_max, probs = 0.025), by = min_]
ra_sub[, up_qu_N := quantile(N/N_max, probs = 0.975), by = min_]
ra_sub = unique(ra_sub[, .(min_, median_N, low_qu_N, up_qu_N)])
#plot
plot(1, 1, xlim = c(3*60, 22*60), ylim = c(-0.6, 1), type = "n", xlab = "Time of day", ylab = "", xaxt = "n", yaxt = "n") #330 - 1215
mtext("Prop. ♀♀ in box", 2, at = 0.5, las = 0, cex = 2, line = 3)
axis(1, at = (1:25)*60, labels = 1:25)
axis(2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = as.character(c(0:5)*2/10))
polygon(c(ra_sub[, min_], rev(ra_sub[,min_])), c(ra_sub[, low_qu_N], rev(ra_sub[,up_qu_N])), border = NA, col = "grey")
lines(ra_sub[, min_], ra_sub[, median_N], lwd = 2)

daily_visits = unique(dat2[sleep == 0, .(counter, in_, out_, box)])
daily_visits[, box_counter := as.numeric(as.factor(box))]
for(i in 1 : nrow(daily_visits)) {
  print(-(daily_visits[i, box_counter]-1)/(2*max(daily_visits[i, box_counter])))
    lines(as.numeric(c(as.ITime(daily_visits[i,in_])/60, as.ITime(daily_visits[i,out_])/60)), rep(-(daily_visits[i, box_counter]-1)/(2*max(daily_visits[, box_counter])), 2)-0.1, col = "black", lwd = 3)#c(-(i-1)/nrow(daily_visits)/2, -(i-1)/nrow(daily_visits)/2)
}
mtext("♀♀ day visits", 2, at = -0.3, las = 0, cex = 2, line = 3)

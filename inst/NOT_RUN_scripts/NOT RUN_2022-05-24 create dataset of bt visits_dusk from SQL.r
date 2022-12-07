setwd("/ds/grpkempenaers/Lotte/R Studio projects/Data for package dc")

require(sdb)
require(SNB2)
require(dc)

df = data.table(box = 1:277, from = "2019-03-15 00:00:00", to = "2019-05-30 00:00:00")
hooray = eva('lschlicht', df)
save(hooray, file = paste0(getwd(), "/SNB Data/", Sys.Date(), "_raw_data.RData"))
#load("/ds/grpkempenaers/Lotte/R Studio projects/Data for package dc/SNB data/2021-06-30_raw_data.RData")

#1.remove data that has no transponder reading and duplicate rows
{
dat = copy(hooray)
dat = dat[!is.na(transp),]
dat = unique(dat)

#2. subset to data where an individual was inside the nest box for at least 2 minutes
dat = dat[direction == "IN-OUT",]
#to be on the safe side, remove data where the out-duration is unnaturally long (based on outlier definition of boxplot)
dat = dat[out_duration <= boxplot(dat$out_duration, plot = FALSE)$stats[5], ]
dat[, time_inside_min := as.numeric(out_ + out_duration - in_)/60]
dat = dat[time_inside_min >= 2, ]

#3. add individual identity and sex and subset to females only
#fetch ID and sex data
con = dbcon("lschlicht")
id = unique(rbind(dbq(con, "SELECT DISTINCT ID, transponder FROM BTatWESTERHOLZ.ADULTS where transponder is not NULL and season <= '2019'"),
           dbq(con, "SELECT DISTINCT ID, transponder FROM BTatWESTERHOLZ.CHICKS where transponder is not NULL and year_ <= '2019'")))
id[, transponder := toupper(transponder)]
#correct bugs: remove row with typo
id = subset(id, !(ID == "B4X1259" & transponder == "AF60000000008001"))

setnames(id, "transponder", "transp")
sex = dbq(con, "SELECT DISTINCT ID, sex FROM BTatWESTERHOLZ.SEX where sex is not NULL")
id = unique(merge(id, sex, by = "ID"))
closeCon(con)
#add to dataset
dat = merge(dat, id, by = "transp")

#subset to females
dat = dat[sex == 2, ]

#add first egg dates and subset to the 5 days around the first egg
con = dbcon("lschlicht")
br = dbq(con, "SELECT IDmale, IDfemale, box, DATE(firstEgg) as firstEgg FROM BTatWESTERHOLZ.BREEDING WHERE year_ = 2019 and IDfemale is not NULL")
closeCon(con)
setnames(br, "IDfemale", "ID")
#check out multiple breeding attempts of individual females
tmp = subset(br, ID %in% br$ID[which(duplicated(br$ID))])
setorder(tmp, ID)
tmp
#manually assign the "correct" breeding attempt from the database, or remove the entries.
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


dat = merge(dat, br, by = "ID")

#calculate days to first egg
dat[, date_ := as.IDate(in_)]
dat[, firstEgg := as.IDate(firstEgg)]
dat[, rel_day := date_ - firstEgg]

#subset to days -5 to 5
dat = subset(dat, rel_day >= -5 & rel_day <= 5)
####

#subset to visits to breeding box
dat = subset(dat, box.x == box.y)
setnames(dat, "box.x", "box"); dat[, box.y := NULL]
}
#add sleep data and remove overnight errors; to make sure remove data where the female was inside the nest box unusually long
dat[, sleep := ifelse(as.IDate(out_) != date_, 1, 0)]
table(dat$sleep)
#subset to only sleep data (this is the data for dusk), but for now keep day data (that isn't unusually long) in order to subset to only days where there is day data.
dat = subset(dat, sleep == 1 | time_inside_min <= boxplot(dat[sleep == 0, time_inside_min])$stats[5])

#change to those days where there is data both during the day and in the morning
############NOTE: THE OTHERS CAN BE ADDED LATERON, IF RELEVANT! BUT NO COMPARISON WITH THE MORNING POSSIBLE! ##################
dat[, keep := length(unique(sleep)), by = .(transp, date_)]
dat = dat[keep == 2, ]
dat[, keep := NULL]
table(dat$sleep)
dat_dusk = subset(dat, sleep == 1)

save(dat_dusk, file = paste0(getwd(), "/SNB Data/", Sys.Date(), "_data_basis_for_sound_extraction_dusk.RData"))
#load("/ds/grpkempenaers/Lotte/R Studio projects/Data for package dc/SNB data/2021-06-30_data_basis_for_sound_extraction.RData")

dat_dusk[, start := in_ - 10*60] # use 10 minutes around female entry
dat_dusk[, end := in_ + 10*60]# use 10 minutes around female entry
dat_dusk[, counter := 1000 + (1:nrow(dat_dusk))] #anything that is large enough to be larger than any counter in dat-dataset
dat_dusk[, sound_data := 10]
dat_dusk[, channel := 10]
dat_dusk[, sleep := -1]
save(dat_dusk, file = paste0(getwd(), "/SNB Data/", Sys.Date(), "_dat dataset_dusk.RData"))

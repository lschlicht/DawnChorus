#create basic datasets
####load cluster analysis data #####
a = data.table(read.csv("/ds/grpkempenaers//Lotte/R Studio projects/Data for package dc/kaleidoscope output/2022-04-11_final data.csv"))

#remove bug
a = subset(a, IN.FILE != "L224R224_1_20190422_091503_000_start620_end1799_start620_end1799.wav")
#subset to manual assignments
a = subset(a, MANUAL.ID != "")

#subset to song
a[, type := (tstrsplit(MANUAL.ID, ",", keep = 1)[[1]])]
a[, song := 0]
a[type == "" | type == "sqs" | substring(type, 1, 3) == "two", song := 1]
a[type == "calls", song := 0.5]
a = subset(a, song > 0)

#subset to dB measurements
a[, dB := as.numeric(tstrsplit(MANUAL.ID, ",", keep = 2)[[1]])]
a = subset(a, !is.na(dB))

a = subset(a, select = c("IN.FILE", "OFFSET", "dB", "song"))

a[, datetime_ := as.POSIXct(substr(IN.FILE, 12, 26), format = "%Y%m%d_%H%M%S", tz = "Etc/GMT-1") +
    as.numeric(lapply(tstrsplit(IN.FILE, "_start", keep = 2), FUN = function(x) tstrsplit(x, "_end", keep = 1))[[1]][[1]]) +
    OFFSET]

a[, location := substr(IN.FILE, 1, 8)]
a[, channel := substring(IN.FILE, 10, 10)]
attr(a$datetime_, "tzone") =  "Etc/GMT-2"

a[, box := as.integer(ifelse(channel == 0, substring(location,6,8), substring(location, 2,4)))]

a = (unique(a, by = c("datetime_", "location", "channel", "box", "song")))

##load dusk data######
a2 = data.table(read.csv("/ds/grpkempenaers//Lotte/R Studio projects/Data for package dc/2022-05-24 Sound analysis dusk.csv", sep = ";", header = FALSE))
a2 = subset(a2, select = c("V2", "V3", "V7", "V8"))
setnames(a2, names(a2), c("IN.FILE", "OFFSET", "type", "dB"))

#subset to song
a2[, song := 0]
a2[type == "song" | type == "sqs", song := 1]
a2[type == "calls", song := 0.5]
a2 = subset(a2, song > 0)

#subset to dB measurements
a2[, dB := as.numeric(dB)]
a2 = subset(a2, !is.na(dB))

a2 = subset(a2, select = c("IN.FILE", "OFFSET", "dB", "song"))
a2[, OFFSET := as.numeric(as.ITime(OFFSET))]

a2[, datetime_ := as.POSIXct(substr(IN.FILE, 105, 119), format = "%Y%m%d_%H%M%S", tz = "Etc/GMT-1") +
     #as.numeric(lapply(tstrsplit(IN.FILE, "_start", keep = 2), FUN = function(x) tstrsplit(x, "_end", keep = 1))[[1]][[1]]) +
     OFFSET]

a2[, location := substr(IN.FILE, 94, 101)]
a2[, channel := substring(IN.FILE, 103, 103)]
attr(a2$datetime_, "tzone") =  "Etc/GMT-2"

a2[, box := as.integer(ifelse(channel == 0, substring(location,6,8), substring(location, 2,4)))]

a2 = (unique(a2, by = c("datetime_", "location", "channel", "box", "song")))


a2[, dB := dB + 4] #because song scope is 4 dB off from the Kaleidoscope measures
###combine day and dusk
a = data.table(rbind(a, a2))
write.table(a, file = paste0(getwd(), "/extdata/SongData.csv"), sep = ",")


####load SNB data #####
load("/ds/grpkempenaers/Lotte/R Studio projects/Data for package dc/SNB data/2022-01-11_dat dataset.RData")
load("/ds/grpkempenaers/Lotte/R Studio projects/Data for package dc/SNB data/2022-05-24_dat dataset_dusk.RData")
dat = data.table(rbind(dat, dat_dusk))
write.table(dat, file = paste0(getwd(), "/extdata/ActivityData.csv"), sep = ",")


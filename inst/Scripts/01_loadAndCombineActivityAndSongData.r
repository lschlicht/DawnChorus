#1. LOAD DATA #####
SongData = data.table(read.table(paste0(getwd(), "/extdata/SongData.csv"), sep = ","))
ActivityData = data.table(read.table(paste0(getwd(), "/extdata/ActivityData.csv"), sep = ","))

#2. FORMAT COLUMNS AND DATASET#####
SongData[, datetime_ := as.POSIXct(datetime_)]

ActivityData[, in_ := as.POSIXct(in_)]
ActivityData[, out_ := as.POSIXct(out_)]
ActivityData[, start := as.POSIXct(start)]
ActivityData[, end := as.POSIXct(end)]

setorder(ActivityData, box, ID, date_, sleep)
ActivityData[, time_inside_min := (time_inside_min[1]), by=.(box, date_, ID, counter)]

#3. MARK RELIABILITY OF DIRECTIONAL ASSIGNMENT#####
ActivityData[, direction_reliable := ifelse(toupper(direction_detail) == "O/I|I/O", 1, 0)]

#4. MARK OVERLAPS BETWEEN PERIODS/EVENTS#####
#warnings are ok
setorder(ActivityData, box, ID, rel_day, in_)
ActivityData[, tmp1 := as.numeric(start) - shift(as.numeric(end), fill = NA), by = .(box, ID, rel_day)]
ActivityData[, tmp2 := shift(as.numeric(start), type = "lead", fill = NA) - as.numeric(end), by = .(box, ID, rel_day)]
ActivityData[, overlap := sign(min(tmp1, tmp2, na.rm = TRUE)), by = rownames(ActivityData)]
ActivityData[, ":=" (tmp1 = NULL, tmp2 = NULL)]

table(ActivityData[,direction_reliable], ActivityData[,overlap])

##################################
#make datasets
#################################
#A. long dataset with one row per minute (interval can be set using variable) #####
copy(ActivityData) -> ActivityDataPerMinute

#make dataset that contains one line per WINdow
#1. create dataset that contains 1 row per WINdow (per minute)
L = list()
WIN = 60 #time window in seconds
for(i in 1 : nrow(ActivityDataPerMinute)) {
  tmp = as.POSIXct((ActivityDataPerMinute[i, ceiling(as.numeric(start)/WIN)] : ActivityDataPerMinute[i, floor(as.numeric(end)/WIN)])*WIN, origin = "1970-01-01", tz = "Etc/GMT-2")
  tmp = tmp[-length(tmp)] #remove last line, because that will generally not be of full length
  L[[length(L)+1]] = data.table(box = ActivityDataPerMinute[i,box], ID = ActivityDataPerMinute[i,ID], time_inside_min = ActivityDataPerMinute[i,time_inside_min], in_ = ActivityDataPerMinute[i,in_], out_ = ActivityDataPerMinute[i,out_], out_duration = ActivityDataPerMinute[i,out_duration], sleep = ActivityDataPerMinute[i,sleep], counter = ActivityDataPerMinute[i,counter], time_min = tmp, direction_reliable = ActivityDataPerMinute[i, direction_reliable], overlap = ActivityDataPerMinute[i, overlap], rel_day = ActivityDataPerMinute[i, rel_day])

}
LL = rbindlist(L)

#2. add the relevant data into that dataset
ActivityDataPerMinute = copy(LL)
ActivityDataPerMinute[,start := time_min]
ActivityDataPerMinute[, end := time_min + (WIN-1)]
ActivityDataPerMinute[end < in_, type :=  "before"]
ActivityDataPerMinute[start > out_+out_duration, type :=  "after"]
ActivityDataPerMinute[start > in_ & end < out_+out_duration, type := "during"]

L = rbindlist(list(ActivityDataPerMinute, ActivityDataPerMinute, ActivityDataPerMinute, ActivityDataPerMinute, ActivityDataPerMinute, ActivityDataPerMinute))
L[, DB := rep(c(-30, -32, -34, -36, -38, -40), each = nrow(ActivityDataPerMinute))]
copy(L) -> ActivityDataPerMinute

Timer = Sys.time() #~ 10 minutes
for(i in 1 : nrow(ActivityDataPerMinute)) {
  if(i%%100 == 0) print(i/nrow(ActivityDataPerMinute))
  ActivityDataPerMinute[i, no_song := nrow(subset(SongData, datetime_ >= ActivityDataPerMinute[i, start] & datetime_ <= ActivityDataPerMinute[i,end] & box == ActivityDataPerMinute[i,box] & song == 1 & dB >= ActivityDataPerMinute[i,DB]))]
  ActivityDataPerMinute[i, no_calls := nrow(subset(SongData, datetime_ >= ActivityDataPerMinute[i, start] & datetime_ <= ActivityDataPerMinute[i,end] & box == ActivityDataPerMinute[i,box] & song == 0.5 & dB >= ActivityDataPerMinute[i,DB]))]
}
Sys.time()-Timer

#test whether number of lines are correct
table(ActivityDataPerMinute[sleep == 0, type]) #for small time Windows numbers are almost equal, as it should be.

ActivityDataPerMinute[, time_to_in := difftime(time_min,in_, units = "mins")]
ActivityDataPerMinute[, time_to_out := difftime(time_min,out_+out_duration, units = "mins")]
ActivityDataPerMinute[, time_to_in_min := round(time_to_in/(WIN/60), digits = 0)*(WIN/60)]
ActivityDataPerMinute[, time_to_out_min := round(time_to_out, digits = 0)]
ActivityDataPerMinute[, noYN := ifelse(no_song > 0, 1, 0)]
ActivityDataPerMinute[, noCallsYN := ifelse(no_calls > 0, 1, 0)]
ActivityDataPerMinute[, N := .N, by = .(type, counter, DB)]
ActivityDataPerMinute[, f_type := factor(type, levels = c("during", "before", "after"))]
ActivityDataPerMinute[, no_vocs := no_song + no_calls]
ActivityDataPerMinute[, noVocsYN := ifelse(noYN + noCallsYN > 0, 1, 0)]
ActivityDataPerMinute[, f_noYN := as.factor(noYN)]
save(ActivityDataPerMinute, file = paste0(getwd(), "/data/ActivityDataPerMinute.RData"))



#2nd dataset: reduce to before-during-after #####
ActivityDataPerPeriod = copy(ActivityDataPerMinute)
ActivityDataPerPeriod[, no_song := sum(no_song), by = .(box, ID, counter, sleep, type, DB)]
ActivityDataPerPeriod[, no_mins_song := sum(noYN), by = .(box, ID, counter, sleep, type, DB)]
ActivityDataPerPeriod[, no_calls := sum(no_calls), by = .(box, ID, counter, sleep, type, DB)]
ActivityDataPerPeriod[, no_mins_calls := sum(noCallsYN), by = .(box, ID, counter, sleep, type, DB)]
ActivityDataPerPeriod[, no_vocs := sum(no_vocs), by = .(box, ID, counter, sleep, type, DB)]
ActivityDataPerPeriod[, no_mins_vocs := sum(noVocsYN), by = .(box, ID, counter, sleep, type, DB)]
ActivityDataPerPeriod[, noYN := ifelse(no_song > 0, 1, 0)]
ActivityDataPerPeriod[, f_noYN := as.factor(noYN)]
ActivityDataPerPeriod[, noCallsYN := ifelse(no_calls > 0, 1, 0)]
ActivityDataPerPeriod[, noVocsYN := ifelse(no_vocs > 0, 1, 0)]

ActivityDataPerPeriod[, start := min(start), by = .(box, ID, counter, sleep, DB)]
ActivityDataPerPeriod[, end := max(end), by = .(box, ID, counter, sleep, DB)]
ActivityDataPerPeriod[, ":=" (time_to_out_min = NULL, time_to_in_min = NULL, time_to_in = NULL, time_to_out = NULL, time_min = NULL)]
ActivityDataPerPeriod = unique(ActivityDataPerPeriod)
ActivityDataPerPeriod[sleep == 1 | sleep == -1, song_per_time := no_song/30]
ActivityDataPerPeriod[sleep == 0, song_per_time := no_song/time_inside_min]
ActivityDataPerPeriod[sleep == 1 | sleep == -1, calls_per_time := no_calls/30]
ActivityDataPerPeriod[sleep == 0, calls_per_time := no_calls/time_inside_min]
ActivityDataPerPeriod[sleep == 1 | sleep == -1, vocs_per_time := (no_calls+no_song)/30]
ActivityDataPerPeriod[sleep == 0, vocs_per_time := (no_calls+no_song)/time_inside_min]
ActivityDataPerPeriod[, f_type := factor(type, levels = c("during", "before", "after"))]
ActivityDataPerPeriod[, date_ := as.IDate(out_)]
ActivityDataPerPeriod[, hour_of_out := hour(as.ITime(out_)+out_duration)]
save(ActivityDataPerPeriod, file = paste0(getwd(), "/data/ActivityDataPerPeriod.RData"))


#backup: save to folder extdata
write.table(ActivityDataPerMinute, file = paste0(getwd(), "/extdata/ActivityDataPerMinute.csv"), sep = ",")
write.table(ActivityDataPerPeriod, file = paste0(getwd(), "/extdata/ActivityDataPerPeriod.csv"), sep = ",")

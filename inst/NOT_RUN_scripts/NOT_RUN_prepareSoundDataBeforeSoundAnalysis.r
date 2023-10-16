require(stringr)

#prepare data for analysis of sound data 2019
#1. load SNB data and add time frame around the visit
load("/ds/grpkempenaers/Lotte/R Studio projects/Data for package dc/SNB data/2021-07-01_data_basis_for_sound_extraction.RData")
#use half an hour around the emergence time
dat[sleep == 1, start := out_+out_duration - 30*60]
dat[sleep == 1, end := out_+out_duration + 30*60]
#use the time length of the visit before and after the visit in addition to the visit itself
dat[sleep == 0, start := in_ - (time_inside_min*60)]
dat[sleep == 0, end := (out_+out_duration) + (time_inside_min*60)]
dat[, counter := 1:nrow(dat)]

#2. check availability of sound data
#fetch content file paths
lf = list.files(path = "/ds/raw_data_kemp/FIELD/Westerholz/breeding_2019/Sound recordings LS/", pattern = "Content", recursive = TRUE, full.names = TRUE)
#make list of existing sound files and their locations
lf2 = lapply(lf, FUN = function(x) data.table(read.table(x, skip = 1), names = x))
lf2 = rbindlist(lf2)
#add start and end of file
lf2[, start := substr(V1, 10, 24)]
lf2[, start := as.POSIXct(start, format = "%Y%m%d_%H%M%S", tz = "Etc/GMT-1")] #Time zone set as it is because sound recorders were by accident on summer time!
lf2[, date_ := as.IDate(start)]
setorder(lf2, V1)
lf2[, location := substr(V1, 1,8)]
lf2[, end := round(as.numeric(difftime(shift(start, type = "lead", fill = NA), start, units = "mins")), digits = 0), by = location]
#correct lengths
lf2[is.na(end), end := 30]
#correct time lengths of recordings during the day
lf2[end > 120 &  as.ITime(start) < as.ITime("17:00:00"), end := 30]
lf2[end > 120, end := 120]
lf2[end > 30 & end < 120, end := 30]
lf2[, end := start + (end*60-1)]

#remove error
lf2 = lf2[location != "23_20000",]

#3. make dataset that contains which sound files should be analyzed within which time frame
FF = list()
dat[, sound_data := 1]
for(i in 1 : nrow(dat)) {
  print(i)
  #grab the correct boxes
  ff = lf2[which(grepl(paste0(str_pad(dat[i,box], 3, "left", pad = "0"), "_"), lf2[, names]) == TRUE),]
  #select the correct dates
  ff = subset(ff, date_ == dat[i, date_])
  #select the correct times
  ff = subset(ff, start <= dat[i,end])
  ff = subset(ff, end >= dat[i,start])
  #define times of analysis
  ff[, start_analysis := as.numeric(difftime(dat[i, start],start, units = "secs"))]
  ff[start_analysis < 0, start_analysis := 0]
  ff[, end_analysis := as.numeric(difftime(end, dat[i,end], units = "secs"))]
  ff[end_analysis > 0, end_analysis := as.numeric(difftime(end,start, units = "secs")-end_analysis)]
  ff[end_analysis < 0, end_analysis := as.numeric(difftime(end,start, units = "secs"))]
  #add counter of original file for assignment
  ff[, counter := dat[i, counter]]
  if(nrow(ff) > 0) FF[[length(FF)+1]] = ff else {print('nope!'); dat[i, sound_data := 0]}
}
FF2 = rbindlist(FF)

FF2

#make list of .zip files that need to be copied and copy manually
zip = sort(unique(FF2[, names]))

#extract zips and delete .zip file and delete unneded files manually
FF3 = split(FF2, FF2$names)
length(FF3)
FF3[[1]]

#extract channels via kaleidoscope

#copy files to grpkempenaers

#mark channel for each event
dat
channels = unique(data.table(box = c(substring(FF2[, V1],1,4), substring(FF2[, V1],5,8))))
channels[, channel := substring(box, 1,1)]
channels[, box := substring(box, 2,)]
#channel 0 is the left channel, and if two microphones are attached to the same nestbox, channel 0 (the left channel) is recording from inside the box.
channels[channel == "R", channel := 1]
channels[channel == "L", channel := 0]
channels[, channel := max(channel), by = box]
channels = unique(channels)
channels[, box := as.integer(box)]
dat = merge(dat, channels, by = "box")
dat = subset(dat, sound_data == 1)


#for an unknown reason, the start and end times are wrong in the currently saved file. I therefore update these to be correct again (I'm rerunning the code, so if the dataset is at this stage correct, it will remain identical)
dat[sleep == 1, start := out_+out_duration - 30*60]
dat[sleep == 1, end := out_+out_duration + 30*60]

#save(dat, file = "/ds/grpkempenaers/Lotte/R Studio projects/Data for package dc/SNB data/2021-07-19_dat dataset.RData")
save(dat, file = "/ds/grpkempenaers/Lotte/R Studio projects/Data for package dc/SNB data/2022-01-11_dat dataset.RData")

FF2 = merge(FF2, unique(dat[, .(counter, channel)]), by = c("counter"))

#read file list of split files
lc = list.files(path = "/ds/grpkempenaers/Lotte/R Studio projects/Data for package dc/sound data", recursive = TRUE, full.names = TRUE)
lc = data.table(path = lc, location = tstrsplit(lc, "/", keep = 9)[[1]])
lc[, c("N1", "channel", "N2", "N3") := tstrsplit(lc$location, "_", keep = c(1,2, 3,4))]
lc[, org_filename := paste0(N1, "_", N2, "_", N3, ".wac")]
FF3 = merge(FF2, lc, by.x = c("V1", "channel"), by.y = c("org_filename", "channel"))
FF3[, start_analysis := round(start_analysis, digits = 0)]
FF3[, end_analysis := round(end_analysis, digits = 0)]


######DO NOT RUN AGAIN!!!####
#NOT RUN!!!
#load wav files and cut to appropriate length and rename (!!!)
  for(i in  1 : nrow(FF3)) {
print(i)
    try((bla = readWave(filename = FF3[i, path], from = FF3[i,start_analysis], to = FF3[i,end_analysis], units = "seconds")), silent = TRUE)
    if(exists("bla")) {
    writeWave(bla, filename =
          paste0(
            tstrsplit(FF3[i,path], "sound data", keep = 1)[[1]],
            "sound data/to analyze/",
            tstrsplit(tstrsplit(FF3[i,path], "/", keep = 9)[[1]], ".wav", keep = 1), "_start", FF3[i,start_analysis], "_end", FF3[i, end_analysis], ".wav")
, extensible = FALSE);
rm(bla)
}
  }


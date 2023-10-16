#check what happens if the minutes around entry and exit are removed
ActivityDataPerMinute
ActivityDataPerMinuteReduced = subset(ActivityDataPerMinute, abs(time_to_in) > 2 & abs(time_to_out) > 2)


#2nd dataset: reduce to before-during-after #####
ActivityDataPerPeriodReduced = copy(ActivityDataPerMinuteReduced)
ActivityDataPerPeriodReduced[, no_song := sum(no_song), by = .(box, ID, counter, sleep, type, DB)]
ActivityDataPerPeriodReduced[, no_mins_song := sum(noYN), by = .(box, ID, counter, sleep, type, DB)]
ActivityDataPerPeriodReduced[, no_calls := sum(no_calls), by = .(box, ID, counter, sleep, type, DB)]
ActivityDataPerPeriodReduced[, no_mins_calls := sum(noCallsYN), by = .(box, ID, counter, sleep, type, DB)]
ActivityDataPerPeriodReduced[, no_vocs := sum(no_vocs), by = .(box, ID, counter, sleep, type, DB)]
ActivityDataPerPeriodReduced[, no_mins_vocs := sum(noVocsYN), by = .(box, ID, counter, sleep, type, DB)]
ActivityDataPerPeriodReduced[, noYN := ifelse(no_song > 0, 1, 0)]
ActivityDataPerPeriodReduced[, f_noYN := as.factor(noYN)]
ActivityDataPerPeriodReduced[, noCallsYN := ifelse(no_calls > 0, 1, 0)]
ActivityDataPerPeriodReduced[, noVocsYN := ifelse(no_vocs > 0, 1, 0)]

ActivityDataPerPeriodReduced[, start := min(start), by = .(box, ID, counter, sleep, DB)]
ActivityDataPerPeriodReduced[, end := max(end), by = .(box, ID, counter, sleep, DB)]
ActivityDataPerPeriodReduced[, ":=" (time_to_out_min = NULL, time_to_in_min = NULL, time_to_in = NULL, time_to_out = NULL, time_min = NULL)]
ActivityDataPerPeriodReduced = unique(ActivityDataPerPeriodReduced)
ActivityDataPerPeriodReduced[sleep == 1 | sleep == -1, song_per_time := no_song/30]
ActivityDataPerPeriodReduced[sleep == 0, song_per_time := no_song/time_inside_min]
ActivityDataPerPeriodReduced[sleep == 1 | sleep == -1, calls_per_time := no_calls/30]
ActivityDataPerPeriodReduced[sleep == 0, calls_per_time := no_calls/time_inside_min]
ActivityDataPerPeriodReduced[sleep == 1 | sleep == -1, vocs_per_time := (no_calls+no_song)/30]
ActivityDataPerPeriodReduced[sleep == 0, vocs_per_time := (no_calls+no_song)/time_inside_min]
ActivityDataPerPeriodReduced[, f_type := factor(type, levels = c("during", "before", "after"))]
ActivityDataPerPeriodReduced[, date_ := as.IDate(out_)]
ActivityDataPerPeriodReduced[, hour_of_out := hour(as.ITime(out_)+out_duration)]



ActivityDataPerPeriodReduced[, var := no_vocs]

dB_threshold = -36
tmp = subset(ActivityDataPerPeriodReduced, direction_reliable == 1 & overlap == 1 & DB == dB_threshold)

def_sleep_type = c(1, 0, -1) #1 = dawn, 0 = day, -1 = dusk
sleep_type = 0
m = glmmTMB(var ~ f_type + (1|box) + (1|counter), data = subset(tmp, sleep == sleep_type & DB == dB_threshold), ziformula = ~1, family = poisson)

summary(m)

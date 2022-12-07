#1. SETUP #####
#require package "dc" OR load ActivityDataPerMinute, ActivityDataPerPeriod, if the latter do not exist, run "load and combine activity and song data.r"  to create them.
#as a backup, these datasets are also saved as .csv files to the extdata folder. In this case, make sure to reformat the relevant columns to POSIXct, that is:
#for ActivityDataPerMinute: in_, out_, time_min, start, end; (the four time_to_... columns are difftime objects with the unit "mins", but that should not matter)
#for ActivityDataPerPeriod: in_, out_, start, end; (f_type is a three-level factor, base level is "during")
#require
require(glmmTMB)
require(DHARMa)



#RUN ALL MODELS (including Sensitivity analysis) for main analysis (before-during-after comparison in Fig. 1, slope in Fig. 2) #####
#MODEL TYPE: models run for: zero-inflated poisson, poisson (>0) + binomial (0,1), log-transformed gaussian
#model checks are saved as a .pdf (see below)
#there are many warnings, because the sensitivity analysis includes models that do not converge properly (see also empty cells in supplementary tables)
#SONG or CALLS or BOTH
def_vocalizations = c("song", "calls", "both")
#DB THRESHOLD
def_dB_threshold = (-15:-20)*2 #use even numbers between -30 and -40
#Sleep: dawn, day, dusk
def_sleep_type = c(1, 0, -1) #1 = dawn, 0 = day, -1 = dusk
#model_type
def_model_type = c("zero-inflated poisson", "poisson", "binomial", "gaussian") #note that the poisson model uses only values > 0, while the binomial model treats all values > 0 as 1.

use = data.table(expand.grid(def_vocalizations, def_dB_threshold, def_sleep_type, def_model_type))
setnames(use, names(use), c("def_vocalizations", "def_dB_threshold", "def_sleep_type", "def_model_type"))


plot_list = list()
model_out_list = list()
#run ALL! If you want to run specific parts (e.g. to check models again), do the definitions yourself and go into loop manually.
check_time = Sys.time()
for(i in 1 : nrow(use)) {
print(i)
vocalizations = use[i, def_vocalizations]
dB_threshold = use[i, def_dB_threshold]
sleep_type = use[i,def_sleep_type]
model_type = use[i,def_model_type]





#Before-During-After Comparison (Figure 1E)#####
tmp = subset(ActivityDataPerPeriod, direction_reliable == 1 & overlap == 1 & DB == dB_threshold)
if(vocalizations == "song") tmp[, var := no_song]
if(vocalizations == "calls") tmp[, var := no_calls]
if(vocalizations == "both") tmp[, var := no_vocs]

  tryCatch(
    {
            if(model_type == "zero-inflated poisson") m = glmmTMB(var ~ f_type + (1|box) + (1|counter), data = subset(tmp, sleep == sleep_type & DB == dB_threshold), ziformula = ~1, family = poisson)
            if(model_type == "poisson") m = glmmTMB(var ~ f_type + (1|box) + (1|counter), data = subset(subset(tmp, var > 0), sleep == sleep_type & DB == dB_threshold), ziformula = ~0, family = poisson)
            if(model_type == "binomial") m = glmmTMB(var1 ~ f_type + (1|box) + (1|counter), data = subset(tmp[, var1 := ifelse(var > 0, 1, 0)], sleep == sleep_type & DB == dB_threshold), ziformula = ~0, family = binomial)
            if(model_type == "gaussian") m = glmmTMB(log(var+0.01) ~ f_type + (1|box) + (1|counter), data = subset(tmp, sleep == sleep_type & DB == dB_threshold), ziformula = ~0, family = gaussian)



            simulationOutput <- simulateResiduals(fittedModel = m, plot = F)
            plot(simulationOutput) #dispersion test will almost always be statistically significant, because of the sample sizes. That's fine, if the VALUE is small enough. See package vignette.
            axis(4, at = 0.5, paste0(vocalizations, ", dB>", dB_threshold, ", sleep:", sleep_type, ", ", model_type, ", Main test"), las = 3, cex.axis= 1.3)
            plot_list[[length(plot_list)+1]] <- recordPlot()
            #testDispersion(simulationOutput)
            #outliers(simulationOutput)
            CO = summary(m)$coef$cond
            model_out = data.table(Int = CO[1,1], Est = CO[-1,1], SE = CO[-1,2], z = CO[-1,3], P = CO[-1,4], type = substring(rownames(CO)[-1], 7,), vocalizations = vocalizations, dB_threshold = dB_threshold, sleep_type = sleep_type, model = "comparisons", model_type = model_type, nobs = summary(m)$nobs, nmales = length(unique(m$modelInfo$reTrms$cond$flist$box)), nvisits = length(unique(m$modelInfo$reTrms$cond$flist$counter)))

    },
    error = function(e)cat("ERROR :",conditionMessage(e), "\n") )












#slope test (Figure 1F, Scenario 2)#####
if(sleep_type == 0) {
  tmp = subset(ActivityDataPerMinute, sleep == sleep_type & DB == dB_threshold & f_type == "during" & direction_reliable == 1) #note that here the overlap is NOT set to 1! This is correct! This analysis concerns only data WHILE the female is inside the nest box and there is by definition not overlap possible (the overlap is between the "after" of one visit and the before or during of the next visit)

  if(vocalizations == "song") tmp[, var := no_song]
  if(vocalizations == "calls") tmp[, var := no_calls]
  if(vocalizations == "both") tmp[, var := no_vocs]

  tryCatch(
    {

      if(model_type == "zero-inflated poisson") m = glmmTMB(var ~ time_to_in + (1|box) +(1|counter), data = tmp, ziformula = ~1, family = poisson) #overlap does not matter, because that is only relevant for the time periods when the fmale is NOT inside the nestbox
      if(model_type == "poisson") m = glmmTMB(var ~ time_to_in + (1|box) +(1|counter), data = subset(tmp, var > 0), ziformula = ~0, family = poisson)
      if(model_type == "binomial") m = glmmTMB(var1 ~ time_to_in + (1|box) + (1|counter), data = subset(tmp[, var1 := ifelse(var > 0, 1, 0)], sleep == sleep_type & DB == dB_threshold), ziformula = ~0, family = binomial)
      if(model_type == "gaussian") m = glmmTMB(log(var+0.01) ~ time_to_in + (1|box) + (1|counter), data = tmp, ziformula = ~0, family = gaussian)




      simulationOutput <- simulateResiduals(fittedModel = m)
      plot(simulationOutput)
      axis(4, at = 0.5, paste0(vocalizations, ", dB>", dB_threshold, ", sleep:", sleep_type, ", ", model_type, ", scenario 2"), las = 3, cex.axis= 1.3)
      plot_list[[length(plot_list)+1]] <- recordPlot()

      #testDispersion(simulationOutput)
      #outliers(simulationOutput)
      summary(m)

      CO = summary(m)$coef$cond
      model_out_slope = data.table(Int = CO[1,1], Est = CO[2,1], SE = CO[2,2], z = CO[2,3], P = CO[2,4], type = substring(rownames(CO)[-1], 7,), vocalizations = vocalizations, dB_threshold = dB_threshold, sleep_type = sleep_type, model = "slope", model_type = model_type, nobs = summary(m)$nobs, nmales = length(unique(m$modelInfo$reTrms$cond$flist$box)), nvisits = length(unique(m$modelInfo$reTrms$cond$flist$counter)))
      model_out = rbind(model_out, model_out_slope)

},
error = function(e)cat("ERROR :",conditionMessage(e), "\n") )

  }



model_out_list[[length(model_out_list)+1]] = model_out
}
check_time = Sys.time() - check_time
check_time

#summarize output #####
#model plots
pdf(paste0(getwd(), "/ResultsAndFigures/", Sys.Date(), '_myplots.pdf'), onefile=TRUE)
for (i in plot_list) {
  replayPlot(i)
}
graphics.off()

#model results
rt = function(x, digits = 2) formatC(x, digits=digits,format="f", flag="#")
out = rbindlist(model_out_list)
out[, round_digits := ifelse(sleep_type == 0 & type == "o_in", 3, 2)]
out[, "Intercept" := rt(Int, round_digits), by = row.names(out)]
out[, "Estimate±SE" := paste0(rt(Est, round_digits), "±", rt(SE, round_digits)), by = row.names(out)]
out[, "z-value" := rt(z, round_digits), by = row.names(out)]
out[, "P-value" := ifelse(P >= 0.01, rt(P), ifelse(P >= 0.001, rt(P, digits = 3), "<0.001"))]
out[, ":=" (Int = NULL, Est = NULL, SE = NULL,z = NULL, P = NULL )]
out[, vocalizations := as.vector(vocalizations)]
setorder(out, -sleep_type, vocalizations, model_type, -dB_threshold)
out_dawn = out[sleep_type == 1 & type == "after",]
out_dusk = out[sleep_type == -1 & type == "before",]
out_day1 = out[sleep_type == 0 & type == "before" & model == "comparisons",]
out_day2 = out[sleep_type == 0 & type == "after",]
out_day3 = out[sleep_type == 0 & type == "before" & model == "comparisons_short_timescale"]
out_day4 = out[sleep_type == 0 & type == "o_in",]
write.table(out_dawn, paste0(getwd(), "/ResultsAndFigures/", Sys.Date(), "_out_dawn.csv"), sep=";", row.names=FALSE, na = "")
write.table(out_dusk, paste0(getwd(), "/ResultsAndFigures/", Sys.Date(), "_out_dusk.csv"), sep=";", row.names=FALSE, na = "")
write.table(out_day1, paste0(getwd(), "/ResultsAndFigures/", Sys.Date(), "_out_day1.csv"), sep=";", row.names=FALSE, na = "")
write.table(out_day2, paste0(getwd(), "/ResultsAndFigures/", Sys.Date(), "_out_day2.csv"), sep=";", row.names=FALSE, na = "")
write.table(out_day3, paste0(getwd(), "/ResultsAndFigures/", Sys.Date(), "_out_day3.csv"), sep=";", row.names=FALSE, na = "")
write.table(out_day4, paste0(getwd(), "/ResultsAndFigures/", Sys.Date(), "_out_day4.csv"), sep=";", row.names=FALSE, na = "")







####Numbers for Methods and Supplement #####
# 2 minutes is the ??% of the average duration of a female's stay
tmp = unique(ActivityDataPerPeriod[sleep == 0 & DB == -36, .(box, ID, time_inside_min, counter)])
2/median(tmp$time_inside_min)
2/range(tmp$time_inside_min)


#number of pairs
tmp = unique(ActivityDataPerPeriod[, .(box, ID)])
nrow(tmp) #80 pairs

#number of visits
nrow(unique(ActivityDataPerPeriod[sleep == 0, .(counter)])) #128

#median duration of visit
tmp = unique(ActivityDataPerPeriod[sleep == 0 & DB == -36, .(box, ID, time_inside_min, counter)])
median(tmp$time_inside_min)
range(tmp$time_inside_min)

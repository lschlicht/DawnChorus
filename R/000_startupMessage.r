#Create startup message
.onLoad <- function(libname, pkgname){
  packageStartupMessage("Dataset and scripts belonging to Schlicht, Schlicht, Santema & Kempenaers 2023 (Proc. R. Soc. B)\n
'A dawn and dusk chorus will emerge if males sing in the absence of their mate'\n
HOW TO USE:
Dataset 1: 'ActivityDataPerMinute' Contains male song and female box visits per minute
Dataset 2: 'ActivityDataPerPeriod' Contains male song and female box visits per period (before, during or after female box visit)
A description of the dataset can be found at\n
> ?ActivitDataPerMinute
and
> ?ActivitDataPerPeriod
To find the location of the r-scripts, type:
paste0(.libPaths(), '/DawnChorus/Scripts/')\n
To get a list of the names of existing scripts, type:
> list.files(paste0(.libPaths(), '/DawnChorus/Scripts/'))
To open a specific scipt, type
> file.edit(paste0(.libPaths(), '/DawnChorus/Scripts/THE_SCRIPT_NAME.r'))
Don't forget to adjust the name of the script!")
}

#Create startup message
.onLoad <- function(libname, pkgname){
  packageStartupMessage("Dataset and scripts belonging to Schlicht, Schlicht, Santema & Kempenaers 2023 (Proc. R. Soc. B)\n
'A dawn and dusk chorus will emerge if males sing in the absence of their mate'\n

HOW TO USE:
Dataset 1: 'ActivityDataPerMinute' Contains male song and female box visits per minute
Dataset 2: 'ActivityDataPerPeriod' Contains male song and female box visits per period (before, during or after female box visit)
To load the datasets type
> data(DawnChorus)\r\n
A description of the dataset can be found at\n
> ?ActivitDataPerMinute
and
> ?ActivitDataPerPeriod
The different scripts can be found by typing
paste0(.libPaths(), '/DawnChorus/inst.r')\n
To open the code, type
> file.edit(paste0(.libPaths(), '/DawnChorus/THE_SCRIPT_NAME.r'))")
}

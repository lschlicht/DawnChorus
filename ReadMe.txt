There are three ways to use this data and code.

(1) Install the package from github. The code is available at ... and should run immediately.
    Installation:
    > install("devtools") #if it is not yet available
    > require(devtools)
    > install_github( "lschlicht/DawnChorus")
    > require(dc)
    #The data is now available.
    #The source code of the analyses and figures is available in the installed packages folder, which can be found at:
    > paste0(find.package("dc"), "/Scripts") #path to code
    #to open the folder containing the scripts on Windows, run
    > shell.exec(paste0(find.package("dc"), "/Scripts"))
    
(2) Download the files from the folders extdata, data and inst and run the functions.

(3) The data in the folder "extdata" are .csv files that can be used independently of R.

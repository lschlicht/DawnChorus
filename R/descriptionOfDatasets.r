#' Male song and female nestbox visits: data per period (during, before, after female visit)
#'
#' @docType data
#'
#' @usage data(ActivityDataPerPeriod)
#'
#' @format A data table with 7086 rows and 32 columns
#' \describe{
#' \item{box}{breeding box identity}
#' \item{ID}{female metal band ID}
#' \item{time_inside_min}{Time spent inside the nestbox (in minutes)}
#' \item{in_}{Date and time of nestbox entry}
#' \item{out_}{Date and time of nestbox exit}
#' \item{out_duration}{Length of exit (i.e. time in seconds between first trigger of inner and last trigger of outer light barrier)}
#' \item{sleep}{1: dawn data, 0: day data, -1: dusk data}
#' \item{counter}{ID of female nestbox visit}
#' \item{direction_reliable}{Internal score of reliability of direction assignment (female going in or going out of nestbox)}
#' \item{overlap}{1 = yes, there is overlap with another visit, 0 = no overlap}
#' \item{rel_day}{Specifies how many days the date on which the current emergence time was recorded is away from the first egg of this bird in the current breeding_season. }
#' \item{start}{Start of time interval.}
#' \item{end}{end of time interval}
#' \item{type}{"before", "during" or "after" female visit to nestbox, or NA}
#' \item{DB}{Decibel threshold used to calculate male song rate. Subset as needed.}
#' \item{no_song}{Number of Type 1 songs (see supplement)}
#' \item{no_calls}{Number of Type 2 songs (see supplement)}
#' \item{noYN}{Any type 1 songs within the given interval? 1 = Yes, 0 = No}
#' \item{noCallsYN}{Any type 2 songs within the given interval? 1 = Yes, 0 = No}
#' \item{N}{Number of datapoints in period}
#' \item{f_type}{factor(type) (see above)}
#' \item{no_vocs}{number of type 1 and type 2 songs}
#' \item{no_vocsYN}{Any type 1 ot type 2 songs within the given period? 1 = yes, 0 = no}
#' \item{f_noYN}{factor(noYN) (see above)}
#' \item{no_mins_song}{Number of minutes with song type 1.}
#' \item{no_mins_calls}{Number of minutes with song type 2.}
#' \item{no_mins_vocs}{Number of minutes with songs type 1 and 2.}
#' \item{song_per_time}{Number of songs type 1 per minute.}
#' \item{calls_per_time}{Number of songs type 2 per minute.}
#' \item{vocs_per_time}{Number of songs type 1 and 2 per minute.}
#' \item{date_}{Date of datapoint (YYYY-MM-DD)}
#' \item{hour_of_out}{Hour of the day within which the female exited the nestbox.}
#' }
#' @keywords datasets
"ActivityDataPerPeriod"

#' Male song and female nestbox visits: data per minute
#'
#' @docType data
#'
#' @usage data(ActivityDataPerMinute)
#'
#' @format A data table with 82838 rows and 29 columns
#' \describe{
#' \item{box}{breeding box identity}
#' \item{ID}{female metal band ID}
#' \item{time_inside_min}{Time spent inside the nestbox (in minutes)}
#' \item{in_}{Date and time of nestbox entry}
#' \item{out_}{Date and time of nestbox exit}
#' \item{out_duration}{Length of exit (i.e. time in seconds between first trigger of inner and last trigger of outer light barrier)}
#' \item{sleep}{1: dawn data, 0: day data, -1: dusk data}
#' \item{counter}{ID of female nestbox visit}
#' \item{time_min}{Starting time of 1-minute time slot. Each 1-min timeslot is one datapoint.}
#' \item{direction_reliable}{Internal score of reliability of direction assignment (female going in or going out of nestbox)}
#' \item{overlap}{1 = yes, there is overlap with another visit, 0 = no overlap}
#' \item{rel_day}{Specifies how many days the date on which the current emergence time was recorded is away from the first egg of this bird in the current breeding_season. }
#' \item{start}{Start of time interval.}
#' \item{end}{end of time interval}
#' \item{type}{"before", "during" or "after" female visit to nestbox}
#' \item{DB}{Decibel threshold used to calculate male song rate. Subset as needed.}
#' \item{no_song}{Number of Type 1 songs (see supplement)}
#' \item{no_calls}{Number of Type 2 songs (see supplement)}
#' \item{time_to_in}{Minutes to box entry}
#' \item{time_to_out}{Minutes to box exit}
#' \item{time_to_in_min}{Minutes to box entry rounded to full minutes}
#' \item{noYN}{Any type 1 songs within the given interval? 1 = Yes, 0 = No}
#' \item{noCallsYN}{Any type 2 songs within the given interval? 1 = Yes, 0 = No}
#' \item{f_type}{factor(type) (see above)}
#' \item{no_vocs}{number of type 1 and type 2 songs}
#' \item{no_vocsYN}{Any type 1 ot type 2 songs within the given period? 1 = yes, 0 = no}
#' \item{f_noYN}{factor(noYN) (see above)}
#' }
#' @keywords datasets
"ActivityDataPerMinute"


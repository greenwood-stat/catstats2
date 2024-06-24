#' Blood Alcohol Contents OSU
#'
#' A group of n=16 student volunteers at The Ohio State University drank a
#' number of beers and their BAC levels were measured. According to the Electronic
#' Encyclopedia of Statistcal Examples and Exercises, "The experiment took place
#' in February of 1986 at a student dormitory. Sixteen students volunteered to be
#' the subjects in the experiment. Each student blew into a breathalyzer ... to
#' indicate that his or her initial BAC was zero. The number (between 1 and 9) of
#' 12 ounce beers to be drunk was assigned to each of the subjects by drawing
#' tickets from a bowl. Thirty minutes after consuming their final beer, students
#' had their BAC measured by a police officer of the OSU police department. The
#' officer also administered a road sobriety test before and after the alcohol consumption.
#' This involved performing four simple tasks, graded on a scale of 1 to 10 (ten being
#' a perfect rating), demonstrating coordination: balancing on one foot, touching
#' the tip of one’s nose with a forefinger, placing one’s head back with one’s eyes
#' closed, and walking heel to toe. The police officer was not aware of how much alcohol
#' each subject had consumed."
#'
#'
#'
#' @format A data frame with 16 rows and 2 variables:
#' \describe{
#'  \item{Gender}{Reported as male or female}
#'  \item{Weight}{Reported weight of subjects, in pounds}
#'  \item{Beers}{number of 12 oz beers consumed}
#'  \item{BAC}{blood alcohol content, in grams per decaliter}
#'  \item{1st_Sober}{Before alcohol consumption sobriety rating}
#'  \item{2nd_Sober}{After alcohol consumption sobriety rating}
#' }
#' @source Laura Dowling for data from her Ohio State class project, discussion based on https://bcs.whfreeman.com/WebPub/Statistics/shared_resources/EESEE/BloodAlcoholContent/index.html
"BB"


#' Blood Alcohol Contents Australia
#'
#' University of Western Sydney
#' In October of 1993, 22 volunteers, mainly university staff and students,
#' attended a lunchtime “cocktail party.” Prior to the start of the experiment,
#' a police officer from the Drug and Alcohol Unit breath-tested all of the participants
#' using a breathalyzer. White wine (10% alcohol/volume) was distributed in 120 ml
#' glasses, and each participant chose his or her own rate of drinking.
#' Snack food and water were available at all times.
#' After 45 minutes participants were told to stop drinking.
#' After waiting an additional 15 minutes each subject was breath-tested again
#' and the number of glasses consumed was recorded for all participants.#'
#'
#'
#' @format A data frame with 16 rows and 2 variables:
#' \describe{
#'  \item{Gend}{Reported as male or female}
#'  \item{Wgt}{Reported weight of subjects, in kg}
#'  \item{Height}{Reported height, in cm}
#'  \item{Age}{Age, in years}
#'  \item{BAC}{blood alcohol content, in grams per decaliter (presumed units)}
#'  \item{Wine}{Number of glasses of wine consumed in 45 minutes}
#' }
#' @source Prof. Kevin Donegan, Univ. of Western Sydney, discussion based on https://bcs.whfreeman.com/WebPub/Statistics/shared_resources/EESEE/BloodAlcoholContent/index.html
"BBA"



#' Montana Fires
#'
#' This data frame contains information about yearly wildfires in Montana from
#' 1985 to 2007.  Each year constitutes one observation in these data so there
#' are 23 rows (one for each year).
#'
#' @format A data frame with 23 rows and 3 columns:
#' \describe{
#'  \item{Year}{year of data}
#'  \item{Temperature}{average daily high temperature, in degrees Fahrenheit}
#'  \item{Hectares}{total area burned by wildfires.  One hectare is equal to
#'    10,000 square meters.}
#'   }
#' @source Gude, P.H., Cookson, A.J., Greenwood, M.C., and Haggerty, M. (2009)
#'   Homes in Wildfire-Prone Areas: An Empirical Analysis of Wildfire Suppression
#'   Costs and Climate Change.
"mtfires"

#' Fitness Measurements Taken on a Treadmill
#'
#' These data come from a study on a set of subjects at a university.  Used in Intermediate Statistics with R in Chapter 1 to help introduce
#' students to R and RStudio.
#'
#' @format a \code{data.frame} with 31 observations on 8 variables:
#' \describe{
#'   \item{Subject}{an integer denoting the person measured}
#'   \item{TreadMillOx}{subjects' treadmill oxygen consumption in ml per kg per
#'   minute}
#'   \item{TreaMillMaxPulse}{subjects' maximum pulse rate in beats per minute}
#'   \item{RunTime}{subjects' minutes to run 1.5 miles}
#'   \item{RunPulse}{subjects' maximum pulse rate during 1.5 mile run in beats per minute}
#'   \item{RestPulse}{subjects' resting pulse rate in beats per minute}
#'   \item{BodyWeight}{subjects' weight in kg}
#'   \item{Age}{subjects' age in years}
#' }
#'
#' @name treadmill
#' @docType data
#' @references
#' Westfall, Peter H., and S. Stanley Young. 1993. Resampling-Based
#' Multiple Testing: Examples and Methods for p-Value Adjustment. New York: Wiley.
#' @keywords data treadmill fitness
#' @examples
#' boxplot(treadmill$RunTime, ylab = "1.5 Mile Run Time (minutes)",
#'   main = "Boxplot of the Run Times of n = 31 participants")
"treadmill"

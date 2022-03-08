#' @title
#' Calculate Time Remaining
#'
#' @description
#' Calculate the time remaining after an iteration of a
#' loop is completed.
#'
#' @param iteration Integer of the iteration.
#' @param total_iterations Integer of total iterations in the loop.
#' @param time_value_per_iteration Numeric of the time per iteration.
#' @param time_unit_per_iteration Time unit that is passed to \code{\link[lubridate]{duration}}. Options: c("seconds", "minutes", "hours", "days", "weeks", "months", "years",
#'    "milliseconds", "microseconds", "nanoseconds", "picoseconds")
#'
#' @return
#' Duration class object
#'
#' @details DETAILS
#' @rdname calculate_time_remaining
#' @export
#' @importFrom lubridate duration



calculate_time_remaining <-
        function(iteration,
                 total_iterations,
                 time_value_per_iteration,
                 time_unit_per_iteration = c("seconds", "minutes", "hours", "days", "weeks", "months", "years", "milliseconds", "microseconds", "nanoseconds", "picoseconds")) {


                time_unit_per_iteration <-
                match.arg(
                        arg = time_unit_per_iteration,
                        choices = c("seconds", "minutes", "hours", "days", "weeks", "months", "years", "milliseconds", "microseconds", "nanoseconds", "picoseconds"),
                        several.ok = F
                )

                lubridate::duration(
                        num = time_value_per_iteration*((total_iterations-iteration)),
                        units = time_unit_per_iteration
                )



}

#' Waypoints defining the simulation route
#'
#' A dataset containing the waypoints that define the route taken by drivers
#' during the simulation. The data includes the lap number and x and y
#' coordinates of each waypoint. Each waypoint refers to a corner in the
#' simulation at which the driver must make a turn. This can be at t-shaped
#' intersections or an L-shaped section of the road that appears on the
#' outer-most sections.
#'
#' @format A data frame (tibble) with 41 rows and 3 variables:
#' \describe{
#'   \item{lap}{Lap number that driver is on at this waypoint}
#'   \item{x}{x-coordinate in the simulated world. Entire world spanned from 0
#'   to 1200, with intersections and corners being found at 200 to 1000 in
#'   increments of 200.}
#'   \item{z}{z-coordinate in the simulated world. Entire world spanned from 0
#'   to 1200, with intersections and corners being found at 200 to 1000 in
#'   increments of 200. This is z rather than y, because y in the 3D environment
#'   defined by Unity indicates upwards (i.e., towards the sky)}
#' }
"waypoints"

#' This commented out section contains the code used to create the
#' sim_lap_routes tibble exported with the package as data

# waypoints <- tibble::tribble(
#   ~lap,  ~x,  ~z,
#   1, 600, 200,
#   1, 600, 400,
#   1, 400, 400,
#   1, 400, 800,
#   1, 800, 800,
#   1, 800, 200,
#   1, 600, 200,
#   2, 600, 200,
#   2, 600, 800,
#   2, 800, 800,
#   2, 800, 200,
#   2, 600, 200,
#   3, 600, 200,
#   3, 600, 600,
#   3, 200, 600,
#   3, 200, 800,
#   3, 600, 800,
#   3, 600, 1000,
#   3, 800, 1000,
#   3, 800, 200,
#   3, 600, 200,
#   4, 600, 200,
#   4, 600, 400,
#   4, 400, 400,
#   4, 400, 800,
#   4, 1200, 800,
#   4, 1200, 600,
#   4, 1000, 600,
#   4, 1000, 400,
#   4, 800, 400,
#   4, 800, 200,
#   4, 600, 200,
#   5, 600, 200,
#   5, 600, 600,
#   5, 800, 600,
#   5, 800, 1000,
#   5, 1000, 1000,
#   5, 1000, 400,
#   5, 800, 400,
#   5, 800, 200,
#   5, 600, 200
# )
# devtools::use_data(waypoints)

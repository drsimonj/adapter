% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extracting.R
\name{get_type}
\alias{get_type}
\alias{get_drivers}
\alias{get_drones}
\title{Get data for drivers or drones only from a mixed user list}
\usage{
get_type(user_list, type)

get_drivers(user_list)

get_drones(user_list)
}
\arguments{
\item{user_list}{List of user data created via a method such as
\code{\link{read_all}}()}

\item{type}{String of the type of user to keep. Must be one of "driver" or
"drone".}
}
\description{
A list of user data generated via methods like \code{\link{read_all}}()
contains a mix of drivers and drone operators. \code{get_drivers}() and
\code{get_drones}() extract only users of a certain type from such a mixed
list.
}
\examples{
\dontrun{
# Given a user-list of data `d`...
get_drivers(d)
get_drones(d)
}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/move_components.R
\name{find_posssible_moves}
\alias{find_posssible_moves}
\title{find_possible_moves}
\usage{
find_posssible_moves(board)
}
\arguments{
\item{board}{board which we want to find moves from}
}
\value{
Named matrix which contains coordinates of possible starting pits.
}
\description{
Find coordinates of possible starting pits.
You can move from a pit if ic contains at least 2 stones.
}

\name{compareRCOruns}
\alias{compareRCOruns}
\title{compareRCOruns}
\usage{compareRCOruns(RCOres_l,
                       xaxis,
                       yaxis,
                       group_by,
                       current_point)}
\description{RCOres_l contains optimization results for different settings
this function compares the results to find out how robust is the optimization}
\arguments{
  \item{RCOres_l}{Portfolio name, represented as a character string; is used to identify the targ-file to load}
  \item{xaxis}{data set for x axis}
  \item{yaxis}{data set for x axis}
  \item{group_by}{How to apply color, by which data set}
  \item{current_point}{User can pick a point to display the optimization details. This point is bigger.}
}
%\examples{isDate(mydate, format = "\%Y-\%m-\%d")}

\name{f.writeRCOresToSQL}
\alias{f.writeRCOresToSQL}
\title{f.writeRCOresToSQL}
\usage{f.writeRCOresToSQL(res_rel,res_p,portfolio,covID,setID,calcdatetime)}
\description{The function uploads optimized asset weights and the corresponding settings to DB}
\arguments{
  \item{res_rel}{(optimized) relative weights}
  \item{res_p}{optimized portfolio weights = res_rel + Benchmarkweight}
  \item{portfolio}{Analyst Portfolio name}
  \item{covID}{RunID of the CovMatrix}
  \item{setID}{RCO setting from betsizing.CalculationRCOSettingSetsID}
  \item{calcdatetime}{the day and time of calculation}
}
%\examples{isDate(mydate, format = "\%Y-\%m-\%d")}
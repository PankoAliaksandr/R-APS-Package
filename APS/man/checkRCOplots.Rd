\name{checkRCOplots}
\alias{checkRCOplots}
\title{checkRCOplots}
\usage{checkRCOplots(rw_v,
                    cov_df,
                    targ_input_df,
                    single_set_df,
                    single_opt_det_v,
                    risk_contributions_v,
                    ShortIndexWithOptimCash = 0,
                    plot_type)}
\description{plots optimization results}
\arguments{
  \item{rw_v}{optimal relative weights}
  \item{cov_df}{covariance data frame}
  \item{targ_input_df}{settings data frame row}
  \item{single_set_df}{settings data frame row}
  \item{single_opt_det_v}{optimization details vector}
  \item{risk_contributions_v}{risk contributions}
  \item{ShortIndexWithOptimCash}{set 0 or 1 to implement the optimized Cash position by shorting the ind   ex via selling proportional weights in each index-member}
  \item{plot_type}{type of plot}
}
%\examples{isDate(mydate, format = "\%Y-\%m-\%d")}

\name{f.runRCO}
\alias{f.runRCO}
\title{f.runRCO}
\usage{f.runRCO(targ_df,set_df,cov_df, MaxAttempts)}
\description{This function is the optimizer}
\arguments{
  \item{targ_df}{table with conviction,ub,lb,tradeability and riskbudget per security}
  \item{set_df}{RCO-Optimization settings. Must be a row of data.frame}
  \item{cov_df}{Covariance matrix (must fit to tickers in targ! )}
  \item{MaxAttempts}{maximun number of attempts)}
}
%\examples{isDate(mydate, format = "\%Y-\%m-\%d")}

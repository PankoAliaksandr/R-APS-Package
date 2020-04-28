\name{runRCOLoops}
\alias{runRCOLoops}
\title{runRCOLoops}
\usage{runRCOLoops(Portfolio,
                    calculation_date,
                    TargetTE,
                    MaxRelWeight,
                    Trials,
                    algo,
                    CashTolerance,
                    LeverageTolerance,
                    CovCalcWay,
                    CovReturns,
                    ShortIndexWithOptimCash,
                    InputParameters,
                    cov_run_id_v,
                    IndexFlex,
                    Ix_eq_Cash,
                    NetInvLambda,
                    LowConvictionExitInDays,
                    ConvictionGroups,
                    targ_from_excel)}
\description{This function executes several loops of optimization to check how robust is the result}
\arguments{
  \item{Portfolio}{Portfolio name, represented as a character string; is used to identify the targ-file to load}
  \item{calculation_date}{calculation date}
  \item{TargetTE}{Target Tracking error of a total portfolio (risk budget). Can be a single number or a numeric vector}
  \item{MaxRelWeight}{Maximum amplitude (difference from BM) of relative weigth (under- or over-)}
  \item{Trials}{Limit of Trials for the optimizer}
  \item{algo}{Algorithm name that nloptr uses for optimization. Can be a character string or character vector  (to vary through settings):  "NLOPT_GN_ISRES" non gradient; "NLOPT_LD_SLSQP" with Gradient, FAILED: "NLOPT_GD_STOGO","NLOPT_GD_STOGO_RAND"}
  \item{CashTolerance}{How much cash is allowed. Boundary for the Cash held (conditon is: -sum(w)>CashTolerance)}
  \item{LeverageTolerance}{Boundary for the Leverage allowed (condition is sum(w)<LeverageTolearnce)}
  \item{CovCalcWay}{Type of covariance matrix to use; c("nlshrink","cov") are supported}
  \item{CovReturns}{Which type of returns to use for covarniance matrix calculation. c("abs","rel") are supported}
  \item{ShortIndexWithOptimCash}{set 0 or 1 to implement the optimized Cash position by shorting the ind   ex via selling proportional weights in each index-member}
  \item{InputParameters}{Character string or a character vector with excel workseet name(s). Each worksheet contains target table input parameters for optimization.}
  \item{cov_run_id_v}{Covariance matrix IDs vector to use in optimization. NA value is used for the newest cov. matrix available for the chosen portfolio}
  \item{IndexFlex}{Is index flexible (futures)}
  \item{Ix_eq_Cash}{todo}
  \item{NetInvLambda}{punishment factor for NetInvestments != 0. NetInvestments != 0 only if Cash or Leverage are allowed }
  \item{LowConvictionExitInDays}{todo}
  \item{ConvictionGroups}{todo}
  \item{targ_from_excel}{If TRUE: Use target table from Excel document (alternative is DB)}
}
%\examples{isDate(mydate, format = "\%Y-\%m-\%d")}

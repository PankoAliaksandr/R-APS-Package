rm(list = ls())

# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# Description
#   This file is a wrapper to call RCO optimizer
#   This script is called from C# using background worker

# Parameters
#   C# calls the script with the next parameters (type is character string):
#   1. Analyst Portfolio Name (Ex. "APSFBEQCHL")
#   2. Caclulation Date (Ex. "2020-02-14")
#   3. Covariance Matrix ID (Ex. "297")
#   4. RCO optimization settings ID (Ex. "37")
#   5. (Still discussed) is Analyst Portfolio real or virtual (Ex. "0" or "1")

# Dependencies (required libraries)
#   1. require("RODBC")
#   2. library(nloptr)
#   3. require("reshape")
#   4. library(dplyr)

# How to test from here
#   0. Check dependencies (libraries)
#   1. After line "call_params = commandArgs(trailingOnly = TRUE)" in the beginning paste:
# call_params[1] = "APSFBEQCHL"
# call_params[2] = "2020-03-11"
# call_params[3] = "297"
# call_params[4] = "40"
# call_params[5] = 0
#   2. Run this script

# Algorithm:
# 1) Based on Portfolio Name and Date prepare a "target table"
# 2) Based on RCOSetID get RCO_settings
# 3) Based on CovMaSetID get CovMa (the whole matrix)
# 4) Execute runRCO() call and get the optimal weights.
# 5) Save the result in DB


# TODO
#   1) Tradeability issue: for AP doesn't matter for Real matters
#   2) Real Portfolios: not clear if used.
#   3) Create R package and put all functions inside


# Assumptions:
#   1. RCOSetID and CovMaSetID are considered to be correctly filled here.
#      This part is done id C# and account for default setting as well as direct user input


# Check and install package if required
# TODO it's better not to use this, but describe carefully
# required libraries, since otherwise it can be unclear which
# location is used during instalation + explicit form is
# everytime safer
# ipak <- function(pkg) {
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#   if (length(new.pkg))
#     install.packages(new.pkg, dependencies = TRUE)
#
#
#   sapply(pkg, require, character.only = TRUE) #require() is equivalent to library()
# }


# Get DB connection
FAFunc.GetDB <- function(database = "Aktienmodell") {
  # Description
  #   This function opens database connection

  # Dependencies:
  #   1. require("RODBC")
  RODBC::odbcDriverConnect(
    connection = paste(
      "Driver={SQL Server};server=sqltara;database=",
      database,
      ";trusted_connection=yes;",
      sep = "",
      collapse = ""))
}

isDate <- function(mydate, format = "%Y-%m-%d") {
  # Description
  #   Check is a date in the right format

  # Dependencies
  #   NO

  tryCatch(
    {
      # Try block
      # Int numbers can be converted to "date" format. Prevent this.
      mydate <- as.character(mydate)

      if(is.na(as.Date(x = mydate, format = format)) == FALSE){
        # If date is in wrong format as.Date returns NA
        # Here it is not NA -> format is correct
        return(TRUE)
      }else{
        # Date format is wrong
        return(FALSE)
      }
    },
    error = function(err){
      # This line for example generates an Exception:
      # as.Date(x = NULL, format = format)
      return(FALSE)
    }
  )
}

getRCOSetSettings <- function(RCOSetID, connection){

  # Description
  #   The function gets all required RCO settings by RCOSetID

  # Dependensies
  #   library(RODBC)

  if(is.null(RCOSetID) == FALSE &&
     is.na(RCOSetID) == FALSE){

    query <- paste0("SELECT ",
                    "[FK_CalculationRCOSettingSetID], ",
                    "[FK_ParameterName], ",
                    "[ParameterValue] " ,
                    "FROM [Aktienmodell].[betsizing].[CalculationRCOSettings] ",
                    "WHERE [FK_CalculationRCOSettingSetID] = '", RCOSetID, "'")

    result_df <- RODBC::sqlQuery(channel = connection, query = query, stringsAsFactors = FALSE)

    if(class(result_df) == "data.frame"){
      if(nrow(result_df) > 0){
        # Good! There are settings
        # Optimization requires parameters with values only

        RCO_settings_m <- matrix(data = result_df$ParameterValue,
                                 nrow = 1,
                                 ncol = length(result_df$ParameterValue))

        RCO_settings_df <- data.frame(RCO_settings_m)

        colnames(RCO_settings_df) <- result_df$FK_ParameterName


        return(RCO_settings_df)
      }
      else{
        print(paste0("NLopt solver status: ", -11))
        paste0("NLopt solver status message: Setting set ", RCOSetID, " is empty (no settings)")
        stop(paste0("getRCOSetSettiongs: Setting set ", RCOSetID, " is empty (no settings)"))
      }
    }else{
      # class(result_df) != "data.frame")
      print(paste0("NLopt solver status: ", -100))
      print(paste0("NLopt solver status message: getRCOSetSettiongs: Select statement contains an error"))
      stop("getRCOSetSettiongs: Select statement contains an error ")
    }

  }else{
    # RCOSetID == NULL OR RCOSetID == NA
    print(paste0("NLopt solver status: ", -100))
    print(paste0("NLopt solver status message: getRCOSetSettiongs: RCO Settings ID parameter is empty"))
    stop("RCO Settings ID parameter is empty")
  }
}

get_ap_bb_ticker_by_name <- function(portfolioName, connection){

  # Description
  #   The function derives analyst portfolio bloomberg ticker by name

  # Dependensies
  #   library(RODBC)

  query <- paste0("SELECT Ticker FROM [Aktienmodell].[config].[CustomPortfolios] ",
                  "WHERE Name = '", portfolioName, "'")

  result_df <- RODBC::sqlQuery(channel = connection, query = query)

  # Check the result
  if(class(result_df) == "data.frame"){
    # The query executed w/o errors
    if(nrow(result_df) == 1){
      # portfolioName has 1:1 relation to bb ap ticker
      return(as.character(result_df$Ticker[1]))
    }
    else{
      print(paste0("NLopt solver status: ", -12))
      print(paste0("NLopt solver status message: Portfolio ", portfolioName, " does not exist (or ambiguous) on the chosen date"))
      stop("Portfolio", portfolioName, " does not exist (or ambiguous) on the chosen date")
    }
  }else{
    # class(result_df) != "data.frame")
    print(paste0("NLopt solver status: ", -100))
    print(paste0("NLopt solver status message: get_ap_bb_ticker_by_name: Select statement contains an error"))
    stop("get_ap_bb_ticker_by_name: Select statement contains an error ")
  }
}

getTargetTable <- function(portfolioName, calculation_date, isReal, connection){

  # Description
  #   The function prepares first set of parameters for RCO:
  #   Target table includes: Convictions, upper and lower bound, risk buget and tradability

  # Dependensies
  #   library(RODBC)

  if(is.null(portfolioName) == FALSE &&
     is.na(portfolioName) == FALSE){

    if(is.null(calculation_date) == FALSE &&
       is.na(calculation_date) == FALSE){

      if(class(calculation_date) == "Date"){

        const_upper_bound <- 0.1
        part1 <- 'APKON'
        # TODO add tradability
        const_tradeability <- 0.001

        # TODO this part is not required any more since real portfolio are not used
        if(isReal == TRUE){
          # the portfolio name corresponds to a real portfolio
          # In this case consensus analyst portfolio is used
          # consensus portfolio looks like: 'APKONEQCHSM', when real like: 'EQ_CH_SM'

          part2 <- gsub(pattern = '_', replacement = '', x = portfolioName)
          portfolioName <- paste0(part1, part2)
        }
        else{
          # the portfolio name corresponds to an analyst portfolio
          # portfolioName is already correct. Nothing to do.
        }

        ap_bb_ticker <- get_ap_bb_ticker_by_name(portfolioName = portfolioName,
                                                 connection = connection)

        query <- paste0("SELECT ",
                        "TickerConsolidated AS Ticker, ",
                        "BenchmarkConstituentWeightInPercent, ",
                        "AdjustedConviction ",
                        "FROM Aktienmodell.betsizing.tvfAnalystPortfolioMembersPerDateWithConvCutOffForOptimization",
                        "('", ap_bb_ticker, "',",
                        "'", calculation_date,"',",
                        "(SELECT Aktienmodell.betsizing.fGetCutoffTimeBeforeAsOfDate",
                        "('", calculation_date ,"'))) ",
                        "WHERE IncludeConstituentInOptimiziation = 1" )

        result_df <- RODBC::sqlQuery(channel = connection, query = query)
        if(class(result_df) == "data.frame"){
          if(nrow(result_df) > 0){
            # Result exists
            # if a ticker is not in a benchmark set weight = 0
            result_df[is.na(result_df)] <- 0
            # Calculate bounds
            result_df$LowerBound <- (result_df$BenchmarkConstituentWeightInPercent * (-1)) * 0.01
            result_df$UpperBound <- const_upper_bound

            # Calculate Risk Budget
            result_df$RiskBudget <- abs(result_df$AdjustedConviction) / sum(abs(result_df$AdjustedConviction), na.rm = TRUE)

            result_df$Tradeability <- const_tradeability

            row.names(result_df) <- result_df$Ticker
            # Delete 2 columns
            result_df <- result_df[,!colnames(result_df) %in% c("Ticker", "BenchmarkConstituentWeightInPercent")]

            targ_col_names_v <- c("conviction", "lb","ub","RiskBudget", "tradeability")
            colnames(result_df) <- targ_col_names_v

            return(result_df)
          }
          else{

            print(paste0("NLopt solver status: ", -13))
            print(paste0("NLopt solver status message: Portfolio", ap_bb_ticker, " does not exist on ", calculation_date))
            stop("Portfolio", ap_bb_ticker, " does not exist on ", calculation_date)
          }
        }else{
          # class(result_df) != "data.frame")
          print(paste0("NLopt solver status: ", -100))
          print("NLopt solver status message: getTargetTable: Select statement contains an error ")
          stop("getTargetTable: Select statement contains an error ")
        }
      }else{
        # calculation date is not in date format
        print(paste0("NLopt solver status: ", -100))
        print("NLopt solver status message: Calculation date is  not in 'Date' format")
        stop("Calculation date is  not in 'Date' format")
      }
    }else{
      # Calculation date is empty
      print(paste0("NLopt solver status: ", -100))
      print("NLopt solver status message: Calculation date parameter is empty")
      stop("Calculation date parameter is empty")
    }
  }else{
    # AP_ID == NULL OR AP_ID == NA
    print(paste0("NLopt solver status: ", -100))
    print("NLopt solver status message: Analyst Portfolio ID parameter is empty")
    stop("Analyst Portfolio ID parameter is empty")
  }
}

# get COV for ID from sql
f.getCovFromSQL <- function(RunID,CalculationMethod,WideOrLong=c("wide","long")){
  # Description
  #   This function downloads covariance matrix from DB
  #   and can convert it to wide/long format

  # Dependencies:
  #   1. require("RODBC")
  #   2. require("reshape")

  # Test example
  # f.getCovFromSQL(RunID = -11, CalculationMethod = "cov", WideOrLong = "wide")


  selstr <- paste("select Ticker1, Ticker2, Covariance from betsizing.CovarianceMatrixFlattened ",
                  "where RunID=", RunID, " and CalculationMethod='", CalculationMethod, "'", sep="")

  con <- FAFunc.GetDB()

  # Get covariance matrix
  cov_df <- RODBC::sqlQuery(con, selstr)

  # Close DB connection
  RODBC::odbcClose(con)

  if(class(cov_df) == "data.frame"){
    if(nrow(cov_df) > 0){
      if(WideOrLong == "wide"){

        # change to wide format
        cov_df <- as.data.frame(reshape::cast(cov_df, Ticker1 ~ Ticker2 , value = "Covariance", fill = 0))
        rownames(cov_df) <- cov_df$Ticker1
        cov_df$Ticker1 <- c()

        # check symmetry
        stopifnot(all(round(cov_df, 7) == t(round(cov_df, 7))))
      }
      else{
        # Long format is required
        # Nothing. CovMa is already in long format
      }
    }else{
      # No rows in CovMa
      print(paste0("NLopt solver status: ", -14))
      print(paste("NLopt solver status message: Chosen Covariance with id", RunID,
                  "and CalculationMethod", CalculationMethod, "does not exist!"))
      stop(paste("NLopt solver status message: Chosen Covariance with id", RunID,
                 "and CalculationMethod", CalculationMethod, "does not exist!"))
    }
  }else{
    # reult has class != data.frame
    print(paste0("NLopt solver status: ", -100))
    print("NLopt solver status message: getCovFromSQL: Query contains a mistake")
    stop("NLopt solver status message: getCovFromSQL: Query contains a mistake")
  }

  return(cov_df)
}

#Write the optimized weights into the SQL-Result-Upload table
f.writeRCOresToSQL <- function(opt_rel_weights_v,
                               opt_abs_weights_v,
                               risk_contribution_v,
                               portfolio,
                               covID,
                               setID,
                               calcdatetime){
  # Description
  #   The function uploads optimized asset weights
  #   and the corresponding settings to DB

  # Dependensies
  #   library(RODBC)

  # Input Parameters:
  #   res_rel: (optimized) relative weights,
  #   res_p: optimized portfolio weights = res_rel + Benchmarkweight
  #   portfolio: Analyst Portfolio name
  #   covID: RunID of the CovMatrix,
  #   setID: RCO setting from betsizing.CalculationRCOSettingSetsID
  #   calcdatetime: the day and time of calculation

  # Test example
  #     opt_weights_relative <- rep(0.05,20)
  #     opt_weights_portfolio <- rep(0.01,20)
  #     names(opt_weights_relative) <- names(opt_weights_portfolio) <- seq(1:20)
  #     portfolio <- "U1355708-7 Client";
  #     covID <- 777;
  #     setID <-  2;
  #     calcdatetime <- "2020-01-01"
  #     f.writeRCOresToSQL(opt_weights_relative,
  #                        opt_weights_portfolio,
  #                        portfolio,covID,setID,calcdatetime)

  if(class(opt_rel_weights_v) == "numeric" &&
     class(opt_abs_weights_v) == "numeric" &&
     class(risk_contribution_v) == "numeric" &&
     class(portfolio) == "character" &&
     class(covID) == "integer" &&
     class(setID) == "integer" &&
     class(calcdatetime) == "character"){

    # Input has right type
    constituents_v <- names(opt_abs_weights_v)

    # Values must be listed in the same order
    if((is.null(names(opt_rel_weights_v)) == FALSE) &&
       (is.null(names(risk_contribution_v)) == FALSE) &&
       (is.null(names(opt_abs_weights_v)) == FALSE) &&
       all(names(opt_rel_weights_v) == constituents_v) &&
       all(names(risk_contribution_v) == constituents_v)){

      # Constituents' optimized absolute and relative weights have the same order
      n_constituents <- length(opt_abs_weights_v)

      # Portfolio must be the same for all constituents
      calcdatetime_v <- rep(x = calcdatetime, times = n_constituents)
      portfolio_v <- rep(x = portfolio, times = n_constituents)
      covID_v <- rep(covID, times = n_constituents)
      setID_v <- rep(setID, times = n_constituents)

      # Prepare table structure
      # This part is very tricky, sqlSave parameters leads to bug.
      df_to_upload <- data.frame(CalculationRCOResultForUploadID = seq(1:n_constituents),
                                 CalculationDate = calcdatetime_v,
                                 FKAPS = portfolio_v,
                                 FKCovMaRunID = covID_v,
                                 FKCalculationRCOSettingSetID = setID_v,
                                 Ticker = constituents_v,
                                 OptimizedRelativeWeight = opt_rel_weights_v,
                                 OptimizedPortfolioWeight = opt_abs_weights_v,
                                 RiskContribution = risk_contribution_v)

      # delete current entries + Insert
      con <- FAFunc.GetDB()
      table <- "betsizing.CalculationRCOResultForUpload"

      try(RODBC::sqlDrop(channel = con, sqtable =  table, errors = FALSE), silent = TRUE)

      columnTypes <- list(CalculationRCOResultForUploadID = "int",
                          CalculationDate = "datetime",
                          FKAPS = "NVARCHAR(50)",
                          FKCovMaRunID = "int",
                          FKCalculationRCOSettingSetID = "int",
                          Ticker = "NVARCHAR(50)",
                          OptimizedRelativeWeight = "decimal(15,3)",
                          OptimizedPortfolioWeight = "decimal(15,3)",
                          RiskContribution = "decimal(15,3)")

      s <- RODBC::sqlSave(channel = con,
                          dat = df_to_upload,
                          tablename = table,
                          varTypes = columnTypes)

      # Close connection
      RODBC::odbcClose(con)

      if(s == 1){
        print(paste("successfully inserted all specificTickerValues for the portfolio: ",
                    portfolio[1],
                    ", set: ",
                    setID,
                    ",cov: ",
                    covID))
        # 0 is sucess
        return(0)
      }else{
        print(paste0("NLopt solver status: ", -100))
        stop("NLopt solver status message: some error in sql insert")
      }
    }else{
      print(paste0("NLopt solver status: ", -100))
      stop("NLopt solver status message: The order of constituents is not the same for absolute and relative optimized weights",
           " or names are empty")
    }
  }else{
    print(paste0("NLopt solver status: ", -100))
    stop("NLopt solver status message: writeRCOresToSQL: at least one of the input parameters is of a wrong type")
  }
  #example
  #res_rel <- RCOres$RW[,1];res_p <- res_rel-RCOres$targ_3D_array[,"lb",1];portfolio <- "EQ_CH_L"; covID <- as.integer(7) ;setID <- as.integer(1) ;calcdatetime <- as.character(Sys.Date())
  #f.writeRCOresToSQL(res_rel,res_p,portfolio,covID,setID,calcdatetime)
}

# TRANSFORM FACTORS INTO NUMERICS
f.as.numeric.factor <- function(x) {
  # check if is already numeric then do nothing
  if (class(x) == "numeric") {
    return(x)
  }
  else
  {
    as.numeric(levels(x))[x]
  }
}

# reorder COV for specific ticker vector
f.matchCOVtoTickers <- function(Tickers,cov_df) {

  # Description
  #   1. Make sure CovMa has the same tickers as Tickers
  #   2. Make sure the sequence is also the same

  # Dependencies:
  #   1. f.as.numeric.factor

  # Return value:
  #  covariance matrix

  if(class(cov_df) == "data.frame"){
    # Check missing tickers
    cov_df_col_names <- colnames(cov_df)
    cov_df_row_names <- rownames(cov_df)

    if(all(cov_df_col_names == cov_df_row_names) == TRUE){
      # The same sequence in CovMa rows and columns is checked


      missTick <- Tickers[is.na(match(Tickers, cov_df_col_names))]
      if(length(missTick) == 0){
        # we know that tickers are the same. Only sequience can be theoretically different
        # change order to make it the same
        # if COV has more tickers than Tickers it will also work
        cov_df <- cov_df[Tickers, Tickers]

        # Convert to numeric matrix
        cov_ma <- as.matrix(sapply(cov_df, f.as.numeric.factor))

        if(isSymmetric.matrix(cov_ma, check.attributes = FALSE) == TRUE){
          return(cov_df)
        }else{
          print(paste0("NLopt solver status: ", -16))
          print("NLopt solver status message: Covariance Matrix is not symmetric")
          stop("NLopt solver status message: Covariance Matrix is not symmetric")
        }
      }else{
        print(paste0("NLopt solver status: ", -17))
        print(paste("NLopt solver status message: The following Tickers lack in Covariance matrix:", paste(missTick, collapse = ", ")))
        stop(paste("NLopt solver status message: The following Tickers lack in Covariance matrix:", paste(missTick, collapse = ", ")))
      }
    }else{
      print(paste0("NLopt solver status: ", -18))
      print("NLopt solver status message: CovMa: column names and row names are not the same")
      stop("NLopt solver status message: CovMa: column names and row names are not the same")
    }
  }else{
    print(paste0("NLopt solver status: ", -100))
    print("NLopt solver status message: cov_df parameter must be of data.frame type")
    stop("NLopt solver status message: cov_df parameter must be of data.frame type")
  }
}

# get the proper functions depending on settings!
f.setOptfunctions <- function(set, k = 0){
  # Dependencies
  #   NO

  # target function
  # A Target function is minimized

  if(set$IndexFlex == TRUE)
  {
    # set$IndexFlex == TRUE meant that you can use index futures
    # index here is like a stock, however it's risk contribution in 0

    ftarget <- function(x,rb_a,COVAR,target_te,A,CashTolerance,LeverageTolerance,NetInvLambda)
    {
      return(list("objective" =  as.matrix(-abs(c(0,t(rb_a[-1]))) %*% log(abs(x))) + NetInvLambda*sum(x)^2,
                  "gradient"  = diag(A)*as.matrix(c(0.00,abs(rb_a[-1])) / abs(x)) + NetInvLambda*2*sum(x)))
    }
  }else{
    # Here set$IndexFlex == FALSE
    ftarget <- function(x,rb_a,COVAR,target_te,A,CashTolerance,LeverageTolerance,NetInvLambda)
    {
      return(list("objective" =  as.matrix(-abs(t(rb_a)) %*% log(abs(x))) + NetInvLambda*sum(x)^2,
                  "gradient"  = diag(A)*as.matrix(abs(rb_a) / abs(x)) + NetInvLambda*2*sum(x)))
    }
  }

  # check if nlcon-settings are allowed
  stopifnot(set$Ix_eq_Cash == FALSE ||
              (set$Ix_eq_Cash == TRUE &&
                 set$SoftNetInvConstraint == FALSE &&
                 set$IndexFlex==FALSE))

  # SoftNetInvConstraint shows if leverage and cash are allowed
  if(set$SoftNetInvConstraint == TRUE)
  {
    # IndexFlex shows if index should be treated as normal asset or not
    if(set$IndexFlex == TRUE)
    {
      print(paste("OPTIMIZE: with flexible index, nlcon keeps NetInvestment within range"))

      nlcon_ineq <- function(w,
                             COVAR,
                             target_te,
                             A,
                             rb_a,
                             CashTolerance,
                             LeverageTolerance,
                             NetInvLambda){

        # Flexible index means that cash and leverage are allowed
        # nlcon algorithm keeps NetInvestment within the range
        # (Index still has to impact TE, just in TargetFunction disregarded!)

        # All constraints has structure: x <= 0
        # w[1] is reserved for the index

        # There are 5 constraints here:
        # 1. Direction constraint: conviction should correspond to weight sign
        # 2. TE-constraint: Overall portfolio should not exceed specified risk budget
        # 3. max postive NetInvestment (Leverage): how much we cash can be borrowed additionally
        # 4. max negative NetInvestemnt (Cash): how much cash can be in a portfolio
        # 5. limit risk contribution of first (index position's)

        direction <- diag(c(0,diag(A[-1,-1]))) %*% w
        te <- sqrt(t(w) %*% COVAR %*% w) - target_te
        leverage <- sum(w) - LeverageTolerance
        cash <- -sum(w) - CashTolerance
        index_rc <- w[1] * (COVAR %*% w)[1] / as.numeric(t(w) %*% COVAR %*% w) - rb_a[1]


        # jacobian of constraints with respect to weights(w)

        # gradient: direction constraint
        grad_dir <- diag(c(0,diag(A[-1,-1])))
        # gradient: TE-constraint
        grad_te <- t((COVAR %*% w) /  as.numeric(sqrt(t(w) %*% COVAR %*% w) ))
        # gradient: leverage constraint
        grad_leverage <- t(rep(1,length(w)))
        # gradient: max Short position (Cash)
        grad_cash <- t(rep(-1,length(w)))

        grad_index_rc <- t(rbind( (((COVAR %*% w)[1] + w[1]*COVAR[1,1])* (t(w)%*%COVAR%*%w) - 2*w[1]*COVAR[1,]%*%w*(COVAR%*%w)[1]) / (t(w) %*% COVAR %*% w)^2, #first row derivative after w[1]
                                  as.numeric(w[1]/(t(w) %*% COVAR %*% w))*COVAR[1,-1]                                                    #row 2:n derivatives after w[2:n]   (1)
                                  - as.numeric((2*w[1]*(COVAR[1,]%*% w))/(t(w) %*% COVAR %*% w)^2)*COVAR[-1,]%*%w ))                    #row 2:n derivatives after w[2:n]   (2)

        return(list("constraints"= c(direction, te, leverage, cash, index_rc),
                    "jacobian" = rbind(grad_dir,
                                       grad_te,
                                       grad_leverage,
                                       grad_cash,
                                       grad_index_rc)))
      }

      #here still make an extension for Ix_eq_Cash (cond. on parameter!!!). A bit tough to adjust the formula!!!
      #else if set$Ix_eq_Cash: .....

    } else {
      # set$IndexFlex == FALSE
      # nlcon keeps NetInvestment within range, index as normal asset"
      print(paste("OPTIMIZE: nlcon keeps NetInvestment within range, ix as normal asset"))

      nlcon_ineq <- function(w,
                             COVAR,
                             target_te,
                             A,
                             rb_a,
                             CashTolerance,
                             LeverageTolerance,
                             NetInvLambda){

        # All constraints has structure: x <= 0

        # There are 4 constraints here:
        # 1. Direction constraint: conviction should correspond to weight sign
        # 2. TE-constraint: Overall portfolio should not exceed specified risk budget
        # 3. max postive NetInvestment (Leverage): how much we cash can be borrowed additionally
        # 4. max negative NetInvestemnt (Cash): how much cash can be in a portfolio

        direction <- as.matrix(A %*% w)
        te <- c(as.matrix(sqrt(t(w) %*% COVAR %*% w)) - as.matrix(target_te))
        leverage <- sum(w) - LeverageTolerance
        cash <- -sum(w) - CashTolerance

        # grads are correct
        grad_dir <- A
        grad_te <- as.matrix(t((COVAR %*% w) /  (rep(sqrt(t(w) %*% COVAR %*% w),length(w)) )))
        grad_leverage <- t(rep(1,length(w)) )
        grad_cash <- t(rep(-1,length(w)))

        return(list("constraints"= c(direction, te, leverage, cash),
                    "jacobian" = rbind(grad_dir, grad_te, grad_leverage, grad_cash)))
      }
    }

  } else {
    # set$SoftNetInvConstraint == FALSE.
    # Leverage and Cash are not allowed (NetInvestment == 0)
    if(set$IndexFlex == TRUE){
      # Logic is not defined
    }else{
      # Here set$IndexFlex == FALSE
      print(paste("OPTIMIZE: eqcon keeps NetInvestment == 0"))

      nlcon_eq <- function(w,
                           COVAR,
                           target_te,
                           A,
                           rb_a,
                           CashTolerance,
                           LeverageTolerance,
                           NetInvLambda){
        # TODO why -sum ? it must be sum(w) = 0
        return(list("constraints" = -sum(w),
                    "jacobian"= rep(-1,length(w))))
      }

      # TODO k is not defined here
      # print(paste("Loop",k,"OPTIMIZE: ix as normal asset"))

      nlcon_ineq <- function(w,
                             COVAR,
                             target_te,
                             A,
                             rb_a,
                             CashTolerance,
                             LeverageTolerance,
                             NetInvLambda){

        # All constraints has structure: x <= 0

        # There are 2 constraints here:
        # 1. Direction constraint: conviction should correspond to weight sign
        # 2. TE-constraint: Overall portfolio should not exceed specified risk budget

        direction <- as.matrix(A %*% w)
        te <- c(as.matrix(sqrt(t(w) %*% COVAR %*% w)) - as.matrix(target_te))

        grad_dir <- A
        grad_te <- as.matrix(t((COVAR %*% w) /  (rep(sqrt(t(w) %*% COVAR %*% w),length(w)) )))

        return(list("constraints"= c(direction, te),
                    "jacobian" = rbind(grad_dir, grad_te)))
      } # end of nlcon_ineq
    } # end of set$IndexFlex == FALSE
  } # end of set$SoftNetInvConstraint == FALSE

  # return ftarget and nlcon
  if(exists("nlcon_eq"))
  {
    return(list(ftarget = ftarget,
                nlcon_eq = nlcon_eq,
                nlcon_ineq = nlcon_ineq))
  }else{
    return(list(ftarget = ftarget,
                nlcon_ineq = nlcon_ineq))
  }
}

# get range for weights of each ticker
f.getWeightRange <- function(lb,
                             ub,
                             conviction,
                             tradeability,
                             MaxRelWeight,
                             LowConvictionExitInDays = 10,
                             ConvictionGroups = 1 ){
  ConvictionGroups <- ConvictionGroups + 1
  groupedConviction <-  dplyr::ntile(abs(conviction),ConvictionGroups)-1
  ConvictionDegree <- pmax(1,groupedConviction)  #-1 as usually group without a bet is last, still ensure that factor is at least 1!

  #plot(jitter(conviction),ConvictionDegree)

  t_restrict <- LowConvictionExitInDays / tradeability * ConvictionDegree *10*10^(-4)  #tradeability quoted in days/10bp

  w_min <- pmax(lb,-t_restrict,-MaxRelWeight)
  w_max <- pmin(ub,t_restrict,MaxRelWeight)

  return(list(w_min = w_min,
              w_max = w_max,
              ConvictionDegree = ConvictionDegree))

  #examples
  #t <- as.data.frame(RCOres$targ_3D_array[,,1]); lb <- t$lb;ub <- t$ub;conviction <- t$conviction;tr <- t$tradeability
  #g1 <- f.getWeightRange(lb,ub,conviction,tr,0.1,LowConvictionExitInDays = 20,ConvictionGroups=7)
  #cbind(rownames(targ_df),g1$w_min,g1$w_max,conviction,g1$ConvictionDegree)

}

#implement short-index in portfolio
ImplIndexPosition <- function(w, w_bm, ZeroNetInvestment = FALSE){
  # Dependencies: NO

  stopifnot(length(w)-1 == length(w_bm)) #w includes index position, w_bm only the index constituents!
  #check bm-weights sum to 1
  if(round(sum(w_bm),3)!= 1) #plausibility check: lb show benchmark-weight?
  {message(paste("the lower bounds provided do not sum to 1, but to:",sum(w_bm)," - correct BM-weights needed to make a fully correct index replication"))
    print("the w_bm are normalized that their sum adds to 1 - might lead to problems in implementation, leads to only indicative target weights")
    w_bm <- w_bm/sum(w_bm)
  }

  #replicate index position of the first asset in portfolio
  #print("first asset assumed as index asset")
  if(ZeroNetInvestment==TRUE)
  {
    print("attention: this is changing the risk structure of the portfolio. probably unintended!")
    w1. <- w[1]-sum(w) #assume overinvestment = index!
    w[-1] <- w[-1]+w1.*w_bm
  }else
  {
    w[-1] <- w[-1]+w[1]*w_bm
  }
  w[1] <- 0
  return(w)

  #example (needs an existing result in RCOres-form)
  # w <- RCOres$RW[,1];w_bm <- -RCOres$targ_3D_array[-1,"lb",1];ZeroNetInvestment <- FALSE
  #ImplIndexPosition(w,w_bm)
}

# done
# get risk contributions of active weights
get_rcb <- function(weight_v, cov){

  # Description: get risk contributions of active weights

  # Dependencies: NO

  # Example (needs an existing result in RCOres-form)
  #   weight <- RCOres$RW[,1]
  #   cov <- RCOres$COV[,,1]
  #   rcb <- get_rcb(weight,cov)

  if(class(weight_v) == "numeric" &&
     isTRUE(any(class(cov) %in% c("data.frame", "matrix", "data.table")))){

    # Dimensions check
    stopifnot(isTRUE(is.vector(weight_v)) &&
              length(weight_v) == nrow(cov)
              && nrow(cov) == ncol(cov))

    RCO_names_v <- names(weight_v)
    cov_names_v <- colnames(cov)

    # Check if weights and covariance columns are named and names are feasible
    if(length(RCO_names_v) > 0 &&
       length(cov_names_v) > 0 &&
       length(RCO_names_v) == length(cov_names_v) &&
       all(RCO_names_v %in% cov_names_v)){

      # names must be listed in the same order to multiply correctly
      if(all(RCO_names_v == cov_names_v)){
        # Expected behavior. Nothing it's already correct
      }else{
        message("get_rcb: names are not the same")
        # mapping is required
        # Here names are the same but the order can differ
        weight_v <- weight_v[cov_names_v]
      }

      # Here weights match covariance
      # Calculate portfolio variance
      VAR <- as.numeric(t(weight_v) %*% as.matrix(cov) %*% weight_v)

      # Calculate risk contribution (not in %)
      rcb_ma <- weight_v * as.matrix(cov) %*% weight_v
      # convert to vector
      rcb_v <- as.vector(rcb_ma)
      # set the same names
      names(rcb_v) <- names(weight_v)
      # Percentage risk contribution
      rcb_v <- rcb_v/VAR

      return(rcb_v)

    }else{
      print(paste0("NLopt solver status: ", -100))
      print("NLopt solver status message: get_rcb: covariance names and weight names are not the same or empty")
      stop("NLopt solver status message: get_rcb: covariance names and weight names are not the same or empty")
    }
  }else{
    print(paste0("NLopt solver status: ", -100))
    print("NLopt solver status message: get_rcb: input has wrong type")
    stop("NLopt solver status message: get_rcb: input has wrong type")
  }
}

# Optimization Results Characteristic
OptimizationResultsCharacteristics <- function(rw, cov, lb, ub, set, rb_target){
  # Functions:
  # 1. ImplIndexPosition
  # 2. get_rcb

  # TODO check that the first position is really index (by name for instance)
  # Replicate index short on the index title if desired to do so
  IndexPosition <- rw[1]
  NetInvestment <- sum(rw)
  w_bm <- -lb[-1]
  sec_nr <- length(rw)

  #calculate weights to implement
  rw_impl <- ImplIndexPosition(rw,w_bm)

  #evaluate boundaries injured
  lb_inj <- ifelse(rw_impl < lb, 1, 0)
  ub_inj <- ifelse(rw_impl > ub, 1, 0)
  const_hit <- ifelse(lb_inj + ub_inj > 0, 1, 0)
  const_hit_pct <- sum(const_hit) / sec_nr

  #tracking error
  te<- as.numeric(sqrt(t(rw) %*% as.matrix(cov) %*% rw))

  if(round(te,4) != round(as.numeric(sqrt(t(rw_impl) %*% as.matrix(cov) %*% rw_impl)),4)   ) #te portfolio needs to be unchanged by
  {message("portfolio risk neutrality to implementation of index position in portfolio violated. Check LowerBounds and optimization quality!!!!")}
  #risk contributions
  rc <- get_rcb(weight_v = rw, cov=cov)

  # Total deviation from target risk budget: sum of rc actual - target
  total_rb_dev <- sum(abs(rc-rb_target))


  #portfolio beta
  p.beta <- t(rw) %*% cov[,1]/cov[1,1]

  return(list(IndexPosition = IndexPosition,
              NetInvestment = NetInvestment,
              rw_impl = rw_impl,
              te = te,
              const_hit = const_hit,
              const_hit_pct = const_hit_pct,
              p.beta = p.beta,
              rc = rc,
              total_rb_dev = total_rb_dev))

  #example (needs an existing result in RCOres-form)
  # rw <- RCOres$RW[,1] ; cov <- RCOres$COV[,,1]; lb <- RCOres$targ_3D_array[,"lb",1]; ub <- RCOres$targ_3D_array[,"ub",1]; rb_target <- RCOres$targ_3D_array[,"RiskBudget",1]
  # (ocr <- OptimizationResultsCharacteristics(rw,cov,lb,ub,set,rb_target)

}

# Optimization Functions
# RCO: Risk Contribution Optimization
f.runRCO <- function(targ,set,cov, MaxAttempts = 10){
  # Description
  #   This function is the optimizer

  # Function used:
  # 1. f.matchCOVtoTickers
  # 2. f.setOptfunctions
  # 3. f.getWeightRange
  # 4. OptimizationResultsCharacteristics

  # Dependencies:
  #   1. require("RODBC")
  #   2. library(nloptr)
  #   3. require("reshape")
  #   4. library(dplyr)
  #   5. library(stats)

  #inputs
  #targ: table with conviction,ub,lb,tradeability and riskbudget per security
  #set:  RCO-Optimization settings. Must be a row of data.frame
  #COV:  Covariance matrix (must fit to tickers in targ! )
  print("f.runRCO started")
  #checks
  stopifnot(all(targ$RiskBudget >= 0))#check if all risk budgets > 0
  stopifnot(all.equal(sum(targ[,"RiskBudget"]),1))#check if defined risk budget sums up to 100%
  stopifnot(colnames(targ) %in% c("conviction","RiskBudget","lb","ub","Ticker","tradeability"))
  stopifnot(is.data.frame(set),nrow(set)==1)
  set$MaxRelWeight <- f.as.numeric.factor(set$MaxRelWeight)
  set$algo <- as.character(set$algo)
  set$Ix_eq_Cash <- as.character(set$Ix_eq_Cash)
  set$IndexFlex <- as.character(set$IndexFlex)
  set$LowConvictionExitInDays <- f.as.numeric.factor(set$LowConvictionExitInDays)
  set$ConvictionGroups <- f.as.numeric.factor(set$ConvictionGroups)
  set$CashTolerance <- f.as.numeric.factor(set$CashTolerance)
  set$LeverageTolerance <- f.as.numeric.factor(set$LeverageTolerance)
  set$NetInvLambda <- f.as.numeric.factor(set$NetInvLambda)
  set$TargetTE <- f.as.numeric.factor(set$TargetTE)
  set$ShortIndexWithOptimCash <- as.character(set$ShortIndexWithOptimCash)
  set$SoftNetInvConstraint <- as.logical(set$SoftNetInvConstraint)
  set$Trials <- f.as.numeric.factor(set$Trials)
  set$xtol_rel <- f.as.numeric.factor(set$xtol_rel)
  print("here further checks if risk budgets are reasonably distributed would be useful!")
  #all risk budgets positive
  #range of individual risks, outliers

  #define target
  sec_nr <- nrow(targ)
  rw <- rep(0,times=sec_nr) #relative weights

  cov <- f.matchCOVtoTickers(Tickers = rownames(targ), cov_df = cov)

  if(grepl("Crncy",rownames(targ)[1]))    #turn sign of bm-index to reflect a long index position (needed for sum to 0 constraint!)
  {
    cov[1,] <- -1*cov[1,]
    cov[,1] <- -1*cov[,1]
    print("The sign of the first row in the Covariance Matrix was switched, Crncy-tickers are supposed to reflect negative index performance ")
  }

  # #check for invertability of cov
  # ipak("matrixcalc")
  # print(paste("the cov is invertible is", is.singular.matrix(as.matrix(cov) )))


  # get the target function and nlcon according to Settings
  optFun <-  f.setOptfunctions(set)

  #save actual index weight before adj. lower bounds
  w_range <- f.getWeightRange(lb = targ$lb,
                              ub = targ$ub,
                              conviction = targ$conviction,
                              tradeability = targ$tradeability,
                              MaxRelWeight = set$MaxRelWeight,
                              LowConvictionExitInDays = set$LowConvictionExitInDays,
                              ConvictionGroups = set$ConvictionGroups)

  targ$lb. <- w_range$w_min
  targ$ub. <- w_range$w_max

  #option settings
  local_opts <- list( "algorithm" = "NLOPT_LD_SLSQP",
                      "xtol_rel" = 10 ^ -13 )

  # Tolerance Options: https://nlopt.readthedocs.io/en/latest/NLopt_Introduction/
  # ftol_rel - relative change in target function value ( better to use if f.val != 0)
  # ftol_abs - absolute change in target tunction value (use if the value is close to 0)
  # xtol_rel - relative change in solution value ( better to use if val != 0)
  # xtol_abs - absolute change in solution value (use if the value is close to 0)

  opts <- list("algorithm" = as.character(set$algo),
               "maxeval" = as.numeric(set$Trials),
               "print_level" = 0,
               "xtol_rel" = 10 ^ -13, # set$xtol_rel,
               "xtol_abs" = 10 ^ -13, # 10^-7
               local_opts = local_opts)#,check_derivatives=TRUE)      #algortihms tried: "NLOPT_GN_ISRES",
  #algorithms used and no result:NLOPT_LN_AUGLAG,NLOPT_GN_DIRECT_L_RAND; algorithms to try: NLOPT_GNL_DIRECT_NOSCAL, NLOPT_GN_DIRECT_L_NOSCAL, and NLOPT_GN_DIRECT_L_RAND_NOSCAL,NLOPT_GN_ORIG_DIRECT and NLOPT_GN_ORIG_DIRECT_L,

  # direction of conviction (where active).
  # The sign shows short or long. Weight are relative to BM
  dir <- sign(targ$conviction)

  # assert that TickDet still fits COV!
  stopifnot(rownames(targ) == as.character(row.names(cov)))

  # print(paste("Cash treated as index asset with risk budget ",round(targ$RiskBudget[1]*100,2), "% and direction ",sign(targ$conviction[1])),sep="")

  x0 <- pmax(pmin(targ$ub., targ$RiskBudget * dir), targ$lb.)

  # define desired bet directions
  # A is a parameter in optimization function and inequality constraints
  A <- -diag(dir)
  #lb_reset=rep(-10,nrow(targ))#lower bounds are seperately specified in side constraint! -> set to very low value for the function!

  # ipak("nloptr")

  # Set up param to enter the loop
  runAnotherOptAttempt <- TRUE
  attempt <- 1
  while(runAnotherOptAttempt)
  {
    print(paste("start  attempt:",attempt,"with nloptr"))

    if(set$SoftNetInvConstraint){
      calctime <- system.time(opt_output <-
                                nloptr::nloptr(x0 = as.matrix(x0), # nloptr param: algorithm starting point
                                               eval_f = optFun$ftarget, # nloptr param: optimization  target function
                                               #eval_grad_f = fgrad,
                                               lb = as.matrix(targ$lb.), # nloptr param: solution lower bound
                                               ub = as.matrix(targ$ub.), # nloptr param: solution upper bound
                                               # eval_g_eq = nlcon_eq,   #nlcon_eq not required
                                               eval_g_ineq = optFun$nlcon_ineq, # nloptr param: constraints function
                                               opts = opts, # nloptr param: optimization settings
                                               target_te = as.matrix(set$TargetTE), # opt. f. and costr. f. param
                                               A = A, # opt. f. and costr. f. param
                                               CashTolerance = set$CashTolerance, # opt. f. and costr. f. param
                                               LeverageTolerance = set$LeverageTolerance, # opt. f. and costr. f. param
                                               COVAR = as.matrix(cov), # costr. f. param
                                               rb_a = as.matrix(targ$RiskBudget), # opt. f. and costr. f. param
                                               NetInvLambda = set$NetInvLambda # opt. f. and costr. f. param
                                ))["elapsed"]
    }else{
      #nlcon_eq required!
      calctime <- system.time(opt_output <-
                                nloptr::nloptr(x0=as.matrix(x0),
                                               eval_f = optFun$ftarget,
                                               #eval_grad_f = fgrad,
                                               lb = as.matrix(targ$lb.),
                                               ub = as.matrix(targ$ub.),
                                               eval_g_eq = optFun$nlcon_eq, #nlcon_eq required!
                                               eval_g_ineq = optFun$nlcon_ineq,
                                               opts = opts,
                                               target_te = as.matrix(set$TargetTE),
                                               A = A,
                                               CashTolerance = set$CashTolerance,
                                               LeverageTolerance = set$LeverageTolerance,
                                               COVAR = as.matrix(cov),
                                               rb_a = as.matrix(targ$RiskBudget),
                                               NetInvLambda = set$NetInvLambda
                                ))["elapsed"]
    }

    # check optimization quality

    # initialize
    validResult <- FALSE
    runAnotherOptAttempt <- FALSE

    # Successful termination (positive return values):
    # NLopt solver status: 1 (Generic success return value)
    # NLopt solver status: 2 (Optimization stopped because stopval (above) was reached)
    # NLopt solver status: 3 (Optimization stopped because ftol_rel or ftol_abs (above) was reached)
    # NLopt solver status: 4 (Optimization stopped because xtol_rel or xtol_abs (above) was reached)
    # NLopt solver status: 5 (Optimization stopped because maxeval (above) was reached)
    # NLopt solver status: 6 (Optimization stopped because maxtime was reached)

    # Error codes (negative return values):
    # NLopt solver status: -1 (Generic failure code)
    # NLopt solver status: -2 (Invalid arguments)
    # NLopt solver status: -3 (Ran out of memory)
    # NLopt solver status: -4 (Roundoff errors led to a breakdown of the optimization algorithm)

    if(opt_output$status >= 0){
      # Algorithm has not failed, however not all constraints have been used

      # check direction constraint:
      # The direction constraint means if conviction is negative
      # the optimal weight (relative to BM) must be also negative
      # if the conviction is positive the optimal weight must be also positive
      # This means that conviction * opt. weight must always be non-negative

      check_sign_v <- sign(targ$conviction) * sign(opt_output$solution)

      # injuries <- -A %*% opt_output$solution < 0
      if(all(check_sign_v >= 0)){
        if(all(check_sign_v > 0)){
          # Valid: convictions and optimal weights have the same sign

          if(is.finite(opt_output$objective)){
            # Valid: Objective function has finite value
            # Actually, means that weights != 0 (x=0 -> log(x) = -Inf), must be always true here

            # Success: We do not need another attempt: default is FALSE
            validResult <- TRUE

          }else{
            # Objective function has Infinite value
            validResult <- FALSE
            runAnotherOptAttempt <- FALSE # original, but I'm not sure
            # TODO do we need another attempt here?
            # Is it possible that target function will not be Inf next time?
          }
        }else{
          # Some convictions or optimal weights are 0
          # This case can be valid only if conviction = 0 and weight = 0
          # Actually, here convictions are not supposed to be = 0, but still...
          ind_0_conv <- which(targ$conviction) == 0
          ind_0_weight <- which(opt_output$solution) == 0

          # at least one is not empty here
          if(length(ind_0_conv) == length(ind_0_weight) &&
             all(ind_0_conv == ind_0_weight)){
            # Valid: 0 weights correspond to 0 convictions

            if(is.finite(opt_output$objective)){
              # Valid: Objective function has finite value
              # Actually, means that weights != 0 (x=0 -> log(x) = -Inf), must be always true here

              # Success: We do not need another attempt: default is FALSE
              validResult <- TRUE

            }else{
              # Objective function has Infinite value
              validResult <- FALSE
              runAnotherOptAttempt <- FALSE # original, but I'm not sure
              # TODO do we need another attempt here?
              # Is it possible that target function will not be Inf next time?
            }
          }else{
            # There are zeroes but they are not at the same places
            # Invalid: conviction and weights are inconsistent
            validResult <- FALSE
            runAnotherOptAttempt <- FALSE # original, but I'm not sure
            # TODO do we need another attempt here?
            # Is it possible that convictions and weights will be consistent next time?
          }
        }

      }else{
        # There are some convictions and optimal weights with different signs
        # Invalid: Conviction and weights are not consistent
        validResult <- FALSE
        runAnotherOptAttempt <- FALSE # original, but I'm not sure
        # TODO do we need another attempt here?
        # Is it possible that convictions and weights will be consistent next time?
        # yes it is possible. Can occur if the tolerance level is too high.
        # so here tolerance options can be adjusted
      }
      }else{
      # opt_output$satus < 0
      # Algorithm has failed
      validResult <- FALSE
      # TODO do we need another attempt here?
      # Is it possible that status will be positive next time?

      if(opt_output$status == -4 || opt_output$status == -1 ){

        # set loop control variable
        runAnotherOptAttempt <- ifelse(attempt <= MaxAttempts, TRUE, FALSE)

        # Update required accuracy
        # TODO check if this is right variable to change. maybe  opts$tol.. is required instead
        set$TargetTE <- set$TargetTE + (stats::runif(1) - 0.5) * 10 ^ -6

        print(paste("The ", attempt - 1, ". try resulted in Error -4 or -1!",
                    ifelse(runAnotherOptAttempt,paste("Execute a ",attempt,". time with a slightly different TE-target of: ", set$TargetTE),
                           paste("No results found after attempt",attempt))))
      }else{
        # opt_output$status != -4,-1 but negative
        runAnotherOptAttempt <- FALSE
      }
    }

    attempt <- attempt + 1

    print(paste0("NLopt solver status: ", opt_output$status))
    print(paste0("NLopt solver status message: ", opt_output$message))

    if(validResult == FALSE){
      print("optimization failed or direction constrained violated or target function Inf ,invalid result. Solution reset to NA") #no stop as loop should continue!!!

      # initialize optimal solution (weights)
      rc <- rw <- pw <- rep(NA, length(rw))
      names(rw) <- colnames(cov)

      # store results in optim_details
      optim_details <- c(Calculated = as.character(Sys.time()),
                         NA,
                         NA,
                         NA,
                         NA,
                         NA,
                         opt_output$objective,
                         opt_output$iterations,
                         calctime,
                         NA,
                         validResult = validResult)

    }else{
      # Result is valid, new attempt is not required calculation:
      # get optimization results characteristics and store the results

      iterations <- opt_output$iterations
      # Prepare optimal relative weights with names
      rw <-  opt_output$solution
      names(rw) <- colnames(cov)

      # Absolute weights. The formula is correct, since lb = -bm
      pw <- rw - targ$lb    #portfolioweights. checks needed???
      orc <- OptimizationResultsCharacteristics(rw = rw,
                                                cov = cov,
                                                lb = targ$lb,
                                                ub = targ$ub,
                                                set = set,
                                                rb_target = targ$RiskBudget) #rw=rw;COV=COV;lb=targ$lb.;ub=targ$ub;set=set
      rc <- orc$rc

      # store results in optim_details
      optim_details <- c(Calculated = as.character(Sys.time()),
                         orc$te - set$TargetTE,
                         orc$const_hit_pct,
                         orc$NetInvestment,
                         orc$p.beta,
                         orc$IndexPosition,
                         opt_output$objective,
                         iterations,
                         calctime,
                         orc$total_rb_dev,
                         validResult = validResult)

      #add checks on result!!!
      TEdeviationTolerance <- 0.0001

      if(abs(orc$te - set$TargetTE) < TEdeviationTolerance)
      {
        cat(paste("The Risk-Contribution-Optimization was successful on attempt ",attempt-1), " and the resulting weights are:", sep="\n")
        print(data.frame(conviction = targ$conviction,
                         rw = rw,
                         RiskContr = rc))

      }else{ cat("The Risk-Contribution-Optimization yielded a result but the Deviation to Target-Tracking Error was:",orc$te - set$TargetTE, " and the resulting weights are:", sep="\n")
        print(data.frame(conviction = targ$conviction,
                         rw = rw,
                         RiskContr = rc))
      }
    }
  }

  return(list(rw = rw,
              pw = pw,
              optim_details = optim_details,
              RiskContribution = rc))
}

runRCOFromAPS <- function(portfolioName,
                          calculation_date,
                          covMaSetID,
                          RCOSetID,
                          isReal = FALSE,
                          calc_method){

  # Description
  #   The function executes the opritizer call with
  #   correct parameters and writes oprimized weights to DB

  # Dependencies:
  #   1. require("RODBC")
  #   2. library(nloptr)
  #   3. require("reshape")
  #   4. library(dplyr)

  # Functions:
  #   1. FAFunc.GetDB
  #   2. getTargetTable
  #   3. getRCOSetSettings
  #   4. f.getCovFromSQL
  #   5. f.runRCO
  #   6. f.writeRCOresToSQL
  #   7. get_rcb

  print("The process has been started")

  # Download [external] functions
  # source("G:/FAP/Equities/Betsizing/Code/RCO.R")
  # debug(f.runRCO)

  connection <- FAFunc.GetDB()

  # Get lower and upper bounds # tradability const
  target_table <- getTargetTable(portfolioName = portfolioName,
                                 calculation_date = calculation_date,
                                 isReal = isReal,
                                 connection = connection)

  if(nrow(target_table) > 0){
    print("GOOD: target table is NOT empty")
    print(target_table, row.names=TRUE, digits=5)
  }else{
    print("BAD: target table is empty")
  }

  # check if boundaries are compatible with the direction convictions: ADDED!
  # TODO simplify it using "all"


  # Weight and convictions must have the same sign.
  # Weights have lower and upper bounds
  # stopifnot(any( (target_table$lb>=0 & target_table$conviction<0) | (target_table$ub<=0 & target_table$conviction>0) )  == FALSE )
  # How to read: "any() == FALSE" means "There are no"
  if(any((target_table$lb >= 0 & target_table$conviction < 0) == FALSE) &&
     any((target_table$ub <= 0 & target_table$conviction > 0) == FALSE)){

    # Get RCO optimization settings
    RCO_settings_df <- getRCOSetSettings(RCOSetID = RCOSetID,
                                         connection = connection)
    # Close connection
    RODBC::odbcClose(connection)

    covMa <- f.getCovFromSQL(RunID = covMaSetID,
                             CalculationMethod = calc_method,
                             WideOrLong = "wide")

    # TODO check dimensions -> delete if not equal to expected value

    # The solver calculates ln(conviction*gewicht), so ln must not be 0
    # Find which stocks have a signal (not neutral)
    stocks_with_signal_ind <- target_table$conviction != 0
    # Take only with a signal
    target_table_signal <- target_table[stocks_with_signal_ind, ]

    # The function should calculate optimal weights and update optimal weights
    tryCatch(
      { # Try section

        # TODO first copy the function here, second use APS package
        optimization_result_l <- f.runRCO(targ = target_table_signal,
                                          set = RCO_settings_df,
                                          cov = covMa)

      }, error = function(e){
        print(paste0("runRCO stopped with an error: ", e))
        stop("NLopt solver status message: runRCO stopped with an error: ", e)
      }
    )

    # check if optimization returned a valid result
    if(isTRUE(as.logical(optimization_result_l$optim_details["validResult"]))){
      # The result is valid
      # prepare output vectors

      n_sec <- nrow(target_table)
      names_sec_v <- row.names(target_table)

      risk_contribution_v <- rep(0, times = n_sec)
      opt_weights_portfolio_v <- rep(0, times = n_sec)
      opt_weights_relative_v <- rep(0, times = n_sec)

      names(risk_contribution_v) <- names_sec_v
      names(opt_weights_portfolio_v) <- names_sec_v
      names(opt_weights_relative_v) <- names_sec_v

      # Optimal weights are updated in SQL table, the rest has deault 0 (BM) weight
      # Make sure that the order is the same
      stopifnot(is.null(names(optimization_result_l$rw)) == FALSE)
      stopifnot(is.null(names(opt_weights_relative_v[stocks_with_signal_ind])) == FALSE)
      stopifnot(names(optimization_result_l$rw) == names(opt_weights_relative_v[stocks_with_signal_ind]))
      opt_weights_relative_v[stocks_with_signal_ind] <- optimization_result_l$rw

      # Optimal absolute weights are (relative + bm); bm = -lb
      stopifnot(is.null(names(optimization_result_l$pw)) == FALSE)
      stopifnot(is.null(names(opt_weights_portfolio_v[stocks_with_signal_ind])) == FALSE)
      stopifnot(names(optimization_result_l$pw) == names(opt_weights_portfolio_v[stocks_with_signal_ind]))
      opt_weights_portfolio_v[stocks_with_signal_ind] <- optimization_result_l$pw

      # Calculation timestamp
      calcdatetime <- optimization_result_l$optim_details["Calculated"]

      # Get risk contribution
      stopifnot(is.null(names(optimization_result_l$RiskContribution)) == FALSE)
      stopifnot(is.null(names(risk_contribution_v[stocks_with_signal_ind])) == FALSE)
      stopifnot(names(optimization_result_l$RiskContribution) == names(risk_contribution_v[stocks_with_signal_ind]))
      risk_contribution_v[stocks_with_signal_ind] <- optimization_result_l$RiskContribution

      # create upload df
      covMaSetID <- as.integer(covMaSetID)
      RCOSetID <- as.integer(RCOSetID)

      print("Try to call f.writeRCOresToSQL from runRCOFromAPS")

      f.writeRCOresToSQL(opt_rel_weights_v = opt_weights_relative_v * 100,#sql-table stores values in Percentage
                         opt_abs_weights_v = opt_weights_portfolio_v * 100, #sql-table stores values in Percentage
                         risk_contribution_v = risk_contribution_v,
                         portfolio = portfolioName,
                         covID = covMaSetID,
                         setID = RCOSetID,
                         calcdatetime = calcdatetime)

    }else{
      print(paste0("NLopt solver status: ", -20))
      print("NLopt solver status message: runRCOFromAPS: no valid result found in optimization. Results have not been saved in DB")
      stop("NLopt solver status message: runRCOFromAPS: no valid result found in optimization. Results have not been saved in DB")
    }
  }else{
    print(paste0("NLopt solver status: ", -21))
    print("NLopt solver status message: runRCOFromAPS: Bounds and convictions are logically inconsistent")
    stop("NLopt solver status message: runRCOFromAPS: Bounds and convictions are logically inconsistent")
  }
}

# delete infeasible settings
f.delete_infeasible_settings <- function(all_possible_settings_df){

  if(class(all_possible_settings_df) == "data.frame"){

    # Sort data frame with settings
    required_col_names <- c("InputParameters", "cov_run_id", "LeverageTolerance",
                            "CashTolerance", "algo", "IndexFlex", "Ix_eq_Cash")

    if(all(required_col_names %in% colnames(all_possible_settings_df))){

      rows_sorted_ind <- with(all_possible_settings_df, order(InputParameters, cov_run_id))
      all_possible_settings_df <- all_possible_settings_df[rows_sorted_ind, ]

      # soft leverage constrained: depends on Cash- and LeverageTolerance, needed in Settings to filter out algos that do not work!
      all_possible_settings_df$SoftNetInvConstraint <- ifelse(all_possible_settings_df$LeverageTolerance != 0 |
                                                              all_possible_settings_df$CashTolerance != 0,
                                                              TRUE, FALSE)

      # Available algorithms
      algo_lagrange <- c("NLOPT_GD_AUGLAG_EQ", "NLOPT_LN_AUGLAG_EQ", "NLOPT_LD_AUGLAG_EQ")
      algo_gradient <- c("NLOPT_GD_AUGLAG_EQ","NLOPT_LD_AUGLAG_EQ","NLOPT_LD_SLSQP")

      # Soft leverage constrained does not work with Lagrangian optimizer (as Lagrangian uses an equality constraint!)
      # find infeasible combinations
      ind_to_delete <- which(all_possible_settings_df$algo %in% algo_lagrange &
                             all_possible_settings_df$SoftNetInvConstraint == TRUE)

      if (length(ind_to_delete) > 0){
        # it means that there is at least one infeasible combination
        # delete such rows from data frame

        all_possible_settings_df <- all_possible_settings_df[-ind_to_delete, ]
        n_del <- length(ind_to_delete)

        print(paste( "Lagrangian Optimization does not work with Soft Leverage Constraint: removed",
                     n_del,
                     "settings"))
      }else{
        # Nothing. This condition is satisfied
      }

      # Index equals Cash is only possible without IndexFlex and without Soft Leverage Constraint and needs a non-gradient algo
      ind_to_delete <- which((all_possible_settings_df$algo %in% algo_gradient |
                              all_possible_settings_df$SoftNetInvConstraint |
                              all_possible_settings_df$IndexFlex) & all_possible_settings_df$Ix_eq_Cash)

      if (length(ind_to_delete) > 0){

        all_possible_settings_df <- all_possible_settings_df[-ind_to_delete, ]
        n_del <- length(ind_to_delete)

        message(paste("Index equals Cash is only possible without IndexFlex and without Soft Leverage Constraint and needs a non-gradient algo : removed", n_del, "settings"))
      }else{
        # Nothing. This condition is satisfied
      }

      # the lambda must not be < 0.
      stopifnot(all(all_possible_settings_df$NetInvLambda >= 0))

      # NetInv-Lambda > 0 only makes sense with SoftNetInvConstraint == TRUE
      ind_to_delete <- which((all_possible_settings_df$NetInvLambda > 0 &
                              all_possible_settings_df$SoftNetInvConstraint == FALSE))

      if (length(ind_to_delete) > 0){

        all_possible_settings_df <- all_possible_settings_df[-ind_to_delete, ]
        n_del <- length(ind_to_delete)

        message(paste("NetInv-Lambda > 0 only makes sense with SoftNetInvConstraint. removed", n_del, "settings"))

      }else{
        # Nothing. This condition is satisfied
      }

      return(all_possible_settings_df)

    }else{
      print(paste0("NLopt solver status: ", -100))
      print("NLopt solver status message: f.delete_infeasible_settings: input data.frame does not have requested columns")
      stop("NLopt solver status message: f.delete_infeasible_settings: input data.frame does not have requested columns")
    }
  }else{
    print(paste0("NLopt solver status: ", -100))
    print("NLopt solver status message: f.delete_infeasible_settings: input must be a data.frame")
    stop("NLopt solver status message: f.delete_infeasible_settings: input must be a data.frame")
  }
}

# FindCovarianceMatrix in DB which is default for a selected portfolio
get_default_CovMaID <- function(Portfolioname){
  # input parameters validations
  stopifnot(class(Portfolioname) == "character")

  def_cov_id_query <- paste0("select max(CovMatrixRunID) as defCovMaID
                            from betsizing.CalculationDefaultSettings
                            where CustomPortfolioName = '", Portfolioname, "'")

  # Execute Query and return the default COVID
  con <- FAFunc.GetDB()

  def_RunID_df <- RODBC::sqlQuery(con, def_cov_id_query)

  if(class(def_RunID_df) == "data.frame"){
    if(nrow(def_RunID_df) > 0){
      # defCovMaID is guaranteed by the query
      cov_id <- as.numeric(def_RunID_df$defCovMaID)
      return(cov_id)
    }else{
      print(paste0("NLopt solver status: ", -22))
      print("NLopt solver status message: get_default_CovMaID: No Covariance matrix as specified ",
           "was calculated yet for the given portfolio")
      stop("NLopt solver status message: get_default_CovMaID: No Covariance matrix as specified ",
           "was calculated yet for the given portfolio")
    }
  }else{
    print(paste0("NLopt solver status: ", -100))
    print("NLopt solver status message: get_default_CovMaID: query failed ")
    stop("NLopt solver status message: get_default_CovMaID: query failed ")
  }
}

# Output optdetails to xlsx
f.writeOptdetails2xlsx <- function(RCOres_l,
                                   Portfolio,
                                   Calcdate="",
                                   ShortIndexWithOptimCash=0,
                                   ZeroNetInvestment = FALSE,
                                   OnlyID_v = NULL){

  stopifnot(class(RCOres_l) == "list")
  names_RCOres_v <- c("settings_df", "details_ma", "rw_ma", "targ_3D_array", "cov_3D_array")
  stopifnot(all(names(RCOres_l) %in% names_RCOres_v))
  stopifnot(class(RCOres_l$settings_df) == "data.frame")
  stopifnot(class(RCOres_l$details_ma) == "matrix")
  stopifnot(class(RCOres_l$rw_ma) == "matrix")
  stopifnot(class(RCOres_l$targ_3D_array) == "array")
  stopifnot(class(RCOres_l$cov_3D_array) == "array")
  stopifnot("setID" %in% colnames(RCOres_l$settings_df))

  settings_df <- RCOres_l$settings_df
  optres_ma <- rbind(RCOres_l$details_ma, RCOres_l$rw_ma)
  targ_3D_array <- RCOres_l$targ_3D_array

  if(is.null(OnlyID_v) == FALSE)
  {
    stopifnot(class(OnlyID_v) == "numeric")
    # Some particular settings are required
    existing_set_ind <- which(OnlyID_v %in% settings_df$setID)

    if(length(existing_set_ind) > 0){

      if(length(existing_set_ind) != length(OnlyID_v)){
        warning("f.writeOptdetails2xlsx: some requested IDs do not exist")
        OnlyID_v <- OnlyID_v[existing_set_ind]
      }else{
        # Nothing. Take all
      }

      print(paste("output only id", OnlyID_v))
      # Take only required rows of settings and all columns
      settings_df <- settings_df[settings_df$setID == OnlyID_v, , drop = FALSE]
      # Take only required optimization results
      optres_ma <- optres_ma[, colnames(optres_ma) == paste("set", OnlyID_v, sep = ""), drop = FALSE]

    }else{
      print(paste0("NLopt solver status: ", -23))
      print("NLopt solver status message: f.writeOptdetails2xlsx: Requested IDs do not exist")
      stop("NLopt solver status message: f.writeOptdetails2xlsx: Requested IDs do not exist")
    }
  }

  t_settings_df <- t(settings_df)
  rownames(t_settings_df) <- paste("set",colnames(settings_df))
  colnames(t_settings_df) <- colnames(optres_ma)

  sec_nr <- length(targ_3D_array[,1,1])

  # implement Index Position in output if desired so
  if(ShortIndexWithOptimCash == 1)
  {
    l <- nrow(optres_ma)
    ix_sec <- (l - sec_nr + 1):l
    w_bm <- -targ_3D_array[-1, "lb", 1]
    for (i in 1:ncol(optres_ma))
    {
      optres_ma[ix_sec,i] <- ImplIndexPosition(as.numeric(optres_ma[ix_sec,i]),w_bm,ZeroNetInvestment)
    }
  }

  pr.optres_ma <- rbind(as.matrix(t_settings_df), as.matrix(optres_ma))

  rownames(pr.optres_ma) <- c(rownames(pr.optres_ma)[1:(nrow(pr.optres_ma)-sec_nr)],names(targ_3D_array[,1,1]))

  file_path_out <- paste("G:/FAP/Equities/Betsizing/R_results/",Portfolio,"/",sep="",collapse="")

  file_path_name <- paste(file_path_out,Portfolio,"_",Calcdate,"_optdetails.xlsx",sep="",collapse="")

  xlsx::write.xlsx(x = pr.optres_ma,
                   file = file_path_name,
                   sheetName="R.output")

  #example
  #RCOres <- RCOres; Portfolio <- "RE_EU";Calcdate="";ShortIndexWithOptimCash=0;ZeroNetInvestment=FALSE;OnlyID=FALSE
  #f.writeOptdetails2xlsx(RCOres,Portfolio,Calcdate,ShortIndexWithOptimCash,ZeroNetInvestment,OnlyID)
}




runRCOLoops <- function(Portfolio,                 #character string of the portfolio. used to identify the targ-file to load
                        TargetTE,                            #Target Tracking error as numeric or numeric vector (to vary through settings)
                        MaxRelWeight = 0.1,                 #Maximum relative Weigth (under- or over-)
                        Trials = 10000,                      #Limit of Trials for the optimizer. as numeric or nusec_nrmeric vector (to vary through settings)
                        algo = "NLOPT_LD_SLSQP",         #algo that nloptr uses as character or character vector  (to vary through settings):  "NLOPT_GN_ISRES" non gradient; "NLOPT_LD_SLSQP" with Gradient, FAILED: "NLOPT_GD_STOGO","NLOPT_GD_STOGO_RAND"
                        CashTolerance = 0.0025,              #Boundary for the Cash held (conditon is: -sum(w)>CashTolerance)
                        LeverageTolerance = 0.0025,          #Boundary for the Leverage held (condition is sum(w)<LeverageTolearnce)
                        CovCalcWay = "nlshrink",               #Covariance calculation setting for SQL-Covariance. #c("nlshrink","cov")
                        CovReturns = "abs",                    #Covarniance calculation setting for SQL-Covariance. #c("abs","rel")
                        ShortIndexWithOptimCash = 0,           #set 0 or 1 to implement the optimized Cash position by shorting the ind   ex via selling proportional weights in each index-member
                        InputParameters = "Signals4R_test",         #character string like fitting the variable input in the specified targ-sheet
                        cov_run_id_v = NA,                        #set specific covariance ID, -10 for testing and NA for most recent available for the chosen portfolio
                        xtol_rel = 10 ^ -7,
                        IndexFlex = FALSE,
                        Ix_eq_Cash = FALSE,
                        NetInvLambda = 0                        #punishment factor for NetInvestments <>0
                        #,FixedCashRisk = 0                     #fixed cash risk in the portfolio that needs to be taken into account!
                        , LowConvictionExitInDays = 10
                        , ConvictionGroups = 3

                        #,convictionInputFrom = c("xls","simts") #new parameter
){

  # Description:
  # This function executes several loops of optimization
  # to check how robust is the result

  # Dependencies:
  # dplyr
  # reshape
  # gridExtra
  # xlsx

  # Functions called:
  # gridExtra::expand.grid
  # f.delete_infeasible_settings
  # FAFunc.GetDB
  # xlsx::read.xlsx
  # get_default_CovMaID
  # f.getCovFromSQL
  # f.matchCOVtoTickers
  # f.runRCO
  # f.writeOptdetails2xlsx
  # read_target_settings_from_excel
  # get_cov_ma

  # Internal(private) functions
  read_target_settings_from_excel <- function(file_path_in, input_file, ws_name){

    # get risk budget targets, lower bounds for each ticker,
    targ_col_names_v <- c("Ticker","lb","ub","conviction","RiskBudget","tradeability")

    # Read Excel worksheet to get lb, ub, convictions, tradability...
    tryCatch({
      # Use 6 required columns (to avoid NA column)
      target_settings_df <- xlsx::read.xlsx(file = paste(file_path_in, input_file, sep = "", collapse = ""),
                                            sheetName = ws_name, colIndex = c(1,2,3,4,5,6))
    },
    error = function(e){
      print(e)
      stop(e)
    })

    if(class(target_settings_df) == "data.frame" &&
       nrow(target_settings_df) > 0){

      if(all(colnames(target_settings_df) %in% targ_col_names_v)){

        # Check that there are no duplicates
        if(length(unique(target_settings_df$Ticker)) == length(target_settings_df$Ticker)){

          # Weight and convictions must have the same sign.
          # Weights have lower and upper bounds
          # stopifnot(any( (target_table$lb>=0 & target_table$conviction<0) | (target_table$ub<=0 & target_table$conviction>0) )  == FALSE )
          # How to read: "any() == FALSE" means "There are no"
          if(any((target_settings_df$lb >= 0 & target_settings_df$conviction < 0) == FALSE) &&
             any((target_settings_df$ub <= 0 & target_settings_df$conviction > 0) == FALSE)){

            target_settings_df <- target_settings_df[!is.na(target_settings_df$Ticker),]
            rownames(target_settings_df) <- target_settings_df$Ticker
            # We don't need tickers
            required_col_ind <- which(!colnames(target_settings_df) %in% "Ticker")
            target_settings_df <- target_settings_df[, required_col_ind]
            # remove possible NA risk budgets
            required_row_ind <- which(is.na(target_settings_df[, "RiskBudget"]) == FALSE)
            target_settings_df <- target_settings_df[required_row_ind, ]

            return(target_settings_df)
          }
          else{
            print("NLopt solver status message: runRCOLoops: Bounds and conviction are inconsistent")
            stop("NLopt solver status message: runRCOLoops: Bounds and conviction are inconsistent")
          }
        }else{
          print("NLopt solver status message: download_target_settings: target settings contains duplicated tickers")
          stop("NLopt solver status message: download_target_settings: target settings contains duplicated tickers")
        }
      }else{
        print("NLopt solver status message: download_target_settings: target settings file structure is wrong")
        stop("NLopt solver status message: download_target_settings: target settings file structure is wrong")
      }
    }else{
      print("NLopt solver status message: download_target_settings: File is empty")
      stop("NLopt solver status message: download_target_settings: File is empty")
    }
  }

  get_cov_ma <- function(cov_id = NA,
                         portfolio,
                         cov_ret,
                         calc_method,
                         sec_names){

    # Check input
    stopifnot(is.character(portfolio) && length(portfolio) == 1)
    stopifnot(is.character(cov_ret) && length(cov_ret) == 1)
    stopifnot(is.character(calc_method) && length(calc_method) == 1)
    stopifnot(is.character(sec_names) && length(sec_names) > 0)
    stopifnot(length(cov_id) == 1)

    if(is.na(cov_id)){
      # If CovMa is not specified download the latest one
      CovID <- get_default_CovMaID(Portfolioname = portfolio)

      if(is.na(CovID)){
        print("NLopt solver status message: get_cov_ma: There are no default covariance matrxi for ", portfolio)
        stop("NLopt solver status message: get_cov_ma: There are no default covariance matrxi for ", portfolio)
      }else{
        # Nothing it is ok
      }
    }else{
      # If secified use it
      CovID <- cov_id
    }

    # Get covariance matrix
    cov <- f.getCovFromSQL(RunID = CovID, CalculationMethod = calc_method, WideOrLong = "wide")

    # Make sure that tickers order is consistent
    cov <- f.matchCOVtoTickers(Tickers = sec_names, cov_df = as.data.frame(cov))

    cov_set_ID <- paste(calc_method, cov_ret, CovID, sep = "_")
    cov_ma <- as.matrix(cov)

    return(list(CovID = CovID, cov_ma = cov_ma, cov_set_ID = cov_set_ID))
  }

  print("starting...")
  #checks
  stopifnot(length(ShortIndexWithOptimCash)==1)

  # define file pathes (add switch to specify exact location for each portfolio)
  if(Portfolio == "RE_EU"){

    file_path_in <- "G:/FAP/Immobilien_Modell/"
    input_file <- "Betsizing.xlsx"

  }else{

    file_path_in <- "G:/FAP/Equities/Betsizing/"
    input_file <- paste("bet_targets_",Portfolio,".xlsx",sep="",collapse="")     #"bet_targets_epra.csv"              #"bet_targets_epra.csv"
  }

  file_path_out <- paste("G:/FAP/Equities/Betsizing/R_results/",
                         Portfolio, "/", sep = "", collapse = "")

  # CovId can be NA this means default covariance matrix should be used
  def_cov_run_id_ind <- which(is.na(cov_run_id_v))

  if(length(def_cov_run_id_ind) > 0){

    # If CovMa is not specified use the default one
    cov_run_id_v[def_cov_run_id_ind] <- get_default_CovMaID(Portfolioname = Portfolio)

    if(any(is.na(cov_run_id_v[def_cov_run_id_ind]))){
      print("runRCOLoops: There are no default covariance matrxi for ", Portfolio)
      stop("runRCOLoops: There are no default covariance matrxi for ", Portfolio)
    }else{
      # Nothing it is ok
    }
  }else{
    # Nothing. Use which is provided
  }

  # Create Settings matrix
  # expand.grid creates a data frame with all possible combinaiton of settings
  # Some of combinations are not valid
  all_possible_settings_df <- expand.grid(
    TargetTE = TargetTE,
    MaxRelWeight = MaxRelWeight,
    Trials = Trials,
    xtol_rel = xtol_rel,
    algo = algo,
    CashTolerance = CashTolerance,
    LeverageTolerance = LeverageTolerance,
    CovCalcWay = CovCalcWay,
    CovReturns = CovReturns,
    InputParameters = InputParameters,
    ShortIndexWithOptimCash = ShortIndexWithOptimCash,
    cov_run_id = cov_run_id_v,
    IndexFlex = IndexFlex,
    Ix_eq_Cash = Ix_eq_Cash,
    NetInvLambda = NetInvLambda,
    LowConvictionExitInDays = LowConvictionExitInDays,
    ConvictionGroups = ConvictionGroups,
    stringsAsFactors = FALSE)

  # Only feasible settings should be used
  valid_settings_df <- f.delete_infeasible_settings(all_possible_settings_df)

  # START LOOP
  valid_settings_number <- nrow(valid_settings_df)
  # Add settings ID column
  valid_settings_df$setID <- 1:valid_settings_number

  # Get DB connection
  con <- FAFunc.GetDB()

  # Note 1: Based on code and output data structures number of securities MUST be the save,
  #         otherwise one cannot store 21 (for example) optimal weights in a matrix with 20 rows
  # Note 2: target settings (lb, ub ...) do not change each iteration.
  #         These values can only differ in different settings Excel worksheets

  # Define structures which are used in the loop.
  # They should not be local in loop, however in R (and only in R)
  # it is not mandatory, it would still work

  optim_details_names_v <- c("Calculated","TEact-TEtarg","%weight_limits_hit",
                                "NetInvestment","PortBeta","IndexPosition",
                                "target_function_value","iterations","calctime",
                                "total_rb_dev","validResult")

  # Define variables just to show which are common for each iteration
  sec_nr <- NULL
  sec_names_v <- NULL
  target_settings_df <- NULL
  opt_rel_weights_ma <- NULL
  optim_details_ma <- NULL
  target_settings_ma <- NULL
  targ_3D_array <- NULL
  cov_3D_array <- NULL
  next_targ_ind <- 1
  next_cov_ind <- 1

  # For each optimization settings set
  for(k in 1:valid_settings_number){

    print(paste(Sys.time()," started loop  ",k, " of ",valid_settings_number,sep=""))

    # Take signle settings set
    set_df <- valid_settings_df[k, ]
    print(set_df)

    current_targ_settings_set <- as.character(set_df$InputParameters)
    current_cov_set_id <- paste(set_df$CovCalcWay, set_df$CovReturns, set_df$cov_run_id, sep = "_")
    print(paste("For the loop",k,"the COV with RunID", set_df$cov_run_id, "is used"))

    # In the first iteration define output structures
    if(k == 1){

      # settings will be saved in target_3D_array
      target_settings_df <- read_target_settings_from_excel(file_path_in = file_path_in,
                                                            input_file = input_file,
                                                            ws_name = current_targ_settings_set)
      sec_names_v <- row.names(target_settings_df)
      sec_nr <- length(sec_names_v)

      # Optimal weights stored as a column. Column name is settings ID
      opt_rel_weights_ma <- matrix(nrow = sec_nr, ncol = valid_settings_number)
      rownames(opt_rel_weights_ma) <- sec_names_v
      colnames(opt_rel_weights_ma) <- paste("set", valid_settings_df$setID, sep="")

      # For each loop we save optimization details in a matrix as a columns
      optim_details_ma <- matrix(nrow = length(optim_details_names_v),
                                 ncol = valid_settings_number)

      rownames(optim_details_ma) <- optim_details_names_v
      colnames(optim_details_ma) <- paste("set", valid_settings_df$setID, sep = "")

      # 3D array to store the different input tables. InputParameters can be a vector
      # note: targ_tab_name is a name in Excel worksheet for a target table to use
      targ_3D_array <- array(dim = c(dim(target_settings_df), length(InputParameters)),
                             dimnames = list(sec_names = sec_names_v,
                                             targ_col_names = colnames(target_settings_df),
                                             targ_tab_names = rep(NA,length(InputParameters))))
      # Save target settings
      target_settings_ma <- as.matrix(target_settings_df)
      targ_3D_array[,,next_targ_ind] <- target_settings_ma
      # Make sure that the name and the matrix are consistent
      dimnames(targ_3D_array)$targ_tab_names[next_targ_ind] <- current_targ_settings_set
      next_targ_ind <- next_targ_ind + 1

      different_covsettings_df <- unique(valid_settings_df[, c("CovCalcWay", "CovReturns", "cov_run_id")])

      # 3D array to store CovMa for each set of settings
      cov_3D_array <- array(dim = c(sec_nr, sec_nr, nrow(different_covsettings_df)),
                   dimnames = list(cov_row_names = sec_names_v,
                                   cov_col_names = sec_names_v,
                                   CovsetID = rep(NA,nrow(different_covsettings_df))))

      # Get covariance matrix for the current iteration
      cov_res_list <- get_cov_ma(cov_id = set_df$cov_run_id,
                                 portfolio = Portfolio,
                                 cov_ret = set_df$CovReturns,
                                 calc_method = set_df$CovCalcWay,
                                 sec_names = sec_names_v)

      # Save covariance matrix for the current settings set
      cov_ma <- cov_res_list$cov_ma
      cov_3D_array[,,next_cov_ind] <- cov_ma
      # Make sure that the name and the matrix are consistent
      dimnames(cov_3D_array)$CovsetID[next_cov_ind] <- current_cov_set_id
      next_cov_ind <- next_cov_ind + 1

    }else{
      # k != 1. Not the first iteration

      # get the vector of downloaded target settings sets
      downloaded_targ_settings_sets_v <- dimnames(targ_3D_array)$targ_tab_names

      ind_targ <- which(current_targ_settings_set %in%  downloaded_targ_settings_sets_v)

      if(length(ind_targ) > 0 ){
        # Use existing
        target_settings_ma <- targ_3D_array[,,ind_targ]
      }else{
        # download new target settings
        target_settings_df <- read_target_settings_from_excel(file_path_in = file_path_in,
                                                              input_file = input_file,
                                                              ws_name = current_targ_settings_set)
        # Save target settings
        # TODO check df possibility
        target_settings_ma <- as.matrix(target_settings_df)
        targ_3D_array[,,next_targ_ind] <- target_settings_ma
        # Make sure that the name and the matrix are consistent
        dimnames(targ_3D_array)$targ_tab_names[next_targ_ind] <- current_targ_settings_set
        next_targ_ind <- next_targ_ind + 1
      }

      # get the vector of downloaded cov ma settings ID
      downloaded_cov_ma_settings_sets <- dimnames(cov_3D_array)$CovsetID

      ind_cov <- which(current_cov_set_id %in% downloaded_cov_ma_settings_sets)

      if(length(ind_cov) > 0 ){
        cov_ma <- cov_3D_array[,,ind_cov]
      }else{
        # download new covariance matrix
        cov_res_list <- get_cov_ma(cov_id = set_df$cov_run_id,
                                   portfolio = Portfolio,
                                   cov_ret = set_df$CovReturns,
                                   calc_method = set_df$CovCalcWay,
                                   sec_names = sec_names_v)

        # Save covariance matrix for the current settings set
        cov_ma <- cov_res_list$cov_ma
        cov_3D_array[,,next_cov_ind] <- cov_ma
        # Make sure that the name and the matrix are consistent
        dimnames(cov_3D_array)$CovsetID[next_cov_ind] <- current_cov_set_id
        next_cov_ind <- next_cov_ind + 1
      }
    }


    # Optimization
    # reduce to active portfolio (with convictions != 0)
    # and seperate TickerInfo from its covariances
    # (necessary as target function cant handle 0-weights as it calculates ......*log(w)*....)
    sec_active <- target_settings_df$conviction != 0
    targ_active <- target_settings_df[sec_active,]
    cov_active <- f.matchCOVtoTickers (Tickers = row.names(targ_active), cov_df = as.data.frame(cov_ma))

    RCOthisLoop <- f.runRCO(targ_active, set_df, cov_active)

    # store the results in Loop-Matrices (non-valid results indicated with NA)
    opt_rel_weights_ma[sec_active, k] <- RCOthisLoop$rw
    opt_rel_weights_ma[!sec_active, k] <- 0
    optim_details_ma[,k] <- RCOthisLoop$optim_details

    # intermediary save output file to not loose everything if a later loop gets interrupted
    RCOres_l <- list(settings_df = valid_settings_df,
                     details_ma = optim_details_ma,
                     rw_ma = opt_rel_weights_ma,
                     targ_3D_array = targ_3D_array,
                     cov_3D_array = cov_3D_array,
                     RiskContribution = RCOthisLoop$RiskContribution)

    output_file_name <- paste(file_path_out, Sys.Date(), " RCOresPAA.R", sep = "")
    save(RCOres_l, file = output_file_name)
  }
  print(paste("all", k ,"loops completed. Storing of results in process"))

  # OUTPUT RESULTS
  RCOres_l <- list(settings_df = valid_settings_df,
                   details_ma = optim_details_ma,
                   rw_ma = opt_rel_weights_ma,
                   targ_3D_array = targ_3D_array,
                   cov_3D_array = cov_3D_array)

  f.writeOptdetails2xlsx(RCOres_l = RCOres_l,
                         Portfolio,
                         Sys.Date(),
                         valid_settings_df$ShortIndexWithOptimCash[1])
  return(RCOres_l)
}

compareRCOruns <- function(RCOres_l,
                           xaxis,
                           yaxis,
                           group_by,
                           current_point = 1){

  # Dependency:
  #  library(ggplot2)

  # Description:
  #   RCOres_l contains optimization results for different settings
  #   this function compares the results to find out how robust is the optimization
  #   This function is not supposed to add additional variants of settings here, just compare
  #   the results from RCOres_l

  # Parameters:
  #   RCOres_l: a list with optimization results
  #   xaxis = "TargetTE"; yaxis="total_rb_dev" means
  #      plot total risk budget deviation depending on Target tracking error.
  #   group_by="setID"
  #   current_point: which point should be bigger than the others,


  # input validations
  stopifnot(is.null(RCOres_l) == FALSE)
  stopifnot(is.null(xaxis) == FALSE)
  stopifnot(is.null(yaxis) == FALSE)
  stopifnot(is.null(group_by) == FALSE)
  stopifnot(class(RCOres_l) == "list")
  stopifnot(length(names(RCOres_l)) > 0)
  required_names <- c("settings_df", "details_ma")
  stopifnot(all(required_names %in% names(RCOres_l)))


  # RCOres_l$details_ma and RCOres_l$settings_df contain optimization details and settings
  # for each feasible optimization settings set.
  # Unite all columns to let later group by a column
  opt_res_df <- cbind(t(RCOres_l$details_ma), RCOres_l$settings_df)

  available_names <- colnames(opt_res_df)

  if(xaxis %in% available_names &&
     yaxis %in% available_names &&
     group_by %in% available_names){

    # set points size
    sz <- rep(3, nrow(opt_res_df))
    sz[current_point] = 10

    opt_res_df[,xaxis] <- as.factor(opt_res_df[,xaxis])
    opt_res_df[,yaxis] <- as.factor(opt_res_df[,yaxis])
    opt_res_df[,group_by] <- as.factor(opt_res_df[,group_by])

    if(yaxis == "TEact-TEtarg"){
      cnames <- colnames(opt_res_df)
      ind <- which(cnames == yaxis)
      cnames[ind] <- "TEactMtarg"
      colnames(opt_res_df) <- cnames
      yaxis <- cnames[ind]

    }else{}#Nothing

    # Plot
    ggplot2::ggplot(data = opt_res_df,
                    ggplot2::aes_string(y = yaxis,
                                        x = xaxis, #jitter(xaxis, factor=Jitter),
                                        color = group_by,
                                        group = group_by,
                                        fill = group_by)) +
      ggplot2::geom_line(size = 0.5) +
      ggplot2::geom_point(size = sz, shape = pmin(sz + 18, 23)) +
      ggplot2::theme(legend.position = "right", legend.direction = "vertical") +
      ggplot2::labs(x = xaxis, y = yaxis)

  }else{
    print("compareRCOruns: xaxis, yaxis or group_by values is(are) not valid")
    stop("compareRCOruns: xaxis, yaxis or group_by values is(are) not valid")
  }
}

# wrapper to plot call checkRCOplots from optimization output
plotSingleSetIDfromRCOres <- function(setid, RCOres_l, plot_type)
{
  # input validations
  stopifnot(class(RCOres_l) == "list")
  stopifnot(length(names(RCOres_l)) > 0)
  RCOres_l_names <- c("settings_df", "details_ma", "rw_ma", "targ_3D_array", "cov_3D_array", "RiskContribution")
  stopifnot(all(names(RCOres_l) %in% RCOres_l_names))
  stopifnot(length(setid) == 1)

  # get info from RCOres_l (output of the RCO function)
  # RCOres_l$setting_df: each ROW corresponds to an iteration settings
  single_set_df <- RCOres_l$settings_df[RCOres_l$settings_df$setID == setid, , drop = FALSE]
  # details are stored by column
  single_opt_det_v <- RCOres_l$details_ma[,setid]
  risk_contributions_v <- RCOres_l$RiskContribution
  # Get optimal relative weights for required settings set (stored in columns)
  rw_v <- RCOres_l$rw_ma[,setid]

  covID <- paste(single_set_df[c("CovCalcWay","CovReturns","cov_run_id")],collapse="_")
  cov_ma <- RCOres_l$cov_3D_array[,,covID]

  # make sure the order of tickers is the same. Must be
  stopifnot(all(names(rw_v) == colnames(cov_ma)) == TRUE)

  # get target table
  targ_input_df <- as.data.frame(RCOres_l$targ_3D_array[,,single_set_df$InputParameters])

  # Structure validation
  names_v <- c("lb", "ub", "conviction", "tradeability", "RiskBudget")
  stopifnot(all(names_v %in% colnames(targ_input_df)) == TRUE)

  names_v <- c("MaxRelWeight", "LowConvictionExitInDays", "ConvictionGroups")
  stopifnot(all(names_v %in% colnames(single_set_df)) == TRUE)

  # call plot function
  checkRCOplots(rw_v = rw_v,
                cov_df = cov_ma,
                targ_input_df = targ_input_df,
                single_set_df = single_set_df,
                single_opt_det_v = single_opt_det_v,
                risk_contributions_v = risk_contributions_v,
                plot_type = plot_type)
}

#short tickers for printing in plots
.pT <- function(x, start = 0, stop = 4){
  sapply(x,substr,start,stop)
}


checkRCOplots <- function(rw_v,
                          cov_df,
                          targ_input_df,
                          single_set_df,
                          single_opt_det_v,
                          risk_contributions_v,
                          ShortIndexWithOptimCash = 0,
                          plot_type){

  #reduce to active securities again for plotting
  get_active_sec <- function(v, convictions_v){
    if((class(v) == "numeric" || class(v) == "character") &&
       class(convictions_v) == "numeric"){

      return(v[convictions_v != 0])
    }else{
      print("get_active_sec: input has wrong type")
      stop("get_active_sec: input has wrong type")
    }
  }

  # recaclulate the effectively set boundaries
  w_range <- f.getWeightRange(targ_input_df$lb,
                              targ_input_df$ub,
                              targ_input_df$conviction,
                              targ_input_df$tradeability,
                              single_set_df$MaxRelWeight,
                              single_set_df$LowConvictionExitInDays,
                              single_set_df$ConvictionGroups)


  tickers_v <- rownames(targ_input_df)
  convictions_v <-  targ_input_df$conviction
  lb_v <- w_range$w_min
  ub_v <- w_range$w_max
  rb_v <- targ_input_df$RiskBudget

  #adjust lower bounds to reflect implementable lbs!
  lb_v[-1] <- (1+rw_v[1])*lb_v[-1]

  lb_act_v <- get_active_sec(lb_v, convictions_v)
  ub_act_v <- get_active_sec(ub_v, convictions_v)
  rw_act_v <- get_active_sec(rw_v, convictions_v)
  rb_act_v <- get_active_sec(rb_v, convictions_v)
  tickers_act_v <- get_active_sec(tickers_v, convictions_v)
  convictions_act_v <- convictions_v[convictions_v != 0]
  rc_act_v <- risk_contributions_v

  all <- c(rb_act_v, rc_act_v)
  range <- c(min(all), max(all))
  # Add 10% to let text be inside a plot
  range <- range * 1.1

  txtsize <- ifelse(length(rc_act_v) > 40, 0.8, 1)

  if(any(is.na(rw_v))){
    # This means that optimization has failed
    print("checkRCOplots: Optimization failed")

    graphics::plot(1,1,main = "Optimization failed")

  }else{
    # Optimization succeeded
    if(plot_type == "act_rel_weights"){

      plot_active_relative_weights <- function(rw_act_v,
                                               tickers_act_v,
                                               txtsize,
                                               lb_act_v,
                                               ub_act_v,
                                               convictions_act_v){

        title_part1 <- paste0("Settings set ",single_set_df$setID,": ")
        # calculate active weights share
        total_ind_dev <- sum(abs(rw_act_v))
        title_part2 <- paste0("Sum of deviations from BM weights is ", 100*round(total_ind_dev,2)," %")

        # Check if a constraint is active
        boundary_hit_lb <- round(lb_act_v,3) == round(rw_act_v,3)
        boundary_hit_ub <- round(ub_act_v,3) == round(rw_act_v,3)

        # If lower or uppder bound is achieved set flag = TRUE
        boundary_hit <- boundary_hit_lb | boundary_hit_ub

        # create range for scales
        range <- c(min(rw_act_v), max(rw_act_v))
        range[1] <- range[1] * 1.2
        range[2] <- range[2] * 1.6

        # Bar Plot: Share of active weights
        bp.rw <- graphics::barplot(rw_act_v,
                                   beside = TRUE,
                                   names.arg = .pT(tickers_act_v),
                                   las = 2,
                                   cex.names = txtsize,
                                   main = paste0(title_part1, title_part2),
                                   col = ifelse(boundary_hit == TRUE, "red", "black"),
                                   xlab = "Tickers",
                                   ylab = "Relative Weights",
                                   ylim = range)

        # Show also lower and upper bounds to make sure that optimization is valid
        graphics::points(x = bp.rw, y = lb_act_v, pch = 20, col = "blue")
        graphics::points(x = bp.rw, y = ub_act_v, pch = 20, col = "blue")

        # Add convictions: to make sure the direction and the amplitude is feasible
        shift <- 0.0015 * sign(rw_act_v)
        graphics::text(x = bp.rw,
                       y = shift + rw_act_v,
                       labels = round(convictions_act_v,0),
                       cex = txtsize,
                       col = "darkgreen")

        # legend: bar plot share of active weights
        graphics::legend("topleft",
                         c("unrestricted", "restricted", "boundary", "convictions"),
                         col = c("black", "red", "blue", "darkgreen"),
                         pch = c(15,15,20,17),
                         bty = "n",
                         cex = 1)

      }

      plot_active_relative_weights(rw_act_v = rw_act_v,
                                   tickers_act_v = tickers_act_v,
                                   txtsize = txtsize,
                                   lb_act_v = lb_act_v,
                                   ub_act_v = ub_act_v,
                                   convictions_act_v = convictions_act_v)


    }else if(plot_type == "risk_contr"){

      plot_risk_contributions <- function(rc_act_v,
                                          rb_act_v,
                                          tickers_act_v,
                                          txtsize,
                                          total_rb_dev){

        # Convert to percentage
        rc_act_v <- rc_act_v * 100
        rb_act_v <- rb_act_v * 100
        title_part1 <- paste0("Settings set ",single_set_df$setID,": ")
        title_part2 <- paste0("Sum of deviations from target RC = ",total_rb_dev, ". sum(RC) = ", sum(rc_act_v), "%")

        # Check if risk budget is violated
        rb_hits <- abs(round(rc_act_v,3)) >= abs(round(rb_act_v,3))

        # get max and min values of RB and RC to create correct range for a plot
        r_min <- min(c(rc_act_v, rb_act_v))
        r_max <- max(c(rc_act_v, rb_act_v))
        range <- c(r_min, r_max)
        # Add a bit more space for text and legend
        range[1] <- range[1] * 1.2
        range[2] <- range[2] * 1.4

        # Risk Contributions Bar plot
        # rc_act_v - risk contributions in percentage from total risk
        x <- graphics::barplot(rc_act_v,
                               ylim = range,
                               ylab = "risk contribution in %",
                               beside = TRUE,
                               names.arg = .pT(tickers_act_v),
                               las = 2,
                               col = ifelse(rb_hits == TRUE, "red", "black"),
                               cex.names = txtsize,
                               main = paste0(title_part1, title_part2))

        # Add risk budget targets
        # rb_act_v -  target risk budget in %
        graphics::points(x = x, y = rb_act_v, pch = 20, col = "blue")

        # Add utilization percentage
        rb_utilization <- round(((rc_act_v / rb_act_v) * 100), 0)
        rb_utilization <- rb_utilization
        shift <- 0.5 * sign(rc_act_v)
        graphics::text(x,
                       shift + rc_act_v,
                       labels = rb_utilization,
                       cex = txtsize,
                       col = "darkgreen")

        # legend: bar plot share of active weights
        graphics::legend("topleft",
                         c("unrestricted", "restricted", "target", "utilization(%)"),
                         col = c("black", "red", "blue", "darkgreen"),
                         pch = c(15,15,20,17),
                         bty = "n",
                         cex = 1)
      }

      plot_risk_contributions(rc_act_v,
                              rb_act_v,
                              tickers_act_v,
                              txtsize,
                              single_opt_det_v["total_rb_dev"])



    }else{
      # plot type is not correct
      print("checkRCOplots: plot type is not supported")
      title_part1 <- paste0("Settings set ",single_set_df$setID,": ")
      title_part2 <- "Check plot type"
      graphics::plot(1,1, main = paste0(title_part1, title_part2))
    }
  }
}

#distance function
mydistfun <- function(test, benchmark)
{
  message("the euclidean norm is used for all values, only for numeric inputs meaningful outputs result")
  #ensure is numeric
  #x
  if(class(test[1,1])!="numeric"){
    if(class(test[1,1])=="factor"){
      message("factors are transformed to numeric. Are results still meaningful?")

      test[,2] <- sapply(test[,1],f.as.numeric.factor)

    }else {
      warning("No procedure for the distance measuring for the class of one axis available -> cant find clicked at point!!!")
    }
  }
  #y
  if(class(test[1,2])!="numeric"){
    if(class(test[1,2])=="factor"){
      message("factors are transformed to numeric. Are results still meaningful?")

      test[,2] <- sapply(test[,2],f.as.numeric.factor)

    }else {warning("No procedure for the distance measuring for the class of one axis available -> cant find clicked at point!!!")}
  }
  #calc distance
  dist <- (test[,1]-benchmark[1])^2 + (test[,2]-benchmark[2])^2

  #more sophisticated solution for distances:
  #ipak("cluster")
  #dist <- cluster::: daisy(rbind(click,values))
}


draw_table <- function(setid, RCOres_l){

  # Description:
  # The function creates a table with optimization details
  # This table is used as output in R shiny

  # set corresponds to a column
  det_df <- RCOres_l$details_ma[, setid, drop = FALSE]
  # set corresponds to a row
  set_df <- t(RCOres_l$settings_df[setid, , drop = FALSE])
  union_df <- rbind(det_df, set_df)

  # Set required lines first
  req_names <- c("total_rb_dev", "target_function_value", "TEact-TEtarg",
                 "NetInvestment", "PortBeta", "IndexPosition")
  ind_v <- match(x = req_names, table = rownames(union_df))
  all_ind_v <- seq(1:nrow(union_df))

  union_df <- union_df[c(ind_v, all_ind_v[!all_ind_v %in% ind_v]), ,drop = FALSE]

  DT::datatable(data =  union_df,
            rownames = TRUE,
            selection = list(mode = 'multiple', selected = c(1,2,3,4,5,6)))
}

writeRCOsettingsToSQL <- function(single_setings_set_df, verbose = FALSE){

  # Description:
  # This function is triggered by R shiny "Save" button, if
  # check box is checked. After the optimization runs comparison
  # one can find the best fit (set of options) for the optimization.
  # In this case this set of input parameters can be saved and set as default.
  # This function saves the set in SQL DB

  # Algorithm:
  # The save is done in 2 steps:
  # Step 1: update (del + ins) temporary table: betsizing.CalculationRCOCurrentSettingSetUnsaved
  # Step 2: execute Stored Procedure: betsizing.uspCalculationRCO_LookupOrAddNewSettingSet
  #         which inserts a new set if required into betsizing.CalculationRCOSettingSets

  if(class(single_setings_set_df) == "data.frame"){
    if(nrow(single_setings_set_df) == 1){

      # define temporary table name
      temptable <- "betsizing.CalculationRCOCurrentSettingSetUnsaved"

      con <- FAFunc.GetDB()
      # Delete a table if exists
      try(RODBC::sqlDrop(channel = con, sqtable =  temptable, errors = FALSE), silent = TRUE)

      columnTypes <- list(FK_ParameterName = "NVARCHAR(50)",
                          ParameterValue = "NVARCHAR(255)")

      #fill in chosen settings
      param_names_v <- colnames(single_setings_set_df)
      # TODO add validation here
      # single_setings_set_df must contain the only one row
      param_values_v <- as.character(single_setings_set_df[1,])
      insert_df <- data.frame(FK_ParameterName = param_names_v,
                              ParameterValue = param_values_v)

      s <- RODBC::sqlSave(channel = con,
                          dat = insert_df,
                          tablename = temptable,
                          varTypes = columnTypes)

      RODBC::odbcClose(con)


      # insert whole settingsSET in tempTable (to check in sql if settings are new!)
      # delete existing table

      # dummydf <- data.frame(ParameterValue="dummyv")
      # rownames(dummydf) <- "TargetTE"
      # sqlSave(con, dummydf,temptable,append=TRUE , rownames="FK_ParameterName")
      # selstr = paste("Delete FROM " , temptable,sep="")
      # RODBC::sqlQuery(con, selstr)

      # if(verbose == TRUE){
      #   print(paste("cleared temporary GlobalSettings table:", temptable))
      # }
      #
      # df.s <- data.frame(t(single_setings_set_df))
      # colnames(df.s) <- "ParameterValue"
      # df.s  <- df.s[!(row.names(df.s) %in% c("LbMax","InputParameters","Specif_COV","setID","SoftLeverageConstrained","CovCalcWay","CovReturns","CovID")),,drop=FALSE]
      #
      # saved <- sqlSave(channel = con,
      #                  dat = df.s,
      #                  tablename = temptable,
      #                  append = TRUE,
      #                  rownames="FK_ParameterName")
      #
      # if(saved == 1){
      #   if(verbose == TRUE){
      #     print("successfully inserted the used GlobalSettings in SQL-Temporary-Table")
      #   }else{
      #     # Nothing
      #   }
      #
      #   # exec sql procedure to give back correspondent GlobalSettingSetID (and create a new one if required)
      #   execstr <- "exec betsizing.uspCalculationRCO_LookupOrAddNewSettingSet"
      #   query_res_df <-  RODBC::sqlQuery(con, execstr)
      #
      #   if(class(query_res_df) == "data.frame"){
      #     if(nrow(query_res_df) > 0){
      #       if(verbose == TRUE){
      #         print(paste("successfully inserted new OR linked used Global settings with existing GLobalSettingSetID: ",query_res_df))
      #       }else{
      #         # Nothing
      #       }
      #       return(query_res_df)
      #     }else{
      #       stop("writeRCOsettingsToSQL:exec betsizing.uspCalculationRCO_LookupOrAddNewSettingSet  gave no results")
      #     }
      #   }else{
      #     stop("writeRCOsettingsToSQL:SQL query failed")
      #   }
      # }else{
      #   print("writeRCOsettingsToSQL: Error during the save orruced")
      #   stop("writeRCOsettingsToSQL: Error during the save orruced")
      # }

    }else{
      print("writeRCOsettingsToSQL:input data.frame must have only 1 row")
      stop("writeRCOsettingsToSQL:input data.frame must have only 1 row")
    }
  }else{
    print("writeRCOsettingsToSQL:input is not a data.frame")
    stop("writeRCOsettingsToSQL:input is not a data.frame")
  }

}

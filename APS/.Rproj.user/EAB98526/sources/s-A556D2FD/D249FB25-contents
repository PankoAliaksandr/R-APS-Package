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
# call_params[2] = "2020-02-26"
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

# done
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


# done
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

# done
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
        print(paste0("Setting set ", RCOSetID, " is empty (no settings)"))
        stop("Setting set ", RCOSetID, " is empty (no settings)")
      }
    }else{
      # class(result_df) != "data.frame")
      print("getRCOSetSettiongs: Select statement contains an error ")
      stop("getRCOSetSettiongs: Select statement contains an error ")
    }

  }else{
    # RCOSetID == NULL OR RCOSetID == NA
    print("RCO Settings ID parameter is not valid")
    stop("RCO Settings ID parameter is not valid")
  }
}

# done
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
      print(paste0("Portfolio", portfolioName, " does not exist (or ambiguous) on the chosen date"))
      stop("Portfolio", portfolioName, " does not exist (or ambiguous) on the chosen date")
    }
  }else{
    # class(result_df) != "data.frame")
    print("get_ap_bb_ticker_by_name: Select statement contains an error ")
    stop("get_ap_bb_ticker_by_name: Select statement contains an error ")
  }
}

# done
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
                        "AnalystPortfolioConstituent AS Ticker, ",
                        "BenchmarkConstituentWeightInPercent, ",
                        "AdjustedConviction ",
                        "FROM Aktienmodell.betsizing.tvfAnalystPortfolioMembersPerDateWithConvCutOffForOptimization",
                        "('", ap_bb_ticker, "',",
                        "'", calculation_date,"',
                        (SELECT Aktienmodell.betsizing.fGetCutoffTimeBeforeAsOfDate ('",calculation_date ,"'))
        )")

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

            rownames(result_df) <- result_df$Ticker
            # Delete 2 columns
            result_df <- result_df[,!colnames(result_df) %in% c("Ticker", "BenchmarkConstituentWeightInPercent")]

            targ_names <- c("conviction", "lb","ub","RiskBudget", "tradeability")
            colnames(result_df) <- targ_names

            return(result_df)
          }
          else{
            print(paste0("Portfolio", ap_bb_ticker, " does not exist on ", calculation_date))
            stop("Portfolio", ap_bb_ticker, " does not exist on ", calculation_date)
          }
        }else{
          # class(result_df) != "data.frame")
          print("getTargetTable: Select statement contains an error ")
          stop("getTargetTable: Select statement contains an error ")
        }
      }else{
        # calculation date is not in date format
        print("Calculation date is  not in 'Date' format")
        stop("Calculation date is  not in 'Date' format")
      }
    }else{
      # Calculation date is empty
      print("Calculation date parameter is empty")
      stop("Calculation date parameter is empty")
    }
}else{
  # AP_ID == NULL OR AP_ID == NA
  print("Analyst Portfolio ID parameter is not valid")
  stop("Analyst Portfolio ID parameter is not valid")
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
      stop(paste("Chosen Covariance with id", RunID,
                 "and CalculationMethod", CalculationMethod, "does not exist!"))
    }
  }else{
    # reult has class != data.frame
    stop("getCovFromSQL: Query contains a mistake")
  }

  return(cov_df)
}

#Write the optimized weights into the SQL-Result-Upload table
f.writeRCOresToSQL <- function(res_rel,
                               res_p,
                               risk_contribution,
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


  # TODO rename res_rel in opt_rel_weights
  # TODO rename res_p in opt_abs_weights
  # TODO rename portfolio in AP_name


  if(class(res_rel) == "numeric" &&
     class(res_p) == "numeric" &&
     class(risk_contribution) == "numeric" &&
     class(portfolio) == "character" &&
     class(covID) == "integer" &&
     class(setID) == "integer" &&
     class(calcdatetime) == "character"){

    # Input has right type
    constituents <- names(res_p)

    # Values must be listed in the same order
    if(all(names(res_rel) == constituents) &&
       all(names(risk_contribution) == constituents)){

      # Constituents' optimized absolute and relative weights have the same order
      n_constituents <- length(res_p)
      # Portfolio must be the same for all constituents
      portfolio <- rep(x = portfolio, times = n_constituents)

      # Prepare table structure
      # This part is very tricky, sqlSave parameters leads to bug.
      df_to_upload <- data.frame(CalculationRCOResultForUploadID = seq(1:n_constituents),
                                 CalculationDate = calcdatetime,
                                 FKAPS = portfolio,
                                 FKCovMaRunID = covID,
                                 FKCalculationRCOSettingSetID = setID,
                                 Ticker = constituents,
                                 OptimizedRelativeWeight = res_rel,
                                 OptimizedPortfolioWeight = res_p,
                                 RiskContribution = risk_contribution)

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
        stop("some error in sql insert")
      }
    }else{
      stop("The order of constituents is not the same for absolute and relative optimized weights")
    }
  }else{
    stop("writeRCOresToSQL: at least one of the input parameters is of a wrong type")
  }
  #example
  #res_rel <- RCOres$RW[,1];res_p <- res_rel-RCOres$TARG[,"lb",1];portfolio <- "EQ_CH_L"; covID <- as.integer(7) ;setID <- as.integer(1) ;calcdatetime <- as.character(Sys.Date())
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
f.matchCOVtoTickers <- function(Tickers,COV) {

  # Description
  #   1. Make sure CovMa has the same tickers as Tickers
  #   2. Make sure the sequence is also the same

  # Dependencies:
  #   1. f.as.numeric.factor

  # Return value:
  #  covariance matrix

  if(class(COV) == "data.frame"){
    # Check missing tickers
    cov_df_col_names <- colnames(COV)
    cov_df_row_names <- rownames(COV)

    if(all(cov_df_col_names == cov_df_row_names) == TRUE){
      # The same sequence in CovMa rows and columns is checked


      missTick <- Tickers[is.na(match(Tickers, cov_df_col_names))]
      if(length(missTick) == 0){
        # we know that tickers are the same. Only sequience can be theoretically different
        # change order to make it the same
        # if COV has more tickers than Tickers it will also work
        COV <- COV[Tickers, Tickers]

        # Convert to numeric matrix
        cov_ma <- as.matrix(sapply(COV, f.as.numeric.factor))

        if(isSymmetric.matrix(cov_ma, check.attributes = FALSE) == TRUE){
          return(COV)
        }else{
          stop("Covariance Matrix is not symmetric")
        }
      }else{
        stop(paste("The following Tickers lack in Covariance matrix:", paste(missTick, collapse = ", ")))
      }
    }else{
      stop("CovMa: column names and row names are not the same")
    }
  }else{
    stop("COV parameter must be of data.frame type")
  }
}

#get the proper functions depending on settings!
f.setOptfunctions <- function(set, k = 0){
  # Dependencies
  #   NO

  #target function
  if(set$IndexFlex == TRUE)
  {
    ftarget <- function(x,rb_a,COVAR,target_te,A,CashTolerance,LeverageTolerance,NetInvLambda)
    {
      return(list("objective" =  as.matrix(-abs(c(0,t(rb_a[-1]))) %*% log(abs(x)))+ NetInvLambda*sum(x)^2, #+ NetInvLambda*abs(sum(x)) ,
                  "gradient"  = diag(A)*as.matrix(c(0.00,abs(rb_a[-1])) / abs(x)) + NetInvLambda*2*sum(x)    # + NetInvLambda*diag(A)
      ))
    }
  } else{
    ftarget <- function(x,rb_a,COVAR,target_te,A,CashTolerance,LeverageTolerance,NetInvLambda)
    {
      return(list("objective" =  as.matrix(-abs(t(rb_a)) %*% log(abs(x))) + NetInvLambda*sum(x)^2, #+ NetInvLambda*abs(sum(x)) ,
                  "gradient"  = diag(A)*as.matrix(abs(rb_a) / abs(x)) + NetInvLambda*2*sum(x)    # + NetInvLambda*diag(A)
      ))
    }
  }

  #nlcons
  stopifnot(set$Ix_eq_Cash==FALSE || (set$Ix_eq_Cash==TRUE && set$SoftNetInvConstraint==FALSE && set$IndexFlex==FALSE)) #check if nlcon-settings are allowed

  #set the right nlcon
  if(set$SoftNetInvConstraint == TRUE)
  {
    #nlcon depend on treatment of index asset
    if(set$IndexFlex==TRUE)
    {
      print(paste("OPTIMIZE: with flexible index, nlcon keeps NetInvestment within range"))
      nlcon_ineq <- function(w,COVAR,target_te,A,rb_a,CashTolerance,LeverageTolerance,NetInvLambda)
      {#OPTIMIZE: with flexible index, nlcon keeps NetInvestment within range (Index still has to impact TE, just in TargetFunction disregarded!)
        return(list("constraints"= c(
          diag(c(0,diag(A[-1,-1]))) %*% w                                #direction constraint
          ,sqrt(t(w) %*% COVAR %*% w) - target_te #TE-constraint
          ,sum(w) - LeverageTolerance      #max postive NetInvestment (Leverage)
          ,-sum(w) - CashTolerance          #max negative NetInvestemnt (Cash)
          ,w[1] * (COVAR %*% w)[1] / as.numeric(t(w) %*% COVAR %*% w) - rb_a[1]#limit risk contribution of first (index position's)
        ),
        "jacobian" = rbind(
          diag(c(0,diag(A[-1,-1])))                                    #gradient direction constraint
          ,t((COVAR %*% w) /  as.numeric(sqrt(t(w) %*% COVAR %*% w) ))  #gradient TE-constraint
          ,t(rep(1,length(w)))                 #gradient: no leverage constraint
          ,t(rep(-1,length(w)))                #gradient max Short position
          ,t(rbind( (((COVAR %*% w)[1] + w[1]*COVAR[1,1])* (t(w)%*%COVAR%*%w) - 2*w[1]*COVAR[1,]%*%w*(COVAR%*%w)[1]) / (t(w) %*% COVAR %*% w)^2, #first row derivative after w[1]
                    as.numeric(w[1]/(t(w) %*% COVAR %*% w))*COVAR[1,-1]                                                    #row 2:n derivatives after w[2:n]   (1)
                    - as.numeric((2*w[1]*(COVAR[1,]%*% w))/(t(w) %*% COVAR %*% w)^2)*COVAR[-1,]%*%w )))                    #row 2:n derivatives after w[2:n]   (2)
        )
        )
      }

      #here still make an extension for Ix_eq_Cash (cond. on parameter!!!). A bit tough to adjust the formula!!!
      #else if set$Ix_eq_Cash: .....

    } else {
      #"OPTIMIZE: nlcon keeps NetInvestment within range, ix as normal asset"
      print(paste("OPTIMIZE: nlcon keeps NetInvestment within range, ix as normal asset"))
      nlcon_ineq <- function(w,COVAR,target_te,A,rb_a,CashTolerance,LeverageTolerance,NetInvLambda){
        return(list(
          "constraints"= c(as.matrix(A %*% w)  #direction constraints
                           ,c(as.matrix(sqrt(t(w) %*% COVAR %*% w)) - as.matrix(target_te)) #TE-constraint
                           , sum(w) - LeverageTolerance               #allow for 25Bp Cash
                           ,-sum(w) - CashTolerance                  #max Short position leverage constraint (s.t. Tolerance)
          ),
          "jacobian" = rbind(A
                             ,as.matrix(t((COVAR %*% w) /  (rep(sqrt(t(w) %*% COVAR %*% w),length(w)) )))#t((COVAR %*% w) /  as.numeric(sqrt(t(w) %*% COVAR %*% w) ))
                             ,t(rep(1,length(w)) )                 #gradient: no leverage constraint
                             ,t(rep(-1,length(w)))                 #gradient max Short position
          )
        ))
      }
    }

  } else {
    #leverage control happens in equality constrained!

    # if(set$CashTolerance != 0 ) {print(paste("Currently this case is not feasible as here is the Non-SoftLeverage part where CashTolerance must =0!)
    #                                          >>move to Soft-Leverage part to become more meaningful! / another Parameter with cash-target!"))}
    #         if(set$Ix_eq_Cash)#equality constrained depends on IX_eq_Cash
    #         {
    #           stopifnot(set$algo %in% c("NLOPT_GN_ISRES","NLOPT_LN_AUGLAG_EQ"))
    #           nlcon_eq <- function(w,COVAR,target_te,A,rb_a,CashTolerance,LeverageTolerance){
    #           return(list(
    #           "constraints" = -sum(w[-1]) - CashTolerance,#replace CashTolerance with CashRequirement!!!
    #           "jacobian"= c(0,rep(-1,(length(w)-1)))
    #           ))
    #
    #         }
    #       }else{ }
    print(paste("OPTIMIZE: eqcon keeps NetInvestment == 0"))
    nlcon_eq <- function(w,COVAR,target_te,A,rb_a,CashTolerance,LeverageTolerance,NetInvLambda){
      #"OPTIMIZE: eqcon keeps NetInvestment == 0"
      return(list(
        "constraints" = -sum(w),
        "jacobian"= rep(-1,length(w))
      ))
    }

    print(paste("Loop",k,"OPTIMIZE: ix as normal asset"))
    nlcon_ineq <- function(w,COVAR,target_te,A,rb_a,CashTolerance,LeverageTolerance,NetInvLambda){
      return(list(
        "constraints"= c(as.matrix(A %*% w)  #direction constraints
                         ,c(as.matrix(sqrt(t(w) %*% COVAR %*% w)) - as.matrix(target_te)) #TE-constraint
                         #  , sum(w) - CashTolerance                   #no leverage constraint (s.t. Tolerance)
                         #   ,-sum(w) - CashTolerance                  #max Short position leverage constraint (s.t. Tolerance)
        ),
        "jacobian" = rbind(A
                           ,as.matrix(t((COVAR %*% w) /  (rep(sqrt(t(w) %*% COVAR %*% w),length(w)) )))#t((COVAR %*% w) /  as.numeric(sqrt(t(w) %*% COVAR %*% w) ))
                           # ,t(rep(1,length(w)) )                 #gradient: no leverage constraint
                           # ,t(rep(-1,length(w)))                 #gradient max Short position
        )
      ))
    }

  }

  #return ftarget and nlcon
  if(exists("nlcon_eq"))
  {return(list(ftarget=ftarget,nlcon_eq=nlcon_eq,nlcon_ineq=nlcon_ineq))}else{
    return(list(ftarget=ftarget,nlcon_ineq=nlcon_ineq))
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
  #t <- as.data.frame(RCOres$TARG[,,1]); lb <- t$lb;ub <- t$ub;conviction <- t$conviction;tr <- t$tradeability
  #g1 <- f.getWeightRange(lb,ub,conviction,tr,0.1,LowConvictionExitInDays = 20,ConvictionGroups=7)
  #cbind(rownames(targ),g1$w_min,g1$w_max,conviction,g1$ConvictionDegree)

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
  # w <- RCOres$RW[,1];w_bm <- -RCOres$TARG[-1,"lb",1];ZeroNetInvestment <- FALSE
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

      # Calculate risk contribution
      rcb_v <- weight_v * as.matrix(cov) %*% weight_v
      rcb_v <- rcb_v/VAR

      return(rcb_v)

    }else{
      stop("get_rcb: covariance names and weight names are not the same or empty")
    }
  }else{
    stop("get_rcb: input has wrong type")
  }
}

#####Optimization Results Characteristic
OptimizationResultsCharacteristics <- function(rw, cov, lb, ub, set, rb_target){
  # Functions:
  # 1. ImplIndexPosition
  # 2. get_rcb

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

  #sum of rc actual - target
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
  # rw <- RCOres$RW[,1] ; cov <- RCOres$COV[,,1]; lb <- RCOres$TARG[,"lb",1]; ub <- RCOres$TARG[,"ub",1]; rb_target <- RCOres$TARG[,"RiskBudget",1]
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

  #checks
  stopifnot(all(targ$RiskBudget >= 0))#check if all risk budgets > 0
  stopifnot(all.equal(sum(targ[,"RiskBudget"]),1))#check if defined risk budget sums up to 100%
  stopifnot(colnames(targ) %in% c("conviction","RiskBudget","lb","ub","Ticker","tradeability"))
  stopifnot(is.data.frame(set),nrow(set)==1)
  #TODO check class
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

  cov <- f.matchCOVtoTickers (Tickers = rownames(targ), COV = cov)
  if(grepl("Crncy",rownames(targ)[1]))    #turn sign of bm-index to reflect a long index position (needed for sum to 0 constraint!)
  {
    cov[1,] <- -1*cov[1,]
    cov[,1] <- -1*cov[,1]
    print("The sign of the first row in the Covariance Matrix was switched, Crncy-tickers are supposed to reflect negative index performance ")
  }

  # #check for invertability of cov
  # ipak("matrixcalc")
  # print(paste("the cov is invertible is", is.singular.matrix(as.matrix(cov) )))


  ###get the target function and nlcon according to Settings
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
                      "xtol_rel" = 1.0e-7 )

  opts <- list("algorithm" = as.character(set$algo),
               "maxeval" = as.numeric(set$Trials),
               "print_level" = 0,
               "xtol_rel" = set$xtol_rel,
               "xtol_abs" = 10 ^ -7,
               local_opts = local_opts)#,check_derivatives=TRUE)      #algortihms tried: "NLOPT_GN_ISRES",
  #algorithms used and no result:NLOPT_LN_AUGLAG,NLOPT_GN_DIRECT_L_RAND; algorithms to try: NLOPT_GNL_DIRECT_NOSCAL, NLOPT_GN_DIRECT_L_NOSCAL, and NLOPT_GN_DIRECT_L_RAND_NOSCAL,NLOPT_GN_ORIG_DIRECT and NLOPT_GN_ORIG_DIRECT_L,

  dir <- sign(targ$conviction) #direction of conviction (where active)

  #assert that TickDet still fits COV!
  stopifnot(rownames(targ) == as.character(row.names(cov)))

  #print(paste("Cash treated as index asset with risk budget ",round(targ$RiskBudget[1]*100,2), "% and direction ",sign(targ$conviction[1])),sep="")

  x0 <- pmax(pmin(targ$ub., targ$RiskBudget*dir),targ$lb.)
  A <- -diag(dir) #define desired bet directions
  #lb_reset=rep(-10,nrow(targ))#lower bounds are seperately specified in side constraint! -> set to very low value for the function!

  # ipak("nloptr")

  runAnotherOptIteration <- TRUE
  attempt <- 1
  while(runAnotherOptIteration)
  {
    print(paste("start  attempt:",attempt,"with nloptr"))
    if(set$SoftNetInvConstraint){
      calctime <- system.time(opt_output <-
                                nloptr::nloptr(x0 = as.matrix(x0),
                                       eval_f = optFun$ftarget,
                                       #eval_grad_f = fgrad,
                                       lb = as.matrix(targ$lb.),
                                       ub = as.matrix(targ$ub.),
                                       # eval_g_eq = nlcon_eq,   #nlcon_eq not required
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
    attempt <- attempt+1
    print(opt_output)

    ####check optimization quality####
    #check direction constraint (should be all <=0)
    injuries <- -A %*% opt_output$solution < 0

    if(sum(injuries) > 1 || opt_output$status %in% c(-1,-2,-3,-4) || abs(opt_output$objective) == Inf )  {
      validResult <- FALSE
      print("optimization failed or direction constrained violated or target function Inf ,invalid result. Solution reset to NA") #no stop as loop should continue!!!
      rw <- pw <- rep(NA, length(rw))

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

      if(opt_output$status == -4 || opt_output$status == -1 ){
        runAnotherOptIteration <- ifelse(attempt <= MaxAttempts,TRUE,FALSE)
        set$TargetTE <- set$TargetTE+(stats::runif(1)-0.5)*10^-6
        print(paste("The ",attempt-1,". try resulted in Error -4 or -1!",
                    ifelse(runAnotherOptIteration,paste("Execute a ",attempt,". time with a slightly different TE-target of: ", set$TargetTE),
                           paste("No results found after attempt",attempt))))
      }else{runAnotherOptIteration <- FALSE}

    }else{
      #valid calculation: get optimization results characteristics and store the results
      validResult <- TRUE ;
      runAnotherOptIteration <- FALSE
      iterations <- opt_output$iterations
      rw <-  opt_output$solution
      pw <- rw - targ$lb    #portfolioweights. checks needed???
      orc <- OptimizationResultsCharacteristics(rw = rw,
                                                cov = cov,
                                                lb = targ$lb,
                                                ub = targ$ub,
                                                set = set,
                                                rb_target = targ$RiskBudget) #rw=rw;COV=COV;lb=targ$lb.;ub=targ$ub;set=set

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
        print(data.frame(conviction=targ$conviction,rw=rw,RiskContr=orc$rc))
      }else{ cat("The Risk-Contribution-Optimization yielded a result but the Deviation to Target-Tracking Error was:",orc$te - set$TargetTE, " and the resulting weights are:", sep="\n")
        print(data.frame(conviction=targ$conviction,rw=rw,RiskContr=orc$rc))

      }
    }
  }
  return(list(rw = rw,
              pw = pw,
              optim_details = optim_details))
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
  source("G:/FAP/Equities/Betsizing/Code/RCO.R")
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
  stopifnot(any( (target_table$lb>=0 & target_table$conviction<0) | (target_table$ub<=0 & target_table$conviction>0) )  == FALSE )

  # Get RCO optimization settings
  RCO_settings_df <- getRCOSetSettings(RCOSetID = RCOSetID,
                                       connection = connection)
  # Close connection
  RODBC::odbcClose(connection)

  covMa <- f.getCovFromSQL(RunID = covMaSetID,
                           CalculationMethod = calc_method,
                           WideOrLong = "wide")

  # TODO check dimensions -> delete if not equal to expected value

  # reduce target_table on those with an active conviction
  opt_weights_relative <- rep(0, length = nrow(target_table))
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
      stop("runRCO stopped with an error: ", e)
    }
  )

  # check if optimization returned a valid result
  if(isTRUE(as.logical(optimization_result_l$optim_details["validResult"]))){
    # The result is valid

    # Optimal weights are updated in SQL table
    opt_weights_relative[stocks_with_signal_ind] <- optimization_result_l$rw

    # Optimal absolute weights are (relative + bm); bm = -lb
    opt_weights_portfolio <- opt_weights_relative - target_table$lb

    # Calculation timestamp
    calcdatetime <- optimization_result_l$optim_details["Calculated"]

    names(opt_weights_relative) <- names(opt_weights_portfolio) <- rownames(target_table)

    # Get risk contribution
    risk_contribution <- get_rcb(weight_v = opt_weights_relative, cov = covMa)

    # create upload df
    covMaSetID <- as.integer(covMaSetID)
    RCOSetID <- as.integer(RCOSetID)

    print("Try to call f.writeRCOresToSQL from runRCOFromAPS")

    f.writeRCOresToSQL(res_rel = opt_weights_relative * 100,#sql-table stores values in Percentage
                       res_p = opt_weights_portfolio * 100, #sql-table stores values in Percentage
                       risk_contribution = risk_contribution,
                       portfolio = portfolioName,
                       covID = covMaSetID,
                       setID = RCOSetID,
                       calcdatetime = calcdatetime)

  }else{
    stop("no valid result found in optimization")
  }
}
#
#
#
# #RCO RISK CONTRIBUTION OPTIMIZER
#
# # First load required functions
# source("G:/FAP/Equities/Betsizing/Code/R_customized_FAFunc.R")
#
# runRCOLoops <- function(Portfolio,                 #character string of the portfolio. used to identify the targ-file to load
#                         TargetTE,                            #Target Tracking error as numeric or numeric vector (to vary through settings)
#                         MaxRelWeight = 0.1,                 #Maximum relative Weigth (under- or over-)
#                         Trials = 10000,                      #Limit of Trials for the optimizer. as numeric or nusec_nrmeric vector (to vary through settings)
#                         algo = "NLOPT_LD_SLSQP",         #algo that nloptr uses as character or character vector  (to vary through settings):  "NLOPT_GN_ISRES" non gradient; "NLOPT_LD_SLSQP" with Gradient, FAILED: "NLOPT_GD_STOGO","NLOPT_GD_STOGO_RAND"
#                         CashTolerance = 0.0025,              #Boundary for the Cash held (conditon is: -sum(w)>CashTolerance)
#                         LeverageTolerance = 0.0025,          #Boundary for the Leverage held (condition is sum(w)<LeverageTolearnce)
#                         CovCalcWay="nlshrink",               #Covariance calculation setting for SQL-Covariance. #c("nlshrink","cov")
#                         CovReturns="abs",                    #Covariance calculation setting for SQL-Covariance. #c("abs","rel")
#                         ShortIndexWithOptimCash=0,           #set 0 or 1 to implement the optimized Cash position by shorting the ind   ex via selling proportional weights in each index-member
#                         InputParameters="Signals4R",         #character string like fitting the variable input in the specified targ-sheet
#                         Specif_COV=NA,                        #set specific covariance ID, -10 for testing and NA for most recent available for the chosen portfolio
#                         xtol_rel=10^-7,
#                         IndexFlex=FALSE,
#                         Ix_eq_Cash =FALSE,
#                         NetInvLambda = 0                        #punishment factor for NetInvestments <>0
#                         #,FixedCashRisk = 0                     #fixed cash risk in the portfolio that needs to be taken into account!
#                         ,LowConvictionExitInDays = 10
#                         ,ConvictionGroups = 3
#                         ,MaxAttempts=2
#                         #,convictionInputFrom = c("xls","simts") #new parameter
# ){
#   ######load libraries
#   ipak(c( "dplyr", "reshape"))
#
#   print("starting...")
#   #checks
#   stopifnot(length(ShortIndexWithOptimCash)==1)
#
#   ######Prepare to get Data######################################
#   #define file pathes (add switch to specify exact location for each portfolio)
#   if(Portfolio=="RE_EU")
#   {file_path_in <- "G:/FAP/Immobilien_Modell/"
#   input_file <- "Betsizing.xlsx"
#   #}else if (Portfolio == "RE_CH")
#   #{file_path_in <- "G:/FAP/Indirekte Immo/osm/Diverses/"
#   #input_file <- "Indirekte_Immobilienanlagen_2018-HJ2_Copy.xlsx"
#   }else {
#     file_path_in <- "G:/FAP/Equities/Betsizing/"
#     input_file <- paste("bet_targets_",Portfolio,".xlsx",sep="",collapse="")     #"bet_targets_epra.csv"              #"bet_targets_epra.csv"
#   }
#
#   file_path_out <- paste("G:/FAP/Equities/Betsizing/R_results/",Portfolio,"/",sep="",collapse="")
#   output_file <- paste(Portfolio,"_",Sys.Date(),".xlsx",sep="",collapse="")
#
#   #####Create Settings matrix#####
#   ipak("gridExtra")
#   SET <- as.data.frame(expand.grid(
#     TargetTE=TargetTE,
#     MaxRelWeight=MaxRelWeight,
#     Trials=Trials,
#     xtol_rel=xtol_rel,
#     algo=algo,
#     CashTolerance=CashTolerance,
#     LeverageTolerance=LeverageTolerance,
#     CovCalcWay=CovCalcWay,
#     CovReturns=CovReturns,
#     InputParameters=InputParameters,
#     ShortIndexWithOptimCash=ShortIndexWithOptimCash,
#     Specif_COV=Specif_COV,
#     IndexFlex=IndexFlex,
#     Ix_eq_Cash=Ix_eq_Cash,
#     NetInvLambda=NetInvLambda
#     #,FixedCashRisk=FixedCashRisk
#     ,LowConvictionExitInDays=LowConvictionExitInDays
#     ,ConvictionGroups=ConvictionGroups,
#     stringsAsFactors=FALSE)
#   )
#   #sort data Frame
#   SET <- SET[with(SET,order(InputParameters,Specif_COV)),]
#
#   SET <- f.delete_infeasible_settings(SET)
#
#   ######START LOOP###############################################
#   loop.nr <- nrow(SET)
#   SET$setID <- 1:loop.nr
#
#   optim_details <- NULL
#   rw <- NULL
#   COV <- NULL
#
#   con<-FAFunc.GetDB()
#
#   for (k in 1:loop.nr)
#   {
#     print(paste(Sys.time()," started loop  ",k, " of ",loop.nr,sep=""))
#     set<-SET[k,]
#
#     #stopifnot(set$algo %in% c(algo_lagrange,algo_gradient,"NLOPT_GN_ISRES"))#implemented algorithms
#     print(set)
#
#     ####get data ready####
#     #reload targ-file if needed
#     if(k == 1 ||                                                                 #reload for first year
#        identical(SET$InputParameters[max(1,k-1)],set$InputParameters) == FALSE) #reload file if input-sheet changed
#     {
#       print(paste("reload data for sheet ",set$InputParameters," and CovarianceID",set$Specif_COV))
#       #get risk budget targets, lower bounds for each ticker,
#       ipak("xlsx")
#       targ_names <- c("lb","ub","conviction","RiskBudget","tradeability")
#       targ <- read.xlsx(paste(file_path_in,input_file,sep="",collapse=""),sheetName=as.character(set$InputParameters))
#       #check for duplicated tickers (not allowed ???)
#       stopifnot(length(unique(targ$Ticker)) == nrow(targ$Ticker))
#       targ <- targ[!is.na(targ$Ticker),]
#       rownames(targ) <- targ$Ticker
#       targ <- targ[,targ_names]
#       targ <- targ[is.na(targ[,"RiskBudget"])==FALSE,] #remove possible NA risk budgets
#
#       if(k!=1)
#       {
#         if(identical(row.names(TARG),row.names(targ))){
#           TARG[,,set$InputParameters] <- as.matrix(targ[,targ_names])
#         }else{stop(paste("Tickers dont match adjust in targ-file!!! sheet:",set$InputParameters))}
#       }
#       sec_nr <- nrow(targ)
#       #targ$targ_id <- 1:sec_nr
#
#
#       #check if boundaries are compatible with the direction convictions
#       stopifnot(any( (targ$lb>=0 & targ$conviction<0) | (targ$ub<=0 & targ$conviction>0) )  == FALSE )
#     }
#
#     ###Define Output Matrices after first run!
#     if(k==1){
#       RW <- matrix(nrow = sec_nr,ncol = loop.nr ,
#                    dimnames=list(rownames(targ),paste("set",SET$setID,sep="")))
#       optim_details_names <- list(c("Calculated","TEact-TEtarg","%weight_limits_hit","NetInvestment","PortBeta","IndexPosition",
#                                     "target_function_value","iterations","calctime","total_rb_dev","validResult"))
#       Optim_Details <- array(dim=c(lengths(optim_details_names),loop.nr),dimnames = optim_details_names)
#       TARG <- array(dim=c(sec_nr,length(targ_names),length(InputParameters)), #to store the different input tables
#                     dimnames=list(rownames(targ),targ_names,InputParameters))
#       TARG[,,set$InputParameters] <- as.matrix(targ[,targ_names])
#       different_covsettings <- unique(SET[,c("CovCalcWay","CovReturns","Specif_COV")])
#       COV <- array(dim=c(sec_nr,sec_nr,nrow(different_covsettings)),
#                    dimnames=list(rownames(targ),rownames(targ),apply(different_covsettings,1,paste,collapse="_")))
#     }
#
#     #reload COV if needed
#     if(k == 1 ||                                                    #reload for first year
#        identical(SET$Specif_COV[max(1,k-1)],set$Specif_COV) == FALSE ||   #reload COV if COV changed
#        identical(SET$CovCalcWay[max(1,k-1)],set$CovCalcWay) == FALSE ||     #reload COV if COV changed
#        identical(SET$CovReturns[max(1,k-1)],set$CovReturns) == FALSE)   #reload COV if COV changed
#
#     {if(is.na(set$Specif_COV)){
#       CovID <- FAFunc.FindNewestCovInDB(RelOrAbsReturn=as.character(set$CovReturns),Portfolioname=Portfolio)
#
#     }else  {CovID <- set$Specif_COV}
#       print(paste("For the loop",k,"the COV with RunID",CovID,"is used"))
#       SET$CovID[k] <- CovID
#
#       #Get covariance matrix
#       cov <- f.getCovFromSQL(RunID = CovID,CalculationMethod = set$CovCalcWay,WideOrLong = "wide")
#       cov <- f.matchCOVtoTickers (Tickers = rownames(targ), COV = as.data.frame(cov))
#       COV[,,paste(set$CovCalcWay,set$CovReturns,set$Specif_COV,sep="_")] <- as.matrix(cov)
#
#     }
#
#
#     ####optimization####  ->new runRCO-function: runRCO <- function(targ,COV,RCOset,save)
#     #reduce to active portfolio and seperate TickerInfo from its covariances (necessary as target function cant handle 0-weights as it calculates ......*log(w)*....)
#     sec_a <- targ$conviction != 0
#     targ_a <- targ[sec_a,]
#     cov_a <- f.matchCOVtoTickers (Tickers = rownames(targ_a), COV = cov)
#
#     RCOthisLoop <- f.runRCO(targ_a,set,cov_a)
#
#     #store the results in Loop-Matrices if valid
#     Optim_Details[,k] <- RCOthisLoop$optim_details
#     if(RCOthisLoop$optim_details["validResult"])
#     {RW[sec_a,k] <- RCOthisLoop$rw
#     RW[!sec_a,k] <- 0
#     }
#     #intermediary save output file to not loose everything if a later loop gets interrupted
#     dat <- list(SET=SET,Optim_Details=Optim_Details,RW=RW,TARG=TARG,COV=COV, reduce=FALSE)
#     save(dat,file=paste(file_path_out,Sys.Date()," RCOres.R",sep=""))
#   }
#   print(paste("all", k ,"loops completed. Storing of results in process"))
#
#   ######OUTPUT RESULTS######################
#   RCOres <- list(SET=SET,Optim_Details=Optim_Details,RW=RW,TARG=TARG,COV=COV)
#   f.writeOptdetails2xlsx(RCOres,Portfolio,Sys.Date(),SET$ShortIndexWithOptimCash[1])
#   return(RCOres)
# }
#

#

#
# #########Funktion um die Outputs der verschiedenen Loops zu plotten####
# f.compare_runs<-function(xaxis,yaxis,plotlabels=NA,RCOres)#x: from rownames(SET),y from rownames(optim_details),labels row of SET
# {
#   #get input and result table from RCOresult-list
#   SET<-t(RCOres$SET)
#   optim_details <- RCOres$Optim_Details
#   n<-ncol(SET)
#
#   #make check for inputs
#   #if(any(xaxis==rownames(SET))==FALSE) paste("xaxis must be one of the following characters",rownames(SET))
#   stopifnot(any(yaxis==row.names(optim_details)),(is.na(plotlabels) |any( plotlabels==rownames(SET))))
#
#   #okTEdev<-(abs(optim_details["TEact-TEtarg",])<0.001)*1:8
#   #plot(SET[xaxis ,],optim_details[yaxis,],xlab=xaxis,ylab=yaxis,col=1:n,pch=1:n,cex=3,bg=okTEdev,main = paste("'",xaxis,"'"," vs. ","'",yaxis,"'"))
#   plot(SET[xaxis ,],optim_details[yaxis,],xlab=xaxis,ylab=yaxis,col=1:n,pch=1:n,cex=3,main = paste("'",xaxis,"'"," vs. ","'",yaxis,"'"))
#   if(is.na(plotlabels)==FALSE)  legend("topleft",legend=paste(plotlabels,":",SET[plotlabels,]),col= 1:n,pch=1:n,box.lwd = 0,bty="n")
#
#   #3D
#   #library(rgl)
#   ##library(lubridate)
#   #?rgl
#   # zaxis<-SET["Trials",]
#   # df <- expand.grid(x=xaxis,y=yaxis,z=zaxis)
#   # open3d()
#   # plot3d(df$x, df$y, df$z,type="s",type='p')
#   #deviation distribution
#
#   #example (needs an existing result in RCOres-form)
#   #xaxis <- "TargetTE";yaxis <- "PortBeta";plotlabels <- NA; RCOres <- RCOres
#   #f.compare_runs( xaxis,yaxis,plotlabels=NA,RCOres)
# }
#
# #########Funktion um die optimierten Gewichte der verschiedenen Loops zu plotten
# f.compare_runWeights <- function(RCOres,colby=NA,showRCB=FALSE,whichCOV=1)
# {
#
#   w <- RCOres$RW
#
#
#   if(is.na(colby))
#   {
#     col <- 1:nrow(w)
#     leg <- paste("set",col,sep="")
#   }else
#   {
#     settypes <- as.factor(RCOres$SET[,colby])
#     col <- as.numeric(settypes)
#     leg <- levels(settypes)
#   }
#
#   if(showRCB)
#   {
#     print("ATTENTION: First Covariance taken for all runs even if several different Covariances have been used for the optimizations!!!!")
#     cov <- as.data.frame(RCOres$COV[,,whichCOV])
#     cov <- f.matchCOVtoTickers(colnames(w),cov)
#     rcb <- t(apply(w,1,get_rcb,cov))
#
#     #add shorter names
#     colnames(w) <- as.character(.pT(colnames(w)))
#     colnames(rcb) <- colnames(w)
#     bp <- barplot(rcb,beside=TRUE,las=2,col=col,main="optimized risk contributions for different settings")
#   }else
#   {
#     #add shorter names
#     colnames(w) <- as.character(.pT(colnames(w)))
#     bp <- barplot(w,beside=TRUE,las=2,col=col,main="optimized weights for different settings")
#   }
#   legend("topright",legend=leg,fill=1:length(leg),title=ifelse(is.na(colby),"",colby))
#
#   #example (needs an existing result in RCOres-form)
#   #RCOres <- RCOres;colby=NA;showRCB=FALSE;whichCOV=1
#   #f.compare_runWeights(RCOres,colby=NA,showRCB=FALSE,whichCOV=1)
# }
#
#
# #####Create 4-plots to evaluate a single setting-run
# checkRCOplots<-function(rw,cov,Conviction,lb,ub,rb,set,Tick,tv,ShortIndexWithOptimCash=0){
#
#   #par(mfrow=c(2,2),oma = c(0, 0,4, 0))
#   ipak("sfsmisc")
#   sfsmisc::mult.fig(4,oma = c(0, 0,4, 0))  #change to mult.fig!
#
#   if(any(is.na(rw))){print("no plots possible, NAs within the result")
#
#     plot(1,1,main = "NA");plot(1,1,main ="NA");plot(1,1,main ="NA");plot(1,1,main ="NA")
#     mtext(paste("Calculated with the settings: ",set$setID,sep=""), outer=TRUE,line=2.5,cex=1.5)
#     ix.fl <- 1:floor(length(set)/2) #columns of settings to plot in the first line below title
#     mtext(paste(names(set[ix.fl]),set[ix.fl],sep="=",collapse="; "), outer = TRUE,line=1, cex = 1)
#     mtext(paste(names(set[-ix.fl]),set[-ix.fl],sep="=",collapse="; "), outer = TRUE,line=0, cex = 1)
#   }else{
#     #get characteristics from function!!!
#     orc <- OptimizationResultsCharacteristics(rw=rw,cov=cov,lb=lb,ub=ub,set=set,rb_target=rb)
#
#     if(ShortIndexWithOptimCash==1)  rw <- orc$rw_impl
#     #rw=rw;COV=COV;lb=lb;ub=ub;set=set
#
#     te.targ <- set$TargetTE
#     te <- orc$te
#     rc <- orc$rc
#     NetInvestment <- as.numeric(orc$NetInvestment)
#     IndexPosition <- as.numeric(orc$IndexPosition)
#     P.Beta <- orc$p.beta
#
#     #adjust lower bounds to reflect implementable lbs!
#     lb[-1] <- (1+rw[1])*lb[-1]
#
#
#     #reduce to active securities again for plotting
#     .r<-function(x){return(x[Conviction !=0])}
#     lb_a <- .r(lb);ub_a <- .r(ub);rw_a<-.r(rw);rb_a<-.r(rb);rc_a<-.r(rc);Tick_a<-.r(Tick);Conviction_a <- .r(Conviction)
#     const_hit1 <- round(lb_a,3) == round(rw_a,3)
#     const_hit2 <- round(ub_a,3) == round(rw_a,3)
#     all <- c(rb_a,rc_a)
#     range <- c(min(all),max(all))
#     plot(rb_a,rc_a,col=sign(.r(Conviction))+3,xlim=range,ylim=range,xlab="risk budget",ylab="risk contribution",cex=abs(rw_a)/mean(abs(rw_a)),main="risk budget use (size prop to weight)")
#     abline(a=0,b=1,lty=3,lwd=0.25)
#     #active share
#     active_share<-0.5*sum(abs(rw_a))
#     legend("topleft",c("long","short"),col= c(4,2),pch=1,bty="n",cex=1)
#
#     #names vs weights and risk_contribution
#     const_hit <- const_hit1 + const_hit2
#     txtsize <- ifelse(length(rc_a)>40,0.8,1)
#     x <- barplot(rc_a,ylim=range,beside=TRUE,names.arg=.pT(Tick_a),las=2,cex.names =txtsize,main="risk contr")
#     text(x,0.0025+rc_a,labels=round(Conviction_a,0),cex=txtsize)
#     sumLong <- paste("LongConvictions:",round(sum(Conviction[Conviction>0]),1))
#     sumShort <- paste("ShortConvictions:",round(sum(Conviction[Conviction<0]),1))
#     ixConviction <- paste("IndexConviction:",round(Conviction[1],1))
#     legend("topleft",c("Conviction",sumLong,sumShort,ixConviction),text.font=c(1,2,2,2),box.lty=0,bg="transparent")
#     fig3main <- paste("active weights (",paste("active share: ",100*round(active_share,2),"%)"))
#     bp.rw <- barplot(rw_a,beside=TRUE,names.arg=.pT(Tick_a),las=2,cex.names=txtsize,main=fig3main,col=1+const_hit)
#     points(x=bp.rw,y=lb_a,pch=3)
#     legend("bottomright",c("","restricted"),col= c(1,2),pch=7,bty="n",cex=1)
#
#     #side constraints
#     sc <- c(IndexPosition = as.numeric(rw[1]),NetInv=sum(rw),
#             NetInvTol=ifelse(sum(rw)<0,-set$CashTolerance,set$LeverageTolerance),
#             TE_act = te, 'TEact-TEtarg' = te - te.targ,beta=P.Beta)
#     x <- barplot(sc,main=paste("The value of the objective function is",round(tv,3)),las=1)
#     text(x,sc-0.0025*sign(sc),labels=round(sc,3))
#
#     #overall title
#     set$algo <- as.character(set$algo)
#     set$CovCalcWay <- as.character(set$CovCalcWay)
#     set$CovReturns <- as.character(set$CovReturns)
#     set$InputParameters <- as.character(set$InputParameters)
#     mtext(paste("Calculated with the settings: ",set$setID,sep=""), outer=TRUE,line=2.5,cex=1.5)
#     ix.fl<-1:floor(length(set)/2) #columns of settings to plot in the first line below title
#     mtext(paste(names(set[ix.fl]),set[ix.fl],sep="=",collapse="; "), outer = TRUE,line=1, cex = 1)
#     mtext(paste(names(set[-ix.fl]),set[-ix.fl],sep="=",collapse="; "), outer = TRUE,line=0, cex = 1)
#   }
#
#   #example (needs an existing result in RCOres-form)
#   #rw <- RCOres$RW[,1] ; Tick <- names(rw); cov <- RCOres$COV[,,1]; lb <- RCOres$TARG[,"lb",1]; ub <- RCOres$TARG[,"ub",1]; Conviction <- RCOres$TARG[,"conviction",1];rb <- RCOres$TARG[,"RiskBudget",1]
#   #set <- RCOres$SET[1,,drop=FALSE] ; tv <- as.numeric(RCOres$Optim_Details["target_function_value",1])
#   #checkRCOplots(rw,cov,Conviction,lb,ub,rb,set,Tick,tv,ShortIndexWithOptimCash=0)
#
# }
#
# ###wrapper to plot call checkRCOplots from optimization output
# plotSingleSetIDfromRCOres<-function(setid,RCOres,covstr="used for opt") #covstr specifying which covariance to use for the calculation
# {
#   #get info from RCOres (output of the RCO function)
#   set <- RCOres$SET[RCOres$SET$setID==setid,]
#   rw <- RCOres$RW[,setid]
#   targv <- as.numeric(RCOres$Optim_Details["target_function_value",setid])
#
#   if(covstr=="used for opt")
#   { cov <- RCOres$COV[,,paste(set[c("CovCalcWay","CovReturns","Specif_COV")],collapse="_")]} else{
#     cov <- RCOres$COV[,,covstr]
#   }
#   cov <- f.matchCOVtoTickers(names(rw),as.data.frame(cov))
#
#
#
#   TickInputs <- as.data.frame(RCOres$TARG[,,set$InputParameters])
#
#   #recaclulate the effectively set boundaries
#   w_range <- f.getWeightRange(
#     TickInputs$lb,
#     TickInputs$ub,
#     TickInputs$conviction,
#     TickInputs$tradeability,
#     set$MaxRelWeight,
#     set$LowConvictionExitInDays,
#     set$ConvictionGroups
#   )
#   #call plot function
#   checkRCOplots(rw=rw,cov=cov,Conviction=TickInputs[,3],lb=w_range$w_min,ub=w_range$w_max,
#                 rb=TickInputs[,4],set=set,Tick=rownames(TickInputs),tv=targv)
#
#
#   #example (needs an existing result in RCOres-form)
#   # setid <- 1; RCOres <- RCOres;covstr <- "used for opt"
#   #plotSingleSetIDfromRCOres(setid,RCOres,covstr="used for opt")
#
# }
#

#
# ###compare RCOres in ggplot
# compareRCOruns <- function(RCOres,xaxis,yaxis,colby,pointat,Jitter=0.5,whichCov="used for opt")
# {
#   #parameters:
#   #pointat: which point should be bigger than the others,
#   #whichCov: if specified calculate all the optimization results characteristics with one COV (ie same COV-settings)
#   #whichCov <- "nlshrink_abs_NA"
#
#   if(whichCov == "used for opt")
#   {
#     OPTRES <- cbind(t(RCOres$Optim_Details),RCOres$SET)
#   }else
#   {
#
#     SET <- RCOres$SET
#
#     RW <- RCOres$RW
#     cov <- as.data.frame(RCOres$COV[,,whichCov])
#     cov <- f.matchCOVtoTickers(rownames(RW),cov)
#     OPTRES <- RCOres$Optim_Details; OPTRES[,] <- NA #output table
#     for(i in 1:nrow(SET))
#     {
#       set <- SET[i,]
#       targ <- RCOres$TARG[,,set$InputParameters]
#       rw <- RW[,i]
#       orc <- OptimizationResultsCharacteristics(rw=rw,cov=cov,lb=targ[,"lb"],ub=targ[,"ub"],set=set,rb_target=targ[,"RiskBudget"]) #rw=rw;COV=COV;lb=targ$lb.;ub=targ$ub;set=set
#       #store results in OPTRES
#       OPTRES[,i] <- c(Calculated=RCOres$Optim_Details["Calculated",i],
#                       orc$te-set$TargetTE,
#                       orc$const_hit_pct,
#                       orc$NetInvestment,
#                       orc$p.beta,orc$IndexPosition,
#                       RCOres$Optim_Details["target_function_value",i],
#                       RCOres$Optim_Details["iterations",i],
#                       RCOres$Optim_Details["calctime",i],
#                       orc$total_rb_dev,
#                       RCOres$Optim_Details["validResult",i]
#       )
#     }
#     OPTRES <- cbind(t(OPTRES),RCOres$SET)
#   }
#
#   OPTRES$DiffSet <- rep("",nrow(OPTRES))
#   for(i in colby)
#   {
#     OPTRES$DiffSet <- paste(OPTRES$DiffSet,as.factor(paste("_",substr(i,1,3),":",OPTRES[,i],";",sep="")),"")
#   }
#
#   library(ggplot2)
#   sz <- rep(3,nrow(OPTRES));sz[pointat]=10;
#   OPTRES[,yaxis] <- f.as.numeric.factor(OPTRES[,yaxis])
#   ggplot(OPTRES,aes(y = get(yaxis), x = jitter(get(xaxis),factor=Jitter), color = DiffSet,group = DiffSet,fill= DiffSet)) +
#     geom_line(size = 0.5) + geom_point(size = sz, shape=pmin(sz+18,23)) +
#     theme(legend.position="right",legend.direction="vertical") +
#     labs(x=xaxis,y=yaxis)
#
#   #example given RCOres
#   #compareRCOruns(RCOres,yaxis="total_rb_dev",xaxis="TargetTE",colby="setID",pointat=1,Jitter=0,whichCov="nlshrink_abs_NA")
#
# }
#
# #####further help functions#############
# #delete infeasible settings
# f.delete_infeasible_settings <- function(SET){
#   # Sort data frame with settings
#   SET <- SET[with(SET,order(InputParameters,Specif_COV)),]
#
#   # remove infeasible combinations
#   # soft leverage constrained: depends on Cash- and LeverageTolerance, needed in Settings to filter out algos that do not work!
#   # Soft leverage constrained does not work with Lagrangian optimizer (as Lagrangian uses an equality constraint!)
#   SET$SoftNetInvConstraint <- ifelse(SET$LeverageTolerance != 0 | SET$CashTolerance != 0, TRUE, FALSE)
#
#   algo_lagrange <- c("NLOPT_GD_AUGLAG_EQ", "NLOPT_LN_AUGLAG_EQ", "NLOPT_LD_AUGLAG_EQ")
#   algo_gradient <- c("NLOPT_GD_AUGLAG_EQ","NLOPT_LD_AUGLAG_EQ","NLOPT_LD_SLSQP")
#
#   if (any(SET$algo %in% algo_lagrange & SET$SoftNetInvConstraint)){
#     # it means that there is at least one infeasible combination
#     # delete such rows from data frame
#     ind_to_delete <- which(SET$algo %in% algo_lagrange & SET$SoftNetInvConstraint)
#
#     SET <- SET[-ind_to_delete, ]
#     n_del <- length(ind_to_delete)
#
#     print(paste( "Lagrangian Optimization does not work with Soft Leverage Constraint: removed",
#                  n_del,
#                  "settings"))
#   }
#
#   # Index equals Cash is only possible without IndexFlex and without Soft Leverage Constraint and needs a non-gradient algo
#   if(any((SET$algo %in% algo_gradient |
#           SET$SoftNetInvConstraint |
#           SET$IndexFlex) & SET$Ix_eq_Cash)) {
#
#     ind_to_delete <- which((SET$algo %in% algo_gradient |
#                               SET$SoftNetInvConstraint |
#                               SET$IndexFlex) & SET$Ix_eq_Cash)
#
#     SET <- SET[-ind_to_delete, ]
#     n_del <- length(ind_to_delete)
#
#     message(paste("Index equals Cash is only possible without IndexFlex and without Soft Leverage Constraint and needs a non-gradient algo : removed", n_del, "settings"))
#   }
#
#   # NetInv-Lambda > 0 only makes sense with SoftNetInvConstraint
#   # TODO check the condition
#   stopifnot(all(SET$NetInvLambda >= 0)) # the lambda must not be < 0
#   if (any(SET$NetInvLambda > 0 & SET$SoftNetInvConstraint == FALSE))
#   {
#     ind_to_delete <- which((SET$NetInvLambda > 0 & SET$SoftNetInvConstraint == FALSE))
#     SET <- SET[-ind_to_delete, ]
#     n_del <- length(ind_to_delete)
#
#     message(paste("NetInv-Lambda > 0 only makes sense with SoftNetInvConstraint. removed", n_del, "settings"))
#   }
#
#   return(SET)
# }
#
#


#
# #short tickers for printing in plots
# .pT <- function(x,start=0,stop=4){sapply(x,substr,start,stop)}
#

#
# ######READ AND WRITE FILES/RESULTS#################
#


#

#
#
# f.writeRCOsettingsToSQL <- function(set,verbose=FALSE)
# {
#   ipak("RODBC")
#   con <- FAFunc.GetDB()
#
#   #insert whole settingsSET in tempTable (to check in sql if settings are new!)
#   #delete existing table
#   temptable <- "betsizing.CalculationRCOCurrentSettingSetUnsaved"
#   dummydf <- data.frame(ParameterValue="dummyv")
#   rownames(dummydf) <- "TargetTE"
#   sqlSave(con, dummydf,temptable,append=TRUE , rownames="FK_ParameterName")
#   selstr = paste("Delete FROM " , temptable,sep="")
#   sqlQuery(con, selstr)
#   if(verbose) print(paste("cleared temporary GlobalSettings table:",temptable))
#   #fill in chosen settings
#   df.set <- data.frame(FK_ParameterName=colnames(set),ParameterValue = set)
#   rownames(df.set) <- c()
#   df.s <- data.frame(t(set))
#   colnames(df.s) <- "ParameterValue"
#   df.s  <- df.s[!(row.names(df.s) %in% c("LbMax","InputParameters","Specif_COV","setID","SoftLeverageConstrained","CovCalcWay","CovReturns","CovID")),,drop=FALSE]
#   saved <- sqlSave(channel=con, dat = df.s, tablename=temptable, append=TRUE , rownames="FK_ParameterName")
#   if(verbose & saved==1) print("successfully inserted the used GlobalSettings in SQL-Temporary-Table")
#   stopifnot(saved==1)
#   #exec sql procedure to give back correspondent GlobalSettingSetID (and create a new one if required)
#   execstr <- "exec betsizing.uspCalculationRCO_LookupOrAddNewSettingSet"
#   QueryResult <-  sqlQuery(con, execstr)
#   stopifnot(is.na(QueryResult)==FALSE)
#   if(verbose) print(paste("successfully inserted new OR linked used Global settings with existing GLobalSettingSetID: ",QueryResult))
#
#   return(QueryResult)
#   #example (needs an existing result in RCOres-form)
#   #set <- RCOres$SET[1,];verbose=TRUE
#   #(r <- f.writeRCOsettingsToSQL(set,verbose=FALSE))
#
# }
#
#
#
#
# #########Output optdetails to xlsx
# f.writeOptdetails2xlsx <- function(RCOres,Portfolio,Calcdate="",ShortIndexWithOptimCash=0,ZeroNetInvestment=FALSE,OnlyID=FALSE)
# {
#   ipak("xlsx")
#   SET <- RCOres$SET
#   OPTRES <- rbind(RCOres$Optim_Details, RCOres$RW)
#   TARG <- RCOres$TARG
#
#
#   if(is.numeric(OnlyID))
#   {
#     stopifnot(any(OnlyID %in% SET$setID))
#     print(paste("output only id",OnlyID))
#     SET <- SET[SET$setID==OnlyID,,drop=FALSE]
#     OPTRES <- OPTRES[,colnames(OPTRES)==paste("set",OnlyID,sep=""),drop=FALSE]
#   }
#
#   trSET<-t(SET)
#   rownames(trSET) <- paste("set",colnames(SET))
#   colnames(trSET) <- colnames(OPTRES)
#   sec_nr <- length(TARG[,1,1])
#
#   if(ShortIndexWithOptimCash == 1) #implement Index Position in output if desired so
#   {
#     l <- nrow(OPTRES)
#     ix_sec <- (l-sec_nr+1):l
#     w_bm <- -TARG[-1,"lb",1]
#     for (i in 1:ncol(OPTRES))
#     {
#       OPTRES[ix_sec,i] <- ImplIndexPosition(as.numeric(OPTRES[ix_sec,i]),w_bm,ZeroNetInvestment)
#     }
#   }
#   pr.OPTRES<-rbind(as.matrix(trSET),as.matrix(OPTRES))
#   rownames(pr.OPTRES)<-c(rownames(pr.OPTRES)[1:(nrow(pr.OPTRES)-sec_nr)],names(TARG[,1,1]))
#
#   file_path_out <- paste("G:/FAP/Equities/Betsizing/R_results/",Portfolio,"/",sep="",collapse="")
#   write.xlsx(pr.OPTRES,paste(file_path_out,Portfolio,"_",Calcdate,"_optdetails.xlsx",sep="",collapse=""),sheetName="R.output")
#
#   #example
#   #RCOres <- RCOres; Portfolio <- "RE_EU";Calcdate="";ShortIndexWithOptimCash=0;ZeroNetInvestment=FALSE;OnlyID=FALSE
#   #f.writeOptdetails2xlsx(RCOres,Portfolio,Calcdate,ShortIndexWithOptimCash,ZeroNetInvestment,OnlyID)
#
#
#
# }
#
#
#
#
#
#
#
#
#
# #FAFunc
#
#
# #####ipak function: install and load multiple R packages.####
# # check to see if packages are installed. Install them if they are not, then load them into the R session.
# # alternatively require() function can be used
# ipak <- function(pkg) {
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#   if (length(new.pkg))
#     install.packages(new.pkg, dependencies = TRUE)
#
#
#   sapply(pkg, require, character.only = TRUE) #require() is equivalent to library()
# }
#
# # usage and load packages... todo: only load those which are really needed
# #packages <-
# #  "RODBC" #c("gridExtra", "nloptr", "dplyr", "reshape", "xlsx") #"ggplot2","xlsx", "reshape2",  "microbenchmark","nlshrink","RODBC"
# #ipak(packages)
# #options(java.parameters = "-Xmx1024m")
#

#
# ####Connect to db ######
# FAFunc.GetDB <- function(database = "Aktienmodell") {
#
#   ipak("RODBC")
#
#   odbcDriverConnect(
#     connection = paste(
#       "Driver={SQL Server};server=sqltara;database=",
#       database,
#       ";trusted_connection=yes;",
#       sep = "",
#       collapse = ""
#     )
#   )
#
# }
#
# ####GET BLOOMBERG-TICKERS-Data######
# #sfb 2019-03-19: when only one ticker is passed, bb returns a data.frame and not a list. we have to convert it to list
# FAFunc.GetFromTickers <-   function(output.as = c("p", "r"),
#                                     Tickers,
#                                     periodicity = "DAILY",
#                                     end_date = Sys.Date(),
#                                     start_date = end_date - 252,
#                                     missing.values = NA,
#                                     printout=FALSE)   {
#   ipak(c("Rblpapi"))
#
#   if (class(Tickers) != "character") {
#     stop("Ticker must be specified as character string")
#   }
#   opt <- c("periodicitySelection" = periodicity)
#
#   print("connecting to bb...")
#   blpConnect()
#   pd <-
#     bdh(Tickers, "px_last", start_date, end_date, options = opt)
#
#   print("output from bb...")
#   if(printout) print(pd)
#
#   #checks
#   sec.nr <- length(Tickers)
#
#   #when only one ticker is queried, bb returns a data.frame and not a list. we convert it to a list to make use of the subsequent code
#   if (class(pd) == "data.frame") {
#     pd <- list(pd)
#     names(pd) <- Tickers[1]
#   }
#
#   #check if input ticker count match returned ticker count
#   if (sec.nr != length(pd)) {
#     stop("not all tickers have been loaded")
#   } else      {
#     print("ok")
#   }
#
#   datapoints.nr <- rep(0, sec.nr)
#   #print(sec.nr)
#   for (i in 1:sec.nr)
#   {
#     #assign the number of datapoints loaded for each security
#     datapoints.nr[i] <- length(pd[[Tickers[i]]][, 2])
#   }
#   #print(Tickers)
#   #print(datapoints.nr)
#   if (min(datapoints.nr) != max(datapoints.nr))
#   {
#     print(
#       paste(
#         "not all time series are  of the same lengths. missing values are set to ",
#         missing.values
#       )
#     )
#   }
#
#   d.nr <-
#     max(datapoints.nr)  #assign the total datapoints (ie the one for the longest series, should be equal to the one of the 1st series)
#
#   if (length(data.frame(pd[Tickers[1]])[, 1]) != d.nr) {
#     stop("the first series' dates are used for all others BUT shorter than at least one other")
#   }
#
#   #create output matrices prices and returns with default values
#   prices <-
#     data.frame(matrix(rep(missing.values, (d.nr) * sec.nr), d.nr , sec.nr))
#   Tickers <- as.character(Tickers)
#   colnames(prices) <- Tickers
#   rownames(prices) <- data.frame(pd[Tickers[1]])[, 1]
#
#   returns <-
#     data.frame(matrix(rep(missing.values, (d.nr - 1) * sec.nr), d.nr - 1, sec.nr))
#   colnames(returns) <- Tickers
#   rownames(returns) <-
#     data.frame(pd[Tickers[1]])[2:datapoints.nr[1], 1]
#
#
#   print(sec.nr)
#   #put prices and calculated returns in the output matrices and set missing values where necessary
#   for (i in 1:sec.nr)
#   {
#     if (d.nr > datapoints.nr[i])
#     {
#       lacking <- d.nr - datapoints.nr[i]
#       prices[(lacking + 1):d.nr, Tickers[i]] <-
#         data.frame(pd[Tickers[i]])[, 2]
#       returns[(lacking + 1):(d.nr - 1), Tickers[i]] <-
#         data.frame(pd[Tickers[i]])[2:(d.nr - lacking), 2] / data.frame(pd[Tickers[i]])[1:(d.nr -
#                                                                                             lacking - 1), 2] - 1
#       print(
#         paste(
#           Tickers[i],
#           ": replaced ",
#           lacking,
#           "lacking prices with the default value ",
#           missing.values
#         )
#       )
#
#       #check if the dates match with those of the first entry as assumed
#       if (all(data.frame(pd[Tickers[1]])[(lacking + 1):(d.nr), 1] !=  data.frame(pd[Tickers[i]])[, 1]))
#       {
#         print(
#           paste(
#             "dates dont match for security: ",
#             Tickers[i],
#             " consider choosing other tickers or another periodicity. output replaced by NAs to avoid inconsistencies"
#           )
#         )
#         prices[, Tickers[i]] <- rep(NA, d.nr)
#         returns[, Tickers[i]] <- rep(NA, d.nr - 1)
#       }
#     }
#     if (d.nr == datapoints.nr[i])
#     {
#       prices[, Tickers[i]] <- data.frame(pd[Tickers[i]])[, 2]
#       returns[, Tickers[i]] <-
#         data.frame(pd[Tickers[i]])[2:d.nr, 2] / data.frame(pd[Tickers[i]])[1:d.nr -
#                                                                              1, 2] - 1
#
#       #check if Tickers without prices (probably wrong set)
#       if (any(datapoints.nr == 0))
#       {
#         stop(
#           paste(
#             "The following Tickers dont find ANY prices. Check spelling or delete.:",
#             Tickers[datapoints.nr == 0]
#           )
#         )
#       }
#
#
#       #check if the dates match with those of the first entry as assumed
#       if (all(data.frame(pd[Tickers[1]])[, 1] != data.frame(pd[Tickers[i]])[, 1]))
#       {
#         print(
#           paste(
#             "dates dont match for security: ",
#             Tickers[i],
#             " . consider choosing other tickers or another periodicity. output replaced by NAs to avoid inconsistencies"
#           )
#         )
#         prices[, Tickers[i]] <- rep(NA, d.nr)
#         returns[, Tickers[i]] <- rep(NA, d.nr - 1)
#       }
#     }
#
#   }
#
#
#   if (output.as == "p") {
#     return(prices)
#   }
#   if (output.as == "r") {
#     return(returns)
#   }
#
# }
#
# ####FindCovarianceMatrix in DB#####
# #Find the newest Covariance Matrix in the DB that is calculated with the chosen Returns and by the chosen Calculation Method.
# #optional argument: 'NotNeverThan' set to a date value to get the newest CovarianceMatrix before that date
#
# FAFunc.FindNewestCovInDB <-
#   function(RelOrAbsReturn,
#            Portfolioname,
#            NotNeverThan = Sys.Date())
#   {
#     ipak("RODBC")
#     #checks
#     if (any(RelOrAbsReturn %in% c("abs", "rel")) == FALSE)
#     {
#       message(
#         paste(
#           "The parameter RelOrAbsReturn is set to '",
#           RelOrAbsReturn,
#           "' only 'rel' or 'abs' are allowed",
#           sep = ""
#         )
#       )
#     }
#
#
#     #Create query according to specification
#
#     #create query depending on Cov-return to use
#     if (RelOrAbsReturn == "abs")
#     {
#       t.query.p1 <- paste(
#         "SELECT max (x.RunID) maxRunID ",
#         "FROM betsizing.CovarianceMatrixCalculationParams x ",
#         "left outer join (select * from betsizing.CovarianceMatrixCalculationParams ",
#         "where ParamName='relativeTo') xcm ",
#         "on xcm.RunID=x.RunID ",
#         "left outer join (select * from betsizing.CovarianceMatrixCalculationParams ",
#         "where ParamName='CalcDT') xdate ",
#         "on xdate.RunID=x.RunID ",
#         "where xcm.ParamName is null ",
#         sep = ""
#       )
#     }
#
#     else if (RelOrAbsReturn == "rel")
#     {
#       t.query.p1 <- paste(
#         "SELECT max (x.RunID) maxRunID ",
#         "FROM betsizing.CovarianceMatrixCalculationParams x ",
#         "left outer join (select * from betsizing.CovarianceMatrixCalculationParams ",
#         "where ParamName='relativeTo') xcm ",
#         "on xcm.RunID=x.RunID ",
#         "left outer join (select * from betsizing.CovarianceMatrixCalculationParams ",
#         "where ParamName='CalcDT') xdate ",
#         "on xdate.RunID=x.RunID ",
#         "where xcm.ParamName is not null ",
#         sep = ""
#       )
#     }
#
#
#     #add to query the selected 'date before' and the selected portfolio
#     t.query <- paste(
#       t.query.p1,
#       "AND x.ParamName = 'porName' AND x.ParamValue = '",
#       Portfolioname,
#       "' ",
#       #"AND xdate.ParamValue  <= ", NotNeverThan,###still needs programming, handle the date values!!!!
#       sep = ""
#     )
#
#     #Execute Query and return the COVID
#     con <- FAFunc.GetDB()
#     maxRunID <- as.numeric(sqlQuery(con, t.query))
#     if (is.na(maxRunID)) {
#       stop(
#         "No Covariance matrix as specified was calculated yet for the given portfolioname and return calc method"
#       )
#     }
#     return(maxRunID)
#   }
#
#
#
#
#
# ####Get Covariance Matrix for a set of Repnos#####
#
# #Find the Covariance Matrix for a set of repnos
# FAFunc.GetCov4Repnos <- function(Repnos, CovarianceID, CovCalcWay)
# {
#
#   #checks
#   AllowedCovCalcWays <- c("nlshrink", "cov")
#   if (any(CovCalcWay %in% AllowedCovCalcWays) == FALSE)
#   {
#     message(
#       paste(
#         "The parameter CovCalcWay is set to '",
#         CovCalcWay ,
#         "' only these values are allowd: ",
#         sep = ""
#       )
#     )
#     (AllowedCovCalcWays)
#   }
#
#   #get Covariance
#   #define sql query
#   ipak("RODBC")
#   t.query <-
#     paste(
#       "SELECT c.Ticker1, c.Ticker2, c.Covariance, v1.RepNo as 'Repno1',v2.RepNo as 'Repno2'",
#       " FROM betsizing.CovarianceMatrixFlattened  c ",
#       "inner join Aktienmodell.tickermappings.vRepnosToSCDextended v1 ",
#       "on c.Ticker1=v1.BloombergTicker or c.Ticker1=v1.BloombergTicker2 ",
#       "inner join Aktienmodell.tickermappings.vRepnosToSCDextended v2 ",
#       "on c.Ticker2=v2.BloombergTicker or c.Ticker2=v2.BloombergTicker2 ",
#       "where c.RunID= ",
#       CovarianceID,
#       " and CalculationMethod='",
#       CovCalcWay,
#       "'",
#       sep = ""
#     )
#
#
#   #get cov from sql
#   con <- FAFunc.GetDB()
#   cov.t <- sqlQuery(con, t.query)
#
#   #convert id_columns from factors to characters
#   cov.t$Repno1 <- as.character(cov.t$Repno1)
#   cov.t$Repno2 <- as.character(cov.t$Repno2)
#   targ$Repno <- as.character(targ$Repno)
#
#
#   #ensure all the target Repnos  exist in the covariance matrix
#   for (i in 1:sec_nr)
#   {
#     if (any(cov.t$Repno1 == targ$Repno[i]) == FALSE) {
#       stop(
#         paste(
#           "the Ticker ",
#           targ$Ticker[i],
#           " is not defined in the corresponding Universe - add to get Covariance",
#           sep = ""
#         )
#       )
#     }
#   }
#
#
#   #form covariance matrix as in tickers (by repno to avoid ticker differences due to different exchange codes)
#
#   COV <- matrix(rep(0, sec_nr ^ 2), sec_nr)
#   rownames(COV) <- targ$Ticker
#   colnames(COV) <- t(targ$Ticker)
#
#   for (i in 1:sec_nr)
#   {
#     for (j in 1:sec_nr)
#     {
#       if (j >= i)
#       {
#         #message(paste("i=",i,",j=",j))
#         COV[i, j] <-
#           cov.t[cov.t$Repno1 %in% targ$Repno[i] &
#                   cov.t$Repno2 %in% targ$Repno[j], 3][1]
#         COV[j, i] <- COV[i, j]
#       }
#     }
#   }
#
#   return(COV)
# }
#
#
# ####add n monthes to a date#####
# add.month <-
#   function(date, n) {
#     seq(date, by = paste(n, "months"), length = 2)[2]
#   }
#
#
#
# ####install zip file from any folder########
# #installFromZip <- function()
# #{install.packages(file.choose(), repos = NULL, type="source")}
#
#
#
# ####get libraries from central folder /backup of central folder as zip########
# #syncLibraries TODO
# #FAFunc.ZipLibsBackup <- function(ZipPath="G:/FAP/Equities/Betsizing/Code/RLibs/test",saveLocation="G:/FAP/Equities/Betsizing/Code/RLibsBackups/",filename="RLibs")
# # {
# #  .libPaths("C:/Rtools/bin")
# #
# #   filename <- paste(filename,Sys.Date(),sep="_")
# #   zipPath <- zip(files=ZipPath,zipfile=paste(saveLocation,filename,sep=""))
# #}
# #FAFunc.ZipLibsBackup(saveLocation="C:/Users/scz/AppData/Local/Temp/",filename="test")
#
# FAFunc.SyncLibraries <- function(syncToPath = .libPaths()[1], syncFromPath= "G:/FAP/Equities/Betsizing/Code/RLibs")
# {
#   #Diese Funktion soll anstatt Packages neu zu installieren, alle lokalen Packages lÃ¶schen
#   #und vom zentralen Directory auf G alle vorhandenen Packages rÃ¼berzukopieren
#   #die Lokale Instanz greift danach nur auf die lokalen Packages zu (zusÃ¤tzliche Packages kÃ¶nnen nac1h belieben dann ebenfalls lokal installiert werden)
#
#   #Assertions
#   stopifnot(substr(syncToPath,1,3)== "C:/" )
#
#   l <- nchar(syncToPath)
#   stopifnot(substr(syncToPath,l-5,l) == "/RLibs")
#
#   #Sync Libraries
#   print(paste("All packages in:",syncToPath,"are going to be deleted and replaced by the packages in:",syncFromPath))
#
#   oldPaths <- .libPaths()
#
#   unlink(syncToPath, recursive = TRUE) #would be nicer to just empty the folder
#   syncToPath <- substr(syncToPath,1,l-6) #would be nicer to just remove the last folder instead of l-6 (just deleted)
#   file.copy(file.path(syncFromPath), to = syncToPath, recursive = TRUE)
#
#   .libPaths(oldPaths)
#   print("Libraries were sycnronized")
#
# }



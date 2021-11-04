#!/usr/bin/R

pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

pkgTest("fredr")
pkgTest("RColorBrewer")
pkgTest("reshape")
pkgTest("quantmod")
pkgTest("zoo")
pkgTest("dplyr")
pkgTest("forecast")
pkgTest("lubridate")
pkgTest("httr")
pkgTest("jsonlite")
pkgTest("rtweet")
pkgTest("httr")
pkgTest("tis")
pkgTest("tm")
pkgTest("gmailr")

setwd("~/Documents/R/")

# Test that FRED is online ####
test.online        <- try(getSymbols("GDPC1",src='FRED'))

if ((inherits(test.online, "try-error"))) {
  stop("\n\n\n----------\n Not online \n----------\n\n")
}

rm(list=ls())

## Credentials

FRED_API_KEY <- ""  # empty string - the "Creditials.R" file contains the FRED access code
source("Credentials.R")   # file where credentials are stored - "bring your own"
fredr_set_key(FRED_API_KEY)


# Data Download

Data.US <- c("US.GDP.Real"="GDPC1", 
             "US.GDP.PCE"="PCECC96",
             "US.GDP.PCE.Goods"="DGDSRX1Q020SBEA",
             "US.GDP.PCE.DurableGoods"="PCDGCC96",
             "US.GDP.PCE.NondurableGoods"="PCNDGC96",
             "US.GDP.PCE.Services"="PCESVC96",
             "US.GDP.Investment"="GPDIC1",
             "US.GDP.Investment.Fixed"="FPIC1",
             "US.GDP.Investment.Nonresidential"="PNFIC1",
             "US.GDP.Investment.Residential"="PRFIC1",
             "US.GDP.Inventories"="CBIC1",
             "US.GDP.NetExports"="NETEXC",
             "US.GDP.Government"="GCEC1",
             "US.GDP.Government.Federal"="FGCEC1",
             "US.GDP.Government.StateLocal"="SLCEC1",
             "US.GDP.FinalSales"="FINSLC1",
             "US.Survey.Empire"="GACDINA066MNFRBNY", 
             "US.IP"= "INDPRO", 
             "US.Payroll"="PAYEMS",
             "US.IP.CapacityUtilization" = "TCU",
             "US.Unemployment"="UNRATE", "US.Unemployment.U1" = "U1RATE", "US.Unemployment.U6" = "U6RATE", "US.Unemployment.U2" = "U2RATE",
             "US.Unemployment.PartTimeEconomicReasons" = "LNS12032194", "US.Unemployment.PartTimeNonEconomicReasons" = "LNS12032200",
             "US.Unemployment.MarginallyAttached" = "LNU05026642",
             "US.Unemployment.ParticipationRate"="CIVPART",
             "US.Unemployment.EmploymentToPopulation"="EMRATIO",
             "US.Activity.ChicagoFed.Employment" = "EUANDH",
             "US.Activity.ChicagoFed" = "CFNAI",
             "US.Activity.PhillyFed.Current" = "USPHCI",
             "US.Activity.PhillyFed.Leading" = "USSLIND",
             "US.Activity.NYFed.Current" = "GACDISA066MSFRBNY",
             "US.Activity.NYFed.Leading" = "GAFDISA066MSFRBNY",
             "US.Activity.RetailSales" = "RSAFS",
             "US.Activity.RetailSalesExAuto" = "RSFSXMV",
             "US.Activity.ADP" = "NPPTTL",
             "US.Activity.InitialClaims" = "ICSA",
             "US.Activity.ContinuedClaims.4W.MA" = "CC4WSA",
             "US.Activity.ContinuedClaims" = "CCSA",
             "US.JOLTS.QuitsRate" = "JTSQUR",
             "US.JOLTS.HireRate" = "JTSHIR",
             "US.JOLTS.JobOpeningsRate" ="JTSJOR",
             "US.Unemployment.WageGrowth" = "CES0500000003",
             "US.CPI.Headline"="CPIAUCSL", 
             "US.CPI.Core"="CPILFENS",
             "US.SOV.1Y"="DGS1", 
             "US.SOV.2Y"="DGS2", 
             "US.SOV.3Y"="DGS3", 
             "US.SOV.5Y"="DGS5", 
             "US.SOV.7Y"="DGS7", 
             "US.SOV.10Y"="DGS10", 
             "US.SOV.20Y"="DGS20", 
             "US.SOV.30Y"="DGS30", 
             "US.FSI.Cleveland"="CFSI",
             
             "US.Transportation.Rail" = "RAILFRTINTERMODALD11",
             "US.Transportation.Railpassenger" = "RAILPMD11",
             "US.Transportation.Air" = "LOADFACTORD11",
             "US.Transportation.Airtraffic.Passenger" = "ASMD11",
             "US.Transportation.Airtraffic.International" = "ASMI",
             "US.Transportation.Index" = "TSIFRGHT",
             "US.Transportation.PublicTransit" = "TRANSITD11",
             
             "US.HouseholdDebt" = "HDTGPDUSQ163N",
             "US.Housing.NewPrivateHousingStarts"="HOUSTNSA",
             "US.Housing.NewPrivateHousingPermits"="PERMIT",
             "US.Housing.NewPrivateHousingConstruction"="UNDCONTNSA",
             "US.Housing.NewPrivateHousingCompleted"="COMPUTSA",
             "US.Housing.NewPrivate1UnitCompleted"="COMPU1USA",
             "US.Housing.NewPrivate2UnitCompleted"="COMPU24USA",
             "US.Housing.NewPrivate5UnitCompleted"="COMPU5MUSA",
             "US.Housing.30YMortgageRate"="MORTGAGE30US",
             "US.Housing.NewHomeSales" = "HSN1F",
             "US.Housing.ExistingHomeSales" = "EXHOSLUSM495S",
             "US.Housing.MonthlySupply"="MSACSR",
             "US.Housing.AllTransactionsPriceIndex"="USSTHPI",
             "US.Housing.CaseShiller" = "CSUSHPINSA",
             
             "US.Auto.Autosales" = "ALTSALES",
             "US.Auto.LoansSecuritized" = "MVLOAS",
             "US.Auto.InventorySalesRatio" = "AISRSA",
             "US.Auto.MilesTraveled" = "M12MTVUSM227NFWA",
             
             "US.H8.BanksCredit.Securities" = "SBCACBW027SBOG",
             "US.H8.BanksCredit.LoansLeases" = "TOTLL",
             "US.H8.BanksCredit.Allowance" = "ALLACBW027SBOG",
             "US.H8.InterBankLoans" = "LCBACBW027SBOG",
             "US.H8.FedFundsSold" = "H8B3092NCBA",
             "US.H8.Cash" = "CASACBW027SBOG",
             "US.H8.TradingAssets" = "H8B3053NCBA",
             
             "US.H8.BanksCredit.Securities.Treasuries" = "TASACBW027SBOG",
             "US.H8.BanksCredit.Securities.Other" = "OSEACBW027SBOG",
             "US.H8.BanksCredit.CI" = "TOTCI",
             "US.H8.BanksCredit.RE" = "RELACBW027SBOG",
             "US.H8.BanksCredit.Consumer" = "CLSACBW027SBOG",
             "US.H8.BanksCredit.Other" = "AOLACBW027SBOG",
             
             "US.FDIC.NetChargeOffRateTotalLoans" = "QBPLNTLNNTCGOFFR",
             "US.FDIC.LoanLossProvisions" = "QBPQYLNLOSS",
             "US.FDIC.UnprofitableInstitutions" = "QBPQYNUMINSTUNPR",
             "US.FDIC.InstitutionsWithEarningsGain" = "QBPQYNUMINSTYY",
             
             "US.GasPrices" = "GASALLW",
             "US.EconomicPolicyUncertaintyIndex" = "USEPUINDXD",
             "US.Commodities.Oilprices" = "DCOILWTICO",
             
             "EU.GDP.Real"="CLVMNACSCAB1GQEA19",
             "UK.GDP.Real"="NAEXKP01GBQ652S",
             "CA.GDP.Real"="NAEXKP01CAQ189S",
             "JP.GDP.Real" = "NAEXKP01JPQ661S"
             
)


Data.Description <- data.frame( 
  Mnemonic = names(Data.US), 
  Code = Data.US,
  Description = "", 
  Frequeny = "",
  Units = "",
  LastUpdate = ""
  )
  
for ( idx in 1:nrow(Data.Description) ) {
  cat(paste("  - ", Data.Description[idx,1], "\n"))
  x = fredr_series(series_id = Data.Description[idx,2])
  Data.Description[idx, 3 ] = x$title
  Data.Description[idx, 4 ] = x$frequency
  Data.Description[idx, 5] = x$units
  Data.Description[idx, 6] = x$last_updated
}


## Misc. functions

misc.FREDdowload <- function(series) {
  
  if (!(exists(series))) {
    cat("\n       Downloading", series)
    x = fredr( series_id = Data.Description[Data.Description$Mnemonic == series,2] )
    x = zoo(x$value, as.Date(x$date))
    if ((series == "US.H8.FedFundsSold") || (series == "US.H8.TradingAssets")) x<- x / 1000
  } else {
    
    cat(series, "exists \n")
    assign("x", get(series))
  }
  
  assign(series, x, envir = .GlobalEnv)
  return(x)
}


misc.NBER.Recessions <- function(){
  NBER.Recessions <- as.Date(as.character(t(nberDates())), format="%Y%m%d")
  Limits <- par('usr')
  for (idx in seq(1, length(NBER.Recessions), 2) ) {
    rect(NBER.Recessions[idx], Limits[3], NBER.Recessions[idx+1], Limits[4], col="#0000FF19", lty=0)
  }
}


# Charting functions

misc.GDPExpansionPlot <- function( series ){
  
    x = misc.FREDdowload(series = series)
    
    NBER.Recessions <- as.Date(as.yearqtr(as.Date(as.character(t(nberDates()[,1])), format="%Y%m%d")))
    NBER.Recessions <- NBER.Recessions[year(NBER.Recessions) > 1958]
    
    x <- data.frame( x, StartExpansion = 0)
    
    idx.Expansion                   <- as.Date(rownames(x)) %in% (NBER.Recessions %m-% months(3))
    x$StartExpansion[idx.Expansion] <- 1
    x$StartExpansion                <- cumsum(x$StartExpansion)
    
    col.plot     <- c( brewer.pal((max(x$StartExpansion)+1), "Paired"), "black")
    type.plot    <- c(rep(1,max(x$StartExpansion)),2,2)
    width.plot   <- c(rep(2,max(x$StartExpansion)),5,1)
    legend.plot  <- character()
    
    # Find min and max values for the plot
    plot.ylim.min <- 100
    plot.ylim.max <- 100
    for (idx.plot in 1:max(x$StartExpansion)) {
      tmp.plot <- x[x$StartExpansion == idx.plot,]
      tmp.plot[,1] <- 100 * ( tmp.plot[,1] / tmp.plot[1,1] )
      if (min(tmp.plot[,1], na.rm=TRUE) < plot.ylim.min ) plot.ylim.min <- min(tmp.plot[,1], na.rm=TRUE)
      if (max(tmp.plot[,1], na.rm=TRUE) > plot.ylim.max ) plot.ylim.max <- max(tmp.plot[,1], na.rm=TRUE)
      cat("\n ylim.max", plot.ylim.max)
    }
    
    if (series == "US.Payroll") {
      xlim.plot  <- c(1, 120)
      xlim.label = "Months Since Pre-Recession Peak (source: FRED, own calculations)"
    } else { 
      xlim.plot  <- c(1, 40) 
      xlim.label = "Quarters Since Pre-Recession Peak (source: FRED, own calculations)"
      }
    
    ylim.plot    <- c(plot.ylim.min, plot.ylim.max)
    
    tmp.plot <- x[x$StartExpansion == 1,]
    
    legend.plot  <- c(legend.plot, format(as.Date(rownames(tmp.plot)), format = "%B %Y")[2])
    tmp.plot[,1] <- 100 * ( tmp.plot[,1] / as.numeric(tmp.plot[1,1] ))
    
    plot(zoo(tmp.plot[,1]), type="l", lwd=2, col=col.plot[1], lty=type.plot[1], cex.main = .8,
         main=paste0("Comparing Recoveries: \n", Data.Description[Data.Description$Mnemonic == series,3]), 
         ylab="Cumulative Growth (in %) Since Recession", xlab=xlim.label,
         ylim=ylim.plot,
         xlim=xlim.plot,
    )
    
    for (idx.plot in 2:max(x$StartExpansion)) {
      tmp.plot <- x[x$StartExpansion == idx.plot,]
      
      legend.plot <- c(legend.plot, format(as.Date(rownames(tmp.plot)), format = "%B %Y")[2])
      tmp.plot[,1] <- 100 * ( tmp.plot[,1] / as.numeric(tmp.plot[1,1] ))
      lines(zoo(tmp.plot[,1]), lwd=width.plot, col=col.plot[idx.plot-3], lty=type.plot[idx.plot-3])
    }
    
    
    points( nrow(tmp.plot),      tail(tmp.plot,1)[1], col = "black", pch = 19, lwd = 3)
    if (series == "US.GDP.Real" ) text(   nrow(tmp.plot),  1.2*tail(tmp.plot,1)[1], paste0("After ", nrow(tmp.plot), " Quarters, the Latest Obs. Stands at \n", round(tail(tmp.plot,1)[1],1), "% of the Pre-Recession Peak"), font = 2)
    if (series == "US.Payroll" ) text(   nrow(tmp.plot),  1.2*tail(tmp.plot,1)[1], paste0("After ", nrow(tmp.plot), " Months, the Latest Obs. Stands at \n", round(tail(tmp.plot,1)[1],1), "% of the Pre-Recession Peak"), font = 2)
    
    
    abline(h=100, col="black", lty=3)
    grid()
    
    if (!(series == "US.Payroll")) {
      plot.trend <- 100*(1.03^(1/4))^seq(0, 40)
      lines(plot.trend, lty=3, lwd=3, col=last(col.plot)) 
      legend.plot <- c(legend.plot, "3% Trendline")
    }
    
    legend("topleft", legend.plot, fill=col.plot, cex=0.75, bty = "n")
  
}


Chart.Panel <- function(x, series) {
  
  chart.col   = brewer.pal(6, "Paired")[6]
  chart.ylim  = c(0.9*range(x, na.rm=TRUE)[1], 1.1*range(x, na.rm=TRUE)[2])
  chart.title = Data.Description[Data.Description$Mnemonic == series,3]
  
  plot(as.zoo(x), main = chart.title, 
       col = chart.col, 
       lwd = 3, 
       ylim = chart.ylim,
       ylab = Data.Description[grep(series, Data.Description[,1]),5],
       xlab = paste0("Period: ", year(index(x[1])), " - ", year(index(tail(x[1]))), " (shaded areas indicate U.S. recessions)"))
  
  points( index(tail(x, 1)), tail(x,1), col = chart.col, pch = 19, lwd = 5)
  abline(v = as.Date(paste0(seq( year(index(x[1])), year(Sys.Date()), 1), "-01-01")), lty = 3, lwd = 0.5)
  misc.NBER.Recessions()
  
  text(index(tail(x,round(.15*length(x)))[1]), 1.1*tail(x,1), paste("Latest obs.\n", round(tail(x,1),2)), font = 2)
  grid()
  
}


Chart.Single <- function(series, periods, tweet = FALSE, tweet.text = ""){
  
  x = misc.FREDdowload(series = series)
  x = x[year(index(x))>(year(Sys.Date())-periods)]
  
  if (tweet) {
    tmp <- tempfile(fileext = ".png")
    png(tmp, 12, 12, "in", res = 127.5)
  } else png(filename=paste0(series, ".png"))
  
  chart.col   = brewer.pal(6, "Paired")[6]
  chart.ylim  = c(0.9*range(x, na.rm=TRUE)[1], 1.1*range(x, na.rm=TRUE)[2])
  chart.title = Data.Description[Data.Description$Mnemonic == series,3]
  
  plot(as.zoo(x), main = chart.title, 
       #ylab = "", 
       col = chart.col, 
       lwd = 3, 
       ylim = chart.ylim,
       ylab = "Source: FRED, Federal Reserve Bank of St. Louis",
       xlab = paste0("Period: ", year(index(x[1])), " - ", year(index(tail(x,1))), " (shaded areas indicate U.S. recessions)"))
  points( index(tail(x, 1)), tail(x,1), col = chart.col, pch = 19, lwd = 5)
  abline(v = as.Date(paste0(seq( year(index(x[1])), year(Sys.Date()), 1), "-01-01")), lty = 3, lwd = 0.5)
  misc.NBER.Recessions()
  
  text(index(tail(x,round(.15*length(x)))[1]), 1.1*tail(x,1), paste("Latest obs.\n", round(tail(x,1),2)), font = 2)
  
  grid()
  dev.off()
  if (tweet) {
    post_tweet(tweet.text, media = tmp)
    
    email <- gm_mime() %>%
      gm_to("pmaier1971@gmail.com") %>%
      gm_from("pmaier1971@gmail.com") %>%
      gm_subject("R Script Successfully Completed") %>%
      gm_attach_file(file.name) %>%
      gm_html_body("<html>R-script <strong>successfully</strong> completed</html>")
    gm_send_message(email)
  } 
}


Chart.Duo <- function(series1, series2, periods, tweet = FALSE, tweet.text = ""){
  
    x1 = misc.FREDdowload(series = series1)
    x2 = misc.FREDdowload(series = series2)
    
    x1 = x1[year(index(x1))>(year(Sys.Date())-periods)]
    x2 = x2[year(index(x2))>(year(Sys.Date())-periods)]
    
    if (tweet) {
      tmp <- tempfile(fileext = ".png")
      png(tmp, 24, 12, "in", res = 127.5)
    } else png(filename=paste0(series1, ".png"), 24, 12, "in", res=127.5)
    
    par(mfrow=c(1,2))
    
    chart.col   = brewer.pal(6, "Paired")[6]
    chart.ylim  = c(0.9*range(x1, na.rm=TRUE)[1], 1.1*range(x1, na.rm=TRUE)[2])
    chart.title = Data.Description[Data.Description$Mnemonic == series1,3]
    
    plot(as.zoo(x1), main = chart.title, 
         col = chart.col, 
         lwd = 3, 
         ylim = chart.ylim,
         #ylab = "Source: FRED, Federal Reserve Bank of St. Louis",
         ylab = Data.Description[grep(series1, Data.Description[,1]),5],
         xlab = paste0("Period: ", year(index(x1[1])), " - ", year(index(tail(x1,1))), " (shaded areas indicate U.S. recessions)"))
    points( index(tail(x1, 1)), tail(x1,1), col = chart.col, pch = 19, lwd = 5)
    abline(v = as.Date(paste0(seq( year(index(x1[1])), year(Sys.Date()), 1), "-01-01")), lty = 3, lwd = 0.5)
    misc.NBER.Recessions()
    
    text(index(tail(x1,round(.15*length(x1)))[1]), 1.1*tail(x1,1), paste("Latest obs.\n", round(tail(x1,1),2)), font = 2)
    grid()
    
    chart.col   = brewer.pal(6, "Paired")[6]
    chart.ylim  = c(0.9*range(x2, na.rm=TRUE)[1], 1.1*range(x2, na.rm=TRUE)[2])
    chart.title = Data.Description[Data.Description$Mnemonic == series2, 3]
    
    plot(as.zoo(x2), main = chart.title, 
         col = chart.col, 
         lwd = 3, 
         ylim = chart.ylim,
         ylab = Data.Description[grep(series2, Data.Description[,1]),5],
         xlab = paste0("Period: ", year(index(x2[1])), " - ", year(index(tail(x2,1))), " (shaded areas indicate U.S. recessions)"))
    points( index(tail(x2, 1)), tail(x2,1), col = chart.col, pch = 19, lwd = 5)
    abline(v = as.Date(paste0(seq( year(index(x2[1])), year(Sys.Date()), 1), "-01-01")), lty = 3, lwd = 0.5)
    misc.NBER.Recessions()
    
    text(index(tail(x2,round(.15*length(x2)))[1]), 1.1*tail(x2,1), paste("Latest obs.\n", round(tail(x2,1),2)), font = 2)
    grid()
    
    
    par(mfrow=c(1,1))
    dev.off()
    
    if (tweet) {
      post_tweet(tweet.text, media = tmp)
    email <- gm_mime() %>%
      gm_to("pmaier1971@gmail.com") %>%
      gm_from("pmaier1971@gmail.com") %>%
      gm_subject("R Script Successfully Completed") %>%
      gm_attach_file(tmp) %>%
      gm_html_body("<html>R-script <strong>successfully</strong> completed</html>")
    gm_send_message(email)
    }

}



Chart.Four <- function(series1, series2, series3, series4, periods, tweet = FALSE, tweet.text = ""){
  
  x1 = misc.FREDdowload(series = series1)
  x2 = misc.FREDdowload(series = series2)
  x3 = misc.FREDdowload(series = series3)
  x4 = misc.FREDdowload(series = series4)
  
  x1 = x1[year(index(x1))>(year(Sys.Date())-periods)]
  x2 = x2[year(index(x2))>(year(Sys.Date())-periods)]
  x3 = x3[year(index(x3))>(year(Sys.Date())-periods)]
  x4 = x4[year(index(x4))>(year(Sys.Date())-periods)]
  
  if (tweet) {
    tmp <- tempfile(fileext = ".png")
    png(tmp, 24, 12, "in", res = 127.5)
  } else png(filename=paste0(series1, ".png"), 24, 12, "in", res=127.5)
  
  par(mfrow=c(2,2))
  
  Chart.Panel(x = x1, series = series1)
  Chart.Panel(x = x2, series = series2)
  Chart.Panel(x = x3, series = series3)
  Chart.Panel(x = x4, series = series4)

  par(mfrow=c(1,1))
  dev.off()
  
  if (tweet) {
    post_tweet(tweet.text, media = tmp)
    email <- gm_mime() %>%
      gm_to("pmaier1971@gmail.com") %>%
      gm_from("pmaier1971@gmail.com") %>%
      gm_subject("R Script Successfully Completed") %>%
      gm_attach_file(tmp) %>%
      gm_html_body("<html>R-script <strong>successfully</strong> completed</html>")
    gm_send_message(email)
  }
  
}



InterestRate.Chart <- function(Data.Rates, tweet = FALSE) {
  Data.Rates.dim <- length(colnames(Data.Rates))
  Data.Rates[,2:Data.Rates.dim] <- Data.Rates[,2:Data.Rates.dim] - Data.Rates[,1:(Data.Rates.dim-1)]
  
  week  <- function(x)format(x, '%Y.W%W')
  month <- function(x)format(x, '%Y.M%m')
  year  <- function(x)format(x, '%Y')
  
  Data.Rates.D        <- as.zoo(Data.Rates[index(Data.Rates)>=Sys.Date() %m-% months(1),])
  Data.Rates.M        <- as.zoo(Data.Rates[index(Data.Rates)>=Sys.Date()-years(5),])
  Data.Rates.M        <- aggregate(Data.Rates.M, by=month, FUN=mean, na.rm=TRUE)
  Data.Rates.Names    <-c("1Y", "2Y", "3Y", "5Y", "7Y", "10Y") 
  index(Data.Rates.M) <- as.yearmon(index(Data.Rates.M), format = "%Y.M%m")
  
  
  if (tweet) {
    tmp <- tempfile(fileext = ".png")
    png(tmp, 12, 12, "in", res = 127.5)
  } else png(filename="US.Rates.Daily.png")
  
  par(mfrow = c(2,1))
  
  chart.col = brewer.pal(Data.Rates.dim, "Paired")
  
  barplot(na.omit(Data.Rates.D), col=chart.col, cex.main=0.75, cex.names = 0.75, border=NA, 
          main="Treasury Rates (Constant Maturity, Daily Yields in %):\n Changes in the last month")
  grid(col="black")
  legend("bottomleft", Data.Rates.Names, fill=chart.col, cex=0.75)
  
  barplot(Data.Rates.M, col=chart.col, cex.main=0.75, cex.names = 0.75, border=NA, #las=2,
          main="Treasury Rates (Constant Maturity, Monthly Average Yield in %):\n Evolution over the past 5 years")
  grid(col="black")
  legend("bottomleft", Data.Rates.Names, fill=chart.col, cex=0.75)
  par(mfrow = c(1,1))
  
  dev.off()
  if (tweet) post_tweet("What's Driving Changes in the US Yield Curve? #rstats", media = tmp)
}



Evolution.Chart <- function(Evolution.Data, tweet = FALSE){
  
  Evolution.Data <- apply(Evolution.Data, 2, function(X) 100*(X/X[1]))
  Evolution.Data <- zoo(Evolution.Data, as.yearqtr(as.Date(rownames(Evolution.Data))) )
  
  if (tweet) {
    tmp <- tempfile(fileext = ".png")
    png(tmp, 12, 12, "in", res = 2* 127.5)
  } else png(filename="EvolutionChart.png")
  
  
  chart.col   = brewer.pal(6, "Paired")
  chart.lty   = c(rep(1, (ncol(Evolution.Data)-1)), 1)
  chart.lwd   = c(rep(3, (ncol(Evolution.Data)-1)), 3)
  line.color  = chart.col
  
  plot((Evolution.Data[,1]), main = "Evolution of real GDP \n (All countries indexed to 100 in Q4 2019)", type = "n",
       xaxt="n", 
       xlab="", ylab="", 
       xlim = c( min(index(Evolution.Data)), as.yearqtr( as.Date( max(index(Evolution.Data)) ) + months(3) )  ),
       ylim=c(min(Evolution.Data, na.rm=TRUE), 1.02*max(Evolution.Data, na.rm=TRUE)))
  axis(1, at=index(Evolution.Data[,1]), label = index(Evolution.Data),
       col.axis="black", cex.axis=0.9)
  for (idx in 1:ncol(Evolution.Data)){
    
    lines((Evolution.Data[,idx]), col = line.color[idx], lwd=chart.lwd[idx], lty=chart.lty[idx])
    points( tail(index(na.omit(Evolution.Data[,idx])),1), as.numeric(tail((na.omit(Evolution.Data[,idx])),1)), 
            col = chart.col[idx], 
            pch = 19, 
            lwd = 5)
    text( 
      tail(index(na.omit(Evolution.Data[,idx])),1), 
      as.numeric(tail((na.omit(Evolution.Data[,idx])),1)), 
      paste0( "  ", colnames(Evolution.Data)[idx], ": ",   round(as.numeric(tail((na.omit(Evolution.Data[,idx])),1)),1), "%" ),
      font = 2, cex = .8,
      adj = c(0,0) )
  }
  abline(h= 100, lty = 2)
  legend("bottomright", legend=c("United States", "Euro Area", "United Kingdom", "Japan", "Canada"), fill=chart.col, cex=0.75) 
  dev.off()
  if (tweet) post_tweet("Who Recovers Faster? Comparing The #Recovery Since the OutSet of the Pandemic Across Major Industrialized Countries #rstats", media = tmp)
}



# End Charting functions


# ----------------
# Weekly Updates
# ----------------

# Monday: Gas Prices
Chart.Duo(series1="US.GasPrices", series2="US.Commodities.Oilprices",
          periods = 25,
          tweet = (weekdays(Sys.Date()) == "Monday"),
          tweet.text = paste0("Paying more for #gas? Weighted av. gas prices currently at $", 
                              tail(US.GasPrices,1),
                              ". Relative to 12 months ago, this is a $", 
                              round( as.numeric(tail(US.GasPrices,1)) - as.numeric(tail(US.GasPrices,53)[1]), 2),
                              ifelse( round( as.numeric(tail(US.GasPrices,1)) - as.numeric(tail(US.GasPrices,53)[1]), 2), 
                                      " increase.  #rstats", " decrease. #inflation #rstats") )
)


# Wednesday: Economic Uncertainty
Chart.Single(series="US.EconomicPolicyUncertaintyIndex",
             periods = 2,
             tweet = (weekdays(Sys.Date()) == "Wednesday"),  
             tweet.text = "The daily news-based Economic Policy Uncertainty Index is based on newspapers in the US. See Baker, Scott R., Bloom, Nick and Davis, Stephen J., Economic Policy Uncertainty Index for United States (source: FRED, USEPUINDXD) #EconomicUncertainty #rstats")



#Thursday: Bank Balance Sheet Data

# BankData <- Reduce(function(...) merge(...), list( misc.FREDdowload("US.H8.BanksCredit.Securities"),
#                                                    misc.FREDdowload("US.H8.BanksCredit.LoansLeases"),
#                                                    misc.FREDdowload("US.H8.InterBankLoans"),
#                                                    misc.FREDdowload("US.H8.Cash"),
#                                                    misc.FREDdowload("US.H8.TradingAssets")
# ))
# 
# BankData    <- BankData[index(BankData) > Sys.Date() - years(15)]
# plot.col    <- brewer.pal(ncol(BankData), "Paired")
# plot.legend <- c("Securities", "Loans and Leases", "Interbank Loans", "Cash", "Trading Assets")
# barplot(BankData, col = plot.col, border="NA", main = "All US Commercial Banks' Balance Sheets: \n Main Categories", ylab = "$bn")
# legend("topleft", plot.legend, fill = plot.col, cex=0.75)
# 



# Friday: Interest rate chart
InterestRate.Chart(Data.Rates = Reduce(function(...) merge(...), list( misc.FREDdowload("US.SOV.1Y"), 
                                                                       misc.FREDdowload("US.SOV.2Y"), 
                                                                       misc.FREDdowload("US.SOV.3Y"), 
                                                                       misc.FREDdowload("US.SOV.5Y"), 
                                                                       misc.FREDdowload("US.SOV.7Y"), 
                                                                       misc.FREDdowload("US.SOV.10Y") )), 
                   tweet = (weekdays(Sys.Date()) == "Friday"))



# ----------------

# ----------------

# GDP Release

if ( as.Date( as.character(tail( fredr_release_dates(release_id = 53L) ,1)[,2]) ) == Sys.Date() ) {
  
  tmp <- tempfile(fileext = ".png")
  png(tmp, 12, 12, "in", res = 127.5)  
  misc.GDPExpansionPlot(series = "US.GDP.Real")
  dev.off()
  post_tweet("How Does the Current U.S Recovery Compare to Prior Recessions? #rstats", media = tmp)
  
  tmp <- tempfile(fileext = ".png")
  png(tmp, 12, 12, "in", res = 127.5)
  par(mfrow=c(2,2))
  misc.GDPExpansionPlot(series = "US.GDP.PCE")
  misc.GDPExpansionPlot(series = "US.GDP.FinalSales")
  misc.GDPExpansionPlot(series = "US.GDP.Investment")
  misc.GDPExpansionPlot(series = "US.GDP.Government")
  par(mfrow=c(1,1))
  dev.off()
  post_tweet("Slow Recovery Across All Major Components of U.S. GDP #recovery #rstats", media = tmp)
  
}

# Retail Sales

Chart.Duo(series1="US.Activity.RetailSales", series2="US.Activity.RetailSalesExAuto",
          periods = 25,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 9L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = "Low vehicles sales and low inventory the result of supply chain discriptions (source: FRED) #vehiclesales #rstats")





# Labor Market

  Chart.Single(series="US.JOLTS.QuitsRate",
               periods = 25,
               tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 192L) ,1)[,2]) ) == Sys.Date() ),  
               tweet.text = "#JOLTS update: Quits rate #rstats")
  
  Chart.Single(series="US.JOLTS.HireRate",
               periods = 25,
               tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 192L) ,1)[,2]) ) == Sys.Date() ),  
               tweet.text = "#JOLTS update: Hire rate #rstats")
  
  Chart.Single(series="US.JOLTS.JobOpeningsRate",
               periods = 25,
               tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 192L) ,1)[,2]) ) == Sys.Date() ),  
               tweet.text = "#JOLTS update: Rate of job openings #rstats")
  
  # Chart.Single(series="US.Activity.ADP",
  #              periods = 25,
  #              tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 194L) ,1)[,2]) ) == Sys.Date() ),  
  #              tweet.text = "#JOLTS update: Quits rate #rstats")
  
  Chart.Duo(series1="US.Activity.InitialClaims", series2="US.Activity.ContinuedClaims.4W.MA",
            periods = 25,
            tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 180L) ,1)[,2]) ) == Sys.Date() ),  
            tweet.text = "Initial and continued claims (source: FRED) #rstats")

  
if ( as.Date( as.character(tail( fredr_release_dates(release_id = 50L) ,1)[,2]) ) == Sys.Date() ) {
  
  tmp <- tempfile(fileext = ".png")
  png(tmp, 12, 12, "in", res = 127.5)  
  
  misc.GDPExpansionPlot(series = "US.Payroll")
  
  dev.off()
  post_tweet("#Payroll Update: Comparing the current #recovery of the #LaborMarket to previous recessions #rstats", media = tmp)
}


Chart.Four(series1 = "US.Unemployment", 
           series2 = "US.Unemployment.ParticipationRate", 
           series3 = "US.Unemployment.PartTimeEconomicReasons",
           series4 = "US.Unemployment.PartTimeNonEconomicReasons", 
           periods = 25,
           tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 50L) ,1)[,2]) ) == Sys.Date() ),
           tweet.text = "#Payroll Update: #LaborMarket is improving with #unemployment down, but the #ParticipationRate still hasn't fully recovered #rstats")

Chart.Four(series1 = "US.Unemployment.EmploymentToPopulation", 
           series2 = "US.JOLTS.JobOpeningsRate", 
           series3 = "US.Unemployment.MarginallyAttached",
           series4 = "US.Unemployment.U1", 
           periods = 25,
           tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 50L) ,1)[,2]) ) == Sys.Date() ),
           tweet.text = "#Payroll Update: Details behind the topline number (source: FRED) #LaborMarket #rstats")



# Housing

Chart.Single(series="US.Housing.CaseShiller",
             periods = 25,
             tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 199L) ,1)[,2]) ) == Sys.Date() ),  
             tweet.text = "S&P/Case-Shiller U.S. National Home Price Index (source: FRED, series CSUSHPINSA) #housing #rstats")


Chart.Single(series="US.Activity.NYFed.Leading",
             periods = 25,
             tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 199L) ,1)[,2]) ) == Sys.Date() ),  
             tweet.text = "#rstats")
 
Chart.Single(series="US.Survey.Empire",
             periods = 25,
             tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 199L) ,1)[,2]) ) == Sys.Date() ),  
             tweet.text = "#rstats")


# Motor Vehicles

Chart.Duo(series1="US.Auto.Autosales", series2="US.Auto.InventorySalesRatio",
          periods = 25,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 93L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = "Low vehicles sales and low inventory the result of supply chain discriptions (source: FRED) #vehiclesales #rstats")



# Banking

Chart.Duo(series1="US.FDIC.NetChargeOffRateTotalLoans", series2="US.FDIC.LoanLossProvisions",
          periods = 35,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 482L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = paste0("Banking Update: Net Charge-Off Rates at ", 
                              round( tail(US.FDIC.NetChargeOffRateTotalLoans,1), 2),
                              "%, ",
                              ifelse( (as.numeric(tail(US.FDIC.NetChargeOffRateTotalLoans,1)) - as.numeric(tail(US.FDIC.NetChargeOffRateTotalLoans,2)[1]) > 0 ),
                                      "up ", "down "),
                              round( as.numeric(tail(US.FDIC.NetChargeOffRateTotalLoans,1)) - as.numeric(tail(US.FDIC.NetChargeOffRateTotalLoans,2)[1]), 2),
                              "p.p. from last Quarter  (source: FRED) #rstats") 
)

Chart.Duo(series1="US.FDIC.InstitutionsWithEarningsGain", series2="US.FDIC.UnprofitableInstitutions",
          periods = 35,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 482L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = paste0("Banking Update: Number of Unprofitable Institutions at ", 
                              round( tail(US.FDIC.UnprofitableInstitutions,1), 2),
                              ", ",
                              ifelse( (as.numeric(tail(US.FDIC.UnprofitableInstitutions,1)) - as.numeric(tail(US.FDIC.UnprofitableInstitutions,2)[1]) > 0 ),
                                      "up ", "down "),
                              round( as.numeric(tail(US.FDIC.UnprofitableInstitutions,1)) - as.numeric(tail(US.FDIC.UnprofitableInstitutions,2)[1]), 2),
                              " from last Quarter  (source: FRED) #rstats") 
)


if  ( as.Date( as.character(tail( fredr_release_dates(release_id = 22L) ,1)[,2]) ) == Sys.Date() ) {
  
  BankData.Details  <- merge(US.H8.BanksCredit.Other=misc.FREDdowload("US.H8.BanksCredit.Other"),
                             US.H8.BanksCredit.Securities.Other=misc.FREDdowload("US.H8.BanksCredit.Securities.Other"),
                             US.H8.TradingAssets=misc.FREDdowload("US.H8.TradingAssets"),
                             US.H8.InterBankLoans=misc.FREDdowload("US.H8.InterBankLoans"),
                             US.H8.FedFundsSold=misc.FREDdowload("US.H8.FedFundsSold"),
                             US.H8.BanksCredit.CI=misc.FREDdowload("US.H8.BanksCredit.CI"),
                             US.H8.BanksCredit.RE=misc.FREDdowload("US.H8.BanksCredit.RE"),
                             US.H8.BanksCredit.Consumer=misc.FREDdowload("US.H8.BanksCredit.Consumer"),
                             US.H8.Cash=misc.FREDdowload("US.H8.Cash"),
                             US.H8.BanksCredit.Securities.Treasuries = misc.FREDdowload("US.H8.BanksCredit.Securities.Treasuries")
  )
  
  BankData.Details  <- BankData.Details[index(BankData.Details) > Sys.Date() - years(7)]
  
  plot.col    <- brewer.pal(10, "Paired")
  plot.legend = character()
  for (idx in 1:ncol(BankData.Details)) plot.legend <- c(plot.legend, Data.Description[Data.Description$Mnemonic == colnames(BankData.Details)[idx],3])
  plot.legend <- gsub(", All Commercial Banks", "", plot.legend)
  plot.ylim = c(0, 1.1*max(rowSums(BankData.Details)))
  
  tmp <- tempfile(fileext = ".png")
  png(tmp, 12, 12, "in", res = 127.5)  
  
  BankDataChart=barplot(BankData.Details, col = plot.col, 
                        border="NA", 
                        main = "All Commercial Bank's Balance Sheets: \n Weekly Bank Credit by Loan Type", 
                        xaxt = "n",
                        ylim = plot.ylim)
  axis(1, at=BankDataChart[week(index(BankData.Details))==25], 
       label = year(index(BankData.Details))[week(index(BankData.Details))==25],
       col.axis="black", cex.axis=1)
  abline(v = BankDataChart[week(index(BankData.Details)) ==1], lty = 2)
  abline(h=0, lwd=4)
  legend("topleft", plot.legend, fill = plot.col, cex=0.75)
  
  dev.off()
  post_tweet("Increase in Bank Balance Sheets Post-Pandemic Mostly Driven by Higher Cash and Treasuries/Agencies #BankLending #rstats", media = tmp)
}



#Transportation

Chart.Four(series1="US.Transportation.Air", 
           series2="US.Transportation.Airtraffic.Passenger", 
           series3="US.Transportation.Railpassenger",
           series4 = "US.Transportation.PublicTransit", 
           periods = 25,
           tweet = ( day(Sys.Date()) == 20),
           tweet.text = "Air Traffic coming back, Public Transportation not so much #rstats")


Chart.Duo(series1="US.Transportation.Rail", series2="US.Transportation.Index",
          periods = 25,
          tweet = ( day(Sys.Date()) == 20),
          tweet.text = "Transportation Update: Public Transit Ridership / Freight transportation services index (for-hire trucking
          freight railroad services, inland waterway traffic, pipeline movements, air freight) #rstats")



# Real GDP
GDP.Comparison.Data <- merge(US = misc.FREDdowload("US.GDP.Real"),
                             EU = misc.FREDdowload("EU.GDP.Real"),
                             UK = misc.FREDdowload("UK.GDP.Real"),
                             JP = misc.FREDdowload("JP.GDP.Real"),
                             CA = misc.FREDdowload("CA.GDP.Real") )

GDP.Comparison.Data <- GDP.Comparison.Data[index(GDP.Comparison.Data)>= as.Date("2019-09-01"),]



Evolution.Chart(Evolution.Data = GDP.Comparison.Data, tweet=FALSE)

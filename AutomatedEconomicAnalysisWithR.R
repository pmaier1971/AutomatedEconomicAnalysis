#!/usr/bin/R

pkgTest <- function(x){
  if (!require(x,character.only = TRUE)) {
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
pkgTest("COVID19")

setwd("~/Documents/R/")

# Test that FRED is online ####
test.online        <- try(getSymbols("GDPC1",src='FRED'))

if ((inherits(test.online, "try-error"))) {
  stop("\n\n\n----------\n Not online \n----------\n\n")
}

rm(list=ls())



# Email setup and credentials -------------------------------------------------------------

# replace with your own email info
email <- gm_mime()
#email <- gm_to(email, toAddress
#email <- gm_from(email, fromAddress)
#email <- gm_subject(email, emailSubject)

FRED_API_KEY <- ""  # put your own FRED API key here
source("Credentials.R")   # file where MY credentials for Fred, Twitter, and Gmail are stored
fredr_set_key(FRED_API_KEY)

# Data Download -------------------------------------------------------------

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
             
             "US.Payroll"="PAYEMS",
             "US.IP.CapacityUtilization" = "TCU",
             "US.Unemployment"="UNRATE", "US.Unemployment.U1" = "U1RATE", "US.Unemployment.U6" = "U6RATE", "US.Unemployment.U2" = "U2RATE",
             "US.Unemployment.PartTimeEconomicReasons" = "LNS12032194", "US.Unemployment.PartTimeNonEconomicReasons" = "LNS12032200",
             "US.Unemployment.MarginallyAttached" = "LNU05026642",
             "US.Unemployment.ParticipationRate"="CIVPART",
             "US.Unemployment.EmploymentToPopulation"="EMRATIO",
             "US.Activity.ChicagoFed.Employment" = "EUANDH",
             "US.Activity.ChicagoFed" = "CFNAI",
             "US.Activity.NYFed.Current" = "GACDISA066MSFRBNY",
             "US.Activity.NYFed.Leading" = "GAFDISA066MSFRBNY",
            
             "US.Activity.RetailSales" = "RSAFS",
             "US.Activity.RetailSalesExAuto" = "RSFSXMV",
             "US.Activity.RetailSalesMotorvehicles" = "RSMVPD",
             "US.Activity.RetailSalesElectronics" = "RSEAS",
             "US.Activity.RetailSalesFoodDBeverageRetail" = "RSDBS",
             "US.Activity.RetailSalesHealth" = "RSHPCS",
             "US.Activity.RetailSalesGasoline" = "RSGASS",
             "US.Activity.RetailSalesGeneralMerchandise" = "RSGMS",
             "US.Activity.RetailSalesMisc" = "RSMSR",
             "US.Activity.RetailSalesNonStore" = "RSNSR",
             "US.Activity.RetailSalesSportsGoods" = "RSSGHBMS",
             "US.Activity.RetailSalesFurniture" = "RSFHFS",
             "US.Activity.RetailSalesBuildingMaterials" = "RSBMGESD",
             "US.Activity.RetailSalesClothing" = "RSCCAS",
             "US.Activity.RetailSalesFoodDrinking" = "RSFSDP",
             
             "US.Activity.ADP" = "NPPTTL",
             "US.Activity.InitialClaims" = "ICSA",
             "US.Activity.ContinuedClaims.4W.MA" = "CC4WSA",
             "US.Activity.ContinuedClaims" = "CCSA",
             "US.JOLTS.QuitsRate" = "JTSQUR",
             "US.JOLTS.HireRate" = "JTSHIR",
             "US.JOLTS.JobOpeningsRate" ="JTSJOR",
             "US.Unemployment.WageGrowth" = "CES0500000003",
             
             "US.CPI.Headline"="CPIAUCSL", 
             "US.CPI.Core"="CPILFESL",
             
             "US.CPI.Headline.mm.yy"="CPIAUCSL", 
             "US.CPI.Core.mm.yy"="CPILFESL",
             "US.CPI.AlcoholicBeverages.mm.yy" = "CUSR0000SEFW",
             "US.CPI.AlcoholicBeveragesAway.mm.yy" = "CUSR0000SEFX",
             "US.CPI.FoodBeverages.mm.yy" = "CPIFABSL",
             "US.CPI.Housing.mm.yy" = "CPIHOSSL",
             "US.CPI.Apparel.mm.yy" = "CPIAPPSL",
             "US.CPI.Medical.mm.yy" = "CPIMEDSL",
             "US.CPI.Recreation.mm.yy" = "CPIRECSL",
             "US.CPI.Education.mm.yy" = "CPIEDUSL",
             "US.CPI.Other.mm.yy" = "CPIOGSSL",
             
             "US.SOV.1Y"="DGS1", 
             "US.SOV.2Y"="DGS2", 
             "US.SOV.3Y"="DGS3", 
             "US.SOV.5Y"="DGS5", 
             "US.SOV.7Y"="DGS7", 
             "US.SOV.10Y"="DGS10", 
             "US.SOV.20Y"="DGS20", 
             "US.SOV.30Y"="DGS30", 

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
             "US.Housing.15YMortgageRate"="MORTGAGE15US",
             "US.Housing.51ARMMortgageRate"="MORTGAGE5US",
             "US.Housing.30YJumboMortgageRate"="OBMMIJUMBO30YF",
             
             "US.Housing.NewHomeSales" = "HSN1F",
             "US.Housing.ExistingHomeSales" = "EXHOSLUSM495S",
             "US.Housing.MonthlySupply"="MSACSR",
             "US.Housing.AllTransactionsPriceIndex"="USSTHPI",
             "US.Housing.CaseShiller" = "CSUSHPINSA",
             
             "US.Auto.Autosales" = "ALTSALES",
             "US.Auto.LightAutos" = "LAUTOSA",
             "US.Auto.LightTrucks" = "LTRUCKSA",
             "US.Auto.AutosDomestic" = "DAUTOSAAR",
             "US.Auto.AutosForeign" = "FAUTOSAAR",
             
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
             
             "US.SLOOS.AutoLoansStandards" = "STDSAUTO",
             "US.SLOOS.AutoLoansDemand" = "DEMAUTO",
             "US.SLOOS.CreditCardsStandards" = "DRTSCLCC",
             "US.SLOOS.CreditCardDemand" = "DEMCC",
             "US.SLOOS.ConsumerLoansStandards" = "STDSOTHCONS",
             "US.SLOOS.ConsumerLoansDemand" = "DEMOTHCONS",
        
             "US.SLOOS.IncreasingSpreadsLargeFirms" = "DRISCFLM",
             "US.SLOOS.CILoanDemandLargeFirms" = "DRSDCILM",
             "US.SLOOS.CILoanStandardsLargeFirms" = "DRTSCILM",
             
             "US.SLOOS.IncreasingSpreadsSmallFirms" = "DRISCFS",
             "US.SLOOS.CILoanDemandSmallFirms" = "DRSDCIS",
             "US.SLOOS.CILoanStandardsSmallFirms" = "DRTSCIS",
             
             "US.SLOOS.CRELoanDemandLand" = "SUBLPDRCDC",
             "US.SLOOS.CRELoanStandardsLand" = "SUBLPDRCSC",
             "US.SLOOS.CRELoanDemandMultifamily" = "SUBLPDRCDM",
             "US.SLOOS.CRELoanStandardsMultifamily" = "SUBLPDRCSM",
             "US.SLOOS.CRELoanDemandNonresidential" = "SUBLPDRCDN",
             "US.SLOOS.CRELoanStandardsNonresidential" = "SUBLPDRCSN",
                  
             "US.Banks.AutoLoansSecuritized.qq.yy" = "MVLOAS",
             "US.Banks.StudentLoans.qq.yy" = "SLOAS",
             
             "US.GasPrices" = "GASALLW",
             "US.EconomicPolicyUncertaintyIndex" = "USEPUINDXD",
             "US.VIX" = "VIXCLS",
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
  
  if (idx %% 100 == 0 ) Sys.sleep(45)
  
}


# Misc. functions ---------------------------------------------------------

misc.FREDdowload <- function(series) {
  
  if (!(exists(series))) {
    cat("\n       Downloading", series)
    x = fredr( series_id = Data.Description[Data.Description$Mnemonic == series,2] )
    x = zoo(x$value, as.Date(x$date))
    if ((series == "US.H8.FedFundsSold") || (series == "US.H8.TradingAssets")) x = x / 1000
    if ( substr(series, (nchar(series)-5), nchar(series)) == ".mm.yy") x <- 100*log(x / stats::lag(x, -12))
    if ( substr(series, (nchar(series)-5), nchar(series)) == ".qq.yy") x <- 100*log(x / stats::lag(x, -4))
    if ( substr(series, (nchar(series)-2), nchar(series)) == ".qq") x <- 100*log(x / stats::lag(x, -1))
    
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


# Charting functions ---------------------------------------------------------

misc.GDPExpansionPlot <- function( series ){
  
    x = misc.FREDdowload(series = series)
    
    NBER.Recessions <- as.Date(as.yearqtr(as.Date(as.character(t(nberDates()[,1])), format="%Y%m%d")))
    NBER.Recessions <- NBER.Recessions[year(NBER.Recessions) > 1958]
    
    x <- data.frame( x, StartExpansion = 0)
    
    idx.Expansion                   <- as.Date(rownames(x)) %in% (NBER.Recessions %m-% months(3))
    x$StartExpansion[idx.Expansion] <- 1
    x$StartExpansion                <- cumsum(x$StartExpansion)
    
    if (series == "US.Payroll") col.plot = brewer.pal((max(x$StartExpansion)+1), "Paired") else col.plot = c( brewer.pal((max(x$StartExpansion)+1), "Paired"), "black")
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
      lines(zoo(tmp.plot[,1]), lwd=width.plot, col=col.plot[idx.plot], lty=type.plot[idx.plot-3])
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
    legend("bottomright", "Data: St. Louis FRED", cex=.5)
  
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
  legend("bottomleft", "Data: St. Louis FRED", cex=.7)
  
}


Chart.Single <- function(series, periods, tweet = FALSE, tweet.text = "", email){
  
  x = misc.FREDdowload(series = series)
  x = x[year(index(x))>(year(Sys.Date())-periods)]
  
  chart.filename = paste0(series, ".png")
  png(filename = chart.filename, 12, 12, "in", res = 127.5 )
  
  Chart.Panel(x = x, series = series)

  dev.off()
  email <<- gm_attach_file(email, chart.filename)
  if (tweet) post_tweet(tweet.text, media = chart.filename)
  
}


Chart.Duo <- function(series1, series2, periods, tweet = FALSE, tweet.text = "", email){
  
    x1 = misc.FREDdowload(series = series1)
    x2 = misc.FREDdowload(series = series2)
    
    x1 = x1[year(index(x1))>(year(Sys.Date())-periods)]
    x2 = x2[year(index(x2))>(year(Sys.Date())-periods)]
    
    chart.filename = paste0(series1, ".png")
    png(filename=chart.filename, 24, 12, "in", res=127.5)
    
    par(mfrow=c(1,2))
    chart.col   = brewer.pal(6, "Paired")[6]
    chart.ylim  = c(0.9*range(x1, na.rm=TRUE)[1], 1.1*range(x1, na.rm=TRUE)[2])
    chart.title = Data.Description[Data.Description$Mnemonic == series1,3]
    
    plot(as.zoo(x1), main = chart.title, 
         col = chart.col, 
         lwd = 3, 
         ylim = chart.ylim,
         #ylab = "Source: FRED, Federal Reserve Bank of St. Louis",
         ylab = ifelse ( substr(series1, (nchar(series1)-5), nchar(series1)) == ".qq.yy", "Y/Y growth (in %)", Data.Description[grep(series1, Data.Description[,1]),5]),
         xlab = paste0("Period: ", year(index(x1[1])), " - ", year(index(tail(x1,1))), " (shaded areas indicate U.S. recessions)"))
    points( index(tail(x1, 1)), tail(x1,1), col = chart.col, pch = 19, lwd = 5)
    abline(v = as.Date(paste0(seq( year(index(x1[1])), year(Sys.Date()), 1), "-01-01")), lty = 3, lwd = 0.5)
    misc.NBER.Recessions()
    
    text(index(tail(x1,round(.15*length(x1)))[1]), 1.1*tail(x1,1), 
         ifelse ( substr(series1, (nchar(series1)-5), nchar(series1)) == ".qq.yy", 
                  paste("Latest obs.\n", round(tail(x1,1),2),"%"),
                  paste("Latest obs.\n", round(tail(x1,1),2))), font = 2)
    grid()
    legend("bottomleft", "Data: St. Louis FRED", cex=.6)
    
    chart.col   = brewer.pal(6, "Paired")[6]
    chart.ylim  = c(0.9*range(x2, na.rm=TRUE)[1], 1.1*range(x2, na.rm=TRUE)[2])
    chart.title = Data.Description[Data.Description$Mnemonic == series2, 3]
    
    plot(as.zoo(x2), main = chart.title, 
         col = chart.col, 
         lwd = 3, 
         ylim = chart.ylim,
         ylab = ifelse ( substr(series2, (nchar(series2)-5), nchar(series2)) == ".qq.yy", "Y/Y growth (in %)", Data.Description[grep(series2, Data.Description[,1]),5]),
         xlab = paste0("Period: ", year(index(x2[1])), " - ", year(index(tail(x2,1))), " (shaded areas indicate U.S. recessions)"))
    points( index(tail(x2, 1)), tail(x2,1), col = chart.col, pch = 19, lwd = 5)
    abline(v = as.Date(paste0(seq( year(index(x2[1])), year(Sys.Date()), 1), "-01-01")), lty = 3, lwd = 0.5)
    misc.NBER.Recessions()
    
    text(index(tail(x2,round(.15*length(x2)))[1]), 1.1*tail(x2,1), 
         ifelse ( substr(series2, (nchar(series2)-5), nchar(series2)) == ".qq.yy",
                  paste("Latest obs.\n", round(tail(x2,1),2),"%"),
                  paste("Latest obs.\n", round(tail(x2,1),2))), font = 2)
    grid()
    legend("bottomleft", "Data: St. Louis FRED", cex=.6)
    
    par(mfrow=c(1,1))
   
    dev.off()
    email <<- gm_attach_file(email, chart.filename)
    if (tweet) post_tweet(tweet.text, media = chart.filename)
    
}


Chart.Four <- function(series1, series2, series3, series4, periods, tweet = FALSE, tweet.text = "", email){
  
  x1 = misc.FREDdowload(series = series1)
  x2 = misc.FREDdowload(series = series2)
  x3 = misc.FREDdowload(series = series3)
  x4 = misc.FREDdowload(series = series4)
  
  x1 = x1[year(index(x1))>(year(Sys.Date())-periods)]
  x2 = x2[year(index(x2))>(year(Sys.Date())-periods)]
  x3 = x3[year(index(x3))>(year(Sys.Date())-periods)]
  x4 = x4[year(index(x4))>(year(Sys.Date())-periods)]
  
  chart.filename = paste0(series1, ".png")
  png(filename=chart.filename, 24, 12, "in", res=127.5)
  
  par(mfrow=c(2,2))
  
  Chart.Panel(x = x1, series = series1)
  Chart.Panel(x = x2, series = series2)
  Chart.Panel(x = x3, series = series3)
  Chart.Panel(x = x4, series = series4)

  par(mfrow=c(1,1))
  dev.off()
  email <<- gm_attach_file(email, chart.filename)
  if (tweet) post_tweet(tweet.text, media = chart.filename)
  
}


Chart.InflationOverview <- function(series,tweet = FALSE, tweet.text = "", email) {
  
  x = data.frame(date = index(get(series[1])),
                 Reduce(function(...) merge(..., all=T), mget(series, ifnotfound = "not found", inherits = TRUE)),
                 year = year(index(get(series[1]))) )
  
  x = aggregate(x, list(x$year), FUN = mean)
  x = x[x$year %in% c("2017", "2018", "2021", "2022"),]
  
  plot.data   =  t( as.matrix(x[3:(ncol(x)-1)]) )
  colnames(plot.data) = x[,"year"]
  plot.ylim   = c(1.15 * min(0, plot.data), 1.15*range(plot.data)[2])
  plot.col    = brewer.pal(nrow(plot.data), "Paired")
  plot.legend = character()
  for (idx in 1 : length(series)) plot.legend = c(plot.legend, gsub("Consumer Price Index for All Urban Consumers: ", "", Data.Description[Data.Description$Mnemonic == series[idx],3]))
  
  chart.filename = paste0(series[1], ".png")
  png(filename=chart.filename, 24, 12, "in", res=127.5)
  
  bp = barplot(plot.data, beside = T, col = plot.col, main = "Consumer Price Index for All Urban Consumers:\n Average Annual Inflation by Component", ylim = plot.ylim )
  
  limits = par('usr')
  rect(mean(c(bp[nrow(bp),2], bp[1,3])), 0, limits[2], limits[4], lty = 3, lwd = 4)
  
  text(bp, 1.1* plot.data, paste0(round(plot.data,1),"%"), font = 2, cex = .8)
  
  legend("topleft", plot.legend, fill = plot.col, cex = .7, bty = 'n')
  legend("bottomleft", "Data: St. Louis FRED", cex=.5)
  dev.off()
  
  email <<- gm_attach_file(email, chart.filename)
  if (tweet) post_tweet(tweet.text, media = chart.filename)
  
}


InterestRate.Chart <- function(Data.Rates, tweet = FALSE, email) {
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
  
 
  chart.filename = "US.Rates.Daily.png"
  png(filename=chart.filename, 24, 24, "in", res=127.5)
  
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
  email <<- gm_attach_file(email, chart.filename)
  if (tweet) post_tweet(tweet.text, media = chart.filename)
  
}


Evolution.Chart <- function(Evolution.Data, tweet = FALSE, email){
  
  Evolution.Data <- apply(Evolution.Data, 2, function(X) 100*(X/X[1]))
  Evolution.Data <- zoo(Evolution.Data, as.yearqtr(as.Date(rownames(Evolution.Data))) )
 
  chart.filename = "EvolutionChart.png"
  png(filename=chart.filename, 24, 24, "in", res=127.5)
  
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
  legend("bottomleft", "Data: St. Louis FRED", cex=.5)
  
  dev.off()
  email <<- gm_attach_file(email, chart.filename)
  if (tweet) post_tweet("Who Recovers Faster? Comparing The #Recovery Since the OutSet of the Pandemic Across Major Industrialized Countries #rstats", media = chart.filename)
}


Chart.DoublePanel <- function(series1, series2, periods) {
  
  x1 = misc.FREDdowload(series1) 
  x2 = misc.FREDdowload(series2)
  
  plot.data = merge(x1, x2)
  plot.data = plot.data[year(index(plot.data))>(year(Sys.Date())-periods)]
  names(plot.data) <- gsub("Motor Vehicle Retail Sales: ", "", c(Data.Description[Data.Description$Mnemonic == series1,3], Data.Description[Data.Description$Mnemonic == series2,3]) )
  
  chart.col   = brewer.pal(6, "Paired")[1:2]
  chart.ylim  = c(0.9*range(plot.data, na.rm=TRUE)[1], 1.1*range(plot.data, na.rm=TRUE)[2])
  chart.title = "Motor Vehicle Retail Sales" #Data.Description[Data.Description$Mnemonic == series1,3]
  
  plot(plot.data[,1], col = chart.col, main = chart.title, type = "n", 
       ylim = chart.ylim, 
       ylab = Data.Description[Data.Description$Mnemonic == series1,5], 
       xlab = "")
  legend("topleft", names(plot.data), fill = chart.col, bty = "n")
  
  for (idx in 1:ncol(plot.data)) {
    lines(plot.data[,idx], col = chart.col[idx])
    points( index(tail(plot.data[,idx], 1)), tail(plot.data[,idx],1), col = chart.col[idx], pch = 19, lwd = 5)
    text(index(tail(plot.data[,idx],round(.15*length(x2)))[1]), 1.1*tail(plot.data[,idx],1), 
         paste("Latest obs.\n", round(tail(plot.data[,idx],1),2)), font = 2)
  }
  misc.NBER.Recessions()
  
  grid()
  legend("bottomleft", "Data: St. Louis FRED", cex=.5)
  
}




# Weekly Update -----------------------------------------------------------

Sys.sleep(45)

# Tuesday: Gas Prices
Chart.Duo(series1="US.GasPrices", series2="US.Commodities.Oilprices",
          periods = 25,
          tweet = (weekdays(Sys.Date()) == "Tuesday"),
          tweet.text = paste0("Paying more for #gas? Weighted av. gas prices currently at $", 
                              round( tail(US.GasPrices,1), 2),
                              ". Relative to 12 months ago, this is a $", 
                              round( as.numeric(tail(US.GasPrices,2)) - as.numeric(tail(US.GasPrices,53)[1]), 2),
                              ifelse( round( as.numeric(tail(US.GasPrices,1)) - as.numeric(tail(US.GasPrices,53)[1]), 2), 
                                      " increase.  #rstats", " decrease. #inflation #rstats")),
                              email=email)


# Wednesday: Economic Uncertainty

if ((weekdays(Sys.Date()) == "Wednesday") ) {

  series = "US.VIX"
  x = misc.FREDdowload(series = series)
  x = x[year(index(x))>(year(Sys.Date())-2)]
  
  chart.filename = paste0(series, ".png")
  png(filename = chart.filename, 24, 12, "in", res = 127.5 )
  
  par(mfrow=c(1,2))
  
  Chart.Panel(x = x, series = series)
  lines(rollmean(x, k = 7, align = "right"), col = brewer.pal(5, "Paired")[2], lwd = 4)

  series = "US.EconomicPolicyUncertaintyIndex"
  x = misc.FREDdowload(series = series)
  x = x[year(index(x))>(year(Sys.Date())-2)]
  
  Chart.Panel(x = x, series = series)
  lines(rollmean(x, k = 7, align = "right"), col = brewer.pal(5, "Paired")[2], lwd = 4)
  
  dev.off()
  email <<- gm_attach_file(email, chart.filename)
  
  tweet.text = paste0("Uncertainty Upate: VIX at ", as.numeric( tail(US.VIX,1) ),"; 7-day average of the Economic Policy Uncertainty Index at ",
                      round( as.numeric( tail(rollmean(x, k = 7, align = "right"),1) ),1),
                      ", ", 
                      ifelse(tail(diff(rollmean(x, k = 7, align = "right")),1)>0, "up ", "down "),
                      round( tail(diff(rollmean(x, k = 7, align = "right")),1),1), " from the prior week #MarketVolatility #EconomicUncertainty #rstats https://whyitmatters.netlify.app/posts/2021-11-11-measures-of-economic-uncertainty-and-market-volatility/")
  post_tweet(tweet.text, media = chart.filename)
  
}



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
                   tweet = (weekdays(Sys.Date()) == "Friday"),
                   email = email)



# GDP Releases -------------------------------------------------------


if ( ( as.Date( as.character(tail( fredr_release_dates(release_id = 53L) ,1)[,2]) ) == Sys.Date() ) ||
     ( as.Date( as.character(tail( fredr_release_dates(release_id = 205L) ,1)[,2]) ) == Sys.Date() ) ){
  
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
  
  GDP.Comparison.Data <- merge(US = misc.FREDdowload("US.GDP.Real"),
                               EU = misc.FREDdowload("EU.GDP.Real"),
                               UK = misc.FREDdowload("UK.GDP.Real"),
                               JP = misc.FREDdowload("JP.GDP.Real"),
                               CA = misc.FREDdowload("CA.GDP.Real") )
  
  GDP.Comparison.Data <- GDP.Comparison.Data[index(GDP.Comparison.Data)>= as.Date("2019-09-01"),]
  Evolution.Chart(Evolution.Data = GDP.Comparison.Data, tweet=FALSE, email = email)
  
}


# Activity Measures -------------------------------------------------------

Chart.Single(series="US.Activity.NYFed.Current",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Activity.NYFed.Current",6] == Sys.Date() ),
             tweet.text = paste0( "#EmpireState Manufacturing Survey for ",
                                  as.yearmon(index( tail(US.Activity.NYFed.Current, 1) )),
                                  " at ",
                                  as.numeric( tail(US.Activity.NYFed.Current, 1) ), 
                                  ", suggesting that current business conditions for the state of New York are ",
                                  ifelse(as.numeric( tail(US.Activity.NYFed.Current, 1) ) >0, "improving, ", "worsening, "),
                                  "relative to last month #rstats"),
             email = email)

Chart.Single(series="US.Activity.NYFed.Leading",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Activity.NYFed.Leading",6] == Sys.Date() ),
             tweet.text = paste0( "#EmpireState Manufacturing Survey for ",
                                  as.yearmon(index( tail(US.Activity.NYFed.Leading, 1) )),
                                  " at ",
                                  as.numeric( tail(US.Activity.NYFed.Leading, 1) ), 
                                  ", suggesting that future (leading) business conditions for the state of New York are ",
                                  ifelse(as.numeric( tail(US.Activity.NYFed.Leading, 1) ) >0, "improving, ", "worsening, "),
                                  "relative to last month #rstats"),
             email = email)

Chart.Single(series="US.IP.CapacityUtilization",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.IP.CapacityUtilization",6] == Sys.Date() ),
             tweet.text = paste0( "#CapacityUtilization for ",
                                  as.yearmon(index( tail(US.IP.CapacityUtilization, 1) )),
                                  " at ",
                                  as.numeric( tail(US.IP.CapacityUtilization, 1) ), 
                                  ". Capacity utilization measures capacity used, relative to the total available capacity, to produce finished products. #rstats"),
             email = email)

Chart.Single(series="US.Activity.ChicagoFed.Employment",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Activity.ChicagoFed.Employment",6] == Sys.Date() ),
             tweet.text = paste0( "Chicago Fed #NationalActivityIndex for Employment for ",
                                  as.yearmon(index( tail(US.Activity.ChicagoFed.Employment, 1) )),
                                  " at ",
                                  as.numeric( tail(US.Activity.ChicagoFed.Employment, 1) ), 
                                  ". Values above/below zero suggest that the national economy is expanding above/below its historical trend rate of growth. #rstats"),
             email = email)

Chart.Single(series="US.Activity.ChicagoFed",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Activity.ChicagoFed",6] == Sys.Date() ),
             tweet.text = paste0( "Chicago Fed Overall #NationalActivityIndex for ",
                                  as.yearmon(index( tail(US.Activity.ChicagoFed, 1) )),
                                  " at ",
                                  as.numeric( tail(US.Activity.ChicagoFed, 1) ), 
                                  ". Values above/below zero suggest that the national economy is expanding above/below its historical trend rate of growth. #rstats"),
             email = email)



Chart.Duo(series1="US.Activity.RetailSales", series2="US.Activity.RetailSalesExAuto",
          periods = 25,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 9L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = "#AdvanceRetailSales: With Travel Restricted, Consumers Continue Their Serious #RetailTherapy #rstats",
          email=email)



if  ( as.Date( as.character(tail( fredr_release_dates(release_id = 9L) ,1)[,2]) ) == Sys.Date() ) {
  
  BankData.Details  <- merge(US.Activity.RetailSalesBuildingMaterials=misc.FREDdowload("US.Activity.RetailSalesBuildingMaterials"),
                             #US.Activity.RetailSalesMisc=misc.FREDdowload("US.Activity.RetailSalesMisc"),
                             US.Activity.RetailSalesGeneralMerchandise=misc.FREDdowload("US.Activity.RetailSalesGeneralMerchandise"),
                             US.Activity.RetailSalesFoodDBeverageRetail=misc.FREDdowload("US.Activity.RetailSalesFoodDBeverageRetail"),
                             US.Activity.RetailSalesGasoline=misc.FREDdowload("US.Activity.RetailSalesGasoline"),
                             US.Activity.RetailSalesClothing = misc.FREDdowload("US.Activity.RetailSalesClothing"),
                             US.Activity.RetailSalesMotorvehicles=misc.FREDdowload("US.Activity.RetailSalesMotorvehicles"),
                             US.Activity.RetailSalesNonStore=misc.FREDdowload("US.Activity.RetailSalesNonStore"),
                             US.Activity.RetailSalesFoodDrinking = misc.FREDdowload("US.Activity.RetailSalesFoodDrinking")
  )
  
  BankData.Details  <- BankData.Details[index(BankData.Details) >= as.Date("2019-01-01")] / 1000
  
  plot.col    <- brewer.pal(8, "Paired")
  plot.legend = character()
  for (idx in 1:ncol(BankData.Details)) plot.legend <- c(plot.legend, 
                                                         gsub("Advance Retail Sales: ", "", 
                                                         Data.Description[Data.Description$Mnemonic == colnames(BankData.Details)[idx],3]))
  plot.ylim = c(0, 1.25*max(rowSums(BankData.Details)))
  
  tmp <- tempfile(fileext = ".png")
  png(tmp, 12, 12, "in", res = 127.5)  
  
  BankDataChart=barplot(BankData.Details, col = plot.col, 
                        border="NA", 
                        main = "Advance Retail Sales: Key Components", 
                        ylab = "Billions of Dollars",
                        xaxt = "n",
                        cex.axis = .8, cex.names = .8,
                        ylim = plot.ylim)
  axis(1, at=BankDataChart,
       as.yearmon( index(BankData.Details)) , cex.axis = .7)
  legend("topleft", plot.legend, fill = plot.col, cex=0.75, bty = "n")
  legend("bottomleft", "Data: St. Louis FRED", cex=.5)
  
  dev.off()
  post_tweet("#AdvanceRetailSales Report: Behind the Headline #rstats", media = tmp)
}

Chart.Duo(series1="US.Activity.RetailSalesSportsGoods", series2="US.Activity.RetailSalesFurniture",
          periods = 25,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 9L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = "Bought a #Peleton During the Pandemic? Sales of #SportingGoods Skyrocketed in 2021. https://bit.ly/3BXLMFM #AdvanceRetailSales #rstats",
          email=email)






# Labor Market -------------------------------------------------------


  # Chart.Single(series="US.JOLTS.QuitsRate",
  #              periods = 25,
  #              tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 192L) ,1)[,2]) ) == Sys.Date() ),  
  #              tweet.text = "#JOLTS update: Quits rate #rstats",
  #              email = email)
  # 
  # Chart.Single(series="US.JOLTS.HireRate",
  #              periods = 25,
  #              tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 192L) ,1)[,2]) ) == Sys.Date() ),  
  #              tweet.text = "#JOLTS update: Hire rate #rstats",
  #              email = email)
  # 
  # Chart.Single(series="US.JOLTS.JobOpeningsRate",
  #              periods = 25,
  #              tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 192L) ,1)[,2]) ) == Sys.Date() ),  
  #              tweet.text = "#JOLTS update: Rate of job openings #rstats",
  #              email = email)
  
  Chart.Duo(series1="US.Activity.InitialClaims", series2="US.Activity.ContinuedClaims.4W.MA",
            periods = 25,
            tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 180L) ,1)[,2]) ) == Sys.Date() ),  
            tweet.text = "Initial and continued claims (source: FRED) #rstats",
            email=email)

  
if ( as.Date( as.character(tail( fredr_release_dates(release_id = 50L) ,1)[,2]) ) == Sys.Date() ) {
  tmp <- tempfile(fileext = ".png")
  png(tmp, 12, 12, "in", res = 127.5)  
  
  misc.GDPExpansionPlot(series = "US.Payroll")
  
  dev.off()
  post_tweet("#Payroll Update: Comparing the current #Recovery of the #LaborMarket to previous recessions #rstats", media = tmp)
}


Chart.Four(series1 = "US.Unemployment", 
           series2 = "US.Unemployment.ParticipationRate", 
           series3 = "US.Unemployment.PartTimeEconomicReasons",
           series4 = "US.Unemployment.PartTimeNonEconomicReasons", 
           periods = 25,
           tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 50L) ,1)[,2]) ) == Sys.Date() ),
           tweet.text = "#Payroll Update: #LaborMarket is improving with #unemployment down, but the #ParticipationRate still hasn't fully recovered #rstats",
           email = email)

Chart.Four(series1 = "US.Unemployment.EmploymentToPopulation", 
           series2 = "US.JOLTS.JobOpeningsRate", 
           series3 = "US.Unemployment.MarginallyAttached",
           series4 = "US.Unemployment.U1", 
           periods = 25,
           tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 50L) ,1)[,2]) ) == Sys.Date() ),
           tweet.text = "#Payroll Update: Details behind the topline number (source: FRED) #LaborMarket #rstats",
           email = email)


# Inflation -------------------------------------------------------

Chart.Single(series = "US.CPI.Headline.mm.yy",
             period = 25,
             tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 10L) ,1)[,2]) ) == Sys.Date() ),
             tweet.text = paste0("Inflation running at ",
                                 round( tail(US.CPI.Headline.mm.yy,1), 2),
                                 "% y/y, ",
                                 ifelse( (as.numeric(tail(US.CPI.Headline.mm.yy,1)) - as.numeric(tail(US.CPI.Headline.mm.yy,2)[1]) > 0 ),
                                         "up ", "down "),
                                 round( as.numeric(tail(US.CPI.Headline.mm.yy,1)) - as.numeric(tail(US.CPI.Headline.mm.yy,2)[1]), 2),
                                 "p.p. from last month. FED target is 2%. #rstats"),
             email = email)

Chart.Four(series1 = "US.CPI.Core.mm.yy",
           series2 = "US.CPI.FoodBeverages.mm.yy",
           series3 = "US.CPI.Housing.mm.yy",
           series4 = "US.CPI.Apparel.mm.yy",
           periods = 25,
           tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 10L) ,1)[,2]) ) == Sys.Date() ),
           tweet.text = "#CPI Update: Details behind the topline number (source: FRED) #Inflation #rstats",
           email = email)


Chart.Four(series1 = "US.CPI.Medical.mm.yy",
           series2 = "US.CPI.Recreation.mm.yy",
           series3 = "US.CPI.Education.mm.yy",
           series4 = "US.CPI.Other.mm.yy",
           periods = 25,
           tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 10L) ,1)[,2]) ) == Sys.Date() ),
           tweet.text = "#CPI Update: Details behind the topline number (source: FRED) #Inflation #rstats",
           email = email)


Chart.InflationOverview(series = c( "US.CPI.Headline.mm.yy","US.CPI.FoodBeverages.mm.yy", "US.CPI.Housing.mm.yy","US.CPI.Apparel.mm.yy",
                                    "US.CPI.Medical.mm.yy", "US.CPI.Recreation.mm.yy", "US.CPI.Education.mm.yy"),
                        tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 10L) ,1)[,2]) ) == Sys.Date() ),
                        tweet.text = "How broad based is the increase in #Inflation? Comparing  today to pre-pandemic shows increases in almost all major #CPI #Components #rstats",
                        email = email)


Chart.Duo(series1="US.CPI.AlcoholicBeverages.mm.yy", series2="US.CPI.AlcoholicBeveragesAway.mm.yy", 
          periods = 25,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 10L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = paste0("On average, prices for #AlcoholicBeverages (away from home) have increased ", 
                              round( tail(US.CPI.AlcoholicBeveragesAway.mm.yy,1), 2),
                              "% y/y, ",
                              ifelse( (as.numeric(tail(US.CPI.AlcoholicBeveragesAway.mm.yy,1)) - as.numeric(tail(US.CPI.AlcoholicBeveragesAway.mm.yy,2)[1]) > 0 ),
                                      "up ", "down "),
                              round( as.numeric(tail(US.CPI.AlcoholicBeveragesAway.mm.yy,1)) - as.numeric(tail(US.CPI.AlcoholicBeveragesAway.mm.yy,2)[1]), 2),
                              "p.p. from last month. Posting for a friend. #rstats"),
                              email=email)





# Housing -------------------------------------------------------

Chart.Single(series="US.Housing.CaseShiller",
             periods = 25,
             tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 199L) ,1)[,2]) ) == Sys.Date() ),  
             tweet.text = "S&P/Case-Shiller U.S. National Home Price Index (source: FRED, series CSUSHPINSA) #housing #rstats",
             email = email)


Chart.Single(series="US.Housing.30YMortgageRate",
             periods = 35,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Housing.30YMortgageRate",6] == Sys.Date() ),
             tweet.text = paste0( "Average US 30-Year Fixed Rate #Mortgage at ",
                                  as.numeric( tail(US.Housing.30YMortgageRate, 1) ), 
                                  ifelse(as.numeric( diff( tail(US.Housing.30YMortgageRate, 2)) )>0, "%, up ", ", down "),
                                  as.numeric( diff( tail(US.Housing.30YMortgageRate, 2)) ),
                                  " p.p. from last week. #rstats"),
             email = email)

Chart.Four(series1="US.Housing.30YMortgageRate",
           series2="US.Housing.15YMortgageRate",
             series3="US.Housing.51ARMMortgageRate",
             series4="US.Housing.30YJumboMortgageRate",
             periods = 35,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Housing.30YMortgageRate",6] == Sys.Date() ),
             tweet.text = paste0( "#Thinking about a new McMansion? Average US 30-Year Fixed Rate #Mortgage last week at ",
                                  as.numeric( tail(US.Housing.30YMortgageRate, 1) ), 
                                  ifelse(as.numeric( diff( tail(US.Housing.30YMortgageRate, 2)) )>0, "%, up ", ", down "),
                                  as.numeric( diff( tail(US.Housing.30YMortgageRate, 2)) ),
                                  " p.p. from last week. 5/1 ARM at ",
                                  as.numeric( tail(US.Housing.51ARMMortgageRate, 1) ),
                                  ".#rstats"),
             email = email)


# Financial Soundness -------------------------------------------------------

Chart.Single(series="US.HouseholdDebt",
             periods = 35,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.HouseholdDebt",6] == Sys.Date() ),
             tweet.text = paste0( "US #HouseholdDebt in ",
                                  as.yearmon(index( tail(US.HouseholdDebt, 1) )),
                                  " at ",
                                  as.numeric( tail(US.HouseholdDebt, 1) ), 
                                  ". This index measures the overall level of household indebtedness (commonly related to consumer loans and mortgages) as a share of GDP. #rstats"),
             email = email)

# Motor Vehicles -------------------------------------------------------

Chart.Duo(series1="US.Auto.Autosales", series2="US.Auto.InventorySalesRatio",
          periods = 25,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 93L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = "Low vehicles sales and low inventory the result of supply chain discriptions (source: FRED) #vehiclesales #rstats",
          email=email)


if ( as.Date( as.character(tail( fredr_release_dates(release_id = 93L) ,1)[,2]) ) == Sys.Date() ) {

    tmp <- tempfile(fileext = ".png")
    png(tmp, 24, 12, "in", res = 127.5)
    
    par(mfrow=c(1,2))
    Chart.DoublePanel(series1 = "US.Auto.LightAutos", series2 = "US.Auto.LightTrucks", periods = 25)
    Chart.DoublePanel(series1 = "US.Auto.AutosDomestic", series2 = "US.Auto.AutosForeign", periods = 25)
    
    par(mfrow=c(1,1))
    dev.off()
    post_tweet(paste0("#SupplyDisruptions continue to hit #trucks: Total #LightVehicle sales at ", 
                      round( tail(US.Auto.Autosales,1), 2),
                      "M units (saar), ",
                      ifelse( (as.numeric(tail(US.Auto.Autosales,1)) - as.numeric(tail(US.Auto.Autosales,2)[1]) > 0 ),
                              "up ", "down "),
                      round( as.numeric(tail(US.Auto.Autosales,1)) - as.numeric(tail(US.Auto.Autosales,2)[1]), 1),
                      "M units from last month. #rstats"), media = tmp)
}


# Banking Sector -------------------------------------------------------

Chart.Duo(series1="US.FDIC.NetChargeOffRateTotalLoans", series2="US.FDIC.LoanLossProvisions",
          periods = 35,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 482L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = paste0("Banking Update: Net Charge-Off Rates at ", 
                              round( tail(US.FDIC.NetChargeOffRateTotalLoans,1), 2),
                              "%, ",
                              ifelse( (as.numeric(tail(US.FDIC.NetChargeOffRateTotalLoans,1)) - as.numeric(tail(US.FDIC.NetChargeOffRateTotalLoans,2)[1]) > 0 ),
                                      "up ", "down "),
                              round( as.numeric(tail(US.FDIC.NetChargeOffRateTotalLoans,1)) - as.numeric(tail(US.FDIC.NetChargeOffRateTotalLoans,2)[1]), 2),
                              "p.p. from last Quarter  (source: FRED) #rstats"),
                              email=email) 


Chart.Duo(series1="US.FDIC.InstitutionsWithEarningsGain", series2="US.FDIC.UnprofitableInstitutions",
          periods = 35,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 482L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = paste0("Banking Update: Number of Unprofitable Institutions at ", 
                              round( tail(US.FDIC.UnprofitableInstitutions,1), 2),
                              ", ",
                              ifelse( (as.numeric(tail(US.FDIC.UnprofitableInstitutions,1)) - as.numeric(tail(US.FDIC.UnprofitableInstitutions,2)[1]) > 0 ),
                                      "up ", "down "),
                              round( as.numeric(tail(US.FDIC.UnprofitableInstitutions,1)) - as.numeric(tail(US.FDIC.UnprofitableInstitutions,2)[1]), 2),
                              " from last Quarter  (source: FRED) #rstats"),
                              email=email) 



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
  legend("bottomleft", "Data: St. Louis FRED", cex=.5)
  
  dev.off()
  post_tweet("Increase in Bank Balance Sheets Post-Pandemic Mostly Driven by Higher Cash and Treasuries/Agencies #BankLending #rstats", media = tmp)
}

Chart.Duo(series1="US.Banks.AutoLoansSecuritized.qq.yy", series2="US.Banks.StudentLoans.qq.yy",
          periods = 35,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 14L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = paste0("#StudentLoanDebt high, but growth is continues to slide: Student loans grow at ", 
                              round( tail(US.Banks.StudentLoans.qq.yy,1), 2),
                              "%, ",
                              ifelse( (as.numeric(tail(US.Banks.StudentLoans.qq.yy,1)) - as.numeric(tail(US.Banks.StudentLoans.qq.yy,2)[1]) > 0 ),
                                      "up ", "down "),
                              round( as.numeric(tail(US.Banks.StudentLoans.qq.yy,1)) - as.numeric(tail(US.Banks.StudentLoans.qq.yy,2)[1]), 2),
                              " p.p. from last quarter #rstats"),
          email=email)



if ( Data.Description[Data.Description$Mnemonic == "US.SLOOS.AutoLoansStandards",6] == Sys.Date() - days(1) ) {
  
  x  <- merge(US.SLOOS.AutoLoansStandards = misc.FREDdowload("US.SLOOS.AutoLoansStandards"),
              US.SLOOS.CreditCardsStandards = misc.FREDdowload("US.SLOOS.CreditCardsStandards"),
              US.SLOOS.ConsumerLoansStandards = misc.FREDdowload("US.SLOOS.ConsumerLoansStandards"),
              US.SLOOS.AutoLoansDemand = misc.FREDdowload("US.SLOOS.AutoLoansDemand"),
              US.SLOOS.CreditCardDemand = misc.FREDdowload("US.SLOOS.CreditCardDemand"),
              US.SLOOS.ConsumerLoansDemand = misc.FREDdowload("US.SLOOS.ConsumerLoansDemand")
  )
  
  x        = tail(x, 2)
  x.names  = names(x)
  x.period = as.character( as.yearqtr( index(x) ) )
  x.period = paste0("Q", quarter(index(x)) )
  x        = as.vector(x)
  names(x) = paste(rep(x.names, each = 2), x.period)
  
  plot.col    <- brewer.pal(12, "Paired")
  
  plot.legend = character()
  for (idx in 1:length(x.names)) plot.legend <- c(plot.legend, Data.Description[Data.Description$Mnemonic == (x.names)[idx],3])
  plot.legend <- gsub("Net Percentage of Domestic Banks Tightening Standards for ", "", plot.legend)
  plot.legend <- gsub("Net Percentage of Domestic Banks Reporting Stronger Demand for ", "", plot.legend)
  plot.legend <- gsub("Consumer Loans Excluding Credit Card and Auto Loans", "Consumer Loans \n(ex. auto/card)", plot.legend)
  
  plot.ylim = 1.5*range(x) 
  
  tmp <- tempfile(fileext = ".png")
  png(tmp, 12, 12, "in", res = 127.5)  
  
  x.plot = barplot((x), col = plot.col, 
                        border="NA", 
                        main = "Senior Loan Officers' Opinion Survey on Bank Lending Practices: \n Net % of Domestic Banks", 
                        xaxt = "n",
                   ylab = Data.Description[Data.Description$Mnemonic == x.names[1],5],
                        ylim = plot.ylim)
  
  text(x.plot, .8*plot.ylim[2], x.period, cex = .8)
  text(mean(x.plot[1:(length(x)/2)]), .95*plot.ylim[2], "Tightening Standards", font = 2)
  text(mean(x.plot[(length(x)/2):(length(x))]), .95*plot.ylim[2], "Reporting Stronger Demand", font = 2)
  text(rowMeans(cbind(x.plot[-(length(x.plot))], x.plot[-1]))[c(TRUE, FALSE)],
       .83*plot.ylim[1], plot.legend, cex = .8)
  
  abline(v = mean(x.plot), lty = 2)
  abline(h=0, lwd=1)
  legend("bottomleft", "Data: St. Louis FRED", cex=.5)
  
  dev.off()
  post_tweet(paste0("Need $$$? Banks still easing lending standards in ",
                    x.period[2],
                    " for auto loans, credit card loans, and consumer loans #SLOOS #rstats"), media = tmp)
}



if ( Data.Description[Data.Description$Mnemonic == "US.SLOOS.IncreasingSpreadsLargeFirms",6] == Sys.Date() - days(1) ) {
  
  x  <- merge(US.SLOOS.IncreasingSpreadsLargeFirms = misc.FREDdowload("US.SLOOS.IncreasingSpreadsLargeFirms"),
              US.SLOOS.CILoanDemandLargeFirms = misc.FREDdowload("US.SLOOS.CILoanDemandLargeFirms"),
              US.SLOOS.CILoanStandardsLargeFirms = misc.FREDdowload("US.SLOOS.CILoanStandardsLargeFirms"),
              US.SLOOS.IncreasingSpreadsSmallFirms = misc.FREDdowload("US.SLOOS.IncreasingSpreadsSmallFirms"),
              US.SLOOS.CILoanDemandSmallFirms = misc.FREDdowload("US.SLOOS.CILoanDemandSmallFirms"),
              US.SLOOS.CILoanStandardsSmallFirms = misc.FREDdowload("US.SLOOS.CILoanStandardsSmallFirms")
  )
  
  x        = tail(x, 2)
  x.names  = names(x)
  x.period = as.character( as.yearqtr( index(x) ) )
  x.period = paste0("Q", quarter(index(x)) )
  x        = as.vector(x)
  names(x) = paste(rep(x.names, each = 2), x.period)
  plot.col = brewer.pal(12, "Paired")
  
  plot.legend = character()
  for (idx in 1:length(x.names)) plot.legend <- c(plot.legend, Data.Description[Data.Description$Mnemonic == (x.names)[idx],3])
  plot.legend <- gsub("Net Percentage of Domestic Banks", "", plot.legend)
  
  plot.ylim = c(min(x,na.rm = TRUE) - 10, max(x,na.rm = TRUE) + 20 ) * 1.25
  
  tmp <- tempfile(fileext = ".png")
  png(tmp, 12, 12, "in", res = 127.5)  
  
  x.plot = barplot((x), col = plot.col, 
                   border="NA", 
                   main = "Senior Loan Officers' Opinion Survey on Bank Lending Practices: \n Net % of Domestic Banks Reporting:", 
                   xaxt = "n",
                   ylab = Data.Description[Data.Description$Mnemonic == x.names[1],5],
                   ylim = plot.ylim)
  
  text(x.plot, .95*plot.ylim[2], x.period, cex = .8)
  text(mean(x.plot[1:(length(x)/2)]), .65*plot.ylim[2], "Commercial and Industrial Loans \n from Large and Middle-Market Firms", font = 2)
  text(mean(x.plot[(length(x)/2):(length(x))]), .65*plot.ylim[2], "Commercial and Industrial Loans \n from Small Firms", font = 2)
  text(rowMeans(cbind(x.plot[-(length(x.plot))], x.plot[-1]))[c(TRUE, FALSE)],.8*plot.ylim[1],
       c("Increasing\n Spreads", "Stronger\n Demand", "Tighter\n Standards"), cex = .8)
  grid()
  abline(v = mean(x.plot), lty = 2)
  abline(h=0, lwd=1)
  legend("bottomleft", "Data: St. Louis FRED", cex=.65, bty = "n")
  
  dev.off()
  post_tweet(paste0("Cheap Financing? Amidst strong demand, fewer banks still easing lending standards for C&I loans in ",
                    x.period[2]
                    ,"; keeping an eye on Spreads of Loan Rates over Banks' Funding Costs #SLOOS #rstats"), media = tmp)
}


if ( Data.Description[Data.Description$Mnemonic == "US.SLOOS.CRELoanDemandLand",6] == Sys.Date() - days(1) ) {
  
  x  <- merge(US.SLOOS.CRELoanDemandLand = misc.FREDdowload("US.SLOOS.CRELoanDemandLand"),
              US.SLOOS.CRELoanStandardsLand = misc.FREDdowload("US.SLOOS.CRELoanStandardsLand"),
              US.SLOOS.CRELoanDemandMultifamily = misc.FREDdowload("US.SLOOS.CRELoanDemandMultifamily"),
              US.SLOOS.CRELoanStandardsMultifamily = misc.FREDdowload("US.SLOOS.CRELoanStandardsMultifamily"),
              US.SLOOS.CRELoanDemandNonresidential = misc.FREDdowload("US.SLOOS.CRELoanDemandNonresidential"),
              US.SLOOS.CRELoanStandardsNonresidential = misc.FREDdowload("US.SLOOS.CRELoanStandardsNonresidential")
  )
  
  x        = tail(x, 2)
  x.names  = names(x)
  x.period = as.character( as.yearqtr( index(x) ) )
  x.period = paste0("Q", quarter(index(x)) )
  x        = as.vector(x)
  names(x) = paste(rep(x.names, each = 2), x.period)
  plot.col = brewer.pal(12, "Paired")
  
  plot.legend = character()
  for (idx in 1:length(x.names)) plot.legend <- c(plot.legend, Data.Description[Data.Description$Mnemonic == (x.names)[idx],3])
  plot.legend <- gsub("Net Percentage of Domestic Banks", "", plot.legend)

  plot.ylim = c(min(x,na.rm = TRUE) - 20, max(x,na.rm = TRUE) + 20 ) * 1.25
  
  tmp <- tempfile(fileext = ".png")
  png(tmp, 12, 12, "in", res = 127.5)  
  
  x.plot = barplot((x), col = plot.col, 
                   border="NA", 
                   main = "Senior Loan Officers' Opinion Survey on Bank Lending Practices: \n Net % of Domestic Banks Reporting:", 
                   xaxt = "n",
                   ylab = Data.Description[Data.Description$Mnemonic == x.names[1],5],
                   ylim = plot.ylim)
  
  text(x.plot, .95*plot.ylim[2], x.period, cex = .8)
  text(mean(x.plot[1:(length(x)/3)]), .65*plot.ylim[2], "CRE Loans for \n Land Development", font = 2)
  text(mean(x.plot[(1+(length(x)/3)):(length(x)*2/3)]), .65*plot.ylim[2], "CRE Loans Secured by\n Multifamily Residential", font = 2)
  text(mean(x.plot[(1+(length(x)*2/3)):(length(x))]), .65*plot.ylim[2], "CRE Loans Secured by\n Nonfarm, Non-Residential", font = 2)
  text(rowMeans(cbind(x.plot[-(length(x.plot))], x.plot[-1]))[c(TRUE, FALSE)],.70*plot.ylim[1],
       c("Stronger\n Demand", "Tighter\n Standards"), cex = .8)
  grid()
  abline(v = mean(x.plot[(length(x.plot)/3):(1+length(x.plot)/3)]), lty = 2)
  abline(v = mean(x.plot[(length(x.plot)*2/3):(1+length(x.plot)*2/3)]), lty = 2)
  
  abline(h=0, lwd=1)
  legend("bottomleft", "Data: St. Louis FRED", cex=.65, bty = "n")
  
  dev.off()
  post_tweet(paste0("Update on #CommercialRealEstate Loans: Demand less strong and tighter standards in ",
                    x.period[2]
                    ," for land development and construction loans suggest CRE loans harder to come by #SLOOS #rstats"), media = tmp)
}


# Transportation -------------------------------------------------------

if ( Data.Description[Data.Description$Mnemonic == "US.Transportation.Air",6] == Sys.Date()  )
  Chart.Four(series1="US.Transportation.Air", 
             series2="US.Transportation.Airtraffic.Passenger", 
             series3="US.Transportation.Railpassenger",
             series4 = "US.Transportation.PublicTransit", 
             periods = 25,
             tweet = TRUE,
             tweet.text = "Air Traffic coming back, Public Transportation not so much #rstats",
             email = email)

if ( Data.Description[Data.Description$Mnemonic == "US.Transportation.Rail",6] == Sys.Date()  )
  Chart.Duo(series1="US.Transportation.Rail", series2="US.Transportation.Index",
            periods = 25,
            tweet = TRUE,
            tweet.text = "Transportation Update: Public Transit Ridership / Freight transportation services index (for-hire trucking
          freight railroad services, inland waterway traffic, pipeline movements, air freight) #rstats",
            email=email)


# COVID -------------------------------------------------------

if (weekdays(Sys.Date()) %in% c("Tuesday", "Thursday", "Sunday")) {
  
  covid.countries = c("United States", "China", "Japan", "Germany", "United Kingdom", "India", "France", "Italy", "Brazil", "Canada")
  covid.cases     = covid19(covid.countries, verbose = FALSE)
  plot.col        = brewer.pal(10, "Paired")
  
  chart.filename = "COVID19.png"
  png(filename=chart.filename, 24, 12, "in", res=127.5)
  
  par(mfrow = c(2,3))
  covid.countries = c("United States", "China", "Japan", "Germany", "United Kingdom", "India")
  for (idx in covid.countries) {
    x = covid.cases[covid.cases$administrative_area_level_1==idx, c("date", "confirmed")]
    x = zoo(diff(x$confirmed), as.Date(x$date))
    plot(x, ylab = "Daily Cases (blue) / 7-day average (red)", main = idx, 
         xlab = "", xaxt = "n", col = plot.col[1], lwd = 3, cex.main = 2.2, cex.axis = 1.4, cex.lab = 1.3)
    lines(rollmean(x, k=7), col = plot.col[6], lwd = 3)
    axis(1, floor_date(index(x), 'month'), as.yearmon(floor_date(index(x), 'month')), cex.axis = 1)
    legend("topleft", "Data: https://covid19datahub.io", cex=1.2, bty = "n")
    grid()
  }
  par(mfrow = c(1,1))
  
  dev.off()
  email <<- gm_attach_file(email, chart.filename)
  post_tweet("Update on daily #COVID19 cases in the 6 largest economies #rstats", media = chart.filename)
  
  chart.filename = "COVID19Hospitalizations.png"
  png(filename=chart.filename, 24, 12, "in", res=127.5)
  
  
  par(mfrow = c(2,3))
  covid.countries = c("United States", "Japan", "Germany", "France", "Italy", "Canada")
  for (idx in covid.countries) {
    x = covid.cases[covid.cases$administrative_area_level_1==idx, c("date", "hosp")]
    x = zoo((x$hosp), as.Date(x$date))
    if (length(na.omit(x)) > 0) {
      plot(x, ylab = "Number of hospitalized patients on date", main = idx,
           xlab = "", xaxt = "n", col = plot.col[2], lwd = 3, cex.main = 2.2, cex.axis = 1.4, cex.lab = 1.3)
      
      points(index(tail(na.omit(x[,idx]),1)), tail(na.omit(x[,idx]),1), pch = 19, lwd = 5, col = plot.col[2])
      # text((index(tail(na.omit(x[,idx]),1)) - months(3)), 1.25 * tail(na.omit(x[,idx]),1), 
      #      paste0("Peak: ", max(x[,idx], na.rm = TRUE),
      #        "\nLatest obs: ", tail(na.omit(x[,idx]),1)), font = 2)
      axis(1, floor_date(index(x), 'month'), as.yearmon(floor_date(index(x), 'month')), cex.axis = 1)
      legend("bottomleft", "Data: https://covid19datahub.io", cex=1.2, bty = "n")
      legend("topleft", paste0("Peak: ", max(x[,idx], na.rm = TRUE),
                               "\nLatest obs: ", tail(na.omit(x[,idx]),1)), cex = 1.5, bty = "n")
      grid()
    } else cat("\n- Skipping ", idx)
  }
  par(mfrow = c(1,1))
  
  dev.off()
  email <<- gm_attach_file(email, chart.filename)
  post_tweet("Update on #COVID19 Hospitalizations in major economies.  #rstats", media = chart.filename)
  
  
  chart.filename = "COVID19ICU.png"
  png(filename=chart.filename, 24, 12, "in", res=127.5)
  
  par(mfrow = c(2,3))
  covid.countries = c("United States", "Japan", "Germany", "France", "Italy", "Canada")
  for (idx in covid.countries) {
    x = covid.cases[covid.cases$administrative_area_level_1==idx, c("date", "icu")]
    x = zoo((x$icu), as.Date(x$date))
    if (length(na.omit(x)) > 0) {
      plot(x, ylab = "Number of hospitalized patients in intensive therapy on date", main = idx,
           xlab = "", xaxt = "n", col = plot.col[2], lwd = 3, cex.main = 2.2, cex.axis = 1.4, cex.lab = 1.3)
      points(index(tail(na.omit(x[,idx]),1)), tail(na.omit(x[,idx]),1), pch = 19, lwd = 5, col = plot.col[2])
      axis(1, floor_date(index(x), 'month'), as.yearmon(floor_date(index(x), 'month')), cex.axis = 1)
      legend("bottomleft", "Data: https://covid19datahub.io", cex=1.2, bty = "n")
      legend("topleft", paste0("Peak: ", max(x[,idx], na.rm = TRUE),
                               "\nLatest obs: ", tail(na.omit(x[,idx]),1)), cex = 1.5, bty = "n")
      grid()
    } else cat("\n- Skipping ", idx)
  }
  par(mfrow = c(1,1))
  dev.off()
  
  email <<- gm_attach_file(email, chart.filename)
  post_tweet("Update on #COVID19 Intensive Care Patients in major economies. #rstats", media = chart.filename)
  
  
  chart.filename = "COVID19VaccinationRatio.png"
  png(filename=chart.filename, 24, 12, "in", res=127.5)
  
  covid.countries = c("United States", "Japan", "Germany", "United Kingdom", "India", "France", "Italy", "Brazil", "Canada")
  
  x = covid.cases[, c("date", "administrative_area_level_1", "people_vaccinated", "population")]
  x$vaccinated_ratio = x$people_vaccinated/x$population*100
  
  y = melt(x, id=c("date", "administrative_area_level_1") )
  x = cast(y[y$variable=="vaccinated_ratio",], date ~ administrative_area_level_1 + variable)
  x = zoo(x, as.Date(x$date))
  colnames(x) = gsub( "_vaccinated_ratio","", colnames(x))
  x= x[index(x) > as.Date("2020-12-01"),covid.countries]
  
  plot(x[,1], type = "n", ylab = "Vaccinated as % of population", main = "Vaccination Rates Among Major Economies", ylim = c(0,100),
       xlab = "", 
       #cex.main = 2.2, cex.axis = 1.4, cex.lab = 1.3, 
       xaxt = "n",
       xlim = c(head(index(x),1), tail(index(x),1))  )
  
  
  axis(1, seq( head(index(x),1), tail(index(x),1) , "months" ),
       as.character( as.yearmon( seq( head(index(x),1), tail(index(x),1), "months" ))), cex.axis = .8)
  
  for (idx in 1:length(covid.countries)) {
    lines( x[,idx], col = plot.col[idx], lwd = 3)
    points(index(tail(na.omit(x[,idx]),1)), tail(na.omit(x[,idx]),1), pch = 19, lwd = 5, col = plot.col[idx])
  }
  
  legend("topleft", colnames(x), fill = plot.col, bty = "n")
  legend("bottomright", "Data: https://covid19datahub.io", bty = "n")
  grid()
  
  dev.off()
  email <<- gm_attach_file(email, chart.filename)
  post_tweet("Update on #VaccinationStatus in major economies. Details here: https://whyitmatters.netlify.app/posts/2021-11-10-covid19-update/  #rstats", media = chart.filename)
  
}


gm_send_message(email)

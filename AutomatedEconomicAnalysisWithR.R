#!/usr/bin/R

# Instructions for running CRON on a Mac: https://stackoverflow.com/questions/32781884/is-crontab-broken-on-osx-el-capitan and https://osxdaily.com/2020/04/27/fix-cron-permissions-macos-full-disk-access/

setwd("~/Documents/R/")
sink("EconomicAnalsysiUsingR.log")

cat("\n---------------------------------------------------\n")
cat("Script execution: ", as.character(Sys.time()) )
cat("---------------------------------------------------\n\n\n")


pkgTest <- function(x){
  suppressMessages(
    suppressWarnings(
      if (!require(x,character.only = TRUE)) {
        install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE)) stop("Package not found")
      }
    )
  )
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

# Test that FRED is online ####
suppressMessages( test.online <- try(getSymbols("GDPC1",src='FRED')) )

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
             "US.GDP.Real.annualizedqq"="A191RL1Q225SBEA", 
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
             "US.GDP.TradeBalance"="BOPGSTB",
             "US.GDP.Imports"="BOPTIMP",
             "US.GDP.Exports"="BOPTEXP",
             "US.GDP.ConstructionsSpending"="MPCTXXXXS",
             "US.GDP.ConstructionsSpendingAbsoluteValue"="TTLCONS",
             
             "US.GDP.RealDisposableIncomePerCapita"="A229RX0",
             "US.GDP.PersonalSavingRate"="PSAVERT",
             
             "US.Government.Debt"="GFDEBTN",
             "US.Government.DebtToGDP"="GFDEGDQ188S",
             
             "US.FlowOfFunds.HouseholdNetWorth"="BOGZ1FL192090005Q",
             
             "US.Payroll"="PAYEMS",
             "US.IP.CapacityUtilization" = "TCU",
             "US.IP.IndustrialProduction" = "INDPRO",
             "US.IP.IndustrialProduction.qq" = "INDPRO",
             "US.Unemployment"="UNRATE", 
             "US.Unemployment.U1" = "U1RATE", 
             "US.Unemployment.U6" = "U6RATE", 
             "US.Unemployment.U2" = "U2RATE",
             "US.Unemployment.PartTimeEconomicReasons" = "LNS12032194", 
             "US.Unemployment.PartTimeNonEconomicReasons" = "LNS12032200",
             "US.Unemployment.MarginallyAttached" = "LNU05026642",
             "US.Unemployment.ParticipationRate"="CIVPART",
             "US.Unemployment.EmploymentToPopulation"="EMRATIO",
             
             "US.Productivity.Nonfarm"="PRS85006092",
             
             
             "US.Activity.ChicagoFed.Employment" = "EUANDH",
             "US.Activity.ChicagoFed.Inventory" = "SOANDI",
             "US.Activity.ChicagoFed" = "CFNAI",
             "US.Activity.NYFed.Current" = "GACDISA066MSFRBNY",
             "US.Activity.NYFed.Leading" = "GAFDISA066MSFRBNY",
             "US.Activity.PhillyFed.DeliveryTime" = "DTCDFSA066MSFRBPHI",
             "US.Activity.PhillyFed.DeliveryIncreasedTime" = "DTCISA156MSFRBPHI",
             "US.Activity.PhillyFed.PricesPaid" = "PPCDFSA066MSFRBPHI",
             "US.Activity.PhillyFed.CurrentWorkHours" = "AWCDFSA066MSFRBPHI",
             "US.Activity.PhillyFed.NonManufacturingCurrentPricesReceived" = "PRBNDIF066MSFRBPHI",
             "US.Activity.PhillyFed.NonManufacturingCurrentPricesPaid" = "PPBNDIF066MSFRBPHI",
             "US.Activity.PhillyFed.NonManufacturingCurrentUnfilledOrders" = "UOBNDIF066MSFRBPHI",
             "US.Activity.PhillyFed.NonManufacturingCurrentWages" = "WBBNDIF066MSFRBPHI",
             "US.Activity.StLouisFed.FinancialStress" = "STLFSI2",
             "US.Activity.Texas.CurrentPricesRawMaterials" = "PRMISAMFRBDAL",
             "US.Activity.Texas.CurrentDeliveryTime" = "DTMSAMFRBDAL",
             "US.Activity.Texas.CurrentWages" = "WGSISAMFRBDAL",
             "US.Activity.Texas.CurrentPricesReceived" = "PFGSAMFRBDAL",
             "US.Activity.RetailSales" = "RSAFS",
             "US.Activity.RetailSalesqq" = "MARTSMPCSM44X72USS",
             "US.Activity.RetailSalesExAuto" = "RSFSXMV",
             "US.Activity.RetailSalesExAutoqq" = "MARTSMPCSM44Y72USS",
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
             "US.Activity.ECommerceAsShareOfRetail" = "ECOMPCTSA",
             "US.Activity.ADP" = "NPPTTL",
             "US.Activity.InitialClaims" = "ICSA",
             "US.Activity.ContinuedClaims.4W.MA" = "CC4WSA",
             "US.Activity.ContinuedClaims" = "CCSA",
             "US.Activity.InventoryRatioManufacturing" = "MNFCTRIRSA",
             "US.Activity.InventoryRatioRetailers" = "RETAILIRSA",
             "US.Activity.InventoryRatioWholesalers" = "WHLSLRIRSA",
             "US.Activity.ExpectedSalesRevenue" = "ATLSBUSRGEP",
             "US.Activity.ExpectedEmploymentGrowth" = "ATLSBUEGEP",
             
             "US.Activity.ManufacturingOrdersDurable" = "DGORDER",
             "US.Activity.ManufacturingOrdersNonDurable" = "AMNMNO",
             "US.Activity.NewBusinessApplicationsTotal" = "BABATOTALSAUS",
             "US.Activity.NewBusinessApplicationsHighPropensity" = "BAHBATOTALSAUS",
             
             
             "US.JOLTS.QuitsRate" = "JTSQUR",
             "US.JOLTS.QuitsNumber" = "JTSQUL",
             "US.JOLTS.HireRate" = "JTSHIR",
             "US.JOLTS.JobOpeningsRate" ="JTSJOR",
             "US.Unemployment.WageGrowth" = "CES0500000003",
             "US.Unemployment.TotalCostIndex" = "ECIALLCIV",
             "US.Indeed.TotalJobs" = "IHLCHGUS",
             "US.Indeed.NewJobs" = "IHLCHGNEWUS",
             
             "US.LaborMarketFlows.UnemployedToEmployed" = "LNS17100000",
             "US.LaborMarketFlows.NotInLaborForceToEmployed" = "LNS17200000",
             "US.LaborMarketFlows.UnemployedToNotInLaborForce" = "LNS17900000",
             "US.LaborMarketFlows.NotInLaborForceToUnemployed" = "LNS17600000",
             "US.LaborMarket.UnitLaborCosts" = "PRS85006111",
             
             "US.PCE.Headline.mm.yy"="PCEPI",
             "US.PCE.Headline"="PCEPI",
             "US.CPI.Headline.mm.yy"="CPIAUCSL", 
             "US.PCE.Core"="PCEPILFE",
             "US.PCE.Core.mm.yy"="PCEPILFE", 
             "US.CPI.Headline"="CPIAUCSL", 
             "US.CPI.Core"="CPILFESL",
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
             
             "US.CPI.Expected1YInflation" = "EXPINF1YR",
             "US.CPI.Expected2YInflation" = "EXPINF2YR",
             "US.CPI.Expected5YInflation" = "EXPINF5YR",
             "US.CPI.Expected10YInflation" = "EXPINF10YR",
             "US.CPI.Expected20YInflation" = "EXPINF20YR",
             "US.CPI.Expected30YInflation" = "EXPINF30YR",
             "US.CPI.5YBreakevenInflation" = "T5YIE",
             "US.CPI.10YBreakevenInflation" = "T10YIE",
             "US.ProducerPrices.FinalDemand" = "PPIFID",
             "US.ProducerPrices.FinalDemandLessFoodEnergyTrade" = "WPUFD49116",
             "US.ProducerPrices.FinalDemand.mm.yy" = "PPIFID",
             "US.ProducerPrices.FinalDemandLessFoodEnergyTrade.mm.yy" = "WPUFD49116",
             "US.ProducerPrices.FinalDemandConstruction.mm.yy" = "PPIFDC",
             "US.ProducerPrices.AllCommodities.mm.yy" = "PPIACO",
        
             "US.SOV.1Y"="DGS1", 
             "US.SOV.2Y"="DGS2", 
             "US.SOV.3Y"="DGS3", 
             "US.SOV.5Y"="DGS5", 
             "US.SOV.7Y"="DGS7", 
             "US.SOV.10Y"="DGS10", 
             "US.SOV.20Y"="DGS20", 
             "US.SOV.30Y"="DGS30", 
             "US.Moodys.AAA" = "DAAA",
             "US.Moodys.BAA" = "DBAA",
             
             "US.Transportation.Rail" = "RAILFRTINTERMODALD11",
             "US.Transportation.Railpassenger" = "RAILPMD11",
             "US.Transportation.Air" = "LOADFACTORD11",
             "US.Transportation.Airtraffic.Passenger" = "ASMD11",
             "US.Transportation.Airtraffic.International" = "ASMI",
             "US.Transportation.PublicTransit" = "TRANSITD11",
             "US.Transportation.FreightTransportationServices" = "TSIFRGHT",
             "US.Transportation.PassengerTransportationServices" = "TSIPSNGR",
             "US.Transportation.TruckTonnageIndex" = "TRUCKD11",
             
             "US.HouseholdDebt" = "HDTGPDUSQ163N",
             "US.Housing.NewPrivateHousingStarts"="HOUST",
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
             "US.Housing.SoldNotStarted"="NHFSEPNT",
             "US.Housing.AverageSalesPrice"="ASPNHSUS",
             "US.Housing.AllTransactionsPriceIndex"="USSTHPI",
             "US.Housing.CaseShiller" = "CSUSHPINSA",
             "US.Housing.AveragePriceHouseSold" = "ASPUS",
             "US.Housing.MedianPriceHouseSold" = "MSPUS",
             "US.Housing.SoldUnder150K" = "NHSUSSPU15",
             "US.Housing.SoldUnder200K" = "NHSUSSP15T19",
             "US.Housing.SoldUnder300K" = "NHSUSSP20T29",
             "US.Housing.SoldUnder400K" = "NHSUSSP30T39",
             "US.Housing.SoldUnder500K" = "NHSUSSP40T49",
             "US.Housing.SoldUnder750K" = "NHSUSSP50T74",
             "US.Housing.SoldOver750K" = "NHSUSSP75O",
             "US.Housing.RentalVacancyRate" = "RRVRUSQ156N",
             "US.Housing.ExistingSingleFamilyInventory" = "HSFINVUSM495N",
             "US.Housing.ExistingSingleFamilySupply" = "HSFSUPUSM673N",
             "US.Housing.AffordabilityIndex" = "FIXHAI",
             
             
             "US.Auto.Autosales" = "ALTSALES",
             "US.Auto.LightAutos" = "LAUTOSA",
             "US.Auto.LightTrucks" = "LTRUCKSA",
             "US.Auto.AutosDomestic" = "DAUTOSAAR",
             "US.Auto.AutosForeign" = "FAUTOSAAR",
             "US.Auto.Inventories" = "AUINSA",
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
             "US.Banking.Delinquency.ResidentialRE" = "DRSFRMACBS",
             "US.Banking.Delinquency.CommercialRE" = "DRCRELEXFACBS",
             "US.Banking.Delinquency.CreditCards" = "DRCCLACBS",
             "US.Banking.Delinquency.BusinessLoans" = "DRBLACBS",
             
             "US.Banking.MortgagedebtIncomeRatio" = "MDSP",
             "US.Banking.ChargeoffRateResidential" = "CORSFRMACBS",
             
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
             "US.DieselPrices" = "GASDESW",
             "US.Transportation.VehicleMilesTravelled" = "TRFVOLUSM227SFWA",
             "US.EconomicPolicyUncertaintyIndex" = "USEPUINDXD",
             "US.VIX" = "VIXCLS",
             "US.Commodities.Oilprices" = "DCOILWTICO",
             
             "US.FinancialStability.HouseholdDebtPayments" = "TDSP",
             "US.Banking.ChicagoFinancialConditionsIndex" = "NFCI",
             "US.Banking.ChicagoFinancialConditionsIndexLeverage" = "NFCINONFINLEVERAGE",
             "US.Banking.ChicagoFinancialConditionsIndexRisk" = "NFCIRISK",
             "US.Banking.ChicagoFinancialConditionsIndexCredit" = "NFCICREDIT",
             "US.Banking.Kansas.FinancialConditionsIndex" = "KCFSI",
             
             "Coinbase.Bitcoin" = "CBBTCUSD",
             "Coinbase.Ethereum" = "CBETHUSD",
             
             "EU.GDP.Real"="CLVMNACSCAB1GQEA19",
             "DE.GDP.Real"="CLVMNACSCAB1GQDE",
             "FR.GDP.Real"="CLVMNACSCAB1GQFR",
             "IT.GDP.Real"="CLVMNACSCAB1GQIT",
             "ES.GDP.Real"="CLVMNACSCAB1GQES",
             "UK.GDP.Real"="NAEXKP01GBQ652S",
             "CA.GDP.Real"="NAEXKP01CAQ189S",
             "JP.GDP.Real" = "NAEXKP01JPQ661S",
             
             "DE.Unemployment" = "LRHUTTTTDEM156S",
             "FR.Unemployment" = "LRHUTTTTFRM156S",
             
             "EU.HICP.Total.mm.yy" = "CP0000EZ19M086NEST",
             "EU.HICP.Energy.mm.yy" = "ENRGY0EZ19M086NEST",
             "UK.Inflation.Total.mm.yy" = "GBRCPIALLMINMEI",
             "UK.Inflation.Core.mm.yy" = "GBRCPICORMINMEI"
             
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


cat("\n---------------------------------------------------\n")
cat("Built the data DB\n")
cat("---------------------------------------------------\n")

# Download most recent tweets
tweet.DB = data.frame( get_timeline("EconomicsShiny", n = 50) )
tweet.DB$text = sapply(strsplit(tweet.DB$text, split='http', fixed=TRUE), function(x) (x[1])) # remove all hyperlinks in the DB, since they get shortened
tweet.DB$text = gsub(" ", "", tweet.DB$text)
tweet.DB$text = gsub("#", "", tweet.DB$text)

tweet.DB$text = substr(tweet.DB$text,1,100)

cat("\n---------------------------------------------------\n")
cat("Built the Twitter DB\n")
cat("---------------------------------------------------\n")

quartzFonts(avenir = c("Avenir Book", "Avenir Black", "Avenir Book Oblique", 
                       "Avenir Black Oblique"))

options(scipen=5)

# library("colorspace")
# choose_palette()


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

  misc.postTweet <- function( tweet.text, media ){
    
    tweet.check = strsplit(tweet.text, split = "http", fixed = TRUE)[[1]][1]
    tweet.check = gsub(" ", "", tweet.check)
    tweet.check = gsub("#", "", tweet.check)
    tweet.check = substr(tweet.check, 1, 100)
    tweet.time  = tweet.DB[tweet.DB$text == as.character(tweet.check), "created_at"]
  
    Sys.sleep(10)
    
    if (length(tweet.time) == 0 || (Sys.time() - tweet.time > hours(24))) {
      cat("\n  - ")
      
      if (is.null(media)) post_tweet(status = tweet.text) else post_tweet(status = tweet.text, media = media) 
    } else cat("\n\nThe following tweet has already been posted: \n  -", tweet.text, "\n")
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
  
  if (series == "US.Payroll") col.plot = brewer.pal((max(x$StartExpansion)+1), "Paired") else col.plot = c( brewer.pal((max(x$StartExpansion)), "Paired"), "black")
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
  
  par(bg = "#f7f7f7")
  par(family = 'avenir')
  
  plot(zoo(tmp.plot[,1]), type="l", lwd=2, col=col.plot[1], lty=type.plot[1], cex.main = .9,
       main=paste0("Comparing the #recovery: \n", Data.Description[Data.Description$Mnemonic == series,3]), 
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
  if (series == "US.GDP.Real" ) text(   nrow(tmp.plot),  1.2*tail(tmp.plot,1)[1], paste0("After ", nrow(tmp.plot), " Quarters, the Latest Observation at \n", round(tail(tmp.plot,1)[1],1), "% of the Pre-Recession Peak"), font = 2)
  if (series == "US.Payroll" ) text(   nrow(tmp.plot),  1.2*tail(tmp.plot,1)[1], paste0("After ", nrow(tmp.plot), " Months, the Latest Observation at \n", round(tail(tmp.plot,1)[1],1), "% of the Pre-Recession Peak"), font = 2)
  
  
  abline(h=100, col="black", lty=2)
  grid()
  
  if (!(series == "US.Payroll")) {
    plot.trend <- 100*(1.03^(1/4))^seq(0, 40)
    lines(plot.trend, lty=3, lwd=3, col=last(col.plot)) 
    legend.plot <- c(legend.plot, "3% Trendline")
  }
  
  legend("topleft", legend.plot, fill=col.plot, cex=0.75, bty = "n")
  legend("bottomright", "Data: St. Louis FRED", cex=.75)
  
}


Chart.Panel <- function(x, series) {
  
  chart.col   = brewer.pal(6, "Paired")[6]
  
  chart.ylim  = c(
    (ifelse(range(x, na.rm=TRUE)[2]>0, 0.9*range(x, na.rm=TRUE)[1], 1.1*range(x, na.rm=TRUE)[1])), 1.1*range(x, na.rm=TRUE)[2])
  chart.title = Data.Description[Data.Description$Mnemonic == series,3]
  
  par(bg = "#f7f7f7")
  par(family = 'avenir')
  
  plot(as.zoo(x), main = chart.title, 
       col = chart.col, 
       lwd = 3, 
       cex = 1.3,
       cex.axis = 1.3,
       cex.lab = 1.3,
       ylim = chart.ylim,
       ylab = ifelse( substr(series, (nchar(series)-5), nchar(series)) == ".mm.yy" ||
                        substr(series, (nchar(series)-5), nchar(series)) == ".qq.yy", 
                      "Y/Y growth (in %)",
                        Data.Description[grep(series, Data.Description[,1]),5]), # qq.yy
       xlab = paste0("Period: ", year(index(x[1])), " - ", year(index(tail(x[1]))), " (shaded areas indicate U.S. recessions)"))
  
  points( index(tail(x, 1)), tail(x,1), col = chart.col, pch = 19, lwd = 5)
  abline(v = as.Date(paste0(seq( year(index(x[1])), year(Sys.Date()), 1), "-01-01")), lty = 3, lwd = 0.5)
  misc.NBER.Recessions()
  
  text(index(tail(x,round(.15*length(x)))[1]), 1.05*tail(x,1), paste("Latest obs.\n", round(tail(x,1),2)), font = 2, cex = 1.3)
  grid()
  legend("bottomleft", "Data: St. Louis FRED", cex=.9)
  
}


Chart.Single <- function(series, periods, tweet = FALSE, tweet.text = "", email){
  
  x = misc.FREDdowload(series = series)
  x = x[year(index(x))>(year(Sys.Date())-periods)]
  
  chart.filename = paste0(series, ".png")
  png(filename = chart.filename, 12, 12, "in", res = 127.5 )
  
  Chart.Panel(x = x, series = series)
  
  dev.off()
  if (tweet) email <<- gm_attach_file(email, chart.filename)
  if (tweet) misc.postTweet(tweet.text, media = chart.filename)
  
}


Chart.Duo <- function(series1, series2, periods, tweet = FALSE, tweet.text = "", email){
  
  x1 = misc.FREDdowload(series = series1)
  x2 = misc.FREDdowload(series = series2)
  
  x1 = x1[year(index(x1))>(year(Sys.Date())-periods)]
  x2 = x2[year(index(x2))>(year(Sys.Date())-periods)]
  
  chart.filename = paste0(series1, ".png")
  png(filename=chart.filename, 24, 12, "in", res=127.5)
  
  par(mfrow=c(1,2))
  
  Chart.Panel(x = x1, series = series1)
  Chart.Panel(x = x2, series = series2)
  
  par(mfrow=c(1,1))
  
  dev.off()
  if (tweet) email <<- gm_attach_file(email, chart.filename)
  if (tweet) misc.postTweet(tweet.text, media = chart.filename)
  
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
  if (tweet) email <<- gm_attach_file(email, chart.filename)
  if (tweet) misc.postTweet(tweet.text, media = chart.filename)
  
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
  
  par(bg = "#f7f7f7")
  par(family = 'avenir')
  
  bp = barplot(plot.data, beside = T, col = plot.col, 
               main = "Consumer Price Index for All Urban Consumers:\n Average Annual Inflation by Component", 
               ylim = plot.ylim )
  
  limits = par('usr')
  rect(mean(c(bp[nrow(bp),2], bp[1,3])), 0, limits[2], limits[4], lty = 3, lwd = 4)
  
  text(bp, 1.1* plot.data, paste0(round(plot.data,1),"%"), font = 2, cex = .8)
  
  legend("topleft", plot.legend, fill = plot.col, cex = .7, bty = 'n')
  legend("bottomleft", "Data: St. Louis FRED", cex=.5)
  dev.off()
  
  if (tweet) email <<- gm_attach_file(email, chart.filename)
  if (tweet) misc.postTweet(tweet.text, media = chart.filename)
  
}


InterestRate.Chart <- function(Data.Rates, tweet = FALSE, tweet.text, email) {
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
  png(filename=chart.filename, 18, 18, "in", res=127.5)
  
  par(mfrow = c(2,1))
  
  chart.col = brewer.pal(Data.Rates.dim, "Paired")
  barplot(na.omit(Data.Rates.D), col=chart.col, cex.main=1.5, cex.names = 1.25, border=NA, 
          main="Treasury Rates (Constant Maturity, Daily Yields in %):\n Changes in the last month")
  grid(col="black")
  legend("bottomleft", Data.Rates.Names, fill=chart.col, cex=1)
  
  barplot(Data.Rates.M, col=chart.col, cex.main=1.5, cex.names = 1, border=NA, #las=2,
          main="Treasury Rates (Constant Maturity, Monthly Average Yield in %):\n Evolution over the past 5 years")
  grid(col="black")
  legend("bottomleft", Data.Rates.Names, fill=chart.col, cex=1)
  
  par(mfrow = c(1,1))
  
  dev.off()
  if (tweet) email <<- gm_attach_file(email, chart.filename)
  if (tweet) misc.postTweet(tweet.text, media = chart.filename)
  
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
  par(bg = "#f7f7f7")
  par(family = 'avenir')
  
  plot((Evolution.Data[,1]), main = "Evolution of real GDP \n (All countries indexed to 100 in Q4 2019)", type = "n",
       xaxt="n", 
       xlab="", ylab="", 
       xlim = c( min(index(Evolution.Data)), as.yearqtr( as.Date( max(index(Evolution.Data)) ) + days(90) )  ),
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
  if (tweet) email <<- gm_attach_file(email, chart.filename)
  if (tweet) misc.postTweet("Who Recovers Faster? Comparing The #Recovery Since the OutSet of the Pandemic Across Major Industrialized Countries #rstats", media = chart.filename)
}


Chart.DoublePanel <- function(series1, series2, periods, chart.title) {
  
  x1 = misc.FREDdowload(series1) 
  x2 = misc.FREDdowload(series2)
  
  plot.data = merge(x1, x2)
  plot.data = plot.data[year(index(plot.data))>(year(Sys.Date())-periods)]
  names(plot.data) <- gsub("Motor Vehicle Retail Sales: ", "", c(Data.Description[Data.Description$Mnemonic == series1,3], Data.Description[Data.Description$Mnemonic == series2,3]) )
  
  chart.col   = brewer.pal(6, "Set1")[1:2]
  chart.ylim  = c(0.9*range(plot.data, na.rm=TRUE)[1], 1.1*range(plot.data, na.rm=TRUE)[2])
  #chart.title = "Motor Vehicle Retail Sales" #Data.Description[Data.Description$Mnemonic == series1,3]
  par(bg = "#f7f7f7")
  par(family = 'avenir')
  
  plot(plot.data[,1], col = chart.col, main = chart.title, type = "n", 
       ylim = chart.ylim, 
       ylab = Data.Description[Data.Description$Mnemonic == series1,5], 
       xlab = "")
  legend("topleft", names(plot.data), fill = chart.col, bty = "n")
  
  for (idx in 1:ncol(plot.data)) {
    lines(plot.data[,idx], col = chart.col[idx], lwd = 2)
    points( index(tail(plot.data[,idx], 1)), tail(plot.data[,idx],1), col = chart.col[idx], pch = 19, lwd = 5)
    text(index(tail(plot.data[,idx],round(.15*length(x2)))[1]), 1.1*tail(plot.data[,idx],1), 
         paste("Latest obs.\n", round(tail(plot.data[,idx],1),2)), font = 2, col = chart.col[idx])
  }
  misc.NBER.Recessions()
  
  grid()
  legend("bottomleft", "Data: St. Louis FRED", cex=.75)
}


Chart.Brexit <- function(x, chart.title, chart.ylab = "") {
  
  par(bg = "#f7f7f7")
  par(family = 'avenir')
  plot.col = brewer.pal(ncol(x), "Paired")
  plot.ylim = range(x, na.rm = TRUE)
  plot(x[,1], type = "l", col = plot.col[1], ylim = plot.ylim, lwd = 4,
       ylab = chart.ylab, xlab = "", main = chart.title)
  for (idx in 2:ncol(x)) lines( x[,idx], col = plot.col[idx], lwd = ifelse(idx == 6, 4, 2), lty = ifelse(idx == 6, 1, 2))
  for (idx in 1:ncol(x)) points( index( tail( x[!is.na(x[,idx]),idx],1) ),
                                 tail( x[!is.na(x[,idx]),idx],1), col = plot.col[idx], pch = 19, lwd = 5)
  text( index( x=tail( x[!is.na(x[,1]),1],2)[1] ),
        y= as.numeric(tail( x[!is.na(x[,1]),1],1)), 
        labels = paste0("EU: ", round (as.numeric(tail( x[!is.na(x[,1]),1],1)), 1), "%"), font = 2, cex = 1.2  ) 
  text( index( x=tail( x[!is.na(x[,6]),6],2)[1] ),
        y= as.numeric(tail( x[!is.na(x[,6]),6],1)), 
        labels = paste0("UK: ", round (as.numeric(tail( x[!is.na(x[,6]),6],1)), 1), "%"), font = 2, cex = 1.2 ) 
  
  
  legend("bottomright", colnames(x), fill = plot.col, bty = "n")
  grid()
  
}



cat("\n---------------------------------------------------\n")
cat("Data and charting functions loaded\n")
cat("---------------------------------------------------\n")



# Weekly Update -----------------------------------------------------------

Sys.sleep(15)

#Monday: Indeed & Bank Balance Sheet Data

if ( (weekdays(Sys.Date()) == "Monday") &&
     (hour(Sys.time()) > 11 ) ) {
  
  chart.filename = paste0("Moodys.png")
  png(filename = chart.filename, 20, 10, "in", res = 127.5 )
  Chart.DoublePanel(series1 = "US.Moodys.BAA", series2 = "US.Moodys.AAA", periods = 25, chart.title = "Moody's Corporate Bond Yields")
  dev.off()
  
  
  
  tweet.text = paste0("Corporate Bond Market Update: Using data as of ",
                      format(index(tail(US.Moodys.BAA,1)), "%d %B"),
                      ", Moody's Seasoned Aaa Corporate Bond Yield for bonds with 20+ years maturity is at ",
                      tail(US.Moodys.AAA,1),
                      "% for the AAA (",
                      ifelse(
                        as.numeric(tail(US.Moodys.AAA,1)) - as.numeric(tail(US.Moodys.AAA,6)[1]) > 0, "up ", "down "),
                      round(as.numeric(tail(US.Moodys.AAA,1)) - as.numeric(tail(US.Moodys.AAA,6)[1]),2),
                      "p.p. from the prior week). The BAA is at ",
                      tail(US.Moodys.BAA,1),
                      "%. #rstats")
  
  email <<- gm_attach_file(email, chart.filename)
  misc.postTweet(tweet.text, media = chart.filename)
  
  chart.filename = paste0("Indeed.png")
  png(filename = chart.filename, 20, 10, "in", res = 127.5 )
  Chart.DoublePanel(series1 = "US.Indeed.TotalJobs", series2 = "US.Indeed.NewJobs", periods = 35, chart.title = "Job Postings on Indeed")
  dev.off()
  
  tweet.text = paste0("High-frequency labor market update: On ",
                      format(index(tail(US.Indeed.NewJobs,1)), "%d %B"),
                      " postings on job site Indeed were ",
                      tail(US.Indeed.TotalJobs,1),
                      "% above their pre-pandemic level. New job postings, defined as on Indeed for seven days or less, are ",
                      tail(US.Indeed.NewJobs,1),
                      "% above their pre-pandemic level #jobs @indeed #rstats")
  
  email <<- gm_attach_file(email, chart.filename)
  misc.postTweet(tweet.text, media = chart.filename)
  
}

if ( (weekdays(Sys.Date()) == "Monday") &&
     (hour(Sys.time()) > 14 ) ) {

  BankData <- Reduce(function(...) merge(...), list( misc.FREDdowload("US.H8.BanksCredit.Securities"),
                                                     misc.FREDdowload("US.H8.BanksCredit.LoansLeases"),
                                                     misc.FREDdowload("US.H8.InterBankLoans"),
                                                     misc.FREDdowload("US.H8.Cash"),
                                                     misc.FREDdowload("US.H8.TradingAssets")
  ))

  BankData    <- BankData[index(BankData) > Sys.Date() - years(35)]

  chart.filename = paste0("BankBalanceSheets.png")
  png(filename = chart.filename, 18, 12, "in", res = 127.5 )

  plot.col    <- brewer.pal(ncol(BankData), "Paired")
  plot.legend <- c("Securities", "Loans and Leases", "Interbank Loans", "Cash", "Trading Assets")
  x.plot = barplot(BankData, col = plot.col, border="NA", main = "Long-term view US Commercial Banks' Balance Sheets: \n Main Asset Categories",
                   xlab = paste0("Period: ",
                                 year(Sys.Date() - years(25)), " - ", year(Sys.Date()),
                                 " (shaded areas indicate U.S. recessions)"), ylab = "$bn", xaxt = "n")
  axis(1, at = x.plot[(week(index(BankData)) %in% c(1, 25))],
       labels = as.yearmon(index(BankData))[(week(index(BankData)) %in% c(1, 35))])

  limits = par('usr')

  NBER.Recessions = as.Date(as.character(t(nberDates())), format="%Y%m%d")
  NBER.Recessions = NBER.Recessions[as.Date(NBER.Recessions) > Sys.Date() - years(35)]
  plot.Limits <- par('usr')
  for (idx in seq(1, length(NBER.Recessions), 2) ) rect(x.plot[grep(as.yearmon(NBER.Recessions[idx]), as.yearmon(index(BankData)))],
                                                        plot.Limits[3],
                                                        x.plot[grep(as.yearmon(NBER.Recessions[idx+1]), as.yearmon(index(BankData)))],
                                                        plot.Limits[4], col=rgb(0, 0, 255, max = 255, alpha = 5, names = "blue50"), lty=0)
  legend("bottomleft", "Data: St. Louis FRED", cex=.8)
  legend("topleft", plot.legend, fill = plot.col, cex=0.75)
  dev.off()

  email <<- gm_attach_file(email, chart.filename)

  tweet.text = "Update on the evolution of Commercial Banks' balance sheets #rstats"
  misc.postTweet(tweet.text, media = chart.filename)
}



# Tuesday: Gas Prices
Chart.Duo(series1="US.GasPrices", series2="US.Commodities.Oilprices",
          periods = 25,
          tweet = ((weekdays(Sys.Date()) == "Tuesday") &&
                     hour(Sys.time()) > 13 ),
          tweet.text = paste0("Paying more for #gas? 7-day weighted av. price for gas currently at $", 
                              round( tail(US.GasPrices,1), 2), ", ",
                              ifelse(tail(diff(US.GasPrices),1)>0, "up ", "down "),
                              round(100*tail(diff(US.GasPrices),1),2),
                              " cents from last week. Relative to 12 months ago, this is a $", 
                              round( as.numeric(tail(US.GasPrices,1)) - as.numeric(tail(US.GasPrices,53)[1]), 2),
                              ifelse( round( as.numeric(tail(US.GasPrices,1)) - as.numeric(tail(US.GasPrices,53)[1]), 2), 
                                      " increase.  #rstats", " decrease. #inflation #rstats")),
          email=email)

if ((weekdays(Sys.Date()) == "Tuesday") &&
    hour(Sys.time()) > 13 )
  Sys.sleep(100)

Chart.Single(series="US.DieselPrices",
             periods = 25,
             tweet = ((weekdays(Sys.Date()) == "Tuesday") &&
                         hour(Sys.time()) > 13 ),
             tweet.text = paste0( "7-day weighted av. for #Diesel currently at $", 
                                  round( tail(US.DieselPrices,1), 2), ", ",
                                  ifelse(tail(diff(US.DieselPrices),1)>0, "up ", "down "),
                                  round(100*tail(diff(US.DieselPrices),1),2),
                                  " cents from last week. Diesel engines in trucks, trains, boats, and barges transport nearly all products people consume. Relative to 12 months ago, this is a $", 
                                  round( as.numeric(tail(US.DieselPrices,1)) - as.numeric(tail(US.DieselPrices,53)[1]), 2),
                                  ifelse( round( as.numeric(tail(US.DieselPrices,1)) - as.numeric(tail(US.DieselPrices,53)[1]), 2), 
                                          " increase.  #rstats", " decrease. #inflation #rstats")),
             email = email)


# Wednesday: Economic Uncertainty

if ((weekdays(Sys.Date()) == "Wednesday") &&
    (hour(Sys.time()) >= 14) ) {
  
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
  
  tweet.text = paste0("Uncertainty Update: #VIX at ", as.numeric( tail(US.VIX,1) ),"; 7-day average of the Economic Policy #Uncertainty Index at ",
                      round( as.numeric( tail(rollmean(x, k = 7, align = "right"),1) ),1),
                      ", ", 
                      ifelse(tail(diff(rollmean(x, k = 7, align = "right")),1)>0, "up ", "down "),
                      round( tail(diff(rollmean(x, k = 7, align = "right")),1),1), " from the prior week #MarketVolatility #rstats https://whyitmatters.netlify.app/posts/2021-11-11-measures-of-economic-uncertainty-and-market-volatility/")
  misc.postTweet(tweet.text, media = chart.filename)
  
}


# Thursday: Bitcoin


Chart.Duo(series1="Coinbase.Bitcoin", series2 = "Coinbase.Ethereum",
          periods = 35,
          tweet = ( (weekdays(Sys.Date()) == "Thursday") && hour (Sys.time() ) > 14),
          tweet.text = paste0("#Bitcoin at $",
                              tail(Coinbase.Bitcoin,1),
                              "; #Ethereum at $",
                              tail(Coinbase.Ethereum,1),
                              ". Bitcoin is about ",
                              round(100*( as.numeric(tail(Coinbase.Bitcoin,1)) / as.numeric(tail(Coinbase.Bitcoin,30)[1]) -1),1),
                              "% ",
                              ifelse( ( as.numeric(tail(Coinbase.Bitcoin,1)) / as.numeric(tail(Coinbase.Bitcoin,30)[1]) -1)>0, "up", "down"),
                              " from a month ago, and about ",
                              round(100*( as.numeric(tail(Coinbase.Bitcoin,1)) / as.numeric(tail(Coinbase.Bitcoin,360)[1]) -1),0),
                              "% ",
                              ifelse( ( as.numeric(tail(Coinbase.Bitcoin,1)) / as.numeric(tail(Coinbase.Bitcoin,360)[1]) -1)>0, "up", "down"),
                              " from a year ago. Ethereum is about ",
                              round(100*( as.numeric(tail(Coinbase.Ethereum,1)) / as.numeric(tail(Coinbase.Ethereum,30)[1]) -1),1),
                              "% ",
                              ifelse( ( as.numeric(tail(Coinbase.Ethereum,1)) / as.numeric(tail(Coinbase.Ethereum,30)[1]) -1)>0, "up", "down"),
                              " from a month ago and about ",
                              round(100*( as.numeric(tail(Coinbase.Ethereum,1)) / as.numeric(tail(Coinbase.Ethereum,360)[1]) -1),0),
                              "% ",
                              ifelse( ( as.numeric(tail(Coinbase.Ethereum,1)) / as.numeric(tail(Coinbase.Ethereum,360)[1]) -1)>0, "up", "down"),
                              " from a year ago. #rstats"),
          email = email)





# Friday: Interest rate chart


InterestRate.Chart(Data.Rates = Reduce(function(...) merge(...), list( misc.FREDdowload("US.SOV.1Y"), 
                                                                       misc.FREDdowload("US.SOV.2Y"), 
                                                                       misc.FREDdowload("US.SOV.3Y"), 
                                                                       misc.FREDdowload("US.SOV.5Y"), 
                                                                       misc.FREDdowload("US.SOV.7Y"), 
                                                                       misc.FREDdowload("US.SOV.10Y") )), 
                   tweet = ( (weekdays(Sys.Date()) == "Friday") && hour (Sys.time() ) > 15),
                   tweet.text = paste0("10Y #Treasury is at ",
                                       tail(US.SOV.10Y,1),
                                       "%. How are shorter-term interest rates contributing to the change in the #YieldCurve? #rstats https://whyitmatters.netlify.app/posts/2021-11-19-how-changes-in-interest-rates-affect-the-yield-curve/"),
                   email = email)


Chart.Duo(series1="US.CPI.5YBreakevenInflation", series2="US.CPI.10YBreakevenInflation", 
          periods = 25,
          tweet = ( (weekdays(Sys.Date()) == "Friday") && (hour(Sys.time()) > 16 ) ),  
          tweet.text = paste0("5Y Breakeven Inflation Rate at ",
                              round( tail(US.CPI.5YBreakevenInflation,1), 2),
                              "%; 10Y Breakeven Inflation Rate at ",
                              round( tail(US.CPI.10YBreakevenInflation,1), 2),
                              "%. Breakeven #inflation rates use the difference between Constant Maturity and Inflation-Indexed Constant Maturity Treasuries to calculate what market participants expect inflation to be #rstats."),
          email=email)




cat("\n---------------------------------------------------\n")
cat("Weekly Posts done\n")
cat("---------------------------------------------------\n")



# GDP Releases -------------------------------------------------------

Chart.Single(series="US.GDP.Real.annualizedqq",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.GDP.Real.annualizedqq",6] == Sys.Date() ),
             tweet.text = paste0( "Real #GDP (qq, annualized) for ",
                                  as.yearqtr(index( tail(US.GDP.Real.annualizedqq, 1) )),
                                  " came in at ",
                                  as.numeric( tail(US.GDP.Real.annualizedqq, 1) ), 
                                  "%, ",
                                  ifelse(as.numeric( tail(diff(US.GDP.Real.annualizedqq), 1) ) >0, "up ", "down "),
                                  "from last quarter's ",
                                  as.numeric( tail(US.GDP.Real.annualizedqq, 2)[1] ), 
                                  "% #rstats"),
             email = email)

Chart.Single(series="US.GDP.RealDisposableIncomePerCapita",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.GDP.RealDisposableIncomePerCapita",6] == Sys.Date() ),
             tweet.text = paste0( "Real disposable #income per capita in ",
                                  as.yearmon(index( tail(US.GDP.RealDisposableIncomePerCapita, 1) )),
                                  " at $",
                                  as.numeric( tail(US.GDP.RealDisposableIncomePerCapita, 1) )/1000, 
                                  "K, ",
                                  ifelse(as.numeric( tail(diff(US.GDP.RealDisposableIncomePerCapita), 1) ) >0, "up ", "down "),
                                  "from last month's $",
                                  as.numeric( tail(US.GDP.RealDisposableIncomePerCapita, 2)[1] )/1000, 
                                  "K. Relative to a year ago, this is a ",
                                  round(100*( as.numeric( tail(US.GDP.RealDisposableIncomePerCapita, 1) ) / as.numeric( tail(US.GDP.RealDisposableIncomePerCapita, 12)[1] ) -1),1)
                                  ,"% ",
                                  ifelse( as.numeric( tail(US.GDP.RealDisposableIncomePerCapita, 1) ) / as.numeric( tail(US.GDP.RealDisposableIncomePerCapita, 12)[1] ) >=1,
                                    "increase", "decline"),
                                  " #rstats"),
             email = email)

Chart.Single(series="US.GDP.PersonalSavingRate",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.GDP.PersonalSavingRate",6] == Sys.Date() ),
             tweet.text = paste0( "Personal Savings Rate, relative to disposable income, in ",
                                  as.yearmon(index( tail(US.GDP.PersonalSavingRate, 1) )),
                                  " at ",
                                  as.numeric( tail(US.GDP.PersonalSavingRate, 1) ), 
                                  "%, ",
                                  ifelse(as.numeric( tail(diff(US.GDP.PersonalSavingRate), 1) ) >0, "up ", "down "),
                                  "from last month's ",
                                  as.numeric( tail(US.GDP.PersonalSavingRate, 2)[1] ), 
                                  "%. Relative to a year ago, this is a ",
                                  ifelse( as.numeric( tail(US.GDP.PersonalSavingRate, 1) ) - as.numeric( tail(US.GDP.PersonalSavingRate, 12)[1] ) >=0,
                                          "increase of ", "decline of "),
                                  round(as.numeric( tail(US.GDP.PersonalSavingRate, 1) ) - as.numeric( tail(US.GDP.PersonalSavingRate, 12)[1] ),1)
                                  ,"% ",
                                  "percentage points #rstats"),
             email = email)



Chart.Duo(series1="US.GDP.ConstructionsSpending", series2 = "US.GDP.ConstructionsSpendingAbsoluteValue",
             periods = 15,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.GDP.ConstructionsSpending",6] == Sys.Date() ),
             tweet.text = paste0( "Total #Construction spending for ",
                                  as.yearmon(index( tail(US.GDP.ConstructionsSpending, 1) )),
                                  " came in at $",
                                  round(as.numeric( tail(US.GDP.ConstructionsSpendingAbsoluteValue, 1) )/1000000, 1),
                                  "B, ",
                                  ifelse(tail(US.GDP.ConstructionsSpending, 1 ) >0, "up ", "down "),
                                  as.numeric( tail(US.GDP.ConstructionsSpending, 2)[1] ), 
                                  "% from prior month #rstats"),
             email = email)


Chart.Single(series="US.GDP.TradeBalance",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.GDP.TradeBalance",6] == Sys.Date() ),
             tweet.text = paste0( "US #TradeBalance for ",
                                  as.yearmon(index( tail(US.GDP.TradeBalance, 1) )),
                                  " came in at $",
                                  round((as.numeric( tail(US.GDP.TradeBalance, 1) )/1000 ),1) , 
                                  "B. In ",
                                  as.yearmon(index( tail(US.GDP.TradeBalance, 2) ))[1],
                                  " the trade balance stood at ",
                                  round((as.numeric( tail(US.GDP.TradeBalance, 2) )[1]/1000 ),1) , 
                                  "B. ",
                                  ifelse(tail(diff(US.GDP.TradeBalance),1) > 0, 
                                         "Narrower trade deficits are a lift for GDP growth #rstats",
                                  "Wider trade deficits are a drag for GDP growth #rstats")),
             email = email)

Chart.Single(series="US.FlowOfFunds.HouseholdNetWorth",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.FlowOfFunds.HouseholdNetWorth",6] == Sys.Date() ),
             tweet.text = paste0( "Flow Of Funds: US Household's Net #Wealth ",
                                  ifelse(tail(diff(US.FlowOfFunds.HouseholdNetWorth),1)>0, "increased by $", "fell by $"),
                                  round( tail(diff(US.FlowOfFunds.HouseholdNetWorth),1)/1000000, 1),
                                  "T to $",
                                  round(tail(US.FlowOfFunds.HouseholdNetWorth,1)/1000000,1),
                                  "T in ",
                                  as.yearqtr(index( tail(US.FlowOfFunds.HouseholdNetWorth, 1) )),
                                  " #rstats"),
             email = email)



if ( Data.Description[Data.Description$Mnemonic == "US.GDP.Imports",6] == Sys.Date()  ) {
  
  chart.filename = paste0("ImportsExports.png")
  png(filename = chart.filename, 20, 10, "in", res = 127.5 )
  Chart.DoublePanel(series1 = "US.GDP.Imports", series2 = "US.GDP.Exports", periods = 35, chart.title = "U.S. Trade Balance: Imports and Exports")
  dev.off()
  
  tweet.text = paste0(as.yearmon(index(tail(US.GDP.Imports,1))),
                      " exports were $",
                      round(tail(US.GDP.Exports,1)/1000,1),
                      "B,",
                      ifelse(tail(diff(US.GDP.Exports),1) > 0, " up $", " down $"),
                      round(tail(diff(US.GDP.Exports),1)/1000,1),
                      "B from the prior month. Imports were $",
                      round(tail(US.GDP.Imports,1)/1000,1),
                      "B,",
                      ifelse(tail(diff(US.GDP.Imports),1) > 0, " up $", " down $"),
                      round(tail(diff(US.GDP.Imports),1)/1000,1),
                      "B. #rstats")
  
  email <<- gm_attach_file(email, chart.filename)
  misc.postTweet(tweet.text, media = chart.filename)
}




if ( Data.Description[Data.Description$Mnemonic == "US.Government.Debt",6] == Sys.Date() ) {
  
  
  Chart.Duo(series1="US.Government.Debt", series2="US.Government.DebtToGDP",
            periods = 25,
            tweet = ( Data.Description[Data.Description$Mnemonic == "US.Government.Debt",6] == Sys.Date() ),
            tweet.text = paste0( "In ",
                                 as.yearqtr(index( tail(US.Government.Debt, 1) )),
                                 " US Total Federal Debt stood at $",
                                 round(as.numeric( tail(US.Government.Debt, 1) )/1000000,1),
                                 "T, or ",
                                 round( tail(US.Government.DebtToGDP,1),1),
                                 "% of GDP, ",
                                 ifelse( tail(diff(US.Government.DebtToGDP),1)>0, "up ", "down "),
                                 round( tail(diff(US.Government.DebtToGDP),1),1),
                                 " p.p. from the last quarter #rstats"),
            email=email)
  
  
  chart.filename = paste0("US Debt.png")
  png(filename = chart.filename, 18, 12, "in", res = 127.5 )
  misc.GDPExpansionPlot(series = "US.Government.Debt")
  dev.off()
  
  email <<- gm_attach_file(email, chart.filename)
  misc.postTweet("The evolution of U.S. Federal #debt after #recessions. The 3% trendline represents (approx.) U.S. trend GDP growth #rstats", media = chart.filename)
  
}

if ( Data.Description[Data.Description$Mnemonic == "US.GDP.Real",6] == Sys.Date() ) {
  
  tmp <- tempfile(fileext = ".png")
  png(tmp, 12, 12, "in", res = 127.5)  
  misc.GDPExpansionPlot(series = "US.GDP.Real")
  dev.off()
  misc.postTweet("How Does the current U.S #recovery compare to prior #recessions? #rstats", media = tmp)
  
  tmp <- tempfile(fileext = ".png")
  png(tmp, 12, 12, "in", res = 127.5)
  
  par(mfrow=c(2,2))
  misc.GDPExpansionPlot(series = "US.GDP.PCE")
  misc.GDPExpansionPlot(series = "US.GDP.FinalSales")
  misc.GDPExpansionPlot(series = "US.GDP.Investment")
  misc.GDPExpansionPlot(series = "US.GDP.Government")
  par(mfrow=c(1,1))
  dev.off()
  misc.postTweet("Comparing Recoveries Across All Major Components of U.S. #GDP #recovery #rstats", media = tmp)
  
}

if ( ( as.Date( as.character(tail( fredr_release_dates(release_id = 53L) ,1)[,2]) ) == Sys.Date() ) ||
     ( as.Date( as.character(tail( fredr_release_dates(release_id = 205L) ,1)[,2]) ) == Sys.Date() ) ){
  
  GDP.Comparison.Data <- merge(US = misc.FREDdowload("US.GDP.Real"),
                               EU = misc.FREDdowload("EU.GDP.Real"),
                               UK = misc.FREDdowload("UK.GDP.Real"),
                               JP = misc.FREDdowload("JP.GDP.Real"),
                               CA = misc.FREDdowload("CA.GDP.Real") )
  
  GDP.Comparison.Data <- GDP.Comparison.Data[index(GDP.Comparison.Data)>= as.Date("2019-09-01"),]
  Evolution.Chart(Evolution.Data = GDP.Comparison.Data, tweet=FALSE, email = email)
  
}


if ( Data.Description[Data.Description$Mnemonic == "EU.GDP.Real",6] == Sys.Date() ||
     Data.Description[Data.Description$Mnemonic == "DE.GDP.Real",6] == Sys.Date() ||
     Data.Description[Data.Description$Mnemonic == "UK.GDP.Real",6] == Sys.Date() ) {
  
  x  <- merge(EU=misc.FREDdowload("EU.GDP.Real"),
              DE=misc.FREDdowload("DE.GDP.Real"),
              FR=misc.FREDdowload("FR.GDP.Real"),
              IT=misc.FREDdowload("IT.GDP.Real"),
              ES=misc.FREDdowload("ES.GDP.Real"),
              UK=misc.FREDdowload("UK.GDP.Real") )
  
  x.BrexitReferendum = x[index(x) >= as.Date("2016-05-30")]
  x.BrexitReferendum = zoo( apply(x.BrexitReferendum, 2, function(x) 100*x/x[1] ), as.Date(index(x.BrexitReferendum)))
  x.PostBrexit       = x[index(x) >= as.Date("2019-12-31")]
  x.PostBrexit       = zoo( apply(x.PostBrexit, 2, function(x) 100*x/x[1] ), as.Date(index(x.PostBrexit)))
  
  chart.filename = paste0("Brexit.png")
  png(filename = chart.filename, 18, 12, "in", res = 127.5 )
  
  par(mfrow=c(1,2))
  Chart.Brexit(x = x.BrexitReferendum, chart.title = "Evolution Since Brexit Referendum (June 23, 2016)", chart.ylab = "Real GDP Index, Q2 2016 = 100" )
  Chart.Brexit(x = x.PostBrexit, chart.title = "Evolution Since Brexit Withdrawal (Jan. 31, 2020)", chart.ylab = "Real GDP Index, Q4 2019 = 100" )
  par(mfrow=c(1,1))
  dev.off()
  
  email <<- gm_attach_file(email, chart.filename)
  misc.postTweet("How's the UK doing since #Brexit? Comparing the evolution of real #GDP since the referendum, and since the actual withdrawal from the EU #rstats", media = chart.filename)
  
}


# Activity Measures -------------------------------------------------------




Chart.Single(series="US.Activity.NewBusinessApplicationsTotal",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Activity.NewBusinessApplicationsTotal",6] == Sys.Date() ),
             tweet.text = paste0( "Business applications for ",
                                  as.yearmon(index( tail(US.Activity.NewBusinessApplicationsTotal, 1) )),
                                  " at ",
                                  round( as.numeric( tail(US.Activity.NewBusinessApplicationsTotal, 1) ) / 1000,1),
                                  ifelse(tail(diff(US.Activity.NewBusinessApplicationsTotal),1)>0, "K, up ", "K, down "),
                                  as.numeric( tail(diff(US.Activity.NewBusinessApplicationsTotal),1) ),
                                  " from prior month. This monthly indicator measures business initiation activity as indicated by applications for an EIN. #rstats"),
             email = email)


Chart.Single(series="US.Activity.NewBusinessApplicationsHighPropensity",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Activity.NewBusinessApplicationsHighPropensity",6] == Sys.Date() ),
             tweet.text = paste0( "High propensity business applications for ",
                                  as.yearmon(index( tail(US.Activity.NewBusinessApplicationsHighPropensity, 1) )),
                                  " at ",
                                  round( as.numeric( tail(US.Activity.NewBusinessApplicationsHighPropensity, 1) ) / 1000,1),
                                  ifelse(tail(diff(US.Activity.NewBusinessApplicationsHighPropensity),1)>0, "K, up ", "K, down "),
                                  as.numeric( tail(diff(US.Activity.NewBusinessApplicationsHighPropensity),1) ),
                                  " from prior month. These are business applications with a high-propensity of turning into businesses with payroll, based on the characteristics of the applications. #rstats"),
             email = email)

Chart.Single(series="US.Activity.NYFed.Current",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Activity.NYFed.Current",6] == Sys.Date() ),
             tweet.text = paste0( "#EmpireState #ManufacturingSurvey for ",
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
             tweet.text = paste0( "#EmpireState #ManufacturingSurvey for ",
                                  as.yearmon(index( tail(US.Activity.NYFed.Leading, 1) )),
                                  " at ",
                                  as.numeric( tail(US.Activity.NYFed.Leading, 1) ),
                                  ", suggesting that future (leading) business conditions for the state of New York are ",
                                  ifelse(as.numeric( tail(US.Activity.NYFed.Leading, 1) ) >0, "improving, ", "worsening, "),
                                  "relative to last month #rstats https://whyitmatters.netlify.app/posts/2021-11-15-activity-measures-whats-available-why-care/"),
             email = email)


Chart.Single(series="US.Activity.StLouisFed.FinancialStress",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Activity.StLouisFed.FinancialStress",6] == Sys.Date() ),
             tweet.text = paste0( "The Financial #Stress index by the St. Louis Fed for the week of ",
                                  format (index( tail(US.Activity.StLouisFed.FinancialStress, 1) ), "%B %d"),
                                  " came in at ",
                                  round( as.numeric( tail(US.Activity.StLouisFed.FinancialStress, 1) ), 2),
                                  ". Values above 0 suggest above-average financial stress, and values below 0 suggest less than average financial stress #rstats"),
             email = email)


Chart.Duo(series1="US.IP.IndustrialProduction",  series2 = "US.IP.IndustrialProduction.qq",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.IP.IndustrialProduction",6] == Sys.Date() ),
             tweet.text = paste0( "#IndustrialProduction ",
                                  ifelse(tail(US.IP.IndustrialProduction.qq,1)>0, "rose by ", "declined by "),
                                  round(as.numeric( tail(US.IP.IndustrialProduction.qq, 1) ), 1),
                                  "% m/m in ", as.yearmon(index( tail(US.IP.IndustrialProduction.qq, 1) )),
                                  ". Industrial Production measures real output for manufacturing, mining, and electric, and gas utilities. #rstats"),
             email = email)

Chart.Duo(series1="US.Activity.ManufacturingOrdersDurable",  series2 = "US.Activity.ManufacturingOrdersNonDurable",
          periods = 25,
          tweet = ( Data.Description[Data.Description$Mnemonic == "US.Activity.ManufacturingOrdersDurable",6] == Sys.Date() ),
          tweet.text = paste0( "Manufacturers' New Orders for Durable Goods ",
                               ifelse(tail(diff(US.Activity.ManufacturingOrdersDurable),1)>0, "rose by ", "declined by "),
                               round( 100*(as.numeric(tail(US.Activity.ManufacturingOrdersDurable,1)) / as.numeric(tail(US.Activity.ManufacturingOrdersDurable,2)[1]) -1),2),
                               "% in ",
                               as.yearmon(index(tail(US.Activity.ManufacturingOrdersDurable,1))),
                               "; for Nondurable Goods new orders ",
                               ifelse(tail(diff(US.Activity.ManufacturingOrdersNonDurable),1)>0, "rose by ", "declined by "),
                               round( 100*(as.numeric(tail(US.Activity.ManufacturingOrdersNonDurable,1)) / as.numeric(tail(US.Activity.ManufacturingOrdersNonDurable,2)[1]) -1),2),
                               "% #rstats"),
          email = email)


Chart.Duo(series1="US.Activity.ExpectedSalesRevenue",  series2 = "US.Activity.ExpectedEmploymentGrowth",
          periods = 25,
          tweet = ( Data.Description[Data.Description$Mnemonic == "US.Activity.ExpectedSalesRevenue",6] == Sys.Date() ),

          tweet.text = paste0("Atlanta Fed's Survey of Business Uncertainty for ",
                              as.yearmon(index( tail(US.Activity.ExpectedSalesRevenue, 1) )),
                              " is out. Surveyed businesses expect sales revenue growth of ",
                              round(as.numeric( tail(US.Activity.ExpectedSalesRevenue, 1) ), 1),
                              "% and employment growth of ",
                              round(as.numeric( tail(US.Activity.ExpectedEmploymentGrowth, 1) ), 1),
                              "% over the next year #rstats"),
          email = email)


Chart.Single(series="US.IP.CapacityUtilization",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.IP.CapacityUtilization",6] == Sys.Date() ),
             tweet.text = paste0( "#CapacityUtilization for ",
                                  as.yearmon(index( tail(US.IP.CapacityUtilization, 1) )),
                                  " at ",
                                  round(as.numeric( tail(US.IP.CapacityUtilization, 1) ),1), 
                                  "% (",
                                  ifelse(tail(diff(US.IP.CapacityUtilization),1)>0, "up ", "down "),
                                  round(tail(diff(US.IP.CapacityUtilization),1),2),
                                  " p.p. from last month). Capacity utilization measures capacity used, relative to the total available capacity, to produce finished products. #rstats"),
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

Chart.Single(series="US.Activity.ChicagoFed.Inventory",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Activity.ChicagoFed.Inventory",6] == Sys.Date() ),
             tweet.text = paste0( "Chicago Fed Index for Sales, Orders, and Inventories for ",
                                  as.yearmon(index( tail(US.Activity.ChicagoFed.Inventory, 1) )),
                                  " at ",
                                  as.numeric( tail(US.Activity.ChicagoFed.Inventory, 1) ), 
                                  ". An average of 23 indicators, this series is an early signal of recessions/expansions. Values above/below zero suggest that the economy is expanding above/below historical trend growth. #rstats"),
             email = email)

Chart.Single(series="US.Activity.ChicagoFed",
             periods = 35,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Activity.ChicagoFed",6] == Sys.Date() ),
             tweet.text = paste0( "Chicago Fed Overall #NationalActivityIndex for ",
                                  as.yearmon(index( tail(US.Activity.ChicagoFed, 1) )),
                                  " at ",
                                  as.numeric( tail(US.Activity.ChicagoFed, 1) ), 
                                  ". Values above/below zero suggest that the national economy is expanding above/below its historical trend rate of growth. #rstats https://whyitmatters.netlify.app/posts/2021-11-15-activity-measures-whats-available-why-care/"),
             email = email)


Chart.Single(series="US.Productivity.Nonfarm",
             periods = 35,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Productivity.Nonfarm",6] == Sys.Date() ),
             tweet.text = paste0( "BLS: Non-Farm labor productivity",
                                  ifelse( tail(US.Productivity.Nonfarm, 1) > 0, " rose by ", " declined by "),
                                  as.numeric( tail(US.Productivity.Nonfarm, 1) ), 
                                  "% q/q in ",
                                  as.yearqtr(index( tail(US.Productivity.Nonfarm, 1) )),
                                  ". Labor productivity, or output/hour, is calculated by dividing an index of real output by an index of hours worked of all persons. #rstats"),
             email = email)

Chart.Single(series="US.LaborMarket.UnitLaborCosts",
             periods = 35,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.LaborMarket.UnitLaborCosts",6] == Sys.Date() ),
             tweet.text = paste0( "BLS: Unit labor costs in the nonfarm business sector",
                                  ifelse( tail(US.LaborMarket.UnitLaborCosts, 1) > 0, " rose by ", " fell by "),
                                  as.numeric( tail(US.LaborMarket.UnitLaborCosts, 1) ), 
                                  "% q/q in ",
                                  as.yearqtr(index( tail(US.LaborMarket.UnitLaborCosts, 1) )),
                                  ". BLS calculates unit labor costs as the ratio of hourly compensation to labor productivity. #rstats"),
             email = email)



Chart.Four(series1="US.Activity.RetailSalesqq", series2="US.Activity.RetailSales",
           series3="US.Activity.RetailSalesExAuto", series4="US.Activity.RetailSalesExAutoqq",
           periods = 25,
           tweet = ( Data.Description[Data.Description$Mnemonic == "US.Activity.RetailSales",6] == Sys.Date() ),  
           tweet.text = paste0("US Census: Advance Retail Sales ",
                               ifelse(tail(US.Activity.RetailSalesqq,1)==0, "flat in ", paste0(
                                      ifelse(tail(US.Activity.RetailSalesqq,1)>0, "increase by ", "decrease by "),
                                      round(tail(US.Activity.RetailSalesqq,1),1),
                                      "% in ")),
                               as.yearmon(index(tail(US.Activity.RetailSalesqq,1))),
                               ". Details in the charts. #RetailTherapy #rstats"),
           email=email)


if  ( Data.Description[Data.Description$Mnemonic == "US.Activity.RetailSalesBuildingMaterials",6] == Sys.Date() ) {
  
  x  <- merge(US.Activity.RetailSalesBuildingMaterials=misc.FREDdowload("US.Activity.RetailSalesBuildingMaterials"),
              US.Activity.RetailSalesGeneralMerchandise=misc.FREDdowload("US.Activity.RetailSalesGeneralMerchandise"),
              US.Activity.RetailSalesFoodDBeverageRetail=misc.FREDdowload("US.Activity.RetailSalesFoodDBeverageRetail"),
              US.Activity.RetailSalesGasoline=misc.FREDdowload("US.Activity.RetailSalesGasoline"),
              US.Activity.RetailSalesClothing = misc.FREDdowload("US.Activity.RetailSalesClothing"),
              US.Activity.RetailSalesMotorvehicles=misc.FREDdowload("US.Activity.RetailSalesMotorvehicles"),
              US.Activity.RetailSalesNonStore=misc.FREDdowload("US.Activity.RetailSalesNonStore"),
              US.Activity.RetailSalesFoodDrinking = misc.FREDdowload("US.Activity.RetailSalesFoodDrinking")
  )
  
  x  <- x[index(x) >= as.Date("2019-01-01")] / 1000
  
  plot.col    <- brewer.pal(8, "Paired")
  plot.legend = character()
  for (idx in 1:ncol(x)) plot.legend <- c(plot.legend, 
                                          gsub("Advance Retail Sales: ", "", 
                                               Data.Description[Data.Description$Mnemonic == colnames(x)[idx],3]))
  plot.ylim = c(0, 1.25*max(rowSums(x)))
  
  tmp <- tempfile(fileext = ".png")
  png(tmp, 12, 12, "in", res = 127.5)  
  
  x.chart=barplot(x, col = plot.col, 
                  border="NA", 
                  main = "Advance Retail Sales: Key Components", 
                  ylab = "Billions of Dollars",
                  xaxt = "n",
                  cex.axis = .8, cex.names = .8,
                  ylim = plot.ylim)
  axis(1, at=x.chart,
       as.yearmon( index(x)) , cex.axis = .7)
  legend("topleft", plot.legend, fill = plot.col, cex=0.75, bty = "n")
  legend("bottomleft", "Data: St. Louis FRED", cex=.5)
  
  dev.off()
  misc.postTweet("US Census: #AdvanceRetailSales: Details Behind the Headline #rstats", media = tmp)
}

Chart.Duo(series1="US.Activity.RetailSalesSportsGoods", series2="US.Activity.RetailSalesFurniture",
          periods = 25,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 9L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = "Anyone Else Got a New #Hometrainer during the pandemic? Sales of #SportingGoods Skyrocketed in 2021, and seem to level off afterwards. https://bit.ly/3BXLMFM #AdvanceRetailSales #rstats",
          email=email)


Chart.Single(series="US.Activity.ECommerceAsShareOfRetail",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Activity.ECommerceAsShareOfRetail",6] == Sys.Date() ),
             tweet.text = paste0( "eCommerce Retail Sales as Percent of Total Sales in ",
                                  as.yearqtr(index( tail(US.Activity.ECommerceAsShareOfRetail, 1) )),
                                  " at ",
                                  as.numeric( tail(US.Activity.ECommerceAsShareOfRetail, 1) ), 
                                  "%, ",
                                  ifelse( tail(diff(US.Activity.ECommerceAsShareOfRetail),1) > 0, "up from ", "down from "),
                                  as.numeric( tail(US.Activity.ECommerceAsShareOfRetail, 2)[1] ), 
                                  "% in the prior quarter #retail #eCommerce #rstats"),
             email = email)


Chart.Four(series1="US.Activity.PhillyFed.DeliveryTime",
           series2="US.Activity.PhillyFed.DeliveryIncreasedTime",
           series3="US.Activity.PhillyFed.PricesPaid",
           series4="US.Activity.PhillyFed.CurrentWorkHours",
           periods = 35,
           tweet = ( Data.Description[Data.Description$Mnemonic == "US.Activity.PhillyFed.DeliveryTime",6] == Sys.Date() ),
           tweet.text = paste0( "Philly Fed Manufacturing Survey: % firms report increased  #delivery times at ",
                                as.numeric( tail(US.Activity.PhillyFed.DeliveryTime, 1) ), 
                                ifelse(as.numeric( diff( tail(US.Activity.PhillyFed.DeliveryTime, 2)) )>0, "%, up ", ", down "),
                                as.numeric( diff( tail(US.Activity.PhillyFed.DeliveryTime, 2)) ),
                                " p.p. from last month #rstats https://whyitmatters.netlify.app/posts/2021-11-15-activity-measures-whats-available-why-care/"),
           email = email)

Chart.Four(series1="US.Activity.PhillyFed.NonManufacturingCurrentPricesPaid",
           series2="US.Activity.PhillyFed.NonManufacturingCurrentPricesReceived",
           series3="US.Activity.PhillyFed.NonManufacturingCurrentUnfilledOrders",
           series4="US.Activity.PhillyFed.NonManufacturingCurrentWages",
           periods = 35,
           tweet = ( Data.Description[Data.Description$Mnemonic == "US.Activity.PhillyFed.NonManufacturingCurrentPricesReceived",6] == Sys.Date() ),
           tweet.text = paste0( "#PhillyFed #Nonmanufacturing Business Survey: ",
                                as.numeric( tail(US.Activity.PhillyFed.NonManufacturingCurrentPricesPaid, 1) ),
                                "% firms report paying higher prices, ",
                                ifelse(as.numeric( diff( tail(US.Activity.PhillyFed.NonManufacturingCurrentPricesPaid, 2)) )>0, "up ", "down "),
                                round( as.numeric( diff( tail(US.Activity.PhillyFed.NonManufacturingCurrentPricesPaid, 2)) ),1),
                                " p.p. from last month #rstats"),
           email = email)

Chart.Four(series1="US.Activity.Texas.CurrentPricesRawMaterials",
           series2="US.Activity.Texas.CurrentDeliveryTime",
           series3="US.Activity.Texas.CurrentWages",
           series4="US.Activity.Texas.CurrentPricesReceived",
           periods = 35,
           tweet = ( Data.Description[Data.Description$Mnemonic == "US.Activity.Texas.CurrentPricesRawMaterials",6] == Sys.Date() ),
           tweet.text = paste0( "#Texas #Manufacturing Outlook Survey: Update on wages and benefits, prices received, and prices paid for raw materials. ",
                                as.numeric( tail(US.Activity.Texas.CurrentDeliveryTime, 1) ),
                                "% firms report higher delivery times, ",
                                ifelse(as.numeric( diff( tail(US.Activity.Texas.CurrentDeliveryTime, 2)) )>0, "up ", "down "),
                                round( as.numeric( diff( tail(US.Activity.Texas.CurrentDeliveryTime, 2)) ),1),
                                " p.p. from last month #rstats"),
           email = email)


if ( Data.Description[Data.Description$Mnemonic == "US.Activity.InventoryRatioManufacturing",6] == Sys.Date()  ) {
  
  plot.col    = brewer.pal(8, "Set1")
  
  chart.filename = paste0("Activity.InventoryRatios.png")
  png(filename = chart.filename, 20, 10, "in", res = 127.5 )
  
  x = misc.FREDdowload(series = "US.Activity.InventoryRatioRetailers")
  periods = 25
  x = x[year(index(x))>(year(Sys.Date())-periods)]
  Chart.Panel(x = x, series = "US.Activity.InventoryRatioRetailers")
  lines(misc.FREDdowload(series = "US.Activity.InventoryRatioManufacturing"), col = plot.col[2], lwd = 2, lty = 2)
  lines(misc.FREDdowload(series = "US.Activity.InventoryRatioWholesalers"), col = plot.col[3], lwd = 2, lty = 2)
  points(index(tail(US.Activity.InventoryRatioManufacturing,1)), tail(US.Activity.InventoryRatioManufacturing,1), pch = 19, col = plot.col[2], lwd = 5)
  points(index(tail(US.Activity.InventoryRatioWholesalers,1)), tail(US.Activity.InventoryRatioWholesalers,1), pch = 19, col = plot.col[3], lwd = 5)
  legend("topleft", c("Retailers", "Manufacturers", "Merchant Wholesalers"), fill = plot.col[1:3])
  
  dev.off()
  
  tweet.text = paste0("Inventory-to-Sales Ratio for Retailers at ",
                      round( tail(US.Activity.InventoryRatioRetailers,1), 2),
                      ifelse( tail(US.Activity.InventoryRatioRetailers,1) > tail(US.Activity.InventoryRatioManufacturing,1), ", above", ", below"),
                      " the ratio for Manufacturers (",
                      round( tail(US.Activity.InventoryRatioManufacturing,1), 2),
                      "). Low ratios or sudden drops can signal surges in demand - or supply chain challenges #supply #rstats")
  
  email <<- gm_attach_file(email, chart.filename)
  misc.postTweet(tweet.text, media = chart.filename)
}


cat("\n---------------------------------------------------\n")
cat("Activity Posts done\n")
cat("---------------------------------------------------\n")


# Labor Market -------------------------------------------------------

Chart.Single(series="US.Activity.ADP",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Activity.ADP",6] == Sys.Date() ),
             tweet.text = paste0("Today's #ADP data suggests ",
                                 ifelse(as.numeric( tail(diff(US.Activity.ADP), 1) ) >0, "an increase ", "a decline "),
                                 "in private sector #employment of ",
                                 round(tail(diff(US.Activity.ADP),1),0), "K jobs", 
                                 ifelse(as.numeric( tail(diff(US.Activity.ADP), 1) ) >0, ", up ", ", down "),
                                 "from ", round(as.numeric( tail(diff(US.Activity.ADP), 2)[1] ) ), "K in ",
                                 as.yearmon(index(tail(US.Activity.ADP,2)[1])),
                                 ". Note that historically the ADP report is not overly accurate in predicting the #payroll number. #rstats"),
             email = email)


Chart.Single(series="US.Payroll",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Payroll",6] == Sys.Date()  ),
             tweet.text = paste0("Today's #Payroll data reports ",
                                 ifelse( tail(diff(US.Payroll),1) > 0, "an increase", "a decrease"),
                                 " in private sector employment of ",
                                 round(tail(diff(US.Payroll),1),0), "K jobs, ",
                                 ifelse (tail(diff(diff(US.Payroll)),1) < 2, "", 
                                         paste0( ifelse(tail(diff(diff(US.Payroll)),1) > 0, "up", "down"),
                                                 " from ",
                                                 round(tail(diff(US.Payroll),2)[1],0), 
                                                 "K in ",
                                                 as.yearmon(index(tail(US.Payroll,2)[1]))
                                         ) ),
                                 " #rstats"),
             email = email)


Chart.Single(series="US.Unemployment.WageGrowth",
             periods = 35,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Unemployment.WageGrowth",6] == Sys.Date() ),
             tweet.text = paste0("Today's #WageData data reports average hourly earnings of $",
                                 tail(US.Unemployment.WageGrowth,1),
                                 ". This represents ",
                                 ifelse( round(tail(diff(US.Unemployment.WageGrowth),1),2)>0, "an increase", "a decrease"),
                                 " in average hourly earnings of ",
                                 round(tail(diff(US.Unemployment.WageGrowth),1),2), " cents, relative to last month, and ",
                                 ifelse( round(tail(diff(US.Unemployment.WageGrowth),1),2)>0, "an increase", "a decrease"),
                                 " of $", 
                                 as.numeric( tail(US.Unemployment.WageGrowth,1)) - as.numeric( tail(US.Unemployment.WageGrowth,13)[1] ),
                                 " or ",
                                 round( 100* (( as.numeric( tail(US.Unemployment.WageGrowth,1)) - as.numeric( tail(US.Unemployment.WageGrowth,13)[1] )) / as.numeric( tail(US.Unemployment.WageGrowth,13)[1] ) )-1, 1),
                                 "%, relative to the same month last year #rstats"),
             email = email)

Chart.Single(series="US.Unemployment.TotalCostIndex",
             periods = 35,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Unemployment.TotalCostIndex",6] == Sys.Date() ),
             tweet.text = paste0("The Employment Total Cost Index is a quarterly index measuring changes in average cost per hour worked. Today's data suggests that relative to the last quarter, costs have increased by ",
                                 round((as.numeric(tail(US.Unemployment.TotalCostIndex,1))/as.numeric(tail(US.Unemployment.TotalCostIndex,2)[1])-1)*100,1),
                                 "% in ",
                                 as.yearqtr(index(tail(US.Unemployment.TotalCostIndex,1))),
                                 ", relative to the prior quarter #rstats"),
             email = email)


if ( Data.Description[Data.Description$Mnemonic == "US.Payroll",6] == Sys.Date()  ) {
  chart.filename = paste0("PayrollExpansion.png")
  png(filename = chart.filename, 16, 10, "in", res = 127.5 )
  
  misc.GDPExpansionPlot(series = "US.Payroll")
  
  dev.off()
  misc.postTweet("#Payroll Update: Comparing the current #Recovery of the #LaborMarket to previous recessions #rstats", media = chart.filename)
}


Chart.Four(series1 = "US.Unemployment", 
           series2 = "US.Unemployment.ParticipationRate", 
           series3 = "US.Unemployment.PartTimeEconomicReasons",
           series4 = "US.Unemployment.PartTimeNonEconomicReasons", 
           periods = 25,
           tweet = ( Data.Description[Data.Description$Mnemonic == "US.Unemployment",6] == Sys.Date()  ),
           tweet.text = paste0("#Unemployment rate in ",
                               as.yearmon(index(tail(US.Unemployment,1))),
                               " at ",
                               tail(US.Unemployment,1),
                               "%",
                               ifelse(tail(diff(US.Unemployment)>0,1), ", up ", ", down "),
                               round(tail(diff(US.Unemployment),1),2),
                               "; participation rate (share of working age population in the labor force) at ",
                               tail(US.Unemployment.ParticipationRate,1),
                               ifelse(tail(diff(US.Unemployment.ParticipationRate)>0,1), ", up ", ", down "),
                               round(tail(diff(US.Unemployment.ParticipationRate),1),2),
                               " from prior month #rstats"),
           email = email)


Chart.Four(series1 = "US.Unemployment.EmploymentToPopulation", 
           series2 = "US.JOLTS.JobOpeningsRate", 
           series3 = "US.Unemployment.MarginallyAttached",
           series4 = "US.Unemployment.U1", 
           periods = 25,
           tweet = ( Data.Description[Data.Description$Mnemonic == "US.Unemployment.EmploymentToPopulation",6] == Sys.Date()  ),
           tweet.text = "#Payroll Update: Details behind the topline number #LaborMarket #rstats",
           email = email)



if ( Data.Description[Data.Description$Mnemonic == "US.LaborMarketFlows.UnemployedToEmployed",6] == Sys.Date()  ) {
  
  chart.filename = paste0("LaborForceFlows.png")
  png(filename = chart.filename, 18, 12, "in", res = 127.5 )
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  
  periods = 25
  
  x = merge(misc.FREDdowload(series = "US.LaborMarketFlows.UnemployedToEmployed"),
            misc.FREDdowload(series = "US.LaborMarketFlows.NotInLaborForceToEmployed") )
  x = x[year(index(x))>(year(Sys.Date())-periods)]
  x = zoo(x, as.Date(index(x)))
  
  plot.col    <- brewer.pal(5, "Paired")[1:2]
  plot.legend <- c("Unemployed to Employed", "Not in Labor Force to Employed")
  x.plot = barplot(x, col = plot.col, border="NA", main = "Labor Market Flows: Coming Back Into Employment", 
                   xlab = paste0("Period: ",
                                 year( index( head(x,1) ) ), " - ", year(Sys.Date()),
                                 " (shaded areas indicate U.S. recessions)"), ylab = "Thousands of Persons", xaxt = "n",  cex.main = 1.3, cex.names = 1.5)
  axis(1, at = x.plot[(month(index(x)) %in% c(1, 6))], 
       labels = as.yearmon(index(x))[(month(index(x)) %in% c(1, 6))], cex = 1.5)
  
  limits = par('usr')
  NBER.Recessions = as.Date(as.character(t(nberDates())), format="%Y%m%d")
  NBER.Recessions = NBER.Recessions[as.Date(NBER.Recessions) > index( head(x,1) )]
  plot.Limits <- par('usr')
  for (idx in seq(1, length(NBER.Recessions), 2) ) rect(x.plot[grep(as.yearmon(NBER.Recessions[idx]), as.yearmon(index(x)))], 
                                                        plot.Limits[3], 
                                                        x.plot[grep(as.yearmon(NBER.Recessions[idx+1]), as.yearmon(index(x)))], 
                                                        plot.Limits[4], col=rgb(0, 0, 255, max = 255, alpha = 15, names = "blue50"), lty=0)
  legend("bottomleft", "Data: St. Louis FRED", cex=.8)
  legend("topleft", plot.legend, fill = plot.col, cex=0.9, bty = "n")
  
  x = misc.FREDdowload(series = "US.LaborMarketFlows.UnemployedToNotInLaborForce")
  x = x[year(index(x))>(year(Sys.Date())-periods)]
  Chart.Panel(x = x, series = "US.LaborMarketFlows.UnemployedToNotInLaborForce")
  
  x = misc.FREDdowload(series = "US.LaborMarketFlows.NotInLaborForceToUnemployed")
  x = x[year(index(x))>(year(Sys.Date())-periods)]
  Chart.Panel(x = x, series = "US.LaborMarketFlows.NotInLaborForceToUnemployed")
  
  par(mfrow = c(1,1))
  dev.off()
  
  tweet.text = paste0("Labor Force Flows: ",
                      tail(US.LaborMarketFlows.NotInLaborForceToUnemployed,1) + tail(US.LaborMarketFlows.NotInLaborForceToEmployed,1), "K re-enter the labor force in ",
                      as.yearmon(index(tail(US.LaborMarketFlows.NotInLaborForceToEmployed,1))), ", ",
                      ifelse( tail( diff(US.LaborMarketFlows.NotInLaborForceToUnemployed + US.LaborMarketFlows.NotInLaborForceToEmployed ),1 ) >0, "up ", "down "),
                      tail( diff(US.LaborMarketFlows.NotInLaborForceToUnemployed + US.LaborMarketFlows.NotInLaborForceToEmployed ),1 ),
                      "K from prior month. ",
                      tail(US.LaborMarketFlows.UnemployedToNotInLaborForce,1), "K unemployed exit the labor force in ",
                      as.yearmon( index( tail(US.LaborMarketFlows.UnemployedToNotInLaborForce,1) )),
                      ifelse( tail(diff(US.LaborMarketFlows.UnemployedToNotInLaborForce),1) >0 , ", up ", ", down "),
                      tail(diff(US.LaborMarketFlows.UnemployedToNotInLaborForce),1),
                      "K from prior month #rstats")
  
  email <<- gm_attach_file(email, chart.filename)
  misc.postTweet(tweet.text, media = chart.filename)
}


Chart.Single(series="US.JOLTS.QuitsNumber",
             periods = 25,
             tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 192L) ,1)[,2]) ) == Sys.Date() ),
             tweet.text = paste0("Today's #JOLTS update suggest that ",
                                 round(tail(US.JOLTS.QuitsNumber,1)/1000,1),
                                 "M people quit their job in ",
                                 as.yearmon(index(tail(US.JOLTS.QuitsNumber,1))),
                                 " #rstats"),
             email = email)

Chart.Single(series="US.JOLTS.QuitsRate",
             periods = 25,
             tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 192L) ,1)[,2]) ) == Sys.Date() ),
             tweet.text = paste0("#JOLTS update: The quits rate for ",
                                 as.yearmon(index(tail(US.JOLTS.QuitsRate,1))),
                                 " at ",
                                 tail(US.JOLTS.QuitsRate,1),
                                 "% #rstats"),
             email = email)


Chart.Single(series="US.JOLTS.HireRate",
             periods = 25,
             tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 192L) ,1)[,2]) ) == Sys.Date() ),
             tweet.text = paste0("#JOLTS update: The hire rate for ",
                                 as.yearmon(index(tail(US.JOLTS.HireRate,1))),
                                 " at ",
                                 tail(US.JOLTS.HireRate,1),
                                 "% #rstats"),
             email = email)

Chart.Single(series="US.JOLTS.JobOpeningsRate",
             periods = 25,
             tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 192L) ,1)[,2]) ) == Sys.Date() ),
             tweet.text = paste0("#JOLTS update: Rate of job openings for ",
                                 as.yearmon(index(tail(US.JOLTS.JobOpeningsRate,1))),
                                 " at ",
                                 tail(US.JOLTS.JobOpeningsRate,1),
                                 "% #rstats"),
             email = email)

Chart.Duo(series1="US.Activity.InitialClaims", series2="US.Activity.ContinuedClaims.4W.MA",
          periods = 25,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 180L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = paste0("Weekly Initial #Claims (first-time applications for #unemployment benefits), a proxy for layoffs,",
                              ifelse(tail(diff(US.Activity.InitialClaims),1)>0, " increase to ", " decrease to "),
                              round(tail(US.Activity.InitialClaims,1)/1000,1),
                              "K (",
                              ifelse(tail(diff(US.Activity.InitialClaims),1)>0, "up ", "down "),
                              round(tail(diff(US.Activity.InitialClaims),1)/1000,1),
                              "K from prior week); 4-week moving average of continued claims ",
                              ifelse(tail(diff(US.Activity.ContinuedClaims.4W.MA),1) > 0, " increase to ", " decrease to "),
                              round(tail(US.Activity.ContinuedClaims.4W.MA,1)/1000,1),
                              "K in ",
                              as.yearmon(index(tail(US.Activity.InitialClaims,1))),
                              " #rstats"),
          email=email)

Chart.Duo(series1="DE.Unemployment", series2="FR.Unemployment",
          periods = 25,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 180L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = paste0("Unemployment update for the 2 largest EU economies: German unemployment is at ",
                              tail(DE.Unemployment,1),
                              "% in ",
                              as.yearmon(index(tail(DE.Unemployment,1))),
                              ", ",
                              ifelse(tail(diff(DE.Unemployment),1)>0, " an increase of ", " a decrease of "),
                              tail(diff(DE.Unemployment),1),
                              "p.p. Unemployment in France at ",
                              tail(FR.Unemployment,1),
                              ifelse(tail(diff(FR.Unemployment),1)>0, "%, up from", 
                                     ifelse(tail(diff(FR.Unemployment),1)==0, "%, flat relative to ", 
                                     "%, down from")) ,
                              ifelse(tail(diff(FR.Unemployment),1)==0, "", 
                                     tail(diff(FR.Unemployment),1)),
                              " prior month #rstats"),
          email=email)


# Inflation -------------------------------------------------------


if ( Data.Description[Data.Description$Mnemonic == "US.CPI.Expected1YInflation",6] == Sys.Date()  ) {
  
  plot.col    = brewer.pal(8, "Set1")
  
  chart.filename = paste0("InflationExpectations.png")
  png(filename = chart.filename, 20, 10, "in", res = 127.5 )
  
  par(mfrow=c(1,2))
  x = misc.FREDdowload(series = "US.CPI.Expected1YInflation")
  periods = 15
  x = x[year(index(x))>(year(Sys.Date())-periods)]
  Chart.Panel(x = x, series = "US.CPI.Expected1YInflation")
  
  x = misc.FREDdowload(series = "US.CPI.Expected2YInflation")
  periods = 5
  x = x[year(index(x))>(year(Sys.Date())-periods)]
  Chart.Panel(x = x, series = "US.CPI.Expected2YInflation")
  lines(misc.FREDdowload(series = "US.CPI.Expected5YInflation"), col = plot.col[2], lwd = 2, lty = 2)
  lines(misc.FREDdowload(series = "US.CPI.Expected10YInflation"), col = plot.col[3], lwd = 2, lty = 2)
  legend("topleft", c("2Y Expected Inflation","5Y Expected Inflation", "10Y Expected Inflation"
                      #, "20Y Expected Inflation", "30Y Expected Inflation"
  ), fill = plot.col[1:3])
  
  dev.off()
  par(mfrow=c(1,1))
  
  tweet.text = paste0("According to the Federal Reserve Cleveland, inflation expectations over a 1-year horizon are ",
                      round( tail(US.CPI.Expected1YInflation,1), 2),
                      "% y/y, ",
                      ifelse( (as.numeric(tail(US.CPI.Expected1YInflation,1)) > 2 ),
                              "above ", "below "),
                      "the Fed's target of 2%. Looking out further, longer-term expectations move closer to 2%. #rstats")
  
  email <<- gm_attach_file(email, chart.filename)
  misc.postTweet(tweet.text, media = chart.filename)
}


if ( Data.Description[Data.Description$Mnemonic == "US.LaborMarket.UnitLaborCosts",6] == Sys.Date()  ) {
  
  plot.col    = brewer.pal(8, "Set1")
  periods     = 35
  
  x = misc.FREDdowload( "US.LaborMarket.UnitLaborCosts")
  x = x[year(index(x))>(year(Sys.Date())-35)]
    
  chart.filename = paste0("ProductivityAndInflation.png")
  png(filename = chart.filename, 20, 10, "in", res = 127.5 )
  
  Chart.Panel(x=x, series = "US.LaborMarket.UnitLaborCosts" )
  lines(misc.FREDdowload(series = "US.PCE.Headline.mm.yy"), col = plot.col[2], lwd = 3, lty = 2)
  legend("topleft", c("Unit Labor Costs", "PCE Headline Inflation"), fill = plot.col[1:2])
  dev.off()
  
  x = aggregate(misc.FREDdowload(series = "US.PCE.Headline.mm.yy"), as.yearqtr, mean)
  x  = merge(misc.FREDdowload( "US.LaborMarket.UnitLaborCosts"), zoo(x, as.Date(index(x))))
  x = x[year(index(x))>(year(Sys.Date())-35)]
  ULCInflationCorr = cor(x[,1], x[,2], use = "complete.obs")
  
  tweet.text = paste0("US Unit Labor Costs (nonfarm, all employed persons)",
                      ifelse(tail(US.LaborMarket.UnitLaborCosts,1)>0, " rose by ", " declined by "),
                      tail(US.LaborMarket.UnitLaborCosts,1),
                      "% in ", as.yearqtr(index(tail(US.LaborMarket.UnitLaborCosts,1))),
                      ". Correlation with headline PCE #inflation over the past 35 years is about ",
                      round(ULCInflationCorr, 2),". #rstats")
  
  email <<- gm_attach_file(email, chart.filename)
  misc.postTweet(tweet.text, media = chart.filename)
}

if ( Data.Description[Data.Description$Mnemonic == "US.PCE.Headline.mm.yy",6] == Sys.Date()  ) {
 
  chart.filename = "InflationTrends.png"
  png(filename = chart.filename, 24, 12, "in", res = 127.5 )
  
  par(mfrow=c(2,2))

  series = "US.CPI.Headline"
  x = misc.FREDdowload(series = series)
  x = x[year(index(x))>(year(Sys.Date())-10)]
  xtrend = x
  xtrend = zoo(as.numeric(xtrend[1])*(1.02^(1/12))^seq(0, 10*12), index(x))
  
  Chart.Panel(x = x, series = series)
  lines(xtrend, col = brewer.pal(5, "Paired")[2], lwd = 4)

    series = "US.PCE.Headline"
  x = misc.FREDdowload(series = series)
  x = x[year(index(x))>(year(Sys.Date())-10)]
  xtrend = x
  xtrend = zoo(as.numeric(xtrend[1])*(1.02^(1/12))^seq(0, 10*12), index(x))
  
  Chart.Panel(x = x, series = series)
  lines(xtrend, col = brewer.pal(5, "Paired")[2], lwd = 4)
  
  
  series = "US.CPI.Core"
  x = misc.FREDdowload(series = series)
  x = x[year(index(x))>(year(Sys.Date())-10)]
  xtrend = x
  xtrend = zoo(as.numeric(xtrend[1])*(1.02^(1/12))^seq(0, 10*12), index(x))
  
  Chart.Panel(x = x, series = series)
  lines(xtrend, col = brewer.pal(5, "Paired")[2], lwd = 4)
  
  series = "US.PCE.Core"
  x = misc.FREDdowload(series = series)
  x = x[year(index(x))>(year(Sys.Date())-10)]
  xtrend = x
  xtrend = zoo(as.numeric(xtrend[1])*(1.02^(1/12))^seq(0, 10*12), index(x))
  
  Chart.Panel(x = x, series = series)
  lines(xtrend, col = brewer.pal(5, "Paired")[2], lwd = 4)
  
  dev.off()
  email <<- gm_attach_file(email, chart.filename)
  
  tweet.text = paste0("#Inflation Update: Comparing monthly U.S. inflation data against a long-term 2% price level trend #PriceLevelTargeting #rstats")
  misc.postTweet(tweet.text, media = chart.filename)
  
}

Chart.Duo(series1 = "US.PCE.Headline.mm.yy", series2 = "US.CPI.Headline.mm.yy",
             period = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.PCE.Headline.mm.yy",6] == Sys.Date()  ),
             tweet.text = paste0("Headline PCE #inflation running at ",
                                 round( tail(US.PCE.Headline.mm.yy,1), 2),
                                 "% y/y, ",
                                 ifelse( (as.numeric(tail(US.PCE.Headline.mm.yy,1)) - as.numeric(tail(US.PCE.Headline.mm.yy,2)[1]) > 0 ),
                                         "up ", "down "),
                                 round( as.numeric(tail(US.PCE.Headline.mm.yy,1)) - as.numeric(tail(US.PCE.Headline.mm.yy,2)[1]), 2),
                                 "p.p. from last month. #CPI inflation at ",
                                 round( tail(US.CPI.Headline.mm.yy,1), 2),
                                 "%. FED target for PCE is 2%. #rstats"),
             email = email)


Chart.Duo(series1 = "US.CPI.Core.mm.yy", series2 = "US.CPI.Headline.mm.yy",
          period = 25,
          tweet = ( Data.Description[Data.Description$Mnemonic == "US.CPI.Core.mm.yy",6] == Sys.Date()  ),
          tweet.text = paste0("Core CPI inflation inflation for ",
                              as.yearmon(index(tail(US.CPI.Headline.mm.yy,1))),
          " came in at ",
                              round( tail(US.CPI.Headline.mm.yy,1), 2),
                              "% y/y, ",
                              ifelse( (as.numeric(tail(US.CPI.Headline.mm.yy,1)) - as.numeric(tail(US.CPI.Headline.mm.yy,2)[1]) > 0 ),
                                      "up ", "down "),
                              round( as.numeric(tail(US.CPI.Headline.mm.yy,1)) - as.numeric(tail(US.CPI.Headline.mm.yy,2)[1]), 2),
                              "p.p. from last month. CPI ex. energy and food at ",
                              round( tail(US.CPI.Core.mm.yy,1), 2),
                              "%. #inflation #rstats"),
          email = email)


Chart.Four(series1 = "US.ProducerPrices.FinalDemand.mm.yy", series2 = "US.ProducerPrices.FinalDemandLessFoodEnergyTrade.mm.yy",
           series3 = "US.ProducerPrices.FinalDemandConstruction.mm.yy", series4 = "US.ProducerPrices.AllCommodities.mm.yy",
           period = 25,
           tweet = ( Data.Description[Data.Description$Mnemonic == "US.ProducerPrices.FinalDemand.mm.yy",6] == Sys.Date()  ),
           tweet.text = paste0("Producer Price Index (#PPI) for final demand at ",
                               round(tail(US.ProducerPrices.FinalDemand.mm.yy,1),1),
                               "% y/y in ",
                               as.yearmon(index(tail(US.ProducerPrices.FinalDemand.mm.yy,1))),
                               ifelse(tail(diff(US.ProducerPrices.FinalDemand.mm.yy),1)>0,", up from ", ", down from "),
                               round(tail(US.ProducerPrices.FinalDemand.mm.yy,2)[1],1),
                               "% in ",
                               as.yearmon(index(tail(US.ProducerPrices.FinalDemand.mm.yy,2)[1])),
                               ". Excluding the most volatile components, Producer Prices for final demand less food, energy and trade at ",
                               round(tail(US.ProducerPrices.FinalDemandLessFoodEnergyTrade.mm.yy,1),1),
                               "% y/y (all data seasonally adjusted) #rstats"),
           email = email)


Chart.Four(series1 = "US.CPI.Core.mm.yy",
           series2 = "US.CPI.FoodBeverages.mm.yy",
           series3 = "US.CPI.Housing.mm.yy",
           series4 = "US.CPI.Apparel.mm.yy",
           periods = 25,
           tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 10L) ,1)[,2]) ) == Sys.Date() ),
           tweet.text = "#CPI Update: Details behind the topline number #Inflation #rstats",
           email = email)


Chart.Four(series1 = "US.CPI.Medical.mm.yy",
           series2 = "US.CPI.Recreation.mm.yy",
           series3 = "US.CPI.Education.mm.yy",
           series4 = "US.CPI.Other.mm.yy",
           periods = 25,
           tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 10L) ,1)[,2]) ) == Sys.Date() ),
           tweet.text = "#CPI Update: Details behind the topline number #Inflation #rstats",
           email = email)


Chart.InflationOverview(series = c( "US.CPI.Headline.mm.yy","US.CPI.FoodBeverages.mm.yy", "US.CPI.Housing.mm.yy","US.CPI.Apparel.mm.yy",
                                    "US.CPI.Medical.mm.yy", "US.CPI.Recreation.mm.yy", "US.CPI.Education.mm.yy"),
                        tweet = ( Data.Description[Data.Description$Mnemonic == "US.CPI.Headline.mm.yy",6] == Sys.Date()  ),
                        tweet.text = "How broad based is the increase in #Inflation? Comparing all major CPI components to pre-pandemic levels #rstats",
                        email = email)


Chart.Duo(series1="US.CPI.AlcoholicBeverages.mm.yy", series2="US.CPI.AlcoholicBeveragesAway.mm.yy", 
          periods = 25,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 10L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = paste0("On average, prices for alcoholic #beverages (away from home) have increased ", 
                              round( tail(US.CPI.AlcoholicBeveragesAway.mm.yy,1), 2),
                              "% y/y, ",
                              ifelse( (as.numeric(tail(US.CPI.AlcoholicBeveragesAway.mm.yy,1)) - as.numeric(tail(US.CPI.AlcoholicBeveragesAway.mm.yy,2)[1]) > 0 ),
                                      "up ", "down "),
                              round( as.numeric(tail(US.CPI.AlcoholicBeveragesAway.mm.yy,1)) - as.numeric(tail(US.CPI.AlcoholicBeveragesAway.mm.yy,2)[1]), 2),
                              " p.p. from last month. Posting for a friend. #rstats"),
          email=email)


Chart.Duo(series1 = "EU.HICP.Total.mm.yy", series2 = "EU.HICP.Energy.mm.yy",
          period = 25,
          tweet = ( Data.Description[Data.Description$Mnemonic == "EU.HICP.Total.mm.yy",6] == Sys.Date()  ),
          tweet.text = paste0("EU-19 headline inflation running at ",
                              round( tail(EU.HICP.Total.mm.yy,1), 2),
                              "% y/y in ",
                              as.yearmon(index(tail(EU.HICP.Total.mm.yy,1))),
                              ifelse( (as.numeric(tail(EU.HICP.Total.mm.yy,1)) - as.numeric(tail(EU.HICP.Total.mm.yy,2)[1]) > 0 ),
                                      ", up ", ", down "),
                              round( as.numeric(tail(EU.HICP.Total.mm.yy,1)) - as.numeric(tail(EU.HICP.Total.mm.yy,2)[1]), 2),
                              " p.p. from last month. Energy price inflation at ",
                              round( tail(EU.HICP.Energy.mm.yy,1), 1),
                              "%. ECB inflation target is 2%. #HICP #inflation #rstats"),
          email = email)

Chart.Duo(series1 = "UK.Inflation.Total.mm.yy", series2 = "UK.Inflation.Core.mm.yy",
          period = 25,
          tweet = ( Data.Description[Data.Description$Mnemonic == "UK.Inflation.Total.mm.yy",6] == Sys.Date()  ),
          tweet.text = paste0("UK headline inflation running at ",
                              round( tail(UK.Inflation.Total.mm.yy,1), 2),
                              "% y/y in ",
                              as.yearmon(index(tail(UK.Inflation.Total.mm.yy,1))),
                              ifelse( (as.numeric(tail(UK.Inflation.Total.mm.yy,1)) - as.numeric(tail(UK.Inflation.Total.mm.yy,2)[1]) > 0 ),
                                      ", up ", ", down "),
                              round( as.numeric(tail(UK.Inflation.Total.mm.yy,1)) - as.numeric(tail(UK.Inflation.Total.mm.yy,2)[1]), 2),
                              " p.p. from last month. Core inflation at ",
                              round( tail(UK.Inflation.Core.mm.yy,1), 1),
                              "%. Bank of England's inflation target is 2%. #UK #inflation #rstats"),
          email = email)

cat("\n---------------------------------------------------\n")
cat("Labor market and Inflation posts done\n")
cat("---------------------------------------------------\n")


# Housing -------------------------------------------------------


if ( Data.Description[Data.Description$Mnemonic == "US.Housing.SoldUnder150K",6] == Sys.Date()  ) {
  
  x <- Reduce(function(...) merge(...), list( misc.FREDdowload("US.Housing.SoldUnder150K"),
                                              misc.FREDdowload("US.Housing.SoldUnder200K"),
                                              misc.FREDdowload("US.Housing.SoldUnder300K"),
                                              misc.FREDdowload("US.Housing.SoldUnder400K"),
                                              misc.FREDdowload("US.Housing.SoldUnder500K"),
                                              misc.FREDdowload("US.Housing.SoldUnder750K"),
                                              misc.FREDdowload("US.Housing.SoldOver750K")  ))
  x  <- x[index(x) > as.Date("2004-12-30")]
  
  chart.filename = paste0("HousesSoldByPrice.png")
  png(filename = chart.filename, 18, 12, "in", res = 127.5 )
  
  plot.col    <- brewer.pal(ncol(x), "Paired")
  plot.legend <- c("Under 150K", "150K-199K", "200K-299K", "300K-399K", "400K-499K", "500K-750K", "Over 750K")
  x.plot = barplot(x, col = plot.col, border="NA", main = "New Houses Sold by Sales Price in the United States", 
                   xlab = paste0("Period: ",
                                 year( index( head(x,1) ) ), " - ", year(Sys.Date()),
                                 " (shaded areas indicate U.S. recessions)"), ylab = "Thousands of Units", xaxt = "n")
  axis(1, at = x.plot[(month(index(x)) %in% c(1, 6))], 
       labels = as.yearmon(index(x))[(month(index(x)) %in% c(1, 6))])
  
  limits = par('usr')
  NBER.Recessions = as.Date(as.character(t(nberDates())), format="%Y%m%d")
  NBER.Recessions = NBER.Recessions[as.Date(NBER.Recessions) > index( head(x,1) )]
  plot.Limits <- par('usr')
  for (idx in seq(1, length(NBER.Recessions), 2) ) rect(x.plot[grep(as.yearmon(NBER.Recessions[idx]), as.yearmon(index(x)))], 
                                                        plot.Limits[3], 
                                                        x.plot[grep(as.yearmon(NBER.Recessions[idx+1]), as.yearmon(index(x)))], 
                                                        plot.Limits[4], col=rgb(0, 0, 255, max = 255, alpha = 15, names = "blue50"), lty=0)
  legend("bottomleft", "Data: St. Louis FRED", cex=.8)
  legend("topright", plot.legend, fill = plot.col, cex=0.75, bty = "n")
  dev.off()
  
  tweet.text = paste0( "#Housing update: Breakdown of new houses sold by sales price in ",
                       as.yearmon(index(tail(x,1))),
                       ". Roughly ", 
                       round( 100 * tail( rowSums(x[,6:7]), 1) / tail( rowSums(x), 1), 0),
                       "% of all new houses sold for more than $500K #rstats" )
  
  email <<- gm_attach_file(email, chart.filename)
  misc.postTweet(tweet.text = tweet.text, media = chart.filename)
}


Chart.Single(series="US.Housing.AffordabilityIndex",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Housing.AffordabilityIndex",6] == Sys.Date() ),
             tweet.text = paste0("The Housing Affordability Index from the National Association of Realtors for ",
                                 as.yearmon( index(tail(US.Housing.AffordabilityIndex,1)) ),
                                 " at ",
                                 tail(US.Housing.AffordabilityIndex,1),
                                 ifelse( tail(diff( US.Housing.AffordabilityIndex) ,1)>0, ", up ", ", down "),
                                 tail(diff(US.Housing.AffordabilityIndex),1) ,
                                 " points. A value of 100 means that a family with the median income has exactly enough income to qualify for a mortgage on a median-priced home. #housing #rstats"),
             email = email)



Chart.Single(series="US.Banking.ChargeoffRateResidential",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Banking.ChargeoffRateResidential",6] == Sys.Date() ),
             tweet.text = paste0("Charge-Off Rate on Single Family Residential Mortgages, net of recoveries, for ",
                                 as.yearqtr( index(tail(US.Banking.ChargeoffRateResidential,1)) ),
                                 " at ",
                                 tail(US.Banking.ChargeoffRateResidential,1),
                                 ifelse( tail(diff( US.Banking.ChargeoffRateResidential) ,1)>0, "%, up ", "%, down "),
                                 tail(diff(US.Banking.ChargeoffRateResidential),1) ,
                                 " p.p from last quarter #housing #rstats"),
             email = email)


Chart.Single(series="US.Banking.MortgagedebtIncomeRatio",
             periods = 25,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Banking.MortgagedebtIncomeRatio",6] == Sys.Date() ),
             tweet.text = paste0("Mortgage Debt Service Payments as % Disposable Income for ",
                                 as.yearqtr( index(tail(US.Banking.MortgagedebtIncomeRatio,1)) ),
                                 " at ",
                                 round(tail(US.Banking.MortgagedebtIncomeRatio,1),1),
                                 ifelse( tail(diff( US.Banking.MortgagedebtIncomeRatio) ,1)>0, "%, up ", "%, down "),
                                 round( 100*( as.numeric( tail(US.Banking.MortgagedebtIncomeRatio, 1)) / as.numeric( tail(US.Banking.MortgagedebtIncomeRatio, 2)[1])-1),1) ,
                                 "% from last quarter #affordability #rstats"),
             email = email)



Chart.Single(series="US.Housing.CaseShiller",
             periods = 25,
             tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 199L) ,1)[,2]) ) == Sys.Date() ),
             tweet.text = paste0("Case-Shiller U.S. National Home Price Index for ",
                                 as.yearmon( index(tail(US.Housing.CaseShiller,1)) ),
                                 " at ",
                                 tail(US.Housing.CaseShiller,1),
                                 ifelse( tail(diff( US.Housing.CaseShiller) ,1)>0, ", up ", ", down "),
                                 round( 100*( as.numeric( tail(US.Housing.CaseShiller, 1)) / as.numeric( tail(US.Housing.CaseShiller, 2)[1])-1),1) ,
                                 "% from last month #housing #rstats"),
             email = email)


Chart.Single(series="US.Housing.RentalVacancyRate",
             periods = 25,
             tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 199L) ,1)[,2]) ) == Sys.Date() ),
             tweet.text = paste0("The rental #vacancy rate (proportion of the rental inventory vacant for rent) for ",
                                 as.yearmon( index(tail(US.Housing.RentalVacancyRate,1)) ),
                                 " at ",
                                 tail(US.Housing.RentalVacancyRate,1),
                                 ifelse( tail(diff( US.Housing.RentalVacancyRate) ,1)>0, ", up ", ", down "),
                                 round( tail(diff( US.Housing.RentalVacancyRate) ,1) ,1) ,
                                 "% from last quarter #housing #rstats"),
             email = email)

Chart.Duo(series1 = "US.Housing.ExistingSingleFamilyInventory",
          series2 = "US.Housing.ExistingSingleFamilySupply",
          period = 35,
          tweet = ( Data.Description[Data.Description$Mnemonic == "US.Housing.ExistingSingleFamilyInventory",6] == Sys.Date() ),
          tweet.text = paste0( "Inventory of Existing Family Homes ",
                               ifelse(tail(diff(US.Housing.ExistingSingleFamilyInventory),1) > 0, "rose to ", "fell to "),
                               as.numeric(tail(US.Housing.ExistingSingleFamilyInventory,1))/1000000,
                               " million units in ",
                               as.yearmon(index(tail(US.Housing.ExistingSingleFamilyInventory,1))),
                               ". Given the current sales pace, it would take ",
                               tail(US.Housing.ExistingSingleFamilySupply,1),
                               " months to sell this existing housing inventory, ",
                               ifelse(tail(diff(US.Housing.ExistingSingleFamilySupply),1) > 0, "up from ", "down from "),
                               tail(US.Housing.ExistingSingleFamilySupply,2)[1],
                               " #rstats"),
          email = email)


Chart.Four(series1="US.Housing.NewHomeSales",
           series2="US.Housing.AverageSalesPrice",
           series3="US.Housing.MonthlySupply",
           series4="US.Housing.SoldNotStarted",
           periods = 35,
           tweet = ( Data.Description[Data.Description$Mnemonic == "US.Housing.NewHomeSales",6] == Sys.Date() ),
           tweet.text = paste0( "Housing update: New home sales in ", 
                                as.yearmon(index(tail(US.Housing.NewHomeSales, 1))),
                                " at ",
                                as.numeric( tail(US.Housing.NewHomeSales, 1) ), 
                                ifelse(as.numeric( diff( tail(US.Housing.NewHomeSales, 2)) )>0, "K, up ", ", down "),
                                as.numeric( round( diff( tail(US.Housing.NewHomeSales, 2)),1) ),
                                "K from last month. Average sales price at $",
                                round(as.numeric( tail(US.Housing.AverageSalesPrice, 1) )/1000 ,1), 
                                "K. Monthly housing supply at ",
                                as.numeric( tail(US.Housing.MonthlySupply, 1) ),
                                " months. #housing #rstats"),
           email = email)



if ( Data.Description[Data.Description$Mnemonic == "US.Housing.AveragePriceHouseSold",6] == Sys.Date()  ) {
  
  chart.filename = paste0("AverageMedianHousePrices.png")
  png(filename = chart.filename, 20, 10, "in", res = 127.5 )
  Chart.DoublePanel(series1 = "US.Housing.AveragePriceHouseSold", series2 = "US.Housing.MedianPriceHouseSold", periods = 25, chart.title = "Average/Median Sales Price of Houses Sold")
  dev.off()
  
  tweet.text = paste0("Average prices of #houses sold for the United States for ",
                      as.yearqtr(index(tail(US.Housing.AveragePriceHouseSold,1))),
                      " at ",
                      
                      round( tail(US.Housing.AveragePriceHouseSold,1)/1000, 2),
                      "K, ",
                      ifelse( (as.numeric(tail(US.Housing.AveragePriceHouseSold,1)) > 2 ),
                              "up ", "down "),
                      round( 100*( as.numeric(tail(US.Housing.AveragePriceHouseSold,1)) / as.numeric(tail(US.Housing.AveragePriceHouseSold,5)[1]) - 1), 2),
                      "% from a year ago #rstats")
  
  email <<- gm_attach_file(email, chart.filename)
  misc.postTweet(tweet.text, media = chart.filename)
}


Chart.Single(series="US.Housing.ExistingHomeSales",
             periods = 35,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Housing.ExistingHomeSales",6] == Sys.Date() ),
             tweet.text = paste0("Existing #homeSales in ",
                                 as.yearmon(index(tail(US.Housing.ExistingHomeSales,1))),
                                 " at ",
                                 round(tail(US.Housing.ExistingHomeSales,1)/1000,1),
                                 "K units, ",
                                 ifelse(as.numeric( diff( tail(US.Housing.ExistingHomeSales, 2)) )>0, " up ", " down "),
                                 round(100*(as.numeric(tail(US.Housing.ExistingHomeSales,1))/as.numeric(tail(US.Housing.ExistingHomeSales,2)[1]) -1),1),
                                 "% q/q #rstats"),
             email = email)


Chart.Single(series="US.Housing.AllTransactionsPriceIndex",
             periods = 35,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Housing.AllTransactionsPriceIndex",6] == Sys.Date() ),
             tweet.text = paste0("#HousePrices",
                                 ifelse(as.numeric( diff( tail(US.Housing.AllTransactionsPriceIndex, 2)) )>0, " up ", " down "),
                                 round(100*(as.numeric(tail(US.Housing.AllTransactionsPriceIndex,1))/as.numeric(tail(US.Housing.AllTransactionsPriceIndex,2)[1]) -1),1),
                                 "% q/q, with the index at ",
                                 as.numeric(tail(US.Housing.AllTransactionsPriceIndex,1)),
                                 ". This is ",
                                 round(100*(as.numeric(tail(US.Housing.AllTransactionsPriceIndex,1)) / max(US.Housing.AllTransactionsPriceIndex[index(US.Housing.AllTransactionsPriceIndex) < as.Date("2010-01-01")]) -1),1),
                                 "% above the peak prior to the 2008/09 recession #rstats"),
             email = email)

Chart.Four(series1="US.Housing.30YMortgageRate",
           series2="US.Housing.15YMortgageRate",
           series3="US.Housing.51ARMMortgageRate",
           series4="US.Housing.30YJumboMortgageRate",
           periods = 25,
           tweet = ( Data.Description[Data.Description$Mnemonic == "US.Housing.30YMortgageRate",6] == Sys.Date() ),
           tweet.text = paste0( "#HouseHunters: Thinking about buying #property? Average US 30-Year Fixed Rate #Mortgage last week at ",
                                as.numeric( tail(US.Housing.30YMortgageRate, 1) ), 
                                ifelse(as.numeric( diff( tail(US.Housing.30YMortgageRate, 2)) )>0, "%, up ", "%, down "),
                                round( as.numeric( diff( tail(US.Housing.30YMortgageRate, 2)) ),2 ),
                                " p.p. from prior week. 5/1 ARM at ",
                                round(as.numeric( tail(US.Housing.51ARMMortgageRate, 1) ),1),
                                "% #rstats"),
           email = email)


Chart.Four(series1="US.Housing.NewPrivateHousingPermits",
           series2="US.Housing.NewPrivateHousingStarts",
           series3="US.Housing.NewPrivateHousingConstruction",
           series4="US.Housing.NewPrivateHousingCompleted",
           periods = 35,
           tweet = ( Data.Description[Data.Description$Mnemonic == "US.Housing.NewPrivateHousingStarts",6] == Sys.Date() ),
           tweet.text = paste0( "Housing update: #housingStarts at ",
                                tail(US.Housing.NewPrivateHousingStarts,1),
                                "K units in ",
                                as.yearmon(index(tail(US.Housing.NewPrivateHousingConstruction, 1))),
                                ". Housing Units under construction at ",
                                as.numeric( tail(US.Housing.NewPrivateHousingConstruction, 1) ), 
                                ifelse(as.numeric( diff( tail(US.Housing.NewPrivateHousingConstruction, 2)) )>0, "K, up ", ", down "),
                                as.numeric( round( diff( tail(US.Housing.NewPrivateHousingConstruction, 2)),1) ),
                                "K from last month. New Housing Permits Authorized at ",
                                as.numeric( tail(US.Housing.NewPrivateHousingPermits, 1) ),
                                "K. #rstats"),
           email = email)


if ( Data.Description[Data.Description$Mnemonic == "US.Housing.NewPrivate1UnitCompleted",6] == Sys.Date()  ) {
  
  x  <- merge(US.Housing.NewPrivate1UnitCompleted=misc.FREDdowload("US.Housing.NewPrivate1UnitCompleted"),
              US.Housing.NewPrivate2UnitCompleted=misc.FREDdowload("US.Housing.NewPrivate2UnitCompleted"),
              US.Housing.NewPrivate5UnitCompleted=misc.FREDdowload("US.Housing.NewPrivate5UnitCompleted") )
  x  <- x[index(x) >= as.Date("2000-01-01")]
  
  plot.col    = brewer.pal(8, "Set1")
  plot.legend = character()
  for (idx in 1:ncol(x)) plot.legend <- c(plot.legend, 
                                          gsub("New Privately-Owned Housing Units Completed: ", "", 
                                               Data.Description[Data.Description$Mnemonic == colnames(x)[idx],3]))
  plot.ylim = c(0, 1.25*max(rowSums(x)))
  
  chart.filename = paste0("HousingUnitsCompleted.png")
  png(filename = chart.filename, 20, 12, "in", res = 127.5 )
  
  x.plot=barplot(x, col = plot.col, 
                 border="NA", 
                 main = "New Privately-Owned Housing Units Completed", 
                 ylab = "Thousands of Units (SAAR)",
                 xaxt = "n", cex.axis = 1.3, cex.names = 1.3, cex.lab = 1.3, ylim = plot.ylim)
  axis(1, at=x.plot, as.yearmon( index(x)) , cex.axis = 1.2)
  
  # Added recession areas
  NBER.Recessions = as.Date(as.character(t(nberDates())), format="%Y%m%d")
  NBER.Recessions = NBER.Recessions[as.Date(NBER.Recessions) > as.Date("2000-01-01")]
  plot.Limits <- par('usr')
  for (idx in seq(1, length(NBER.Recessions), 2) ) rect(x.plot[grep(as.yearmon(NBER.Recessions[idx]), as.yearmon(index(x)))], 
                                                        plot.Limits[3], 
                                                        x.plot[grep(as.yearmon(NBER.Recessions[idx+1]), as.yearmon(index(x)))], 
                                                        plot.Limits[4], col="#0000FF19", lty=0)
  legend("topleft", plot.legend, fill = plot.col, cex=1.3, bty = "n")
  legend("bottomleft", "Data: St. Louis FRED", cex=.8)
  
  dev.off()
  email <<- gm_attach_file(email, chart.filename)
  misc.postTweet(paste0("#Housing Details: Breakdown of New Privately-Owned Housing Units Completed for ",
                        as.yearmon(index(tail(x,1)))," shows #SingleFamilyHomes account for roughly ",
                        round( 100*tail(US.Housing.NewPrivate1UnitCompleted,1) / tail(rowSums(x),1), 1),
                        "% of all completions #rstats"), 
                 media = chart.filename)
}

# Financial Soundness -------------------------------------------------------

Chart.Four(series1="US.Banking.ChicagoFinancialConditionsIndex", 
           series2 = "US.Banking.ChicagoFinancialConditionsIndexRisk",
           series3 = "US.Banking.ChicagoFinancialConditionsIndexCredit", 
           series4 = "US.Banking.ChicagoFinancialConditionsIndexLeverage",
           periods = 35,
           tweet = ( Data.Description[Data.Description$Mnemonic == "US.Banking.ChicagoFinancialConditionsIndex",6] == Sys.Date() ),
           tweet.text = paste0( "Chicago Fed Financial Conditions Index in ",
                                as.yearmon(index( tail(US.Banking.ChicagoFinancialConditionsIndex, 1) )),
                                " at ",
                                round(as.numeric( tail(US.Banking.ChicagoFinancialConditionsIndex, 1) ), 2),
                                ". Positive (negative) values indicate financial conditions that are tighter (looser) than average. #rstats"),
                                #, with risk subindex capturing volatility and funding risk in the financial sector; credit reflecting credit conditions; and leverage subindex consisting of debt and equity measures. #rstats"),
           email = email)


Chart.Single(series="US.Banking.Kansas.FinancialConditionsIndex",
             periods = 35,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.Banking.Kansas.FinancialConditionsIndex",6] == Sys.Date() ),
             tweet.text = paste0( "The Kansas City Financial Stress index in ",
                                  as.yearmon(index( tail(US.Banking.Kansas.FinancialConditionsIndex, 1) )),
                                  " at ",
                                  round( as.numeric( tail(US.Banking.Kansas.FinancialConditionsIndex, 1) ),2), 
                                  ifelse(tail(diff(US.Banking.Kansas.FinancialConditionsIndex),1)>0, ", up ", ", down"),
                                  round(tail(diff(US.Banking.Kansas.FinancialConditionsIndex),1),1),
                                  " from prior month. Positive values indicate #stress above the long-run average, while negative values signify below-average stress #rstats"),
             email = email)

Chart.Single(series="US.HouseholdDebt",
             periods = 35,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.HouseholdDebt",6] == Sys.Date() ),
             tweet.text = paste0( "US #HouseholdDebt in ",
                                  as.yearmon(index( tail(US.HouseholdDebt, 1) )),
                                  " at ",
                                  round( as.numeric( tail(US.HouseholdDebt, 1) ),2) , 
                                  "%. This index measures the overall level of household #indebtedness (commonly related to consumer loans and mortgages) as a share of GDP. #rstats"),
             email = email)



Chart.Single(series="US.FinancialStability.HouseholdDebtPayments",
             periods = 35,
             tweet = ( Data.Description[Data.Description$Mnemonic == "US.FinancialStability.HouseholdDebtPayments",6] == Sys.Date() ),
             tweet.text = paste0( "#HouseholdDebt Service Payments as % of Disposable Personal Income in ",
                                  as.yearmon(index( tail(US.FinancialStability.HouseholdDebtPayments, 1) )),
                                  " at ",
                                  round( as.numeric( tail(US.FinancialStability.HouseholdDebtPayments, 1) ), 2), 
                                  "%, ",
                                  ifelse( as.numeric( tail(US.FinancialStability.HouseholdDebtPayments, 1)) > US.FinancialStability.HouseholdDebtPayments[index(US.FinancialStability.HouseholdDebtPayments)=="2020-01-01"],
                                          "above ", "below "),
                                  "pre-pandemic levels of ",
                                  round( US.FinancialStability.HouseholdDebtPayments[index(US.FinancialStability.HouseholdDebtPayments)=="2020-01-01"], 2),
                                  "%. Ratio measures debt payments, relative to disposable income. #rstats"),
             email = email)


if ( Data.Description[Data.Description$Mnemonic == "US.Banking.Delinquency.ResidentialRE",6] == Sys.Date()  )
  Chart.Four(series1 = "US.Banking.Delinquency.ResidentialRE", 
             series2 = "US.Banking.Delinquency.CommercialRE", 
             series3 = "US.Banking.Delinquency.CreditCards",
             series4 = "US.Banking.Delinquency.BusinessLoans", 
             periods = 25,
             tweet = TRUE,
             tweet.text = paste0( "#CreditCard delinquency rate in ",
                                  as.yearmon(index( tail(US.Banking.Delinquency.CreditCards, 1) )),
                                  " at ",
                                  round( as.numeric( tail(US.Banking.Delinquency.CreditCards, 1) ), 2), 
                                  "%, ",
                                  ifelse( as.numeric( tail(US.Banking.Delinquency.CreditCards, 1)) > US.Banking.Delinquency.CreditCards[index(US.Banking.Delinquency.CreditCards)=="2020-01-01"],
                                          "above ", "below "),
                                  "pre-pandemic levels of ",
                                  round( US.Banking.Delinquency.CreditCards[index(US.Banking.Delinquency.CreditCards)=="2020-01-01"], 2),
                                  "%. Chart shows #delinquency rates for all Banks across loan categories #rstats"),
               
             email = email)


# Motor Vehicles -------------------------------------------------------

Chart.Single(series="US.Auto.Autosales",
             periods = 25,
             tweet = Data.Description[Data.Description$Mnemonic == "US.Auto.Autosales", 6] == Sys.Date(),  
             tweet.text = paste0("#Vehicle sales for ",
                                 as.yearmon(tail(index(US.Auto.Autosales),1)),
                                 " at ",
                                 tail(US.Auto.Autosales,1),
                                 "M units, ",
                                 ifelse(tail(diff(US.Auto.Autosales),1)>0, "up ", "down "),
                                 round(tail(diff(US.Auto.Autosales),1),2),
                                 "M units from prior month #rstats"),
             email=email)

Chart.Single(series="US.Auto.Inventories",
             periods = 35,
             tweet = Data.Description[Data.Description$Mnemonic == "US.Auto.Inventories", 6] == Sys.Date(),  
             tweet.text = paste0("Domestic Auto Inventory at ",
                                 tail(US.Auto.Inventories,1),
                                 "K units in ",
                                 as.yearmon(tail(index(US.Auto.Inventories),1)),
                                 ", ",
                                 ifelse(tail(diff(US.Auto.Inventories),1)>0, "up ", "down "),
                                 round(tail(diff(US.Auto.Inventories),1),2),
                                 "K units from prior month #rstats"),
             email=email)

Chart.Single(series="US.Transportation.VehicleMilesTravelled",
             periods = 35,
             tweet = Data.Description[Data.Description$Mnemonic == "US.Transportation.VehicleMilesTravelled", 6] == Sys.Date(),  
             tweet.text = paste0("Is the change in gas prices affecting driving habits? Vehicle Miles Traveled in ",
                                 as.yearmon(tail(index(US.Transportation.VehicleMilesTravelled),1)),
                                 " at ",
                                 round( tail(US.Transportation.VehicleMilesTravelled,1)/1000, 1),
                                 " billions of miles, ",
                                 
                                 ifelse(tail(diff(US.Transportation.VehicleMilesTravelled),1)>0, "up ", "down "),
                                 round(tail(diff(US.Transportation.VehicleMilesTravelled),1),2),
                                 " million miles from prior month #rstats"),
             email=email)

Chart.Single(series="US.Auto.InventorySalesRatio",
             periods = 35,
             tweet = Data.Description[Data.Description$Mnemonic == "US.Auto.InventorySalesRatio", 6] == Sys.Date(),  
             tweet.text = paste0("#Auto Inventory-to-sales ratio at ",
                                 tail(US.Auto.InventorySalesRatio,1),
                                 " in ",
                                 as.yearmon(tail(index(US.Auto.InventorySalesRatio),1)),
                                 ", ",
                                 ifelse(tail(diff(US.Auto.InventorySalesRatio),1)>0, "up ", "down "),
                                 round(tail(diff(US.Auto.InventorySalesRatio),1),2),
                                 " from prior month. Historically, dealers typically hold over 2 months of #inventory. #rstats"),
             email=email)

if ( as.Date( as.character(tail( fredr_release_dates(release_id = 93L) ,1)[,2]) ) == Sys.Date() ) {
  
  tmp <- tempfile(fileext = ".png")
  png(tmp, 24, 12, "in", res = 127.5)
  
  par(mfrow=c(1,2))
  Chart.DoublePanel(series1 = "US.Auto.LightAutos", series2 = "US.Auto.LightTrucks", periods = 25, chart.title = "Motor Vehicle Retail Sales")
  Chart.DoublePanel(series1 = "US.Auto.AutosDomestic", series2 = "US.Auto.AutosForeign", periods = 25, chart.title = "Motor Vehicle Retail Sales")
  
  par(mfrow=c(1,1))
  dev.off()
  misc.postTweet(paste0("Breakdown of #LightVehicle sales in ", 
                        as.yearmon(index( tail(US.Auto.Autosales,1) )),
                        " #rstats"), media = tmp)
}


# Banking Sector -------------------------------------------------------

Chart.Duo(series1="US.FDIC.NetChargeOffRateTotalLoans", series2="US.FDIC.LoanLossProvisions",
          periods = 35,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 482L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = paste0("#Banking Update: Net Charge-Off Rates at ", 
                              round( tail(US.FDIC.NetChargeOffRateTotalLoans,1), 2),
                              "%, ",
                              ifelse( (as.numeric(tail(US.FDIC.NetChargeOffRateTotalLoans,1)) - as.numeric(tail(US.FDIC.NetChargeOffRateTotalLoans,2)[1]) > 0 ),
                                      "up ", "down "),
                              round( as.numeric(tail(US.FDIC.NetChargeOffRateTotalLoans,1)) - as.numeric(tail(US.FDIC.NetChargeOffRateTotalLoans,2)[1]), 2),
                              "p.p. from last Quarter #rstats"),
          email=email) 


Chart.Duo(series1="US.FDIC.InstitutionsWithEarningsGain", series2="US.FDIC.UnprofitableInstitutions",
          periods = 35,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 482L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = paste0("#Banking Update: Number of Unprofitable Institutions at ", 
                              round( tail(US.FDIC.UnprofitableInstitutions,1), 2),
                              ", ",
                              ifelse( (as.numeric(tail(US.FDIC.UnprofitableInstitutions,1)) - as.numeric(tail(US.FDIC.UnprofitableInstitutions,2)[1]) > 0 ),
                                      "up ", "down "),
                              round( as.numeric(tail(US.FDIC.UnprofitableInstitutions,1)) - as.numeric(tail(US.FDIC.UnprofitableInstitutions,2)[1]), 2),
                              " from last Quarter #rstats"),
          email=email) 



if  ( Data.Description[Data.Description$Mnemonic == "US.H8.TradingAssets",6] == Sys.Date() ) {
  
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
                        main = "All Commercial Bank's Balance Sheets: \n Weekly #BankCredit by Loan Type (in $B)", 
                        xaxt = "n",
                        ylim = plot.ylim)
  axis(1, at=BankDataChart[week(index(BankData.Details))==25], 
       label = year(index(BankData.Details))[week(index(BankData.Details))==25],
       col.axis="black", cex.axis=1)
  abline(v = BankDataChart[week(index(BankData.Details)) ==1], lty = 2)
  abline(h=0, lwd=4)
  legend("topleft", plot.legend, fill = plot.col, cex=1)
  legend("bottomleft", "Data: St. Louis FRED", cex=.75)
  
  dev.off()
  email <<- gm_attach_file(email, tmp)
  misc.postTweet("Banks' Balance Sheets Post-Pandemic Fueled by Higher Cash and Treasuries/Agencies Holdings #BankLending #rstats", media = tmp)
}


Chart.Single(series="US.Banks.AutoLoansSecuritized.qq.yy",
          periods = 35,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 14L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = paste0("Motor Vehicle Loans grow at ", 
                              round( tail(US.Banks.AutoLoansSecuritized.qq.yy,1), 2),
                              "% in ",
                              as.yearqtr(tail(US.Banks.AutoLoansSecuritized.qq.yy),1),
                              ifelse( (as.numeric(tail(US.Banks.AutoLoansSecuritized.qq.yy,1)) - as.numeric(tail(US.Banks.AutoLoansSecuritized.qq.yy,2)[1]) > 0 ),
                                      ", up ", ", down "),
                              round( as.numeric(tail(US.Banks.AutoLoansSecuritized.qq.yy,1)) - as.numeric(tail(US.Banks.AutoLoansSecuritized.qq.yy,2)[1]), 2),
                              " p.p. from last quarter #rstats"),
          email=email)

Chart.Single(series="US.Banks.StudentLoans.qq.yy",
          periods = 35,
          tweet = ( as.Date( as.character(tail( fredr_release_dates(release_id = 14L) ,1)[,2]) ) == Sys.Date() ),  
          tweet.text = paste0("#StudentLoan debt remains high, but growth rate has slowed over time: Student loans grow at ", 
                              round( tail(US.Banks.StudentLoans.qq.yy,1), 2),
                              "% in ",
                              as.yearqtr(tail(US.Banks.StudentLoans.qq.yy),1),
                              ifelse( (as.numeric(tail(US.Banks.StudentLoans.qq.yy,1)) - as.numeric(tail(US.Banks.StudentLoans.qq.yy,2)[1]) > 0 ),
                                      ", up ", ", down "),
                              round( as.numeric(tail(US.Banks.StudentLoans.qq.yy,1)) - as.numeric(tail(US.Banks.StudentLoans.qq.yy,2)[1]), 2),
                              " p.p. from last quarter #rstats"),
          email=email)



if ( Data.Description[Data.Description$Mnemonic == "US.SLOOS.AutoLoansStandards",6] == Sys.Date()  ) {

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
                   main = "Senior Loan Officers' Opinion Survey (SLOOS) on Bank Lending Practices: \n Net % of Domestic Banks",
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
  misc.postTweet(paste0("Need $$$? The Senior Loan Officers Survey detailes changes in banks' lending standards in ",
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
  misc.postTweet(paste0("From the Senior Loan Officers Survey: Changes in lending standards for Commercial and Industrial loans in ",
                        x.period[2]
                        ,"; keep an eye on spread of loan rates over Banks' funding costs #SLOOS #rstats"), media = tmp)
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
  misc.postTweet(paste0("Update on lending standards for Commercial Real Estate Loans in ",
                        x.period[2]
                        ,". Changes in lending standards proxy changes in development and construction activity going forward. #SLOOS #rstats"), media = tmp)
}


cat("\n---------------------------------------------------\n")
cat("Housing, Banking and Financial Soundness posts done\n")
cat("---------------------------------------------------\n")


# Transportation -------------------------------------------------------

if ( Data.Description[Data.Description$Mnemonic == "US.Transportation.Air",6] == Sys.Date()  )
  Chart.Four(series1="US.Transportation.Air", 
             series2="US.Transportation.Airtraffic.Passenger", 
             series3="US.Transportation.Railpassenger",
             series4 = "US.Transportation.PublicTransit", 
             periods = 25,
             tweet = TRUE,
             tweet.text = "Air Traffic coming back, Public Transportation not so much #rstats https://whyitmatters.netlify.app/posts/2021-11-15-activity-measures-whats-available-why-care/",
             email = email)

Chart.Single(series="US.Transportation.Rail",
            periods = 25,
            tweet = ( Data.Description[Data.Description$Mnemonic == "US.Transportation.Rail",6] == Sys.Date()  ),
            tweet.text = "Transportation Update: Rail Freight Intermodal Traffic. Intermodal refers to transportation of freight in an intermodal container or vehicle, using multiple modes of transportation (e.g., rail, ship, truck), without any handling of the freight itself when changing modes. #rstats",
            email=email)

  Chart.Single(series="US.Transportation.TruckTonnageIndex",
               periods = 25,
               tweet = ( Data.Description[Data.Description$Mnemonic == "US.Transportation.TruckTonnageIndex",6] == Sys.Date()  ),
               tweet.text = paste0("The Truck Tonnage Index ",
                                   ifelse(tail(diff(US.Transportation.TruckTonnageIndex),1)>0, "rose", "fell"),
                                   " by ", 
                                   round(
                                     100*(as.numeric(tail(US.Transportation.TruckTonnageIndex,1)) / 
                                                  as.numeric(tail(US.Transportation.TruckTonnageIndex,2)[1])-1), 2),
                                   "% in ", as.yearmon(index(tail(US.Transportation.TruckTonnageIndex,1))),
                                   ". Trucking is a barometer of the U.S. economy, representing over 70% of tonnage carried (incl. manufactured and retail goods). The index is dominated by contract freight with minimal amounts of spot market loads #rstats"),
               email=email)



Chart.Duo(series1="US.Transportation.FreightTransportationServices", series2="US.Transportation.PassengerTransportationServices",
          periods = 50,
          tweet = Data.Description[Data.Description$Mnemonic == "US.Transportation.PassengerTransportationServices",6] == Sys.Date() ,
          tweet.text = paste0("Transportation indices are volatile, but serve a purpuse as leading indicator. In ",
                              as.yearmon(index(tail(US.Transportation.FreightTransportationServices,1))),
                              " the Passenger Transportation Services Index stood at ",
                              tail(US.Transportation.FreightTransportationServices,1),
                              "; the Freight Transportation Index stood at ",
                              tail(US.Transportation.PassengerTransportationServices,1),
                              " in ",
                              as.yearmon(index(tail(US.Transportation.PassengerTransportationServices,1))),
                              " #rstats Details: https://whyitmatters.netlify.app/posts/2022-06-13-the-value-of-transportation-indices/"),
          email=email)


cat("\n---------------------------------------------------\n")
cat("Transportation posts done\n")
cat("---------------------------------------------------\n")


# COVID -------------------------------------------------------

covid.countries = c("United States", "China", "Japan", "Germany", "United Kingdom", "India", "France", "Italy", "Brazil", "Canada", "Spain", "Israel")
covid.cases     = covid19(covid.countries, verbose = FALSE)
plot.col        = brewer.pal(10, "Paired")

if ((hour(Sys.time()) > 15) && (weekdays(Sys.Date()) %in% c("Sunday"))) {
  
  chart.filename = "COVID19.png"
  png(filename=chart.filename, 20, 16, "in", res=127.5)
  
  par(mfrow = c(3,4))
  par(bg = "#f7f7f7")
  par(family = 'avenir')
  
  for (idx in covid.countries) {
    x = covid.cases[covid.cases$administrative_area_level_1==idx, c("date", "confirmed")]
    x = x[!is.na(x$confirmed),]
    x = zoo(diff(x$confirmed), as.Date(x$date [1:(nrow(x)-1)] ))
    x[x<0] = 0
    
    plot(x, ylab = "Daily Cases (blue) / 7-day average (red)", main = idx, 
         xlab = "", xaxt = "n", col = plot.col[1], lwd = 3, cex.main = 2.2, cex.axis = 1.4, cex.lab = 1.3)
    lines(rollmean(x, k=7, align="right", na.rm = TRUE), col = plot.col[6], lwd = 3)
    points(index(tail(na.omit(x),1)), tail(rollmean(x, k=7, align="right", na.rm = TRUE),1), pch = 19, lwd = 5, col = plot.col[6])
    axis(1, floor_date(index(x), 'month'), as.yearmon(floor_date(index(x), 'month')), cex.axis = 1)
    legend("topright", "Data: https://covid19datahub.io", cex=1.0, bty = "n")
    legend("topleft", paste0("7-day rolling average: ", round( tail(rollmean(x, k=7, align="right", na.rm = TRUE),1)/1000,1), "K"), cex=1.2, bty = "n")
    grid()
    
    if ( (weekdays(Sys.Date()) %in% c("Sunday")) &&
         (hour(Sys.time()) > 15) &&
         (idx %in% c("United States", "Germany", "United Kingdom", "Italy", "Brazil", "Canada") ) ) {
      tweet.text = paste0(idx, ": New #COVID19 cases, as of ", format( index(tail(x,1)), "%d %b" ), ", at ", format( tail(x,1), big.mark=","),
                          "; the 7-day rolling average is at ",
                          format( round(tail( rollmean(x, k=7, align="right", na.rm = TRUE),1),0), big.mark=","),
                          ifelse( tail( diff( rollmean(x, k=7, align="right", na.rm = TRUE)) ,1) > 0                          ,
                                  ", up from ", ", down from "),
                          format( round(tail( rollmean(x, k=7, align="right", na.rm = TRUE),2)[1],0), big.mark=","),
                          " the prior day (source: https://covid19datahub.io) #rstats") 
      cat("\n   * ", tweet.text)
      misc.postTweet(tweet.text, media = NULL)
      Sys.sleep(100)
    }
    
  }
  par(mfrow = c(1,1))
  dev.off()
  
  email <<- gm_attach_file(email, chart.filename)
  misc.postTweet(tweet.text = "Daily new #COVID19 cases in key economies #rstats", media = chart.filename)
}



if (weekdays(Sys.Date()) %in% c("Tuesday")) {
  
  chart.filename = "COVID19NewInfectionsPer100K.png"
  png(filename=chart.filename, 18, 12, "in", res=127.5)
  
  
  covid.countries    = c("United States", "Japan", "Germany", "United Kingdom", "India", "Italy", "Brazil", "Canada", "Israel")
  x                  = covid.cases[, c("date", "administrative_area_level_1", "confirmed", "population")]
  x$confirmed        = c(0, diff(x$confirmed))
  x$confirmed        = c(rep(0, 6), rollmean(x$confirmed, k=7, align = "right"))
  x$confirmed = x$confirmed/x$population*100000
  
  y             = melt(x, id=c("date", "administrative_area_level_1") )
  x             = cast(y[y$variable=="confirmed",], date ~ administrative_area_level_1 + variable)
  x             = zoo(x, as.Date(x$date))
  colnames(x)   = gsub( "_confirmed","", colnames(x))
  x             = x[index(x) > as.Date("2020-12-01"),covid.countries]
  country.order = covid.countries[order(tail(na.locf(x),1), decreasing = T)] # because we will use custom text labels to indicate last values, we need to determine the country's order
  
  par(oma=c(3,3,3,3))
  par(mar=c(5,5,4,2) + 0.1)
  par(bg = "#f7f7f7")
  par(family = 'avenir')
  plot(x[,1], type = "n", 
       main = "New confirmed cases of COVID 19 in Major Economies", 
       xlab = "", ylab = "7-day rolling average of new cases (per 100K)", ylim = c(0,1.1*max(na.locf(x))),
       cex.main = 2,cex.axis = 1.5, cex.lab = 2, 
       xaxt = "n",
       xlim = c(head(index(x),1), index(tail( x,1))+days(90))  )
  
  
  axis(1, seq( head(index(x),1), tail(index(x),1) , "months" ),
       as.character( as.yearmon( seq( head(index(x),1), tail(index(x),1), "months" ))), cex.axis = 1.0)
  
  for (idx in 1:length(covid.countries)) {
    lines( x[,idx], col = plot.col[idx], lwd = 4)
    points(index(tail(na.omit(x[,idx]),1)), tail(na.omit(x[,idx]),1), pch = 19, lwd = 5, col = plot.col[idx])
    
    text(index(tail(na.omit(x[,1]),1))+days(10), max(x, na.rm = TRUE)-grep(covid.countries[idx], country.order)*50, 
         paste0(covid.countries[idx], ": ", round( tail(na.omit(x[,idx]),1),1)), col = plot.col[idx],
         adj = 0, cex = 1.25, font = 2)
  }
  
  legend("bottomright", "Data: https://covid19datahub.io", bty = "n")
  abline(h=c(20, 40, 60, 80), col =  "black", lty = 3)
  par(bg = "white")
  
  dev.off()
  
  email <<- gm_attach_file(email, chart.filename)
  tweet.text = paste0("Cross-Country Comparison: New confirmed cases of #COVID19 in major economies, using data as of ",  format( tail(index(x),1), "%d %b"),
                      ", per 100K people #rstats")
  misc.postTweet(tweet.text, media = chart.filename)
  
}



if ((hour(Sys.time()) > 15) && weekdays(Sys.Date()) %in% c("Saturday")) {
  
  chart.filename = "COVID19Hospitalizations.png"
  png(filename=chart.filename, 18, 12, "in", res=127.5)

  par(mfrow = c(2,4))
  par(bg = "#f7f7f7")
  par(family = 'avenir')

  for (idx in covid.countries) {
    x = covid.cases[covid.cases$administrative_area_level_1==idx, c("date", "hosp")]
    x = zoo(x$hosp, as.Date(x$date))
    if (length(na.omit(x)) > 0) {
      plot(x, ylab = "Number of hospitalized patients on date", main = idx,
           xlab = "", xaxt = "n", col = plot.col[2], lwd = 3, cex.main = 2.2, cex.axis = 1.4, cex.lab = 1.3)

      points(index(tail(na.omit(x[,idx]),1)), tail(na.omit(x[,idx]),1), pch = 19, lwd = 5, col = plot.col[2])
      axis(1, floor_date(index(x), 'month'), as.yearmon(floor_date(index(x), 'month')), cex.axis = 1)
      legend("bottomleft", "Data: https://covid19datahub.io", cex=1.2, bty = "n")
      legend("topleft", paste0("Peak: ", max(x, na.rm = TRUE),
                               "\nLatest obs: ", tail(na.omit(x),1)), cex = 1.5, bty = "n")
      grid()
    } else cat("\n- Skipping ", idx)
  }
  par(mfrow = c(1,1))
  dev.off()

  email <<- gm_attach_file(email, chart.filename)
  misc.postTweet(tweet.text = paste0("#COVID19 Hospitalizations in major economies. Since the arrival of the more contagious, but typically less severe Omicron variant, hospitalizations may be a better indicator of the severity of the situation than infections (data as of ",
                                     format(tail(covid.cases$date,1), "%b %d"),
                                     ") #rstats")
                 , media = chart.filename)
}

if ((hour(Sys.time()) > 15) && weekdays(Sys.Date()) %in% c("Friday")) {
  
  chart.filename = "COVID19ICU.png"
  png(filename=chart.filename, 18, 12, "in", res=127.5)
  
  par(mfrow = c(3,3))
  #covid.countries = c("United States", "Japan", "Germany", "France", "Italy", "Canada")
  for (idx in covid.countries) {
    x = covid.cases[covid.cases$administrative_area_level_1==idx, c("date", "icu")]
    x = zoo(x$icu, as.Date(x$date))
    if (length(na.omit(x)) > 0) {
      plot(x, ylab = "Number of hospitalized patients in intensive therapy", main = idx,
           xlab = "", xaxt = "n", col = plot.col[2], lwd = 3, cex.main = 2.2, cex.axis = 1.4, cex.lab = 1.3)
      points(index(tail(na.omit(x),1)), tail(na.omit(x),1), pch = 19, lwd = 5, col = plot.col[2])
      axis(1, floor_date(index(x), 'month'), as.yearmon(floor_date(index(x), 'month')), cex.axis = 1)
      legend("bottomleft", "Data: https://covid19datahub.io", cex=1.2, bty = "n")
      legend("topleft", paste0("Peak: ", max(x, na.rm = TRUE),
                               "\nLatest obs: ", tail(na.omit(x),1)), cex = 1.5, bty = "n")
      grid()
    } else cat("\n- Skipping ", idx)
  }
  par(mfrow = c(1,1))
  dev.off()
  
  email <<- gm_attach_file(email, chart.filename)
  misc.postTweet(tweet.text = paste0("#COVID19 Patients in Intensive Care Units (data as of ",
                                     format(tail(covid.cases$date,1), "%b %d"),
                                     ") #rstats")
                 , media = chart.filename)
  
  chart.filename = "COVID19FullyVaccinationRatio.png"
  png(filename=chart.filename, 18, 12, "in", res=127.5)
  
  #covid.countries = c("United States", "Japan", "Germany", "United Kingdom", "India", "France", "Italy", "Brazil", "Canada")
  x                  = covid.cases[, c("date", "administrative_area_level_1", "people_fully_vaccinated", "population")]
  x$vaccinated_ratio = x$people_fully_vaccinated/x$population*100
  
  y             = melt(x, id=c("date", "administrative_area_level_1") )
  x             = cast(y[y$variable=="vaccinated_ratio",], date ~ administrative_area_level_1 + variable)
  x             = zoo(x, as.Date(x$date))
  colnames(x)   = gsub( "_vaccinated_ratio","", colnames(x))
  x             = x[index(x) > as.Date("2020-12-01"),covid.countries]
  country.order = covid.countries[order(tail(na.locf(x),1), decreasing = T)] # because we will use custom text labels to indicate last values, we need to determine the country's order
  
  par(oma=c(3,3,3,3))
  par(mar=c(5,5,4,2) + 0.1)
  par(bg = "#f7f7f7")
  par(family = 'avenir')
  
  plot(x[,1], type = "n", 
       main = "Fully Vaccinated People as % of Total Population: Major Economies", 
       xlab = "", ylab = "Fully vaccinated as % of population", ylim = c(0,100),
       cex.main = 2,cex.axis = 1.5, cex.lab = 2, 
       xaxt = "n",
       xlim = c(head(index(x),1), tail(index(x),1)+days(90))  )
  
  
  axis(1, seq( head(index(x),1), tail(index(x),1) , "months" ),
       as.character( as.yearmon( seq( head(index(x),1), tail(index(x),1), "months" ))), cex.axis = 1.0)
  
  for (idx in 1:length(covid.countries)) {
    lines( x[,idx], col = plot.col[idx], lwd = 4)
    points(index(tail(na.omit(x[,idx]),1)), tail(na.omit(x[,idx]),1), pch = 19, lwd = 5, col = plot.col[idx])
    
    text(index(tail(na.omit(x[,1]),1))+days(10), 1.1*max(x, na.rm = TRUE)-grep(covid.countries[idx], country.order)*5, 
         paste0(covid.countries[idx], ": ", round( tail(na.omit(x[,idx]),1),1), "%"), col = plot.col[idx],
         adj = 0, cex = 1.0, font = 2)
  }
  
  legend("bottomright", "Data: https://covid19datahub.io", bty = "n")
  abline(h=c(20, 40, 60, 80), col =  plot.col[2], lty = 3)
  dev.off()
  
  email <<- gm_attach_file(email, chart.filename)
  misc.postTweet("COVID 19 #FullVaccination in major economies (defined as number of people who received all doses prescribed by the vaccination protocol over total population). Details: https://whyitmatters.netlify.app/posts/2021-11-10-covid19-update/  #rstats", media = chart.filename)
  
  
  chart.filename = "COVID19PartialVaccinationRatio.png"
  png(filename=chart.filename, 18, 12, "in", res=127.5)
  
  #covid.countries = c("United States", "Japan", "Germany", "United Kingdom", "India", "France", "Italy", "Brazil", "Canada")
  
  x                  = covid.cases[, c("date", "administrative_area_level_1", "people_vaccinated", "population")]
  x$vaccinated_ratio = x$people_vaccinated/x$population*100
  
  y             = melt(x, id=c("date", "administrative_area_level_1") )
  x             = cast(y[y$variable=="vaccinated_ratio",], date ~ administrative_area_level_1 + variable)
  x             = zoo(x, as.Date(x$date))
  colnames(x)   = gsub( "_vaccinated_ratio","", colnames(x))
  x             = x[index(x) > as.Date("2020-12-01"),covid.countries]
  country.order = covid.countries[order(tail(na.locf(x),1), decreasing = T)] # because we will use custom text labels to indicate last values, we need to determine the country's order
  
  par(oma=c(3,3,3,3))
  par(mar=c(5,5,4,2) + 0.1)
  par(bg = "#f7f7f7")
  par(family = 'avenir')
  
  plot(x[,1], type = "n", 
       main = "Partially Vaccinated People as % of Total Population: Major Economies", 
       xlab = "", ylab = "Fully or partially vaccinated as % of population", ylim = c(0,100),
       cex.main = 2,cex.axis = 1.5, cex.lab = 2, 
       xaxt = "n",
       xlim = c(head(index(x),1), tail(index(x),1)+days(90))  )
  
  
  axis(1, seq( head(index(x),1), tail(index(x),1) , "months" ),
       as.character( as.yearmon( seq( head(index(x),1), tail(index(x),1), "months" ))), cex.axis = 1.0)
  
  for (idx in 1:length(covid.countries)) {
    lines( x[,idx], col = plot.col[idx], lwd = 4)
    points(index(tail(na.omit(x[,idx]),1)), tail(na.omit(x[,idx]),1), pch = 19, lwd = 5, col = plot.col[idx])
    
    text(index(tail(na.omit(x[,1]),1))+days(10), 1.1*max(x, na.rm = TRUE)-grep(covid.countries[idx], country.order)*5, 
         paste0(covid.countries[idx], ": ", round( tail(na.omit(x[,idx]),1),1), "%"), col = plot.col[idx],
         adj = 0, cex = 1.25, font = 2)
  }
  
  legend("bottomright", "Data: https://covid19datahub.io", bty = "n")
  abline(h=c(20, 40, 60, 80), col =  plot.col[2], lty = 3)
  dev.off()
  
  email <<- gm_attach_file(email, chart.filename)
  misc.postTweet(tweet.text = "COVID 19 #PartialVaccination in major economies (defined as number of people who received at least one dose over total population). Details: https://whyitmatters.netlify.app/posts/2021-11-10-covid19-update/  #rstats", media = chart.filename)
  
}


cat("\n---------------------------------------------------\n")
cat("COVID posts done\n")
cat("---------------------------------------------------\n")


gm_send_message(email)


cat("\n---------------------------------------------------\n")
cat("Email sent\n")
cat("---------------------------------------------------\n")
sink()

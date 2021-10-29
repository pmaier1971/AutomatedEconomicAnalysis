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

# Test the online connection ####
test.online        <- try(getSymbols("GDPC1",src='FRED'))

if ((inherits(test.online, "try-error"))) {
  stop("\n\n\n----------\n Not online \n----------\n\n")
}

rm(list=ls())

## Credentials

#include("")

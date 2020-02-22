# Load packages
library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)
library(shinyalert)
library(echarts4r)
library(quantmod)
library(edgarWebR)
library(finreportr)
#library(plotly)
#library(echarts4r)

#library(dplyr)
library(lubridate)
library(tidyverse)

library(zoo)
library(scales)
library(DT)
library(tableHTML)

library(aRpsDCA)

library(plotly)
#library(echarts4r)
library(billboarder)

library(leaflet)
library(leaflet.extras)

library(sp)
library(maptools)
library(maps)
library(tools)
library(geosphere)
library(tigris)
library(sf)
library(openintro)
library(RColorBrewer)
library(rgdal)
library(akima)

library(kernlab)
library(caret)

library(httr)
library(rvest)
library(tableHTML)
library(xml2)
library(purrr)
library(stringr)
library(jsonlite)

library(tidyRSS)


wellData <- readRDS('./data/wellData.rds')
prodData <- readRDS('./data/prod.data.RDS')
prodData[is.na(prodData)] <- 0
prodSummary <- readRDS('./data/prodSummary.rds')
gasFrac <- readRDS('./data/gasFrac.rds')
perfUplift <- readRDS('./data/perfUplift.rds')
propUplift <- readRDS('./data/propUplift.rds')
twnRng <- readRDS('./data/twnRng.rds')
countyLines <- readRDS('./data/ndLines.rds')
sectionLocs <- readRDS('./data/sectionLocs.rds')
acreage <- readRDS('./data/acreage.rds')
flaring <- readRDS('./data/flaring.rds') %>% filter(operator %in% wellData$operator)
# acreage1 <- acreage[,c('surfLoc', 'operator')]
# names(acreage1)[1] <- c('TWPRNGSEC')
# sectionLocs <- merge(sectionLocs, acreage1, by='TWPRNGSEC', all.x=TRUE)
# rm(acreage1)

IRRcalc <- function(cf, months){
  if(sum(cf) > 0){
    IRR1 <- 3
    loop <- 1
    while(sum(cf/((1+IRR1)^(months/12)))
          < 0){
      IRR1 <- IRR1 - 0.01
      loop = loop + 1
    }
    
  }else {
    IRR1 <- 0
  }
  return(IRR1)
}

opList1 <- c('AXAS', 'CLR', 'COP', 'ECA','EOG',
              'HES', 'HPR', 
             'MRO',    'OAS', 'QEP', 
              'SM',   'WLL', 'WPX', 'XOM')  

css <- HTML(
  "#invTable > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
        transform:rotateX(180deg);
    }
    #invTable > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
        transform:rotateX(180deg);
    }
  #pdpValue > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
        transform:rotateX(180deg);
    }
    #pdpValue > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
        transform:rotateX(180deg);
    }
  #pudValue > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
        transform:rotateX(180deg);
    }
    #pudValue > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
        transform:rotateX(180deg);
    }"
)

# color statuses
statusColors <- c(
  "navy",
  "gray-dark",
  "gray",
  "secondary",
  "indigo",
  "purple",
  "primary",
  "info",
  "success",
  "olive",
  "teal",
  "lime",
  "warning",
  "orange",
  "danger",
  "fuchsia",
  "maroon",
  "pink",
  "light"
)

is2List <- c('AVERAGE SHARES OUTSTANDING', 'Average Shares Outstanding', 'Average shares outstanding', 'average shares outstanding',
             'COMMON SHARES OUTSTANDING', 'Common Shares Outstanding', 'Common shares outstanding', 'common shares outstanding',
             'AVERAGE NUMBER OF', 'Average Number of', 'Average number of', 'average number of', 
             'AVERAGE COMMON SHARES', 'Average Common Shares', 'Average common shares', 'average common shares',
             'A Common Stock', 'of Common Stock', 'TO COMMON STOCK', 'To Common Stock', 'to Common Stock', 'to common stock',
             'WEIGHTED AVERAGE SHARES', 'Weighted Average Shares', 'Weighted average shares', 'weighted average shares',
             'per share att', 'Earnings per Share', 'EARNINGS PER SHARE', 'Earnings per share', 'attributable to common stockh')
is1List <- c('COSTS AND EXPENSES', 'Oil and gas sales', 'Costs and Expenses', 'Costs and expenses',
             'COSTS AND OTHER', 'Costs and Other', 'Costs and other', 
             'OPERATING EXPENSES', 'Operating expenses', 'Operating Expenses',
             'OPERATING COSTS AND EXPENSES', 'Operating Costs and Expenses', 'Operating costs and expenses',
             'TOTAL REVENUES', 'Total Revenues', 'Total revenues', 'REVENUES', 'Revenues')
bs2List <- c('TOTAL LIABILITIES AND', 'Total Liabilities and', 'Total liabilities and', 'Total liabilities', 'TOTAL EQUITY', 'Total stockholders', 'LIABILITIES AND EQUITY')
bs1List <- c('Total Other Assets', 'TOTAL ASSETS',  'CURRENT ASSETS', 'Current Assets', 'Current assets',
             'ASSETS', 'Total Assets','Total assets', 'Assets')
cf1List <- c('FROM OPERATING ACTIVITIES', 'From Operating Activities', 'from operating activities',
             'OPERATING ACTIVITIES','Operating activities', 'Operating Activities', 'operating activities')

cf2List <- c('IN CASH AND CASH', 'In Cash and Cash', 'in Cash and Cash', 'in cash and cash',
             'NET CHANGE IN CASH', 'Net Change in Cash', 'Net change in cash', 'CASH EQUIVALENTS AND RESTRICTED',
             'Cash Equivalents and Restricted', 'Cash equivalents and restricted', 'cash equivalents and restricted',
             'CASH AND CASH', 'Cash and Cash', 'Cash and cash', 'cash and cash', 'SUPPLEMENTAL CASH', 'Supplemental Cash',
             'Supplemental cash', 'PROVIDED BY FINANCING ACTIV', 'Provided by Financing Activ', 'Provided by financing activ', 'provided by financing activ')

prodList <- c('Natural gas liquids production', 'Cash settled deriv', 'Sales volume detail', 'Summary Operating', 'Volume Variances', 'Total STACK Areas', 'Production data', 
              'Total MMBoe','Net Production Data', 'Average Net Production', 'Barrels of Oil Equivalent', 'Crude Oil & Condensate','Total Production ',
              'Total company', 'Net production', 'Production Data',
              'Volumes reconciliation', 'mboed', 'mboepd',
              'Production volumes', 'All Fields', 'Production Volumes','Total production volumes','Production information', 'BOEs', 'Average daily production', 'Natural gas production volumes', 
              'per day', 'Sales volumes','Net sales volume', 'Net Sales Volume','Production Sales Volume', 'Total production',
              'Oil equivalent', 'NGLs and gas sales including', 'LIQUIDS', 'Eagle Ford', 'NGLs', 'Crude oil equivalent', 'per Boe', 'Bbls', 'Average Daily Production',
              'Average net sales', 'Sales Volume')

reserveList <- c('Proved Developed', 'PROVED DEVELOPED', 'Proved developed', 'Standardized measure of discount', 'Discounted future net')


derivList <- c('SWAP', 'Swap', 'swap', 'COLLAR','Collar', 'collar', 'Natural gas MMbtu',
               'Weighted average index', 'Weightedaverage in', 
               'Mont Belvieu', 'NYMEX to TCO',  'Ceiling price', 'Settlement Index', 'Volumes Hedged', 
               'Natural gas and power', 'Daily Volume', 'Location', 'NYMEX H', 'NYMEX W', 'Gulfport Receives', 'Threshold', 'Weighted Average Price',
               'Puts', 'PUTS')

debtList <- c('Notes and deb', 'Senior Notes due', 'Commercial paper', 'Credit Facility', 'Credit facility', 'Senior secured credit', 'Senior Unsecured', 
              'Revolving credit', 'Revolving Credit A', 'Percent of total debt', 'Debt issuance', 'Contractual Obligation', 'Senior Second Lien', 'Notes due 2', 'notes due 2')

firmList <- c('Firm Tr', 'Firm tr', 'FIRM TR', 'firm trans')

ebitdaList <- c('EBITDA', 'ebitda')

bsScrape <- function(nodes, string1, check) {
  nodes <- nodes
  check <- check
  #html_nodes('a') %>% 
  #html_attr("href")
  #tbls <- tbls[!duplicated(tbls)]
  strMatch <- string1
  
  signal <- FALSE
  my_tables <- list()
  #my_length <- list()
  j <- 0
  for (i in 1:length(nodes)) {
    signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    # if title signal previously set and this is a table tag
    if (signal & html_name(nodes[i]) == "table") {
      cat("Match..\n")
      print(i)
      # get the table (data frame)
      list1 <- nodes[i]# %>% html_nodes('tr')
      this_table <- list1 %>% paste(collapse='\n')
      this_table <- data.frame(list1 = this_table)
      list1 <- nodes[i] %>% html_nodes('tr')
      this_table$rows <- length(list1)
      if(TRUE %in% grepl('Subsidi', this_table) & !check$comp.ticker %in% c('CNX', 'SBOW', 'PXD', 'GPOR', 'FANG')){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Third Party', this_table) & check$comp.ticker %in% c('CNX')){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Operating Expense', this_table) & check$comp.ticker %in% c('CNX')){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Earnings Before', this_table) & check$comp.ticker %in% c('CNX')){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Basis Only', this_table) & check$comp.ticker %in% c('CNX')){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Total Revenue', this_table) & check$comp.ticker %in% c('CNX')){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Total Reportable', this_table) & check$comp.ticker %in% c('CNX')){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Variable Lease Cost', this_table) & check$comp.ticker %in% c('CNX')){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Obtained in Exchange', this_table) & check$comp.ticker %in% c('CNX')){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Customer Relationships', this_table) & check$comp.ticker %in% c('CNX')){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Utica Shale', this_table) & check$comp.ticker %in% c('CNX')){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Fresh-Start', this_table) & check$comp.ticker %in% c('BCEI')){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Consideration Given', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Total liabilities of discontinued operations', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Guarantor', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Fair Value', this_table)& !check$comp.ticker %in% c('SBOW')){
        this_table <- NULL
      }
      if(TRUE %in% grepl('swaps', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Operating A', this_table)){
        this_table <- NULL
      }
      
      
      # if(TRUE %in% grepl('Income', this_table)&  !check$comp.ticker %in% c('GPOR', 'FANG')){
      #   this_table <- NULL
      # }
      
      j = j + 1
      my_tables[[j]] <- this_table
      
      
      
      # and reset the signal so we search for the next one
      signal <- FALSE
    }
    
    # if the signal is clear look for matching title
    if (!signal) {
      signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    }
  }
  return(my_tables)
}

isScrape <- function(nodes, string1, check) {
  nodes <- nodes
  check <- check
  #html_nodes('a') %>% 
  #html_attr("href")
  #tbls <- tbls[!duplicated(tbls)]
  strMatch <- string1
  
  signal <- FALSE
  my_tables <- list()
  #my_length <- list()
  j <- 0
  for (i in 1:length(nodes)) {
    signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    # if title signal previously set and this is a table tag
    if (signal & html_name(nodes[i]) == "table") {
      cat("Match..\n")
      print(i)
      # get the table (data frame)
      list1 <- nodes[i]# %>% html_nodes('tr')
      this_table <- list1 %>% paste(collapse='\n')
      this_table <- data.frame(list1 = this_table)
      list1 <- nodes[i] %>% html_nodes('tr')
      this_table$rows <- length(list1)
      
      if(TRUE %in% grepl('Guarantor', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('LIABILI', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Changes in future development', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Column C', this_table)){
        this_table <- NULL
      }
      
      if(TRUE %in% grepl('Predecess', this_table) & substr(check$date,2,2) == '4' & check$comp.ticker == 'CRK'){
        this_table <- NULL
      }
      
      if((TRUE %in% grepl('Subsid', this_table)) & (check$comp.ticker == 'COP')){
        this_table <- NULL
      }
      
      if((TRUE %in% grepl('Nine Months', this_table)) & (check$comp.ticker == 'AR')){
        this_table <- NULL
      }
      
      if((TRUE %in% grepl('Under Successful', this_table)) & (check$comp.ticker == 'CHK')){
        this_table <- NULL
      }
      
      if((TRUE %in% grepl('Retained Earnings', this_table)) & (check$comp.ticker == 'BRY')){
        this_table <- NULL
      }
      
      if((TRUE %in% grepl('Certificate', this_table)) & (check$comp.ticker == 'BRY')){
        this_table <- NULL
      }
      
      if((TRUE %in% grepl('per Bbl', this_table))){
        this_table <- NULL
      }
      if((TRUE %in% grepl('Boe', this_table))){
        this_table <- NULL
      }
      if((TRUE %in% grepl('Per Unit', this_table))){
        this_table <- NULL
      }
      if((TRUE %in% grepl('Current assets', this_table))){
        this_table <- NULL
      }
      if((TRUE %in% grepl('per BOE', this_table))){
        this_table <- NULL
      }
      if((TRUE %in% grepl('EPS calculation', this_table))){
        this_table <- NULL
      }
      if((TRUE %in% grepl('Oil and gas sales', this_table) & check$comp.ticker == 'JAG')){
        this_table <- NULL
      }
      if((TRUE %in% grepl('excluding corporate overhead', this_table) & check$comp.ticker == 'MUR')){
        this_table <- NULL
      }
      j = j + 1
      my_tables[[j]] <- this_table
      
      
      
      # and reset the signal so we search for the next one
      signal <- FALSE
    }
    
    # if the signal is clear look for matching title
    if (!signal) {
      signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    }
  }
  return(my_tables)
}

cfScrape <- function(nodes, string1, check) {
  nodes <- nodes
  check <- check
  #html_nodes('a') %>% 
  #html_attr("href")
  #tbls <- tbls[!duplicated(tbls)]
  strMatch <- string1
  
  signal <- FALSE
  my_tables <- list()
  #my_length <- list()
  j <- 0
  for (i in 1:length(nodes)) {
    signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    # if title signal previously set and this is a table tag
    if (signal & html_name(nodes[i]) == "table") {
      cat("Match..\n")
      print(i)
      # get the table (data frame)
      list1 <- nodes[i]# %>% html_nodes('tr')
      this_table <- list1 %>% paste(collapse='\n')
      this_table <- data.frame(list1 = this_table)
      list1 <- nodes[i] %>% html_nodes('tr')
      this_table$rows <- length(list1)
      if(TRUE %in% grepl('Guarantor', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Activity Type', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('retrospective', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Subsid', this_table) & check$comp.ticker == 'COP'){
        this_table <- NULL
      }
      
      #if((TRUE %in% grepl('Nine Months', this_table)) & (check$comp.ticker == 'AR')){
      #  this_table <- NULL
      #}
      
      if((TRUE %in% grepl('Under Successful', this_table)) & (check$comp.ticker == 'CHK')){
        this_table <- NULL
      }
      
      
      
      j = j + 1
      my_tables[[j]] <- this_table
      
      
      
      # and reset the signal so we search for the next one
      signal <- FALSE
    }
    
    # if the signal is clear look for matching title
    if (!signal) {
      signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    }
  }
  return(my_tables)
}


prodScrape <- function(nodes, string1, check) {
  nodes <- nodes
  check <- check
  #html_nodes('a') %>% 
  #html_attr("href")
  #tbls <- tbls[!duplicated(tbls)]
  strMatch <- string1
  
  signal <- FALSE
  my_tables <- list()
  #my_length <- list()
  j <- 0
  for (i in 1:length(nodes)) {
    signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    # if title signal previously set and this is a table tag
    if (signal & html_name(nodes[i]) == "table") {
      cat("Match..\n")
      print(i)
      # get the table (data frame)
      list1 <- nodes[i]# %>% html_nodes('tr')
      this_table <- list1 %>% paste(collapse='\n')
      this_table <- data.frame(list1 = this_table)
      list1 <- nodes[i] %>% html_nodes('tr')
      this_table$rows <- length(list1)
      
      if(TRUE %in% grepl('Guarantor', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('compared to the same period', this_table)){
        this_table <- NULL
      }
      
      if(TRUE %in% grepl('QRE', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('means one million', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('means one thousand', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('barrel of oil equivalent per day', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Proved Developed Reserves', this_table)){
        this_table <- NULL
      }
      # if(TRUE %in% grepl('Eagle Ford', this_table) & !check$comp.ticker %in% c('CHK', 'DVN')){
      #   this_table <- NULL
      # }
      if(TRUE %in% grepl('Production information', this_table) & check$comp.ticker != 'PXD'){
        this_table <- NULL
      }
      if(TRUE %in% grepl('All Fields', this_table) & check$comp.ticker != 'SBOW'){
        this_table <- NULL
      }
      
      if(TRUE %in% grepl('Access Pipeline', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('refers to production', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Lucid Energy', this_table)){
        this_table <- NULL
      }
      
      if(TRUE %in% grepl('Company record average', this_table)){
        this_table <- NULL
      }
      
      if(TRUE %in% grepl('Completion', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('EBITDA', this_table) & check$comp.ticker == 'MTDR'){
        this_table <- NULL
      }
      
      if(check$comp.ticker == 'XEC'){
        if(TRUE %in% grepl('Variance', this_table) & check$comp.ticker != 'COG'){
          this_table <- NULL
        }
      }
      if(TRUE %in% grepl('Changes due', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('net dollar effect of change', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Guidance', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Swap', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Other Metrics', this_table)){
        this_table <- NULL
      }
      
      
      if(TRUE %in% grepl('noncontrolling', this_table) & check$comp.ticker != 'MUR'){
        this_table <- NULL
      }
      if(TRUE %in% grepl('WTI index', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Total average production of', this_table) & check$comp.ticker == 'CNX'){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Net sales volumes produced ', this_table) & check$comp.ticker == 'CNX'){
        this_table <- NULL
      }
      
      j = j + 1
      my_tables[[j]] <- this_table
      
      
      
      # and reset the signal so we search for the next one
      signal <- FALSE
    }
    
    # if the signal is clear look for matching title
    if (!signal) {
      signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    }
  }
  return(my_tables)
}

derivScrape <- function(nodes, string1, check) {
  nodes <- nodes
  check <- check
  #html_nodes('a') %>% 
  #html_attr("href")
  #tbls <- tbls[!duplicated(tbls)]
  strMatch <- string1
  
  signal <- FALSE
  my_tables <- list()
  #my_length <- list()
  j <- 0
  for (i in 1:length(nodes)) {
    signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    # if title signal previously set and this is a table tag
    if (signal & html_name(nodes[i]) == "table") {
      cat("Match..\n")
      print(i)
      # get the table (data frame)
      list1 <- nodes[i]# %>% html_nodes('tr')
      this_table <- list1 %>% paste(collapse='\n')
      this_table <- data.frame(list1 = this_table)
      list1 <- nodes[i] %>% html_nodes('tr')
      this_table$rows <- length(list1)
      
      if(TRUE %in% grepl('Cash settle', this_table)){
        this_table <- NULL
      }
      
      if(TRUE %in% grepl('PURCHASES OF EQUITY S', this_table)){
        this_table <- NULL
      }
      
      if(TRUE %in% grepl('Paraxylene', this_table)){
        this_table <- NULL
      }
      
      j = j + 1
      my_tables[[j]] <- this_table
      
      
      
      # and reset the signal so we search for the next one
      signal <- FALSE
    }
    
    # if the signal is clear look for matching title
    if (!signal) {
      signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    }
  }
  return(my_tables)
}

debtScrape <- function(nodes, string1, check) {
  nodes <- nodes
  check <- check
  #html_nodes('a') %>% 
  #html_attr("href")
  #tbls <- tbls[!duplicated(tbls)]
  strMatch <- string1
  
  signal <- FALSE
  my_tables <- list()
  #my_length <- list()
  j <- 0
  for (i in 1:length(nodes)) {
    signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    # if title signal previously set and this is a table tag
    if (signal & html_name(nodes[i]) == "table") {
      cat("Match..\n")
      print(i)
      # get the table (data frame)
      list1 <- nodes[i]# %>% html_nodes('tr')
      this_table <- list1 %>% paste(collapse='\n')
      this_table <- data.frame(list1 = this_table)
      list1 <- nodes[i] %>% html_nodes('tr')
      this_table$rows <- length(list1)
      
      
      if(TRUE %in% grepl('Bylaws', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Wellbore', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('investing activities', this_table)){
        this_table <- NULL
      }
      
      if(TRUE %in% grepl('initial public offering', this_table)){
        this_table <- NULL
      }
      
      if(TRUE %in% grepl('Certificat', this_table)){
        this_table <- NULL
      }
      
      if(TRUE %in% grepl('Investing Activities', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('Exhibit', this_table)){
        this_table <- NULL
      }
      
      if(TRUE %in% grepl('CURRENT LIABILITIES', this_table)){
        this_table <- NULL
      }
      
      if(TRUE %in% grepl('Current liabilities', this_table)){
        this_table <- NULL
      }
      
      if(TRUE %in% grepl('Balance Sheet Data', this_table)){
        this_table <- NULL
      }
      
      if(TRUE %in% grepl('Other assets', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('OPERATING EXPENSES', this_table)){
        this_table <- NULL
      }
      
      j = j + 1
      my_tables[[j]] <- this_table
      
      
      
      # and reset the signal so we search for the next one
      signal <- FALSE
    }
    
    # if the signal is clear look for matching title
    if (!signal) {
      signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    }
  }
  return(my_tables)
}

firmScrape <- function(nodes, string1, check) {
  nodes <- nodes
  check <- check
  #html_nodes('a') %>% 
  #html_attr("href")
  #tbls <- tbls[!duplicated(tbls)]
  strMatch <- string1
  
  signal <- FALSE
  my_tables <- list()
  #my_length <- list()
  j <- 0
  for (i in 1:length(nodes)) {
    signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    # if title signal previously set and this is a table tag
    if (signal & html_name(nodes[i]) == "table") {
      cat("Match..\n")
      print(i)
      # get the table (data frame)
      list1 <- nodes[i]# %>% html_nodes('tr')
      this_table <- list1 %>% paste(collapse='\n')
      this_table <- data.frame(list1 = this_table)
      list1 <- nodes[i] %>% html_nodes('tr')
      this_table$rows <- length(list1)
      
      
      if(TRUE %in% grepl('Bylaws', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('principles generally', this_table)){
        this_table <- NULL
      }
      
      j = j + 1
      my_tables[[j]] <- this_table
      
      
      
      # and reset the signal so we search for the next one
      signal <- FALSE
    }
    
    # if the signal is clear look for matching title
    if (!signal) {
      signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    }
  }
  return(my_tables)
}

reserveScrape <- function(nodes, string1, check) {
  nodes <- nodes
  check <- check
  #html_nodes('a') %>% 
  #html_attr("href")
  #tbls <- tbls[!duplicated(tbls)]
  strMatch <- string1
  
  signal <- FALSE
  my_tables <- list()
  #my_length <- list()
  j <- 0
  for (i in 1:length(nodes)) {
    signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    # if title signal previously set and this is a table tag
    if (signal & html_name(nodes[i]) == "table") {
      cat("Match..\n")
      print(i)
      # get the table (data frame)
      list1 <- nodes[i]# %>% html_nodes('tr')
      this_table <- list1 %>% paste(collapse='\n')
      this_table <- data.frame(list1 = this_table)
      list1 <- nodes[i] %>% html_nodes('tr')
      this_table$rows <- length(list1)
      
      
      if(TRUE %in% grepl('Bylaws', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('principles generally', this_table)){
        this_table <- NULL
      }
      
      j = j + 1
      my_tables[[j]] <- this_table
      
      
      
      # and reset the signal so we search for the next one
      signal <- FALSE
    }
    
    # if the signal is clear look for matching title
    if (!signal) {
      signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    }
  }
  return(my_tables)
}

ebitdaScrape <- function(nodes, string1, check) {
  nodes <- nodes
  check <- check
  #html_nodes('a') %>% 
  #html_attr("href")
  #tbls <- tbls[!duplicated(tbls)]
  strMatch <- string1
  
  signal <- FALSE
  my_tables <- list()
  #my_length <- list()
  j <- 0
  for (i in 1:length(nodes)) {
    signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    # if title signal previously set and this is a table tag
    if (signal & html_name(nodes[i]) == "table") {
      cat("Match..\n")
      print(i)
      # get the table (data frame)
      list1 <- nodes[i]# %>% html_nodes('tr')
      this_table <- list1 %>% paste(collapse='\n')
      this_table <- data.frame(list1 = this_table)
      list1 <- nodes[i] %>% html_nodes('tr')
      this_table$rows <- length(list1)
      
      
      if(TRUE %in% grepl('Bylaws', this_table)){
        this_table <- NULL
      }
      if(TRUE %in% grepl('principles generally', this_table)){
        this_table <- NULL
      }
      
      j = j + 1
      my_tables[[j]] <- this_table
      
      
      
      # and reset the signal so we search for the next one
      signal <- FALSE
    }
    
    # if the signal is clear look for matching title
    if (!signal) {
      signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    }
  }
  return(my_tables)
}
#' home tab ---
#' 
#' 
#' 


home_tab <- bs4TabItem(
  tabName = "home",
  br(),
  br(),
  fluidRow(
    bs4Timeline(
      width = 5,
      bs4TimelineEnd(status = "primary"),
      bs4TimelineLabel("1953", status = "info"),
      bs4TimelineItem(
        elevation = 4,
        title = "Discovery",
        icon = "wpexplorer",
        status = "success",
        time = "1953",
        footer = "Drilled by Stanolind Oil and Gas",
        "#1 Woodrow Star Well"
      ),
      bs4TimelineItem(
        elevation = 4,
        title = "Elm Coulee",
        icon = "industry",
        status = "info",
        time = "2000",
        footer = "Ultimate Recovery of 270 million bbls",
        "Elm Coulee Field Discovered in neighboring Montana"
      ),
      bs4TimelineItem(
        elevation = 4,
        title = "Development",
        icon = "industry",
        status = "danger",
        time = "mid-2000's",
        footer = "North Dakota Bakken grows dramatically",
        "Horizontal Development begins in earnest"
      ),
      bs4TimelineItem(
        elevation = 4,
        title = 'Today',
        icon = 'chart-line',
        status = 'primary',
        time = 'December 2019',
        footer = tags$a('Wikipedia', href='https://en.wikipedia.org/wiki/Bakken_Formation', target = '_blank', style="color:blue"),
        
        "Bakken/Three Forks North Dakota Production Reaches >1.3 MMBBL Per Day"
      ),
      bs4TimelineEnd(status = 'primary')
      
    ),
    bs4Card(
      title = 'Bakken Map', closable = FALSE,
      width = 7,
      status = "info",
      solidHeader = FALSE,
      collapsible = TRUE,
      tags$img(src = 'Bakken_map.png', width = '100%'),
      h6('By US Energy Information Administration - http://www.eia.gov/oil_gas/rpd/shaleoil1.pdf,
        Public Domain, https://commons.wikimedia.org/w/index.php?curid=25725650')
      
    ),
    
    bs4Card(
      title = 'Bakken/Three Forks', closable = FALSE,
      width = 6,
      status = "info",
      solidHeader = FALSE,
      collapsible = TRUE,
      p('The Bakken and Three Forks are reservoirs primarily
        produced unconventionally in North Dakota.  Regardless
        of your view on the industry, there is a wealth of
        data available publicly to allow for robust data
        analytics and mapping workflows.'),
      p('The typical use of Type Curves to value oil and gas reserves
        is an outdated and stale process and prone to large optimistic/biased
        answers.  This App employs some analytics workflows to quantify
        remaining inventory in the play based on both price
        and economic assumptions and largely removes the bias of historical methodology.
        It also allows the user to create a Net Asset Value model based
        on the currently producing wells and the future developed inventory.  This inventory is
        only added if it reaches the defined IRR threshold.'),
      p('This workflow allows each individual well in a unit to be assigned an
        ultimate recovery (EUR) based on both location and well design (proppant loading and lateral length).
        EUR is determined via an autocast model (aRpsDCA package) that is trained on older wells in order to tune parameters.
        Future well EUR estimates are based on a geo-located SVM model trained on the data with the Caret Package.'),
      p('The data sources for the app are the North Dakota State Government Website (for production and lateral lengths/locations)
        and FracFocus, for proppant and water usage data in the fracture stimulation.  Timeline Information from Wikipedia.')
    ),
    bs4Card(
      title = "Production Summary - By First Production Year",
      closable = FALSE,
      width = 6,
      status = "info",
      solidHeader = FALSE,
      collapsible = TRUE,
      selectizeInput(inputId =  "fluidAll", choices = c('oil', 'gas', 'water'),
                     label = "Product Stream", selected = "oil", multiple = FALSE),
      plotlyOutput('prodPlot2')%>% withSpinner(color="#0dc5c1")
    )
  )
)


news_tab <- bs4TabItem(
  tabName = 'news',
  br(),
  br(),
  fluidRow(
    bs4Card(title = 'Bakken News', width = 12,
            status = 'info',
            closable = FALSE,
            maximizable = TRUE,
            collapsible = TRUE,
            DT::dataTableOutput('news'))
  )
)


  
  
financials_tab <- bs4TabItem(
  tabName = 'financials',
  br(),
  br(),
  
  fluidRow(
    bs4Card(title = 'Stock Performance', width = 6,
            status = "info",
            closable = FALSE,
            maximizable = TRUE,
            collapsible = TRUE,
            selectizeInput('opTicker', 'Operator', choices = opList1, multiple = FALSE),
            dateInput('start_date', 'Select Start Date', value = as.POSIXct('2019-01-01', format = '%Y-%m-%d')),
            echarts4rOutput('hcplot')),

    bs4Card(title = 'Comparative Stock Performance', width = 6,
            status = "info",
            closable = FALSE,
            maximizable = TRUE,
            collapsible = TRUE,
            pickerInput('opTicker1', 'Operator(s)', choices = opList1, multiple = TRUE),
            dateInput('start_date1', 'Select Start Date', value = as.POSIXct('2019-01-01', format = '%Y-%m-%d')),
            echarts4rOutput('hcplot1')
    )
  ),
  
  #selectizeInput('operatorFiling', 'Operator', choices = opList1, multiple = FALSE),
  fluidRow(
    column(6,
           bs4Card(
             title ='SEC Quarterly/Annual Filings' ,
             width = 12,
             status = "info",
             closable = FALSE,
             maximizable = TRUE,
             collapsible = TRUE,
             
             
             
             bsButton('loadFilings', 'LOAD', style='primary', size='extra-small'),
             DT::dataTableOutput('filingList')
             
           )
           
    ),
    column(6,
           bs4Card(
             title ='Load Tables' ,
             width = 12,
             status = "info",
             closable = FALSE,
             maximizable = TRUE,
             collapsible = TRUE,
             selectizeInput('Filing', 'Available Periods', choices = '', selected = NULL),
             
             
             
             bsButton('loadTables', 'LOAD', style='primary', size='extra-small')
             
           )
           
    )
  ),
  
  fluidRow(
    
    column(6,
           bs4Card(
             title = 'Balance Sheet',
             width = 12,
             status = "info",
             closable = FALSE,
             maximizable = TRUE,
             collapsible = TRUE,
             tableHTML_output('bs')
           )),
    
    column(6,
           bs4Card(
             title = 'Income Statement',
             width = 12,
             status = "info",
             closable = FALSE,
             maximizable = TRUE,
             collapsible = TRUE,
             tableHTML_output('is')
           ))
  ),
  fluidRow(
    
    column(6,
           bs4Card(
             title = 'Cash Flow Statement',
             width = 12,
             status = "info",
             closable = FALSE,
             maximizable = TRUE,
             collapsible = TRUE,
             tableHTML_output('cf')
           )),
    column(6,
           
           bs4Card(
             title = 'Production Tables',
             width = 12,
             status = "info",
             closable = FALSE,
             maximizable = TRUE,
             collapsible = TRUE,
             tableHTML_output('prod')
           ))
  ),
  fluidRow(
    
    column(6,
           bs4Card(
             title = 'Derivatives',
             width = 12,
             status = "info",
             closable = FALSE,
             maximizable = TRUE,
             collapsible = TRUE,
             tableHTML_output('deriv')
           )),
    
    column(6,
           bs4Card(
             title = 'Debt Tables',
             width = 12,
             status = "info",
             closable = FALSE,
             maximizable = TRUE,
             collapsible = TRUE,
             DT::dataTableOutput('debtLink'),
             tableHTML_output('debt')
           ))
  ),
  fluidRow(
    
    column(6,
           bs4Card(
             title = 'Firm Transportation',
             width = 12,
             status = "info",
             closable = FALSE,
             maximizable = TRUE,
             collapsible = TRUE,
             tableHTML_output('firm')
           )),
    
    column(6,
           bs4Card(
             title = 'Reserves Information',
             width = 12,
             status = "info",
             closable = FALSE,
             maximizable = TRUE,
             collapsible = TRUE,
             DT::dataTableOutput('reportLink'),
             tableHTML_output('reserves')
           ))
  ),
  fluidRow(
    
    column(6,
           bs4Card(
             title = 'EBITDA tables',
             width = 12,
             status = "info",
             closable = FALSE,
             maximizable = TRUE,
             collapsible = TRUE,
             tableHTML_output('ebitda')
           ))
  )
)


flaring_tab <- bs4TabItem(
  tabName = 'flaring',
  br(),
  br(),
  
  fluidRow(
    bs4Card(title = 'Flaring Comparisons', width = 12,
            status = "info", 
            closable = FALSE,
            maximizable = TRUE, 
            collapsible = TRUE,
            selectizeInput('operatorFlare', 'Operator', choices = sort(unique(flaring$operator)), multiple = FALSE),
            billboarderOutput('opFlaring')%>% withSpinner(color="#0dc5c1")),
    bs4Card(title = 'Latest Month Flared Gas', width = 12,
            status = "info", 
            closable = FALSE,
            maximizable = TRUE, 
            collapsible = TRUE,
            billboarderOutput('flaring')%>% withSpinner(color="#0dc5c1")),
    bs4Card(title = 'Latest Month Flared Percent', width = 12,
            status = "info", 
            closable = FALSE,
            maximizable = TRUE, 
            collapsible = TRUE,
            billboarderOutput('flaring1')%>% withSpinner(color="#0dc5c1"))
  )
)
#' basic_cards_tab ----
basic_cards_tab <- bs4TabItem(
  tabName = "cards",
  br(),
  br(),
  #useShinyjs(),
  fluidRow(

    bs4Card(
      title = "Normalized EUR Map",
      width = 9,
      status = "info",
      closable = FALSE,
      maximizable = TRUE,
      collapsible = TRUE,
      div(class="inner",

          tags$head(
            # Include our custom CSS
            includeCSS("styles.css"),
            includeScript("gomap2.js")
          ),
                 leafletOutput("map", height = 750)%>% withSpinner(color="#0dc5c1"),

                 tags$div(id="cite",
                          'Data compiled for ', tags$em('Bakken Explorer'))

      )
    ),
    bs4Card(title = "Map Options", color = "blue", width = 3,
            status = "info",
            closable = FALSE,
            maximizable = FALSE,
            collapsible = TRUE,

            numericInput('perfSelect', 'Select Lateral Length, ft',
                         value = 9000),
            br(),
            numericInput('ppfSelect', 'Select Proppant Loading, lb/ft',
                         value = 1200),
            br(),
            selectizeInput('operatorSelect', 'Select Operator',
                           choices = sort(unique(wellData$operator)),
                           selected = NULL, multiple = FALSE),
            br(),
            pickerInput('operatorX', 'Select Offset Operator',
                        choices = sort(unique(wellData$operator)),
                        selected = NULL, options = list(
                          `actions-box` = TRUE,
                          size = 5,
                          `selected-text-format` = "count > 3"
                        ),multiple = TRUE)
        )
  ),
  fluidRow(
    bs4Card(
      title = "Operator Activity",
      closable = FALSE,
      width = 12,
      status = "info",
      solidHeader = FALSE,
      collapsible = TRUE,
      billboarderOutput('operatorActivity')%>% withSpinner(color="#0dc5c1")
    )
  )
)

#' card API
cards_api_tab <- bs4TabItem(
  tabName = "cardsAPI",
  br(),
  br(),
  #useShinyjs(),
  fluidRow(
    bs4Card(
      title = 'Productivity Comparison',
      closable = FALSE,
      width = 6,
      status = "info",
      solidHeader = FALSE,
      collapsible = TRUE,
      selectInput('fluidCompare', 'Select Fluid', choices = c('oil', 'gas', 'boe'), selected = 'oil', multiple = FALSE),
      pickerInput('operatorCompare', 'Select Operator(s)', choices = sort(unique(wellData$operator)),
                  selected = NULL, options = list(
                    `actions-box` = TRUE,
                    size = 5,
                    `selected-text-format` = "count > 3"
                  ),multiple = TRUE),
      pickerInput('yearCompare', 'Select Comparison Years', choices = sort(unique(wellData$fp.year)),
                  selected = NULL, options = list(
                    `actions-box` = TRUE,
                    size = 5,
                    `selected-text-format` = "count > 3"
                  ),multiple = TRUE),
      billboarderOutput('opProdCompare', height = '600px')%>% withSpinner(color="#0dc5c1")
    ),
    bs4Card(
      title = 'Parent-Child Risk',
      closable = FALSE,
      width = 6,
      status = "info",
      solidHeader = FALSE,
      collapsible = TRUE,
      pickerInput('countyParent', 'Select County', choices = sort(unique(wellData$county)),
                  selected = NULL, options = list(
                    `actions-box` = TRUE,
                    size = 5,
                    `selected-text-format` = "count > 3"
                  ),multiple = TRUE),
      selectizeInput('wellsInSection', 'Wells Per Section', choices = '4', selected = '4', multiple = FALSE),
      
      billboarderOutput('parentChild')%>% withSpinner(color="#0dc5c1")
    )
  ),
  fluidRow(
    bs4Card(
      title = 'Lateral Length Trend',closable = FALSE,
      width = 6,
      status = "info",
      solidHeader = FALSE,
      collapsible = TRUE,
      billboarderOutput('perfPlot1')%>% withSpinner(color="#0dc5c1")
    ),
    bs4Card(
      title = 'Proppant Loading Trend', closable = FALSE,
      width = 6,
      status = "info",
      solidHeader = FALSE,
      collapsible = TRUE,
      billboarderOutput('ppfPlot1')%>% withSpinner(color="#0dc5c1")
    ),
    bs4Card(
      title = 'Fluid Loading Trend', closable = FALSE,
      width = 6,
      status = "info",
      solidHeader = FALSE,
      collapsible = TRUE,
      billboarderOutput('fpfPlot1')%>% withSpinner(color="#0dc5c1")
    ),
    bs4Card(
      title = 'True Vertical Depth Trend',closable = FALSE,
      width = 6,
      status = "info",
      solidHeader = FALSE,
      collapsible = TRUE,
      billboarderOutput('tvdPlot1')%>% withSpinner(color="#0dc5c1")
    )
  )
)





tcapi_tab <- bs4TabItem(
  tabName = 'tcAPI',
  #useShinyjs(),
  br(),
  br(),
  fluidRow(

    bs4Card(title = 'Selection Parameters', closable = FALSE,
            width = 3,
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,

           pickerInput(inputId =  "operatorTC", choices = sort(unique(wellData$operator)),
                       label = "Operator", options = list(
                         `actions-box` = TRUE,
                         size = 5,
                         `selected-text-format` = "count > 3"
                       ),  selected = 'CONTINENTAL', multiple = TRUE),

           pickerInput(inputId =  "countyTC", choices = sort(unique(wellData$county)),
                       label = "County", options = list(
                         `actions-box` = TRUE,
                         size = 5,
                         `selected-text-format` = "count > 3"
                       ),  multiple = TRUE)
    ),

    bs4Card(
      title = 'Productivity Comparison',  closable = FALSE,
      width = 9,
      status = "info",
      solidHeader = FALSE,
      collapsible = TRUE,
      selectizeInput(inputId =  "fluid3", choices = c('oil', 'gas', 'boe'),
                     label = "Product Stream", selected = "oil", multiple = FALSE),
      plotlyOutput('spPlot')%>% withSpinner(color="#0dc5c1")
    )
  ),
  fluidRow(
    bs4Card(
      title = 'Spaghetti Plot', closable = FALSE,
      width = 9,
      status = "info",
      solidHeader = FALSE,
      collapsible = TRUE,
      selectizeInput(inputId =  "fluid2", choices = c('oil', 'gas', 'water'),
                     label = "Product Stream", selected = "oil", multiple = FALSE),
      plotlyOutput('spPlot1')%>% withSpinner(color="#0dc5c1"),
      textOutput('eurCalc')
    ),
    bs4Card(
      title = 'Type Curve Parameters', closable = FALSE,
      width = 3,
      status = "info",
      solidHeader = FALSE,
      collapsible = TRUE,
      selectizeInput('selectYr', 'Select Year', choices = 2019, multiple = TRUE),
      numericInput('perfSelect1', 'Normalized Lateral Length', value= 9000, min =  1),

      numericInput('wellLifeS', 'Well Life, years', value = 30, min = 5),


      numericInput('qiOilS', 'Oil IP Rate, bbl/d', value = 500, min = 0),

      numericInput('bOilS', 'Oil B-Factor',  value = 1, min = 0, max = 2),

      numericInput('DiOilS', 'Oil Initial Decline', value = 0.9, min = 0.201, max = 0.99999),

      numericInput('DfOilS', 'Oil Terminal Decline', value = 0.1, min = 0.01, max = 0.2),

      numericInput('curtailOilS', 'Oil choke period, months', value = 1, min = 0),

      numericInput('qiGasS', 'Gas IP Rate, mcf/d', value = 500, min = 0),

      numericInput('bGasS', 'Gas B-Factor',  value = 1, min = 0, max = 2),

      numericInput('DiGasS', 'Gas Initial Decline', value = 0.9, min = 0.201, max = 0.99999),

      numericInput('DfGasS', 'Gas Terminal Decline', value = 0.1, min = 0.01, max = 0.2),

      numericInput('curtailGasS', 'Gas choke period, months', value = 1, min = 0),

      numericInput('qiWaterS', 'Water IP Rate, bbl/d', value = 500, min = 0),

      numericInput('bWaterS', 'Water B-Factor',  value = 1, min = 0, max = 2),

      numericInput('DiWaterS', 'Water Initial Decline', value = 0.9, min = 0.201, max = 0.99999),

      numericInput('DfWaterS', 'Water Terminal Decline', value = 0.1, min = 0.01, max = 0.2),

      numericInput('curtailWaterS', 'Water choke period, months', value = 1, min = 0)
    )
  )
)

price_tab <- bs4TabItem(
  tabName = 'prices',
  br(),
  br(),
  fluidRow(
    bs4Card(title = 'Revenue Assumptions', width = 3,
            status = "info", 
            closable = FALSE,
            maximizable = TRUE, 
            collapsible = TRUE,
            prettyRadioButtons('priceType', 'Price Scenario', choices = c('Strip', 'Flat'), selected = 'Strip'),
            numericInput('wti', 'Flat WTI Assumption, $/BBL', value = 60, min = 1),
            numericInput('hh', 'Flat HH Assumption, $/MCF', value = 2, min = 0.1)
            
    ),
    bs4Card(title = 'Price Assumptions', width = 9,
            status = "info", 
            closable = FALSE,
            maximizable = TRUE, 
            collapsible = TRUE,
            billboarderOutput('stripPrice')%>% withSpinner(color="#0dc5c1")
    )
  )
  
)


pdp_tab <- bs4TabItem(
  tabName = "pdpVal",
  br(),
  br(),
  #useShinyjs(),
  tags$head(tags$style(css)),
  
  fluidRow(
    column(3,
      pickerInput(inputId =  "operator1", choices = sort(unique(wellData$operator)),
                  label = "Operator", options = list(
                    `actions-box` = TRUE,
                    size = 5,
                    `selected-text-format` = "count > 3"
                  ),  selected = 'CONTINENTAL', multiple = TRUE)),
    column(3,
      pickerInput(inputId =  "county1", choices = sort(unique(wellData$county)),
                  label = "County", options = list(
                    `actions-box` = TRUE,
                    size = 5,
                    `selected-text-format` = "count > 3"
                  ),  multiple = TRUE)
      )),
  fluidRow(
    bs4Card(title = 'PDP Profile',closable = FALSE,
            width = 9,
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            selectizeInput('prodValue', 'Component', choices = c('netOil', 'netSalesGas', 'netNGL', 'oilRevenue', 'gasRevenue', 'nglRevenue', 'revenue', 'opex', 'tax', 'nocf', 'capex', 'fcf'),
                           selected = 'netOil', multiple = FALSE),
            billboarderOutput('pdpStack')%>% withSpinner(color="#0dc5c1")
    ),
    bs4Card(title = 'PDP Inputs', closable = FALSE,
            width = 3,
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            numericInput('wi', 'Working Interest, %', value = 100, min = 1),
            numericInput('pdpDiscRate', 'Discount Rate', value = 10, min = 0, max = 30),
            awesomeRadio('pdpView', 'Table Date View', choices = c('Monthly', 'Quarterly', 'Annual'), selected = 'Monthly', status = "info"),
            br(),
            dateInput('pdpEffDate', 'Effective Date', value = max(prodData$date) %m+% months(1), min = today() %m+% years(-1)),
            bsButton('generatePDP', 'Calculate', style = 'primary', size = 'small'))

  ),
  fluidRow(
    bs4Card(title = 'Well Assumptions', closable = FALSE,
            width = 3,
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            numericInput('ppfSelect1', 'Proppant Loading, lb/ft', value = 1200, min = 500, max = 3000),
            
            numericInput('wellSpacing', 'Wells Per Unit', value = 6, min = 2, max = 10),
            
            prettyRadioButtons('econCutoff', 'Abandon at Economic Limit?', choices = c('Yes', 'No'), shape='curve'),
            
            numericInput('irrCutoff', 'IRR Cutoff, %', value = 20, min = 0),
            
            
            numericInput('nri', 'Net Revenue Interest, %', value = 80, min = 60, max = 100),
            
            numericInput('drillCapex', 'Drilling Capex, $/Lateral Ft', value = 400, min = 0),
            
            numericInput('compCapex', 'Completion + Equipment Capex, $/Lateral Ft', value = 350, min = 0),
            
            numericInput('pna', 'Plugging & Abandonment, $', value = 80000, min =1),
            
            numericInput('spudToProd', 'Spud to First Production (months)', value = 3, min = 1)
    ),
    bs4Card(title = 'Revenue Assumptions',  closable = FALSE,
            width = 3,
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            numericInput('oilDiff', 'Oil Basis, $/bbl', value = 5),
            
            numericInput('gasDiff', 'Gas Basis, $/mcf', value = 2),
            
            numericInput('btu', 'BTU Uplift', value = 1, min = 0, max = 3),
            
            numericInput('shrink', 'Gas Shrink (sales gas percentage of wellhead)', value = 75, min = 0,max = 100),
            
            numericInput('nglYield', 'NGL Yield Assumption, bbl/mmcf', value = 100, min = 0, max = 200),
            
            numericInput('nglPrice', 'NGL Price % of WTI', value = 10, min = 0, max = 100)
    ),
    bs4Card(title = 'Expense Assumptions',  closable = FALSE,
            width = 3,
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            numericInput('yr1Fixed', 'Monthly Fixed Expense, Year 1', value = 15000, min = 0),
            
            numericInput('yr2Fixed', 'Monthly Fixed Expense, Year 2', value = 7500, min = 0),
            
            numericInput('finalFixed', 'Monthly Fixed Expense, Final', value = 2500, min = 0),
            
            numericInput('varOilExp', 'Variable Oil Expense, $/bbl', value = 2, min = 0),
            
            numericInput('varGasExp', 'Variable Gas Expense, $/mcf', value = 0.75, min = 0),
            
            numericInput('varNGLExp', 'Variable NGL Expense, $/bbl', value = 1, min = 0),
            
            numericInput('varWaterExp', 'Variable Water Expense, $/bbl', value = 2, min = 0),
            numericInput('wrkExp', 'Monthly Workover Expense', value = 1000, min = 0)
    ),
    bs4Card(title = 'Tax Assumptions',  closable = FALSE,
            width = 3,
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            numericInput('sevOil', 'Oil Severance Tax, % of Revenue', value = 5, min = 0, max = 6),
            
            numericInput('sevGas', 'Gas Severance Tax, $/mcf', value = 0.0712, min = 0),
            
            numericInput('atx', 'Ad Valorem, % Revenue', value = 1, min = 0)
    )
    ),

  fluidRow(
    bs4Card(title = 'PDP Cash Flow Summary', closable = FALSE,
            width = 12,
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,

            textOutput('pdpPV'),

            DT::dataTableOutput('pdpValue')#, style = paste0("color:", semantic_palette[["blue"]], ";"))


    )
  )
)



pud_tab <- bs4TabItem(
  tabName = 'pudVal',
  br(),
  br(),
  fluidRow(
    column(
    6,
                  
    bs4Card(title = 'Inventory Forecast', closable = FALSE,
            width = 12,
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
        bsButton('calcInv', 'Calculate Inventory', style = 'primary', size = 'small'),
        DT::dataTableOutput('invTable'),
        h5('Inventory Calculations are for Selected Operator(s) and County(s) from PDP Valuation')),
    bs4Card(title = 'Well Risking',
            closable= FALSE,
            width = 12,
            status = 'info',
            solidHeader = FALSE,
            collapsible = TRUE,
            fluidRow(
              column(6,
                     numericInput('well1Risk', 'Parent Well Risk', value = 100, min = 0, max = 100)),
              column(6,
                     numericInput('well2Risk', 'Second Well Risk', value = 90, min = 0, max = 100)),
              column(6,
                     numericInput('well3Risk', 'Third Well Risk', value = 85, min = 0, max = 100)),
              column(6,
                     numericInput('well4Risk', 'Fourth Well Risk', value = 80, min = 0, max = 100)),
              column(6,
                     numericInput('well5Risk', 'Fifth Well Risk', value = 75, min = 0, max = 100)),
              column(6, 
                     numericInput('well6Risk', 'Sixth Well Risk', value = 70, min = 0, max = 100)),
              column(6,
                     numericInput('well7Risk', 'Seventh Well Risk', value = 65, min = 0, max = 100)),
              column(6,
                     numericInput('well8Risk', 'Eighth Well Risk', value = 60, min = 0, max = 100))
            )
            )
    ),
    bs4Card(title = 'Development Parameters',closable = FALSE,
            width = 6,
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            dateInput('pudStart', 'Drilling Start', value = max(prodData$date) %m+% months(-3), min = today() %m+% years(-1)),
            fluidRow(
              column(6,
                numericInput('yr1Dev', 'Year 1 Wells Drilled', value = 0, min = 0),
                textOutput('rem1')
                ),
              column(6,
                numericInput('yr2Dev', 'Year 2 Wells Drilled', value = 0, min = 0),
                textOutput('rem2'),
              ),
              column(6,
                numericInput('yr3Dev', 'Year 3 Wells Drilled', value = 0, min = 0),
                textOutput('rem3'),
              ),
              column(6,
                numericInput('yr4Dev', 'Year 4 Wells Drilled', value = 0, min = 0),
                textOutput('rem4'),
              ),
              column(6,
                numericInput('yr5Dev', 'Year 5 Wells Drilled', value = 0, min = 0),
                textOutput('rem5'),
              ),
              column(6,
                numericInput('yr6Dev', 'Year 6 Wells Drilled', value = 0, min = 0),
                textOutput('rem6'),
              ),
              column(6,
                numericInput('yr7Dev', 'Year 7 Wells Drilled', value = 0, min = 0),
                textOutput('rem7'),
              ),
              column(6,
                numericInput('yr8Dev', 'Year 8 Wells Drilled', value = 0, min = 0),
                textOutput('rem8'),
              ),
              column(6,
                numericInput('yr9Dev', 'Year 9 Wells Drilled', value = 0, min = 0),
                textOutput('rem9'),
              ),
              column(6,
                numericInput('yr10Dev', 'Year 10+ Wells Drilled Annually', value = 0, min = 0),
                textOutput('rem10')
              )
              ),
            bsButton('addPud', 'Add to Forecast', style = 'primary', size='small')
    )
  ),
  fluidRow(
    bs4Card(title = 'PUD Inputs', closable = FALSE,
            width = 3,
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            numericInput('wi2', 'Working Interest, %', value = 100, min = 1),
            numericInput('pudDiscRate', 'Discount Rate', value = 20, min = 0, max = 40),
            awesomeRadio('pudView', 'Table Date View', choices = c('Monthly', 'Quarterly', 'Annual'), selected = 'Monthly', status = "info"),
            bsButton('generatePUD', 'Calculate', style = 'primary', size = 'small')),
    bs4Card(title = 'Development Profile',closable = FALSE,
            width = 9,
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            selectizeInput('prodValue2', 'Component', choices = c('netOil', 'netSalesGas', 'netNGL', 'oilRevenue', 'gasRevenue', 'nglRevenue', 'revenue', 'opex', 'tax', 'nocf', 'capex', 'fcf'),
                           selected = 'netOil', multiple = FALSE),
            billboarderOutput('pudStack')%>% withSpinner(color="#0dc5c1")
    )
  ),
  fluidRow(
    bs4Card(title = 'PUD Cash Flow Summary', closable = FALSE,
            width = 12,
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            
            textOutput('pudPV'),
            
            DT::dataTableOutput('pudValue')#, style = paste0("color:", semantic_palette[["blue"]], ";"))
            
            
    )
  )
)

info_tab <- bs4TabItem(
  tabName = 'info',
  br(),
  br(),
  fluidRow(
    
    bs4Card(
      title = "Packages",
      bs4Carousel(
        id = "mycarousel",
        width = 12,
        bs4CarouselItem(
          active = FALSE,
          src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png"
        ),
        bs4CarouselItem(
          active = TRUE,
          src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png"
        ),
        bs4CarouselItem(
          active = FALSE,
          src = "bs4Dash.svg"
        ),
        bs4CarouselItem(
          active = FALSE,
          src = "tidyverse.png"
        ),
        bs4CarouselItem(
          active = FALSE,
          src = "dplyr.png"
        ),
        bs4CarouselItem(
          active = FALSE,
          src = "rvest.png"
        ),
        bs4CarouselItem(
          active = FALSE,
          src = "lubridate.png"
        ),
        bs4CarouselItem(
          active = FALSE,
          src = "echarts.png"
        )
        ,
        bs4CarouselItem(
          active = FALSE,
          src = "plotly.png"
        ),
        bs4CarouselItem(
          active = FALSE,
          src = "billboard.png"
        ),
        bs4CarouselItem(
          active = FALSE,
          src = "love.png"
        ),
        bs4CarouselItem(
          active = FALSE,
          src = "sf.gif"
        ),
        bs4CarouselItem(
          active = FALSE,
          src = 'openintro.png'
        ),
        bs4CarouselItem(
          active = FALSE,
          src = 'scales.png'
        ),
        bs4CarouselItem(
          active = FALSE,
          src = 'shinyalert.png'
        ),
        bs4CarouselItem(
          active = FALSE,
          src = 'purrr.png'
        ),
        bs4CarouselItem(
          active = FALSE,
          src = 'tableHTML.png'
        ),
        bs4CarouselItem(
          active = FALSE,
          src = 'stringr.png'
        )
        
      )
      
    ),
    
    
    bs4ListGroup(
      bs4ListGroupItem(
        type = "basic",
        (a("    shinyBS    ", href="https://ebailey78.github.io/shinyBS/index.html",target='_blank'))
      ),
      bs4ListGroupItem(
        type = "basic",
        (a("    shinyWidgets    ", href="https://github.com/dreamRs/shinyWidgets",target='_blank'))
      ),
      bs4ListGroupItem(
        type = "basic",
        (a("    shinycssloaders    ", href="https://github.com/daattali/shinycssloaders",target='_blank'))
      ),
      bs4ListGroupItem(
        type = "basic",
        (a('    quantmod    ', href = 'https://github.com/joshuaulrich/quantmod',target='_blank'))
      ),
      bs4ListGroupItem(
        type = "basic",
        (a('    DT    ', href = 'https://github.com/rstudio/DT',target='_blank'))
      ),
      bs4ListGroupItem(
        type = "basic",
        (a('    aRpsDCA    ', href = 'https://github.com/derrickturk/aRpsDCA',target='_blank'))
      ),
      bs4ListGroupItem(
        type = "basic",
        (a('    tidyRSS    ', href = 'https://github.com/RobertMyles/tidyRSS',target='_blank'))
      ),
      bs4ListGroupItem(
        type = "basic",
        (a('    leaflet    ', href = 'https://rstudio.github.io/leaflet/',target='_blank'))
      ),
      bs4ListGroupItem(
        type = "basic",
        (a('    leaflet.extras    ', href = 'https://bhaskarvk.github.io/leaflet.extras/',target='_blank'))
      ),
      
      bs4ListGroupItem(
        type = "basic",
        (a('    edgarWebR    ', href = 'https://mwaldstein.github.io/edgarWebR/',target='_blank'))
      ),
      bs4ListGroupItem(
        type = "basic",
        (a('    finreportr    ', href = 'https://github.com/sewardlee337/finreportr',target='_blank'))
        
      ),
      bs4ListGroupItem(
        type = "basic",
        (a(' akima ', href = 'https://github.com/cran/akima', target='_blank'))
        
      ),
      bs4ListGroupItem(
        type = "basic",
        (a(' zoo ', href = 'https://cran.r-project.org/web/packages/zoo/index.html', target ='_blank'))
        
      ),
      
      
      bs4ListGroupItem(
        type = "basic",
        
        (a('maps', href='https://cran.r-project.org/web/packages/maps/maps.pdf', target='_blank'))
      ),
      bs4ListGroupItem(
        type = "basic",
        (a('maptools', href='https://cran.r-project.org/web/packages/maptools/maptools.pdf', target = '_blank'))
      ),
      bs4ListGroupItem(
        type = "basic",
        (a('geosphere', href='http://r-forge.r-project.org/projects/geosphere/', target = '_blank'))
      ),
      bs4ListGroupItem(
        type = "basic",
        (a('tools', href='https://cran.r-project.org/web/packages/utile.tools/index.html', target = '_blank'))
      ),
      bs4ListGroupItem(
        type = "basic",
        (a('tigris', href='https://github.com/walkerke/tigris', target = '_blank'))
      ),
      bs4ListGroupItem(
        type = "basic",
        (a('RColorBrewer', href='https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf', target = '_blank'))
        
      ),
      bs4ListGroupItem(
        type = "basic",
        (a('sp', href='https://github.com/edzer/sp', target = '_blank'))
      ),
      bs4ListGroupItem(
        type = "basic",
        (a('rgdal', href = 'https://cran.r-project.org/web/packages/rgdal/index.html', target='_blank'))
      ),
      bs4ListGroupItem(
        type = "basic",
        (a(' kernlab ', href = 'https://cran.r-project.org/web/packages/kernlab/index.html', target ='_blank'))
        
      ),
      bs4ListGroupItem(
        type = "basic",
        (a(' caret ', href = 'https://topepo.github.io/caret/', target ='_blank'))
        
      ),
      bs4ListGroupItem(
        type = "basic",
        (a(' httr ', href = 'https://github.com/r-lib/httr', target ='_blank'))
      ),
      bs4ListGroupItem(
        type = "basic",
        (a(' xml2 ', href = 'https://github.com/r-lib/xml2', target ='_blank'))
        
      ),
      
      bs4ListGroupItem(
        type = "basic",
        (a(' jsonlite ', href = 'https://github.com/jeroen/jsonlite', target ='_blank'))
        
      )
      
      
    )
  )
  
)

source("global.R")
#library(shiny)
shiny::shinyApp(
  ui = bs4DashPage(
    sidebar_collapsed = TRUE,
    enable_preloader = TRUE,
    loading_duration = 3,
    controlbar_overlay = FALSE,
    navbar = bs4DashNavbar(
      status = "white",
      fixed = TRUE,
      prettyRadioButtons(
        inputId = "reservoirSelect",
        label = "Select Reservoir:", 
        choices = sort(unique(wellData$reservoir)),
        icon = icon("check"), 
        bigger = TRUE,
        status = "info",
        animation = "jelly"
      ),
      rightUi = tagList(
       
        bs4UserMenu(
          name = "BD", 
          status = "primary",
          src = "bd.jpg", 
          title = "bakkenExplorer",
          subtitle = "energyFinExplorer@gmail.com", 
          footer = p("2020", class = "text-center"),
          "Exposing the ease of shale play analytics."
        )
      )
    ),
    sidebar = bs4DashSidebar(
      expand_on_hover = TRUE,
      skin = "light",
      status = "primary",
      title = "Bakken Explorer",
      brandColor = "primary",
      url = "https://twitter.com/chiefB17832728",
      src = "bd.jpg",
      elevation = 3,
      opacity = 0.8,
      bs4SidebarMenu(
        id = "current_tab",
        flat = FALSE,
        compact = FALSE,
        child_indent = TRUE,
        bs4SidebarHeader(""),
        bs4SidebarMenuItem(
          "Home",
          tabName = "home",
          icon = "home"
        ),
        bs4SidebarMenuItem(
          "News",
          tabName = "news",
          icon = "newspaper"
        ),
        bs4SidebarMenuItem(
          "Financial Statements",
          tabName = "financials",
          icon = "usd"
        ),
        bs4SidebarMenuItem(
          "Flaring",
          tabName = "flaring",
          icon = "fire"
        ),
        bs4SidebarMenuItem(
          "Play Summary",
          tabName = "cards",
          icon = "map"
        ),
        bs4SidebarMenuItem(
          "Play Benchmarking",
          tabName = "cardsAPI",
          icon = "chart-bar"
        ),
        bs4SidebarMenuItem(
          "Type Curves",
          tabName = "tcAPI",
          icon = "chart-line"
        ),
        bs4SidebarMenuItem(
          "Pricing Model",
          tabName = 'prices',
          icon = "usd"
        ),
        bs4SidebarMenuItem(
          "PDP Valuation",
          tabName = 'pdpVal',
          icon = "table"
        ),
        bs4SidebarMenuItem(
          "Development",
          tabName = 'pudVal',
          icon = "oil-can"
        ),
        bs4SidebarMenuItem(
          "Info",
          tabName = "info",
          icon = "info"
        )
 
      )
    ),
    body = bs4DashBody(
      shinyjs::useShinyjs(),
      shinyalert::useShinyalert(),
      bs4TabItems(
        home_tab,
        news_tab,
        financials_tab,
        flaring_tab,
        basic_cards_tab,
        cards_api_tab,
        tcapi_tab,
        price_tab,
        pdp_tab,
        pud_tab,
        info_tab
        # social_cards_tab,
        # tab_cards_tab,
        # sortable_cards_tab,
        # statsboxes_tab,
        # boxes_tab,
        # value_boxes_tab,
        # colors_tab,
        # gallery_1_tab,
        # gallery_2_tab
      )
    ),
    controlbar = bs4DashControlbar(
      inputId = "controlbar",
      skin = "light",
      title = "Pending"
      
      
      
      
    ),
    footer = bs4DashFooter(
      fixed = TRUE,
      copyrights = a(
        href = "https://twitter.com/chiefB17832728", 
        target = "_blank", "@chiefB17832728"
      ),
      right_text = "2020"
    ),
    title = "Bakken Explorer"
  ),
  server = function(input, output, session) {
    
    
    values <- reactiveValues()
    
    latBinStart <- 24.116123
    longBinStart <- -121.496311
    
    
    values$latBinStart <- 24.116123
    values$longBinStart <-  -121.496311

    values$perfUplift <- perfUplift
    values$propUplift <- propUplift
    
    fn <- function(x, y){
      distHaversine(c(x, y), c(x, values$latBinStart))*3.28084
      
    }
    fn1 <- function(x, y){
      distHaversine(c(x, y), c(values$longBinStart, y))*3.28084
    }
    fnx <- function(y){
      distHaversine(c(values$longBinStart, y), c(values$longBinStart, values$latBinStart))*3.28084
      
    }
    
    
    output$news <- DT::renderDataTable({
     
      comp.name <- 'Oasis%20Petroleum'
      comp.name2 <- 'Whiting%20Petroleum'
      comp.name3 <- 'Bakken'
      comp.name4 <- 'Three%20Forks'
      
      url1 <- paste0("https://news.google.com/rss/search?q=", comp.name)
      #print(url1)
      tidy1 <- tidyRSS::tidyfeed(url1)
      url1 <- paste0("https://news.google.com/rss/search?q=", comp.name2)
      
      tidy2 <- tidyRSS::tidyfeed(url1)
      tidy2 <- tidy2 %>% filter(!feed_link %in% tidy1$feed_link)
      tidy1 <- rbind(tidy1, tidy2)
      
      
      url1 <- paste0("https://news.google.com/rss/search?q=", comp.name3)
      
      tidy2 <- tidyRSS::tidyfeed(url1)
      tidy2 <- tidy2 %>% filter(!feed_link %in% tidy1$feed_link)
      tidy1 <- rbind(tidy1, tidy2)
      
      url1 <- paste0("https://news.google.com/rss/search?q=", comp.name4)
      
      tidy2 <- tidyRSS::tidyfeed(url1)
      tidy2 <- tidy2 %>% filter(!feed_link %in% tidy1$feed_link)
      tidy1 <- rbind(tidy1, tidy2)
      
      tidy1$item_link <- paste0("<a target='_blank' href='",tidy1$item_link,"'>",tidy1$item_title,"</a>")
      tidy1 <- tidy1[,c('item_link', 'item_date_published')]
      tidy1$item_date_published <- as.Date(tidy1$item_date_published)
      tidy1 <- tidy1 %>% arrange(desc(item_date_published))
      names(tidy1) <- c('Article', 'Published Date')
      DT::datatable(tidy1, rownames = FALSE, escape = FALSE,extensions = c('Buttons', 'ColReorder', 'FixedHeader','KeyTable',  'Scroller'),
                    options = list(pageLength = 10,
                                   lengthMenu = c(5, 10, 15, 20)))
    })
    
    output$hcplot <- renderEcharts4r({
      ticker <- input$opTicker
      stock <- getSymbols(ticker, src='yahoo', auto.assign = FALSE, setSymbolLookup('stock'))
      df <- data.frame(Date = index(stock), coredata(stock))
      df <- df %>% filter(Date >= input$start_date)
      rm(stock)
      removeSymbols()
      names(df)[1:5] <- c('date', 'opening', 'high', 'low', 'closing')
      #print(names(df))
      df$date <- as.character(df$date)
      df %>%
        e_charts(date) %>%
        e_candle(opening, closing, low, high, name = input$opTicker) %>% 
        e_datazoom(type = "slider") %>% 
        e_title("Candlestick chart", "Quantmod data")%>%
        e_theme('auritus')%>%
        e_tooltip(trigger = "axis")
      
    })
    
    output$hcplot1 <- renderEcharts4r({
      tickers <- input$opTicker1
      tickers <- append('^GSPC', tickers)
      #print(tickers)
      i<-1
      df1 <- data.frame()
      while(i <= length(tickers)){
        stock <- getSymbols(tickers[i], src='yahoo', auto.assign = FALSE, setSymbolLookup('stock'))
        
        df <- data.frame(Date = index(stock), coredata(stock))
        df <- df %>% filter(Date >= input$start_date1)
        #print(head(df))
        df$percentChange <- df[,5]/df[1,5]-1
        df$ticker <- tickers[i]
        rm(stock)
        removeSymbols()
        names(df) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted', 'percentChange', 'ticker')
        df <- as.data.frame(df)
        df1 <- rbind(df1, df)
        i <- i +1
      }
      df1$ticker[df1$ticker == '^GSPC'] <- 'SP500'
      df1 <- as.data.frame(df1)
      df1$Date <- as.character(df1$Date)
      #print(head(df1))
      df1$percentChange <- df1$percentChange*100
      
      df1 %>% 
        group_by(ticker) %>% 
        e_charts(Date) %>% 
        e_line(percentChange) %>% 
        e_title("", "Percent Change") %>%
        #e_datazoom(type = "slider") %>% 
        e_theme('auritus')%>%  # theme
        e_legend(right = 0)%>%  # move legend to the bottom
        e_tooltip(trigger = "axis")
    })
    
    url1 <- function(x) {
      paste0('https://www.sec.gov', ((xml2::read_html(x) %>%
                                        rvest::html_nodes('table') %>% .[1] %>%
                                        rvest::html_nodes('a') %>% 
                                        rvest::html_attr('href')))[1])
    }
    
    observeEvent(input$loadFilings, {
      updateButton(session, 'loadFilings', label = 'Gathering...', style = 'danger')
      shinyjs::disable('loadFilings')
      filingList <- data.frame(edgarWebR::company_details(input$opTicker, type = '10-K', count = 8)) %>% filter(!grepl('A', filings.type))
      filingList1 <- data.frame(edgarWebR::company_details(input$opTicker, type = '10-Q', count = 24)) %>% filter(!grepl('A', filings.type))
      
      filingList <- rbind(filingList, filingList1) %>% arrange(desc(filings.filing_date))
      compInfo <- finreportr::CompanyInfo(input$opTicker)
      filingList$Company <- compInfo$company
      rm(filingList1)
      
      filingList$url1 <- lapply(filingList$filings.href,  url1)
      filingList$url1 <- gsub('/ix?doc=', '', filingList$url1, fixed=TRUE)
      values$check <- filingList
      filingList <- filingList[,c('Company', 'filings.filing_date', 'filings.type', 'url1')] %>% arrange(desc(filings.filing_date))
      filingList$quarter <- lubridate::quarter(filingList$filings.filing_date) - 1
      filingList$year <- lubridate::year(filingList$filings.filing_date)
      filingList$quarter[filingList$quarter == 0] <- 4
      filingList$year[filingList$quarter == 4] <- filingList$year[filingList$quarter == 4]-1
      filingList$period <- paste0('Q', filingList$quarter, filingList$year)
      updateSelectizeInput(session, 'Filing', choices = filingList$period)
      names(filingList)[1:4] <- c('Company', 'filingDate', 'type', 'url1')
      filingList <- filingList[,c('Company', 'period', 'filingDate', 'type', 'url1')]
      filingList <- data.frame(lapply(filingList, function(x){
        gsub("iXBRL", "", x)
      }))
      filingList <- data.frame(lapply(filingList, function(x){
        gsub("\\s+", "", x)
      }))
      
      filingList$type <- paste0('<a href="', filingList$url1, '" target="_blank">', filingList$type,'</a>')
      values$filingList <- filingList
      filingList <- filingList[,c('Company', 'period', 'filingDate', 'type')]
      names(filingList) <- c('Company', 'Filing Period', 'Filing Date', 'Report')
      filingList <- as.data.frame(filingList)
      values$filingList1 <- filingList
      updateButton(session, 'loadFilings', label = 'SUCCESS', style = 'success')
      updateButton(session, 'loadFilings', label = 'LOAD', style = 'primary')
      shinyjs::enable('loadFilings')
    })
    
    
    observeEvent(input$opTicker, {
      values$bs <- NULL
      values$is <- NULL
      values$cf <- NULL
      values$prod <- NULL
      values$deriv <- NULL
      values$debt <- NULL
      values$reserves <- NULL
      values$firm <- NULL
      values$ebitda <- NULL
      #updateMultiInput(session, 'operatorSelect1', selected=input$operatorSelect)
      #shinyjs::hide('hideTables')
    })
    
    observeEvent(input$Filing, {
      values$bs <- NULL
      values$is <- NULL
      values$cf <- NULL
      values$prod <- NULL
      values$deriv <- NULL
      values$debt <- NULL
      values$reserves <- NULL
      values$firm <- NULL
      values$ebitda <- NULL
      #shinyjs::hide('hideTables')
    })
    
    
    
    output$filingList <- DT::renderDataTable({
      if(is.null(values$filingList1)){
        NULL
      } else {
        filingList <- values$filingList1
        filingList <- subset(filingList, select = -c(Company))
        DT::datatable(filingList, escape = FALSE, rownames = FALSE, options = list(paging = FALSE, searching = FALSE))
      }
    })
    
    
    output$debtLink <- DT::renderDataTable({
      if(is.null(values$debt)){
        NULL
      } else {
        my_test <- values$filingList
        names(my_test)[2] <- 'date'
        my_test <- my_test %>% filter(date %in% input$Filing)
        check <- my_test
        check$comp.ticker <- input$opTicker
        nodes1 <- as.data.frame(read_html(my_test$url1) %>% html_nodes('a') %>% html_attr('href'))
        names(nodes1) <- 'url1'
        nodes2 <- as.data.frame(read_html(my_test$url1) %>% html_nodes('a') %>% html_text())
        names(nodes2) <- 'label'
        
        nodes1 <- cbind(nodes1, nodes2)
        
        nodes1 <- nodes1 %>% filter(grepl('Indenture', label)|
                                      grepl('Notes due', label)|
                                      grepl('Credit Agreement', label)) %>% filter(!duplicated(url1))
        if(nrow(nodes1) == 0|is.null(nodes1)){
          NULL
        } else {
          nodes1$url1 <- paste0("<a target='_blank' href='",nodes1$url1,"'>",'Link',"</a>")
        }
        
        if(nrow(nodes1) == 0|is.null(nodes1)){
          NULL
        } else {
          names(nodes1) <- c('', '')
          DT::datatable(nodes1,  rownames = FALSE, escape = FALSE,extensions = c('Buttons', 'ColReorder', 'FixedHeader','KeyTable',  'Scroller'),
                        options = list(pageLength = 5,
                                       lengthMenu = c(5, 10, 15)))
        }
      }
      
    })
    
    
    output$reportLink <- DT::renderDataTable({
      if(is.null(values$reserves)){
        NULL
      } else {
        my_test <- values$check
        my_test$quarter <- lubridate::quarter(my_test$filings.filing_date) - 1
        my_test$year <- lubridate::year(my_test$filings.filing_date)
        my_test$quarter[my_test$quarter == 0] <- 4
        my_test$year[my_test$quarter == 4] <- my_test$year[my_test$quarter == 4]-1
        my_test$period <- paste0('Q', my_test$quarter, my_test$year)
        my_test <- my_test %>% filter(period %in% input$Filing)
        names(my_test)[2] <- 'cik'
        
        names(my_test)[20] <- 'accession_number'
        check <- my_test
        check$comp.ticker <- input$opTicker
        nodes1 <- as.data.frame(read_html(my_test$url1) %>% html_nodes('a') %>% html_attr('href'))
        names(nodes1) <- 'url1'
        nodes2 <- as.data.frame(read_html(my_test$url1) %>% html_nodes('a') %>% html_text())
        names(nodes2) <- 'label'
        
        nodes1 <- cbind(nodes1, nodes2)
        nodes1 <- nodes1 %>% filter(grepl('99', url1)|grepl('99', label)|grepl('Ryder', label)|
                                      grepl('DeGol', label)|
                                      grepl('NSAI', label))
        
        nodes1 <- nodes1 %>% filter(grepl('99', label)|
                                      grepl('Ryder', label)|
                                      grepl('DeGol', label)|
                                      grepl('NSAI', label)|
                                      grepl('Sewell', label)|
                                      grepl('Cawley', label)|
                                      grepl('CGA', label)|
                                      grepl('LaRoche', label)|
                                      grepl('Schlum', label)|
                                      grepl('Reserves', label)) %>%
          filter(!grepl('Consent', label)) %>% filter(!grepl('http', url1))
        if(nrow(nodes1) == 0|is.null(nodes1)){
          NULL
        } else {
          accessionNo <- gsub("-", "", check$accession_number)
          nodes1 <- nodes1 %>% mutate(url1 = paste('https://www.sec.gov/Archives/edgar/data', as.numeric(check$cik), accessionNo, url1, sep='/'))
          nodes1$url1 <- paste0("<a target='_blank' href='",nodes1$url1,"'>",'Go to Report',"</a>")
        }
        
        
        if(nrow(nodes1) == 0){
          NULL
        } else {
          names(nodes1) <- c('', '')
          DT::datatable(nodes1,  rownames = FALSE, escape = FALSE, options=list(paging = TRUE, info = FALSE, ordering=FALSE))
        }
      }
      
    })
    observeEvent(input$loadTables, {
      updateButton(session, 'loadTables', label = 'Calculating...', style = 'danger')
      shinyjs::disable('loadTables')
      
      my_test <- values$filingList
      # print(head(my_test))
      names(my_test)[2] <- 'date'
      my_test <- my_test %>% filter(date %in% input$Filing)
      check <- my_test
      check$comp.ticker <- input$opTicker
      nodes <- read_html(my_test$url1) %>% html_nodes('table') #%>% #.[7] %>% html_table(fill=TRUE)
      id <- 1
      i <- 1
      my_tables <- NULL
      while(i <= length(bs1List)){
        print(bs1List[i])
        if(length(my_tables) == 0){
          my_tables <- bsScrape(nodes, bs1List[i], check)
        }
        i <- i+1
      }
      
      #print(my_tables[[1]]$list1)
      my_tables <- Filter(Negate(is.null), my_tables)
      tableSize <- NA
      i <- 1
      while(i <= length(my_tables)){
        counts <- my_tables[[i]]$rows
        tableSize <- rbind(tableSize, counts)
        i <- i+1
      }
      tableSize <- tableSize[!is.na(tableSize)]
      tableSize <- which(tableSize == max(tableSize))[1]
      test1 <- data.frame(my_tables[[tableSize]])
      test1 <- test1$list1
      #test1 <- gsub('', NA, test1)
      #test1 <- data.frame(my_tables[[1]])
      bs <- test1
      
      
      i <- 1
      my_tables <- NULL
      while(i <= length(bs2List)){
        print(bs2List[i])
        if(length(my_tables) == 0){
          my_tables <- bsScrape(nodes, bs2List[i], check)
        }
        i <- i+1
      }
      
      my_tables <- Filter(Negate(is.null), my_tables)
      tableSize <- NA
      i <- 1
      while(i <= length(my_tables)){
        counts <- my_tables[[i]]$rows
        tableSize <- rbind(tableSize, counts)
        i <- i+1
      }
      tableSize <- tableSize[!is.na(tableSize)]
      tableSize <- which(tableSize == max(tableSize))[1]
      test1 <- data.frame(my_tables[[tableSize]])
      test1 <- test1$list1
      #test1 <- gsub('', NA, test1)
      #test1 <- data.frame(my_tables[[1]])
      
      if(is.null(test1)){
        NULL
      } else {
        if((test1) == (bs)){
          NULL
        } else {
          
          bs <- paste0(bs, test1)
        }
      }
      
      
      values$bs <- bs
      
      
      i <- 1
      my_tables <- NULL
      while(i <= length(is1List)){
        print(is1List[i])
        if(length(my_tables) == 0){
          my_tables <- isScrape(nodes, is1List[i], check)
        }
        i <- i+1
      }
      
      my_tables <- Filter(Negate(is.null), my_tables)
      tableSize <- NA
      i <- 1
      while(i <= length(my_tables)){
        counts <- my_tables[[i]]$rows
        tableSize <- rbind(tableSize, counts)
        i <- i+1
      }
      tableSize <- tableSize[!is.na(tableSize)]
      tableSize <- which(tableSize == max(tableSize))[1]
      
      if((check$comp.ticker == 'AR' & substr(check$date, 2, 2) == 3)|(check$comp.ticker == 'AR' & substr(check$date, 2, 2) == 2)){
        test1 <- data.frame(my_tables[[1]])
        test1 <- test1$list1
      } else {
        
        test1 <- data.frame(my_tables[[tableSize]])
        test1 <- test1$list1
      }
      
      is <- test1
      
      
      i <- 1
      my_tables <- NULL
      while(i <= length(is2List)){
        print(is2List[i])
        if(length(my_tables) == 0){
          my_tables <- isScrape(nodes, is2List[i], check)
        }
        i <- i+1
      }
      
      my_tables <- Filter(Negate(is.null), my_tables)
      tableSize <- NA
      i <- 1
      while(i <= length(my_tables)){
        counts <- my_tables[[i]]$rows
        tableSize <- rbind(tableSize, counts)
        i <- i+1
      }
      tableSize <- tableSize[!is.na(tableSize)]
      tableSize <- which(tableSize == max(tableSize))[1]
      
      if((check$comp.ticker == 'AR' & substr(check$date, 2, 2) == 3)|(check$comp.ticker == 'AR' & substr(check$date, 2, 2) == 2)){
        test1 <- data.frame(my_tables[[1]])
        test1 <- test1$list1
      } else {
        
        test1 <- data.frame(my_tables[[tableSize]])
        test1 <- test1$list1
      }
      
      is2 <- test1
      
      i <- 1
      my_tables <- NULL
      while(i <= length(cf1List)){
        print(cf1List[i])
        if(length(my_tables) == 0){
          my_tables <- cfScrape(nodes, cf1List[i], check)
        }
        i <- i+1
      }
      
      #print(my_tables[[1]]$list1)
      my_tables <- Filter(Negate(is.null), my_tables)
      tableSize <- NA
      i <- 1
      while(i <= length(my_tables)){
        counts <- my_tables[[i]]$rows
        tableSize <- rbind(tableSize, counts)
        i <- i+1
      }
      tableSize <- tableSize[!is.na(tableSize)]
      tableSize <- which(tableSize == max(tableSize))[1]
      test1 <- data.frame(my_tables[[tableSize]])
      test1 <- test1$list1
      
      cf <- test1
      
      
      i <- 1
      my_tables <- NULL
      while(i <= length(cf2List)){
        print(cf2List[i])
        if(length(my_tables) == 0){
          my_tables <- cfScrape(nodes, cf2List[i], check)
        }
        i <- i+1
      }
      
      my_tables <- Filter(Negate(is.null), my_tables)
      tableSize <- NA
      i <- 1
      while(i <= length(my_tables)){
        counts <- my_tables[[i]]$rows
        tableSize <- rbind(tableSize, counts)
        i <- i+1
      }
      tableSize <- tableSize[!is.na(tableSize)]
      tableSize <- which(tableSize == max(tableSize))[1]
      test1 <- data.frame(my_tables[[tableSize]])
      test1 <- test1$list1
      
      if(is.null(test1)){
        NULL
      } else {
        if((test1) == (cf)){
          NULL
        } else {
          
          cf <- paste0(cf, test1)
        }
      }
      if(is.null(is2)){
        NULL
      }else{
        if((is2) == (is)){
          NULL
        } else {
          if((cf) == (is2)){
            NULL
          } else{
            is <- paste0(is, is2)
          }
          
        }
      }
      values$is <- is 
      
      i <- 1
      my_tables <- NULL
      while(i <= length(cf1List)){
        print(cf1List[i])
        if(length(my_tables) == 0){
          my_tables <- cfScrape(nodes, cf1List[i], check)
        }
        i <- i+1
      }
      
      #print(my_tables[[1]]$list1)
      my_tables <- Filter(Negate(is.null), my_tables)
      tableSize <- NA
      i <- 1
      while(i <= length(my_tables)){
        counts <- my_tables[[i]]$rows
        tableSize <- rbind(tableSize, counts)
        i <- i+1
      }
      tableSize <- tableSize[!is.na(tableSize)]
      tableSize <- which(tableSize == max(tableSize))[1]
      test1 <- data.frame(my_tables[[tableSize]])
      test1 <- test1$list1
      
      cf <- test1
      
      
      i <- 1
      my_tables <- NULL
      while(i <= length(cf2List)){
        print(cf2List[i])
        if(length(my_tables) == 0){
          my_tables <- cfScrape(nodes, cf2List[i], check)
        }
        i <- i+1
      }
      
      my_tables <- Filter(Negate(is.null), my_tables)
      tableSize <- NA
      i <- 1
      while(i <= length(my_tables)){
        counts <- my_tables[[i]]$rows
        tableSize <- rbind(tableSize, counts)
        i <- i+1
      }
      tableSize <- tableSize[!is.na(tableSize)]
      tableSize <- which(tableSize == max(tableSize))[1]
      test1 <- data.frame(my_tables[[tableSize]])
      test1 <- test1$list1
      
      if(is.null(test1)){
        NULL
      } else {
        if((test1) == (cf)){
          NULL
        } else {
          
          cf <- paste0(cf, test1)
        }
      }
      values$cf <- cf
      
      
      i <- 1
      my_tables <- NULL
      while(i <= length(prodList)){
        print(prodList[i])
        if(length(my_tables) == 0){
          my_tables <- prodScrape(nodes, prodList[i], check)
        }
        i <- i+1
      }
      
      if(length(my_tables) == 0){
        values$prod <- NULL
      } else {
        #print(my_tables[[1]]$list1)
        my_tables <- Filter(Negate(is.null), my_tables)
        tableSize <- NA
        i <- 1
        while(i <= length(my_tables)){
          counts <- my_tables[[i]]$rows
          tableSize <- rbind(tableSize, counts)
          i <- i+1
        }
        
        un <- unlist(my_tables)
        res.list <- Map(`[`, my_tables, relist(!duplicated(un), skeleton = my_tables))
        rm(un)
        
        my_tables <- res.list
        
        i <- 1
        test1 <- NULL
        
        while(i <= length(my_tables)){
          if(ncol(my_tables[[i]]) > 0){
            my_tables[[i]] <- my_tables[[i]]$list1
            test1 <- rbind(test1, my_tables[[i]])
          } else {
            my_tables[[i]] <- NULL
          }
          i <- i + 1
        }
        
        prod <- test1
        values$prod <- prod
      }
      
      
      
      
      i <- 1
      my_tables <- NULL
      while(i <= length(derivList)){
        print(derivList[i])
        if(length(my_tables) == 0){
          my_tables <- derivScrape(nodes, derivList[i], check)
        } else {
          my_tables1 <- derivScrape(nodes, derivList[i], check)
          my_tables <- rbind(my_tables, my_tables1)
        }
        i <- i+1
      }
      
      my_tables <- Filter(Negate(is.null), my_tables)
      
      un <- unlist(my_tables)
      res.list <- Map(`[`, my_tables, relist(!duplicated(un), skeleton = my_tables))
      rm(un)
      
      my_tables <- res.list
      
      i <- 1
      test1 <- NULL
      
      while(i <= length(my_tables)){
        if(ncol(my_tables[[i]]) > 0){
          my_tables[[i]] <- my_tables[[i]]$list1
          test1 <- rbind(test1, my_tables[[i]])
        } else {
          my_tables[[i]] <- NULL
        }
        i <- i + 1
      }
      values$deriv <- test1
      
      
      
      
      i <- 1
      my_tables <- NULL
      while(i <= length(debtList)){
        print(debtList[i])
        if(length(my_tables) == 0){
          my_tables <- debtScrape(nodes, debtList[i], check)
        } else {
          my_tables1 <- debtScrape(nodes, debtList[i], check)
          my_tables <- rbind(my_tables, my_tables1)
        }
        i <- i+1
      }
      
      my_tables <- Filter(Negate(is.null), my_tables)
      
      un <- unlist(my_tables)
      res.list <- Map(`[`, my_tables, relist(!duplicated(un), skeleton = my_tables))
      rm(un)
      
      my_tables <- res.list
      
      i <- 1
      test1 <- NULL
      
      while(i <= length(my_tables)){
        if(ncol(my_tables[[i]]) > 0){
          my_tables[[i]] <- my_tables[[i]]$list1
          test1 <- rbind(test1, my_tables[[i]])
        } else {
          my_tables[[i]] <- NULL
        }
        i <- i + 1
      }
      
      values$debt <- test1
      
      i <- 1
      my_tables <- NULL
      while(i <= length(reserveList)){
        print(reserveList[i])
        if(length(my_tables) == 0){
          my_tables <- reserveScrape(nodes, reserveList[i], check)
        } else {
          my_tables1 <- reserveScrape(nodes, reserveList[i], check)
          my_tables <- rbind(my_tables, my_tables1)
        }
        i <- i+1
      }
      
      my_tables <- Filter(Negate(is.null), my_tables)
      
      un <- unlist(my_tables)
      res.list <- Map(`[`, my_tables, relist(!duplicated(un), skeleton = my_tables))
      rm(un)
      
      my_tables <- res.list
      
      i <- 1
      test1 <- NULL
      
      while(i <= length(my_tables)){
        if(ncol(my_tables[[i]]) > 0){
          my_tables[[i]] <- my_tables[[i]]$list1
          test1 <- rbind(test1, my_tables[[i]])
        } else {
          my_tables[[i]] <- NULL
        }
        i <- i + 1
      }
      
      
      
      values$reserves <- test1
      
      
      i <- 1
      my_tables <- NULL
      while(i <= length(firmList)){
        print(firmList[i])
        if(length(my_tables) == 0){
          my_tables <- firmScrape(nodes, firmList[i], check)
        } else {
          my_tables1 <- firmScrape(nodes, firmList[i], check)
          my_tables <- rbind(my_tables, my_tables1)
        }
        i <- i+1
      }
      
      my_tables <- Filter(Negate(is.null), my_tables)
      
      un <- unlist(my_tables)
      res.list <- Map(`[`, my_tables, relist(!duplicated(un), skeleton = my_tables))
      rm(un)
      
      
      my_tables <- res.list
      
      
      i <- 1
      test1 <- NULL
      
      while(i <= length(my_tables)){
        if(ncol(my_tables[[i]]) > 0){
          my_tables[[i]] <- my_tables[[i]]$list1
          test1 <- rbind(test1, my_tables[[i]])
        } else {
          my_tables[[i]] <- NULL
        }
        i <- i + 1
      }
      #test1 <- dplyr::bind_rows(my_tables)
      values$firm <- test1
      
      
      
      i <- 1
      my_tables <- NULL
      while(i <= length(ebitdaList)){
        print(ebitdaList[i])
        if(length(my_tables) == 0){
          my_tables <- ebitdaScrape(nodes, ebitdaList[i], check)
        } else {
          my_tables1 <- ebitdaScrape(nodes, ebitdaList[i], check)
          my_tables <- rbind(my_tables, my_tables1)
        }
        i <- i+1
      }
      
      my_tables <- Filter(Negate(is.null), my_tables)
      
      un <- unlist(my_tables)
      res.list <- Map(`[`, my_tables, relist(!duplicated(un), skeleton = my_tables))
      rm(un)
      
      my_tables <- res.list
      
      i <- 1
      test1 <- NULL
      
      while(i <= length(my_tables)){
        if(ncol(my_tables[[i]]) > 0){
          my_tables[[i]] <- my_tables[[i]]$list1
          test1 <- rbind(test1, my_tables[[i]])
        } else {
          my_tables[[i]] <- NULL
        }
        i <- i + 1
      }
      
      values$ebitda <- test1
      
      shinyjs::enable('loadTables')
      shinyjs::show('hideTables')
      #updateButton(session, 'loadTables', label = 'SUCCESS!', style = 'success')
      updateButton(session, 'loadTables', label = 'LOAD', style = 'primary')
    })
    output$bs <- render_tableHTML({
      if(is.null(values$bs)){
        h3('Not Available')
      } else {
        
        bs <- values$bs
        # labels1 <- bs[1,]
        # labels1[1,] <- ''
        # names(bs) <- labels1
        # bs[is.na(bs)] <- ''
        HTML(bs)
        #DT::datatable(bs,  rownames = FALSE, options=list(dom = 'B', paging = FALSE, buttons = c('copy', 'csv', 'excel'), info = FALSE, ordering=FALSE))
      }
    })
    
    output$is <- render_tableHTML({
      if(is.null(values$is)){
        h3('Not Available')
      } else {
        is <- values$is
        # labels1 <- is[1,]
        # labels1[1,] <- ''
        # names(is) <- labels1
        # is[is.na(is)] <- ''
        HTML(is)
        #DT::datatable(is, rownames = FALSE, options=list(dom = 'B', paging = FALSE, buttons = c('copy', 'csv', 'excel'), info = FALSE, ordering=FALSE))
      }
    })
    output$cf <- render_tableHTML({
      if(is.null(values$cf)){
        h3('Not Available')
      } else {
        cf <- values$cf
        # labels1 <- cf[1,]
        # labels1[1,] <- ''
        # names(cf) <- labels1
        # cf[is.na(cf)] <- ''
        HTML(cf)
        #DT::datatable(cf, rownames = FALSE, options=list(dom = 'B', paging = FALSE, buttons = c('copy', 'csv', 'excel'), info = FALSE, ordering=FALSE))
      }
    })
    output$prod <- render_tableHTML({
      if(is.null(values$prod)){
        h3('Not Available')
      } else {
        cf <- values$prod
        #labels1 <- cf[1,]
        #labels1[1,] <- ''
        #names(cf) <- labels1
        #cf[is.na(cf)] <- ''
        #tableHTML(cf)
        HTML(cf)
        #DT::datatable(cf, rownames = FALSE, options=list(dom = 'B', paging = FALSE, buttons = c('copy', 'csv', 'excel'), info = FALSE, ordering=FALSE))
      }
    })
    output$deriv <- render_tableHTML({
      if(is.null(values$deriv)){
        h3('Not Available')
      } else {
        cf <- values$deriv
        #labels1 <- cf[1,]
        #labels1[1,] <- ''
        #names(cf) <- labels1
        #cf[is.na(cf)] <- ''
        #tableHTML(cf)
        HTML(cf)
        #DT::datatable(cf, rownames = FALSE, options=list(dom = 'B', paging = FALSE, buttons = c('copy', 'csv', 'excel'), info = FALSE, ordering=FALSE))
      }
    })
    output$debt <- render_tableHTML({
      if(is.null(values$debt)){
        h3('Not Available')
      } else {
        cf <- values$debt
        #labels1 <- cf[1,]
        #labels1[1,] <- ''
        #names(cf) <- labels1
        #cf[is.na(cf)] <- ''
        #tableHTML(cf)
        HTML(cf)
        #DT::datatable(cf, rownames = FALSE, options=list(dom = 'B', paging = FALSE, buttons = c('copy', 'csv', 'excel'), info = FALSE, ordering=FALSE))
      }
    })
    output$firm <- render_tableHTML({
      if(is.null(values$firm)){
        h3('Not Available')
      } else {
        cf <- values$firm
        #labels1 <- cf[1,]
        #labels1[1,] <- ''
        #names(cf) <- labels1
        #cf[is.na(cf)] <- ''
        #tableHTML(cf)
        HTML(cf)
        #DT::datatable(cf, rownames = FALSE, options=list(dom = 'B', paging = FALSE, buttons = c('copy', 'csv', 'excel'), info = FALSE, ordering=FALSE))
      }
    })
    output$reserves <- render_tableHTML({
      if(is.null(values$reserves)){
        h3('Not Available')
      } else {
        cf <- values$reserves
        #labels1 <- cf[1,]
        #labels1[1,] <- ''
        #names(cf) <- labels1
        #cf[is.na(cf)] <- ''
        #tableHTML(cf)
        HTML(cf)
        #DT::datatable(cf, rownames = FALSE, options=list(dom = 'B', paging = FALSE, buttons = c('copy', 'csv', 'excel'), info = FALSE, ordering=FALSE))
      }
    })
    output$ebitda <- render_tableHTML({
      if(is.null(values$ebitda)){
        h3('Not Available')
      } else {
        cf <- values$ebitda
        #labels1 <- cf[1,]
        #labels1[1,] <- ''
        #names(cf) <- labels1
        #cf[is.na(cf)] <- ''
        #tableHTML(cf)
        HTML(cf)
        #DT::datatable(cf, rownames = FALSE, options=list(dom = 'B', paging = FALSE, buttons = c('copy', 'csv', 'excel'), info = FALSE, ordering=FALSE))
      }
    })
    
    output$opFlaring <- renderBillboarder({
      df <- flaring %>% filter(operator %in% input$operatorFlare)
      #df$DATE <- as.character(df$DATE)
      
    
        billboarder(data = df) %>%
          bb_linechart(
            mapping = bbaes(x=date, y=flared), type = 'line', width = 2
          ) %>%
          bb_linechart(
            mapping = bbaes(x=date, y=percentFlared), type = 'line', width = 2
          )  %>% 
          bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE)) %>% 
          bb_data(axes = list(
            "flared" = "y",
            "percentFlared" = "y2"
          )) %>% 
          bb_colors_manual(
            WTI = 'green',
            HH = 'red'
          )%>%
          bb_axis(
            y2 = list(show = TRUE, label = "Percent of Gas Flared"),
            y = list(label = "Total Flared Volumes, mcf")
          )%>%
          bb_labs(title = 'Flaring by Operator',
                  caption = 'Data source: North Dakota State Government (https://www.dmr.nd.gov/oilgas/stats/statisticsvw.asp)')
      
      
      
    })
    
    output$flaring <- renderBillboarder({
        maxMonth <- month(max(flaring$date))
        maxYear <- year(max(flaring$date))
        df <- flaring %>% filter(month(date) == maxMonth) %>% filter(year(date) == maxYear) %>%
          arrange(desc(flared)) %>% mutate(flared = flared/1000)
        df <- df[1:15,]
        billboarder() %>%
          bb_barchart(data = df, mapping = bbaes(operator, flared), rotated = FALSE)%>%
          bb_y_grid(show = TRUE) %>%
          bb_y_axis(tick = list(format = suffix(" mmcf")),
                    label = list(text = "Total Gas Flared", position = "outer-top")) %>%
          bb_colors_manual(
            flared = 'red'
          ) %>%
          #bb_x_axis(tick = list(rotate = 75)) %>%
          bb_legend(show = FALSE) %>%
          bb_labs(title = "Flared Volume Comparison",
                  caption = 'Data source: North Dakota State Government (https://www.dmr.nd.gov/oilgas/stats/statisticsvw.asp)')
      
      
    })
    
    output$flaring1 <- renderBillboarder({
      maxMonth <- month(max(flaring$date))
      maxYear <- year(max(flaring$date))
      df <- flaring %>% filter(month(date) == maxMonth) %>% filter(year(date) == maxYear) %>%
        arrange(desc(flared)) #%>% top_n(10) %>% arrange(desc(percentFlared))
      df <- df[1:15,]
      df <- df %>% arrange(desc(percentFlared))
      billboarder() %>%
        bb_barchart(data = df, mapping = bbaes(operator, percentFlared), rotated = FALSE)%>%
        bb_y_grid(show = TRUE) %>%
        bb_colors_manual(
          percentFlared = 'red'
        ) %>%
        bb_y_axis(tick = list(format = suffix(" %")),
                  label = list(text = "Percent Total Gas Flared", position = "outer-top")) %>%
        bb_legend(show = FALSE) %>%
        bb_labs(title = "Percent Flared Comparison",
                caption = 'Data source: North Dakota State Government (https://www.dmr.nd.gov/oilgas/stats/statisticsvw.asp)')
      
      
    })

    wellData1 <- reactive({
      wellData %>% filter(reservoir %in% input$reservoirSelect)
    })

    observeEvent(input$reservoirSelect, {
      #wellData4 <- wellData %>% filter(reservoir %in% input$reservoirSelect)

      updateSelectizeInput(session, 'operatorSelect', 'Select Operator', choices = sort(unique(wellData1()$operator)))

    })

    observeEvent(input$operatorSelect, {
      wellData4 <- wellData1() %>% filter(operator != input$operatorSelect)
      updatePickerInput(session, 'operatorX', 'Select Offset Operator', choices = sort(unique(wellData4$operator)))

    })

    capexValues <- reactive({
      data.frame(
        Component = c( 'perfSelect', 'ppfSelect', 'perfSelect1', 'ppfSelect1', 'wellSpacing',
                       'drillCapex', 'compCapex', 'spudToProd', 'pna'),
        Value = c(input$perfSelect, input$ppfSelect, input$perfSelect1, input$ppfSelect1, input$wellSpacing,
                  input$drillCapex, input$compCapex, input$spudToProd, input$pna),
        stringsAsFactors = FALSE) %>% spread(Component, Value)



    })

    declineValues <- reactive({
      data.frame(
        Component = c( 'wellLifeS',
                       'qiOilS', 'bOilS', 'DiOilS', 'DfOilS', 'curtailOilS',
                       'qiGasS', 'bGasS', 'DiGasS', 'DfGasS', 'curtailGasS',
                       'qiWaterS', 'bWaterS', 'DiWaterS', 'DfWaterS', 'curtailWaterS',
                       'well1Risk', 'well2Risk', 'well3Risk', 'well4Risk', 'well5Risk',
                       'well6Risk', 'well7Risk', 'well8Risk'),
        Value = c(input$wellLifeS,
                  input$qiOilS, input$bOilS, input$DiOilS, input$DfOilS, input$curtailOilS,
                  input$qiGasS, input$bGasS, input$DiGasS, input$DfGasS, input$curtailGasS,
                  input$qiWaterS, input$bWaterS, input$DiWaterS, input$DfWaterS, input$curtailWaterS,
                  input$well1Risk, input$well2Risk, input$well3Risk, input$well4Risk,input$well5Risk,
                  input$well6Risk, input$well7Risk, input$well8Risk),
        stringsAsFactors = FALSE) %>% spread(Component, Value)



    })


    priceValues <- reactive({
      data.frame(
        Component = c( 'oilDiff', 'gasDiff', 'btu', 'shrink',
                       'nglYield', 'nglPrice'),
        Value = c(input$oilDiff, input$gasDiff, input$btu, input$shrink,
                  input$nglYield, input$nglPrice),
        stringsAsFactors = FALSE) %>% spread(Component, Value)
    })

    expenseValues <- reactive({
      data.frame(
        Component = c('wi', 'nri', 'yr1Fixed', 'yr2Fixed', 'finalFixed', 'varOilExp',
                      'varGasExp', 'varNGLExp', 'varWaterExp',
                      'wrkExp', 'sevOil', 'sevGas', 'atx', 'irrCutoff', 'pdpDiscRate', 'wi2', 'pudDiscRate'),
        Value = c(input$wi, input$nri, input$yr1Fixed, input$yr2Fixed, input$finalFixed, input$varOilExp,
                  input$varGasExp, input$varNGLExp, input$varWaterExp,
                  input$wrkExp, input$sevOil, input$sevGas, input$atx, input$irrCutoff, input$pdpDiscRate, input$wi2, input$pudDiscRate),
        stringsAsFactors = FALSE) %>% spread(Component, Value)
    })

    
    
    gasFrac1 <- reactive({
      gasFrac %>% filter(reservoir %in% input$reservoirSelect)
    })

    observe( {
      if(is.null(input$reservoirSelect)){
        NULL
      } else {
        
        values$dd2 <- NULL


        df4 <- gasFrac1()# %>% filter(reservoir %in% input$reservoirSelect)


        df4 <-df4[,c('avLong', 'avLat', 'risk', 'twnRngLocation')]
        names(df4) <- c('longitude', 'latitude', 'risk', 'twnRngLocation')


        df5 <- df4 %>% group_by(twnRngLocation) %>% summarise(latitude = mean(latitude), longitude = mean(longitude))

        df51 <- df5 %>% mutate(latitude = latitude - 0.5, longitude = longitude - 0.5)
        df52 <- df5 %>% mutate(latitude = latitude + 0.5, longitude = longitude - 0.5)
        df53 <- df5 %>% mutate(latitude = latitude - 0.5, longitude = longitude + 0.5)
        df54 <- df5 %>% mutate(latitude = latitude + 0.5, longitude = longitude + 0.5)

        df51 <- rbind(df51, df52, df53, df54)

        k <- unique(df51$latitude)


        list1 <- mapply(fnx, k)
        list1 <- as.data.frame(list1)
        names(list1) <- 'distLat'
        list1$latitude <- k
        df51 <- merge(df51, list1, by='latitude', all.x=TRUE)

        df51 <- df51 %>%  mutate(distLong = pmap_dbl(list(x = longitude, y = latitude),fn1))
        df51 <- as.data.frame(df51)


        df51 <- df51 %>% mutate(twnRngLocation = paste(round(distLat/(5280*6), digits=0),',', (round(distLong/(5280*6),digits=0)),sep='')) %>%
          group_by(twnRngLocation) %>% summarise(latitude = mean(latitude), longitude = mean(longitude)) %>% filter(!twnRngLocation %in% df5$twnRngLocation)

        df5 <- df51
        df5$risk <- -0.1
        df5 <- df5[,c('longitude', 'latitude', 'risk')]


        df4 <- rbind(df4[,c('longitude', 'latitude', 'risk')], df5)
        df5 <- df4[1:4,]
        df5$latitude[1:2] <- max(df4$latitude)+.1
        df5$latitude[3:4] <- min(df4$latitude)-.1
        df5$longitude[2:3] <- max(df4$longitude)+.1
        df5$longitude[c(1,4)] <- min(df4$longitude)-.1
        df5$risk <- -0.1
        df4 <- rbind(df4, df5)



        df4$perf <- as.numeric(capexValues()$perfSelect)
        df4$logPPF <- log(as.numeric(capexValues()$ppfSelect))

        df4$EUR <- exp(predict(propUplift, df4))
        df4$EUR <- df4$EUR*df4$risk
        df4$perfRisk <- perfUplift$coefficients[1] + perfUplift$coefficients[2]*df4$perf + perfUplift$coefficients[3]*(df4$perf**2)
        df4$z <- df4$EUR*df4$perfRisk/1000

        values$df4 <- df4

        df <- df4
        df <- df %>% arrange(longitude, latitude)
        d2d = interp(df$longitude, df$latitude, df$z)
        contour(d2d$x, d2d$y, d2d$z)

        lines = contourLines(x=d2d$x, y=d2d$y, z=d2d$z, nlevels=8)

        d1 <- sapply(1:length(lines),function(i) Polygon(as.matrix(cbind(lines[[i]]$x,lines[[i]]$y))))
        d2 <- sapply(1:length(lines), function(i) Polygons(list(d1[[i]]),i))

        poly_data <- data.frame(Value = sapply(1:length(lines),function(i) lines[[i]]$level))

        dd2 <- SpatialPolygonsDataFrame(SpatialPolygons(d2),data=poly_data)
        proj4string(dd2) <- CRS("+proj=longlat +datum=WGS84")

        df <- wellData1()
        LONGITUDE1 <- mean(df$avLong)
        LATITUDE1 <- mean(df$avLat)
        values$LONGITUDE1 <- LONGITUDE1
        values$LATITUDE1 <- LATITUDE1


        factpal2 = colorFactor(rev(brewer.pal(n=11, name='Spectral')), dd2$Value)

        values$dd2 <- dd2
        values$factpal2 <- factpal2



        values$map <- leaflet() %>%
          addTiles(group = "OSM (default)") %>%
          addProviderTiles("Esri.WorldTopoMap", group = "ESRI") %>%
          addProviderTiles("Stamen.Toner", group = "Toner") %>%
          addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
          addLayersControl(baseGroups = c("OSM (default)", "ESRI", "Toner", "Toner Lite"),
                           options = layersControlOptions(collapsed = FALSE)) %>%

          addPolygons(
            data = dd2,
            stroke=FALSE, fillOpacity = 0.8, smoothFactor = 0.5,
            color='grey',
            fillColor = ~factpal2(Value),
            layerId=dd2@data$Value) %>%
          leaflet::addLegend(pal=factpal2, values=dd2$Value, opacity = 0.7, title='Normalized EUR per Well (20:1) MBOE', position = 'bottomright') %>%
          setView(lng = mean(LONGITUDE1),
                  lat = mean(LATITUDE1), zoom = 8)%>%
          addPolygons(data = twnRng$geometry, weight = 0.75, color = 'gray', label = paste0(twnRng$TWPTEXT, twnRng$RNGTEXT)) %>%

          addPolygons(data = countyLines, weight = 1, opacity = 0.5, fill = FALSE, color = 'black',popup = countyLines@data$NAME) %>%

          addDrawToolbar(
            targetGroup='Selected',
            polylineOptions=FALSE,
            markerOptions = FALSE,
            polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'white'
                                                                              ,weight = 3)),
            rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                                  ,color = 'white'
                                                                                  ,weight = 3)),
            circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'white'
                                                                              ,weight = 3)),
            editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) 
        
        
      }

    })


    output$map <- renderLeaflet({
      shinyalert::shinyalert(title = 'Loading', text = 'Generaring Map', 
                             closeOnEsc = FALSE, showCancelButton = FALSE, showConfirmButton = FALSE, timer = 5000)
      values$map
    })

    observeEvent(input$operatorSelect, {
      if(is.null(values$map)){
        NULL
      } else {
        if(is.null(input$operatorX)){
          dd2 <- values$dd2
          factpal2 <- values$factpal2
          wellData5 <- wellData1() %>% filter(operator %in% input$operatorSelect)
          values$wellData <- wellData5
          #print(head(sectionLocs1()))
          proxy <- leafletProxy("map")
          proxy %>% clearShapes() %>%
            addPolygons(
              data = dd2,
              stroke=FALSE, fillOpacity = 0.8, smoothFactor = 0.5,
              color='grey',
              fillColor = ~factpal2(Value),
              layerId=dd2@data$Value) %>%
            addPolygons(data = countyLines, weight = 1,  fill = FALSE, color = 'black',label = countyLines@data$NAME) %>%
            addPolygons(data = twnRng$geometry, weight = 0.75, color = 'gray', label = paste0(twnRng$TWPTEXT, twnRng$RNGTEXT)) %>%
            addPolygons(data = sectionLocs1()$geometry, color = 'black', weight = 0.5) %>%
            addPolygons(data = wellData5$geom)
        } else {
          dd2 <- values$dd2
          factpal2 <- values$factpal2
          wellData5 <- wellData1() %>% filter(operator %in% input$operatorSelect)
          wellData6 <- wellData1() %>% filter(operator %in% input$operatorX)
          values$wellData <- wellData5
          proxy <- leafletProxy("map")
          proxy %>% clearShapes() %>%
            addPolygons(
              data = dd2,
              stroke=FALSE, fillOpacity = 0.8, smoothFactor = 0.5,
              color='grey',
              fillColor = ~factpal2(Value),
              layerId=dd2@data$Value) %>%
            addPolygons(data = countyLines, weight = 1, opacity = 0.5, fill = FALSE, color = 'black', label = countyLines@data$NAME) %>%
            addPolygons(data = twnRng$geometry, weight = 0.75, color = 'gray', label = paste0(twnRng$TWPTEXT, twnRng$RNGTEXT)) %>%
            addPolygons(data = sectionLocs1()$geometry, color = 'black', weight = 0.5) %>%
            addPolygons(data = wellData5$geom) %>%
            addPolygons(data = wellData6$geom)
        }
      }
    })

    sectionLocs1 <- reactive({
      if(nrow(sectionLocs%>% filter(operator == input$operatorSelect))==0){
        sectionLocs[1,]
      } else {
        sectionLocs %>% filter(operator == input$operatorSelect)# %>% st_transform(geometry, '+proj=lcc +lat_1=46.18333333333333 +lat_2=47.48333333333333 +lat_0=45.66666666666666 +lon_0=-100.5 +x_0=600000 +y_0=0 +datum=NAD83 +units=ft +no_defs')

      }
    })

    observeEvent(input$operatorX, {
      if(is.null(values$map)){
        NULL
      } else {
        dd2 <- values$dd2
        factpal2 <- values$factpal2
        wellData6 <- values$wellData
        wellData5 <- wellData1() %>% filter(operator %in% input$operatorX)
        proxy <- leafletProxy("map")
        proxy %>% clearShapes() %>%
          addPolygons(
            data = dd2,
            stroke=FALSE, fillOpacity = 0.8, smoothFactor = 0.5,
            color='grey',
            fillColor = ~factpal2(Value),
            layerId=dd2@data$Value) %>%
          addPolygons(data = countyLines, weight = 1, opacity = 0.5, fill = FALSE, color = 'black',label = countyLines@data$NAME) %>%
          addPolygons(data = twnRng$geometry, weight = 0.75, color = 'gray', label = paste0(twnRng$TWPTEXT, twnRng$RNGTEXT)) %>%
          addPolygons(data = sectionLocs1()$geometry, color = 'black', weight = 0.5) %>%
          addPolygons(data = wellData6$geom)%>%
          addPolygons(data = wellData5$geom, color = 'red')
      }
    })

    output$operatorActivity <- renderBillboarder({


      df <- wellData1() %>% filter(fp.year == max(wellData1()$fp.year)) %>%
        group_by(operator, fp.year) %>%
        summarise(Count=n()) %>%
        ungroup()

      billboarder(data = df) %>%
        bb_aes(operator, Count) %>%
        bb_piechart() %>%
        bb_pie(expand = TRUE) %>%
        bb_legend(show = FALSE) %>%
        bb_labs(title = 'Wells Brought Online in 2019 - By Operator',
                caption = 'Data source: North Dakota State Government (https://www.dmr.nd.gov/oilgas/stats/statisticsvw.asp)')
    }
    )

    prodSummary1 <- reactive({

      prodSummary %>% filter(API %in% wellData1()$API) %>%
        merge(wellData1()[,c('API', 'fp.year')], by='API', all.x=TRUE)%>% group_by(date, fp.year) %>%
        summarise_all(sum) %>% ungroup()%>% select(fp.year, date, oil, gas, water) %>%
        arrange(fp.year, date) %>% gather(Component, Value, -c(date, fp.year)) %>%
        mutate(fp.year = as.factor(fp.year)) %>% mutate(Value = if_else(year(date) <= 2019, Value/days_in_month(date), Value/30.45))

    })



    output$prodPlot2 <- renderPlotly({
      storeWarn<- getOption("warn")
      options(warn = -1)

      plott <- plotly::plot_ly(prodSummary1() %>% filter(Component %in% input$fluidAll),
                      x=~date, y=~Value,type='area',  mode = 'stack', stackgroup='one',fillcolor = ~fp.year, fill = ~fp.year)  %>%
        layout(title = 'Daily Production By Year', yaxis = list(title='Daily Rate (bbl/mcf)'), xaxis=  list(title = ''))

      shinyjs::delay(expr =({
        options(warn = storeWarn)
      }) ,ms = 100)

      plott
    })

    prodData1 <- reactive({
      prodData %>% filter(API %in% wellData1()$API)
    })


    observe({
      updatePickerInput(session, 'operatorCompare', 'Select Operator(s)', choices = sort(unique(wellData1()$operator)))

    })

    observeEvent(input$operatorCompare, {
      df1 <- wellData1() %>% filter(operator %in% input$operatorCompare)
      updatePickerInput(session, 'yearCompare', 'Select Comparison Years', choices = sort(unique(df1$fp.year)))
    })


    observe({
      if(is.null(input$yearCompare)){
        values$prodCompare <- NULL
      } else {
        df1 <- wellData1() %>% filter(operator %in% input$operatorCompare) %>% filter(as.character(fp.year) %in% input$yearCompare)
        df2 <- prodData1() %>% filter(API %in% df1$API)
        df2 <- merge(df2, df1[,c('API', 'operator', 'perf')])
        df2 <- df2 %>% arrange(API, date) %>% filter(oil > 0) %>% group_by(API) %>% filter(n() >= 8) %>% ungroup() %>%
          mutate(oil = oil/perf, gas = gas/perf, boe = oil + gas/20) %>% filter(!is.na(boe)) %>% group_by(API) %>%
          mutate(monthsOn = cumsum(API/API), oil = cumsum(oil), gas = cumsum(gas), boe = cumsum(boe)) %>% ungroup() %>% filter(monthsOn == 6) %>%
          group_by(operator) %>% summarise(oil = mean(oil), gas = mean(gas), boe = mean(boe), count = n()) %>% ungroup() %>% arrange(desc(count)) %>%
          top_n(10) %>%
          gather(Component, Value, -c(operator))

        #print(head(df2))
        values$prodCompare <- df2
      }
    })

    output$opProdCompare <- renderBillboarder({
      if(is.null(values$prodCompare)){
        NULL
      }else{
        df <- values$prodCompare %>% filter(Component %in% input$fluidCompare) %>% arrange(desc(Value)) #%>%
          # e_charts(operator) %>%
          # e_bar(Value, name = 'Volume Per Ft') %>%
          # e_title('Operator Productivity Comparison', 'Six Month Volume Per Ft - BOE is 20:1') %>%
          # e_theme("infographic") %>%
          # #e_x_axis(axisLabel = list(interval = 0, rotate = 45), margin = 1) %>%
          # e_legend(right = 0)%>%  # move legend to the bottom
          # e_tooltip(trigger = "item"
          # ) %>% e_flip_coords()%>% e_grid(left = 150)
          billboarder() %>%
            bb_barchart(data = df, mapping = bbaes(operator, Value), rotated = TRUE)%>%
            bb_y_grid(show = TRUE) %>%
            bb_y_axis(tick = list(format = suffix(" per Ft")),
                      label = list(text = "Six Month Volume in bbld/ft or mcfd/ft", position = "outer-top")) %>%
            bb_legend(show = FALSE) %>%
            bb_labs(title = "Volume Comparison",
                    caption = 'Data source: North Dakota State Government (https://www.dmr.nd.gov/oilgas/stats/statisticsvw.asp)')
      }

    })

    observe({

      updatePickerInput(session, 'countyParent', 'Select County', choices = sort(unique(wellData1()$county)))
    })


    observe({
      wells <- wellData1() %>% filter(county %in% input$countyParent) %>% group_by(loc) %>% summarise(wells = max(cumWell)) %>% ungroup() %>% filter(wells <= 8)
      wells <- sort(unique(wells$wells))
      updateSelectizeInput(session, 'wellsInSection', 'Wells Per Section', choices = wells)
    })

    output$parentChild <- renderBillboarder({
      if(is.null(input$countyParent)){
        NULL
      } else {
        wellData1 <- wellData1() %>% filter(county %in% input$countyParent) %>% group_by(loc) %>% filter(max(cumWell) >= as.numeric(input$wellsInSection)) %>%
          ungroup() %>% group_by(loc) %>% mutate(firstYear = min(fp.year)) %>% ungroup() %>%
          mutate(firstYear = replace(firstYear, cumWell > 1 & firstYear != fp.year, 0)) %>%
          mutate(firstYear = replace(firstYear, firstYear > 1, 1)) %>%
          group_by(loc) %>% mutate(firstYear = sum(firstYear)) %>% ungroup() %>%
          filter(firstYear == 1) %>%
          filter(cumWell <= as.numeric(input$wellsInSection)) %>%
          mutate(oilPerFt = oilEUR/perf) %>% group_by(cumWell) %>%
          summarise(oilPerFt = mean(oilPerFt, na.rm=TRUE), count =n()) %>% ungroup() %>% mutate(oilPerFt = as.integer((oilPerFt/oilPerFt[1])*100))


        billboarder() %>%
          bb_barchart(data = wellData1, mapping = bbaes(cumWell, oilPerFt), rotated = FALSE)%>%
          bb_y_grid(show = TRUE) %>%
          bb_y_axis(tick = list(format = suffix(" %")),
                    label = list(text = "Oil EUR Percent Parent Well", position = "outer-top")) %>%
          bb_legend(show = FALSE) %>%
          bb_labs(title = paste0('Oil EUR as Percent of Parent by Well Drilled - Units in Sample: ', wellData1$count[1]),
                  caption = 'Data source: North Dakota State Government (https://www.dmr.nd.gov/oilgas/stats/statisticsvw.asp)')
      }

    })

    output$perfPlot1 <- renderBillboarder({
        df <- wellData1() %>% group_by(fp.year) %>% summarise(perf = mean(perf, na.rm=TRUE)) %>% ungroup() %>% mutate(fp.year = as.factor(fp.year))


        billboarder() %>%
          bb_barchart(data = df, mapping = bbaes(fp.year, perf), rotated = FALSE)%>%
          bb_y_grid(show = TRUE) %>%
          bb_y_axis(tick = list(format = suffix(" Ft")),
                    label = list(text = "Average Lateral Length", position = "outer-top")) %>%
          bb_legend(show = FALSE) %>%
          bb_labs(title = 'Average Lateral Length over Time, ft',
                  caption = 'Data source: North Dakota State Government (https://www.dmr.nd.gov/oilgas/stats/statisticsvw.asp)')


    })

    output$ppfPlot1 <- renderBillboarder({
      df <- wellData1() %>% group_by(fp.year) %>% summarise(ppf = mean(ppf, na.rm=TRUE)) %>% ungroup()%>% mutate(fp.year = as.factor(fp.year))


      billboarder() %>%
        bb_barchart(data = df, mapping = bbaes(fp.year, ppf), rotated = FALSE)%>%
        bb_y_grid(show = TRUE) %>%
        bb_y_axis(tick = list(format = suffix(" Lb/Ft")),
                  label = list(text = "Average Proppant Loading", position = "outer-top")) %>%
        bb_legend(show = FALSE) %>%
        bb_labs(title = 'Average Proppant Loading over Time, lb/ft',
                caption = 'Data source: Frac Focus (https://fracfocus.org/)')


    })

    output$fpfPlot1 <- renderBillboarder({
      df <- wellData1() %>% group_by(fp.year) %>% summarise(fpf = mean(fpf, na.rm=TRUE)) %>% ungroup()%>% mutate(fp.year = as.factor(fp.year))


      billboarder() %>%
        bb_barchart(data = df, mapping = bbaes(fp.year, fpf), rotated = FALSE)%>%
        bb_y_grid(show = TRUE) %>%
        bb_y_axis(tick = list(format = suffix(" Gal/Ft")),
                  label = list(text = "Average Fluid Loading", position = "outer-top")) %>%
        bb_legend(show = FALSE) %>%
        bb_labs(title = 'Average Fluid Loading over Time, gal/ft',
                caption = 'Data source: Frac Focus (https://fracfocus.org/)')


    })

    output$tvdPlot1 <- renderBillboarder({
      df <- wellData1() %>% group_by(fp.year) %>% summarise(tvd = mean(tvd, na.rm=TRUE)) %>% ungroup()%>% mutate(fp.year = as.factor(fp.year))


      billboarder() %>%
        bb_barchart(data = df, mapping = bbaes(fp.year, tvd), rotated = FALSE)%>%
        bb_y_grid(show = TRUE) %>%
        bb_y_axis(tick = list(format = suffix(" Ft")),
                  label = list(text = "True Vertical Depth", position = "outer-top")) %>%
        bb_legend(show = FALSE) %>%
        bb_labs(title = 'Average TVD over Time, ft',
                caption = 'Data source: North Dakota State Government (https://www.dmr.nd.gov/oilgas/stats/statisticsvw.asp)')

    })

    observe({
      updatePickerInput(session, 'operatorTC', 'Operator', choices = sort(unique(wellData1()$operator)))

    })

    observeEvent(input$operatorTC, {
      df1 <- wellData1() %>% filter(operator %in% input$operatorTC)
      updatePickerInput(session, 'countyTC', 'County', choices = sort(unique(df1$county)))
    })


    wellDataTC <- reactive({
      wellData %>% filter(operator %in% input$operatorTC) %>% filter(county %in% input$countyTC)
    })

    prodData2 <- reactive({
      prodData %>% filter(API %in% wellDataTC()$API)%>% merge(wellDataTC()[,c('API', 'perf')], by='API', all.x=TRUE)
    })

    output$spPlot <- renderPlotly({
      if(is.null(input$countyTC)){
        NULL
      } else {

        #df <- wellDataX()



        prod.data <- as.data.frame(prodData2())   %>%
          mutate(oil = oil/perf, gas = gas/perf, water = water/perf)  %>% arrange(API, date) %>% group_by(API) %>%
          mutate(month = cumsum(API/API), fp.year = year(min(date))) %>% ungroup() %>% group_by(fp.year, month) %>%
          summarise(oil = mean(oil, na.rm=TRUE), gas = mean(gas, na.rm=TRUE), count=n()) %>%
          ungroup() %>% group_by(fp.year) %>% filter(count >= max(count)*0.4) %>% ungroup() %>%
          mutate(boe = oil + gas/20)%>% group_by(fp.year) %>%
          mutate(cumOil = cumsum(oil), cumGas = cumsum(gas), cumBOE = cumsum(boe)) %>% ungroup() %>%
          mutate(fp.year = as.factor(fp.year))
        #print('check9')
        storeWarn<- getOption("warn")
        options(warn = -1)
        
        if(input$fluid3 == 'oil'){
          plott <- plotly::plot_ly(prod.data, x=~cumOil, y = ~oil/30.45*1000, color=~as.factor(fp.year), type = 'scatter', mode='lines') %>%
            layout(title = 'Normalized Oil Rate', yaxis = list(title='Daily Oil Rate Per 1000 Ft', type='log'), xaxis=  list(title = 'Cumulative Production Per Ft'))
        } else if(input$fluid3 == 'gas'){
          plott <-  plotly::plot_ly(prod.data, x=~cumGas, y = ~gas/30.45*1000, color=~as.factor(fp.year), type = 'scatter', mode='lines') %>%
            layout(title = 'Normalized Gas Rate', yaxis = list(title='Daily Gas Rate Per 1000 Ft', type='log'), xaxis=  list(title = 'Cumulative Production Per Ft'))
          } else {

           plott <-  plotly::plot_ly(prod.data, x=~cumBOE, y = ~boe/30.45*1000, color=~as.factor(fp.year), type = 'scatter', mode='lines') %>%
              layout(title = 'Normalized 20:1 BOE Rate', yaxis = list(title='Daily BOE (20:1) Rate Per 1000 Ft', type='log'), xaxis=  list(title = 'Cumulative Production Per Ft'))
          }
        shinyjs::delay(expr =({
          options(warn = storeWarn)
        }) ,ms = 100)
        
        plott
        
        plott


      }
    })

    observe({
      if(is.null(input$countyTC)){
        NULL
      } else {
        updateSelectizeInput(session, 'selectYr', label = 'Select Year', choices = sort(unique(wellDataTC()$fp.year)))

      }

    })

    observeEvent(input$fluid2, {
      if(input$fluid2 == 'oil'){
        shinyjs::hide('qiGasS')
        shinyjs::hide('DiGasS')
        shinyjs::hide('bGasS')
        shinyjs::hide('DfGasS')
        shinyjs::hide('curtailGasS')
        shinyjs::hide('qiWaterS')
        shinyjs::hide('DiWaterS')
        shinyjs::hide('bWaterS')
        shinyjs::hide('DfWaterS')
        shinyjs::hide('curtailWaterS')
        shinyjs::show('qiOilS')
        shinyjs::show('DiOilS')
        shinyjs::show('bOilS')
        shinyjs::show('DfOilS')
        shinyjs::show('curtailOilS')
        
      } else if(input$fluid2 == 'gas'){
        shinyjs::show('qiGasS')
        shinyjs::show('DiGasS')
        shinyjs::show('bGasS')
        shinyjs::show('DfGasS')
        shinyjs::show('curtailGasS')
        shinyjs::hide('qiWaterS')
        shinyjs::hide('DiWaterS')
        shinyjs::hide('bWaterS')
        shinyjs::hide('DfWaterS')
        shinyjs::hide('curtailWaterS')
        shinyjs::hide('qiOilS')
        shinyjs::hide('DiOilS')
        shinyjs::hide('bOilS')
        shinyjs::hide('DfOilS')
        shinyjs::hide('curtailOilS')
      } else {
        shinyjs::hide('qiGasS')
        shinyjs::hide('DiGasS')
        shinyjs::hide('bGasS')
        shinyjs::hide('DfGasS')
        shinyjs::hide('curtailGasS')
        shinyjs::hide('qiOilS')
        shinyjs::hide('DiOilS')
        shinyjs::hide('bOilS')
        shinyjs::hide('DfOilS')
        shinyjs::hide('curtailOilS')
        shinyjs::show('qiWaterS')
        shinyjs::show('DiWaterS')
        shinyjs::show('bWaterS')
        shinyjs::show('DfWaterS')
        shinyjs::show('curtailWaterS')
      }
    })
    
    
    observeEvent(input$selectYr, {
      if(is.null(input$selectYr)){
        NULL
      } else {
        values$p <- NULL

        
        prod.data <- prodData2() %>% arrange(API, date) %>% filter(fp.year %in% input$selectYr)
        
        
        if(input$fluid2 == 'oil'){
          shinyjs::hide('qiGasS')
          shinyjs::hide('DiGasS')
          shinyjs::hide('bGasS')
          shinyjs::hide('DfGasS')
          shinyjs::hide('curtailGasS')
          shinyjs::hide('qiWaterS')
          shinyjs::hide('DiWaterS')
          shinyjs::hide('bWaterS')
          shinyjs::hide('DfWaterS')
          shinyjs::hide('curtailWaterS')
          shinyjs::show('qiOilS')
          shinyjs::show('DiOilS')
          shinyjs::show('bOilS')
          shinyjs::show('DfOilS')
          shinyjs::show('curtailOilS')
          
          prod.data <- prod.data %>%  mutate(id = paste0(API, date)) %>% filter(!duplicated(id)) %>%
            filter(oil > 0) %>% mutate(oil = oil/perf*as.numeric(capexValues()$perfSelect1)) %>% arrange(API, date) %>% 
            group_by(API) %>% mutate(month=cumsum(API/API)) %>% ungroup() %>% arrange(API, month)
          prod.data <- prod.data[,c('API', 'month', 'oil')]
          
          meanProd <- prod.data %>% group_by(month) %>% summarise(oil = mean(oil, na.rm=TRUE), count=n()) %>% ungroup() %>%
            filter(!is.nan(oil))
          meanProd <- meanProd %>%
            filter(count >= max(meanProd$count)*0.4)
          

          p <- plot_ly(prod.data %>% arrange(API, month) %>% group_by(API), x=~month, y=~oil/30.45,  color=I('gray'), type='scatter', mode='lines', name='Actuals')%>%
            add_trace(data = meanProd, x=~month, y=~oil/30.45, color=I('black'), name = 'Average', type='scatter', mode='lines')%>%
            layout(yaxis = list(title='Daily Oil Rate, bopd', type='log'), xaxis=  list(title = 'Month'))
          
        } else if(input$fluid2 == 'gas'){
          shinyjs::show('qiGasS')
          shinyjs::show('DiGasS')
          shinyjs::show('bGasS')
          shinyjs::show('DfGasS')
          shinyjs::show('curtailGasS')
          shinyjs::hide('qiWaterS')
          shinyjs::hide('DiWaterS')
          shinyjs::hide('bWaterS')
          shinyjs::hide('DfWaterS')
          shinyjs::hide('curtailWaterS')
          shinyjs::hide('qiOilS')
          shinyjs::hide('DiOilS')
          shinyjs::hide('bOilS')
          shinyjs::hide('DfOilS')
          shinyjs::hide('curtailOilS')
          prod.data <- prod.data %>% filter(gas > 0) %>%  mutate(id = paste0(API, date)) %>% filter(!duplicated(id)) %>%
            mutate(gas = gas/perf*as.numeric(capexValues()$perfSelect1))  %>% arrange(API, date) %>% 
            group_by(API) %>% mutate(month=cumsum(API/API)) %>% ungroup()%>% arrange(API, month)
          prod.data <- prod.data[,c('API', 'month', 'gas')]
          meanProd <- prod.data %>% group_by(month) %>% summarise(gas = mean(gas, na.rm=TRUE), count=n()) %>% ungroup()%>%
            filter(!is.nan(gas))
          meanProd <- meanProd %>%
            filter(count >= max(meanProd$count)*0.4)
          
          
          p <- plot_ly(prod.data %>% arrange(API, month) %>% group_by(API), x=~month, y=~gas/30.45, color=I('gray'), type='scatter', mode='lines', name='Actuals') %>%
            #add_trace(data = fitGas, x=~month, y=~gasFCST/30.45, color=I('red'), name = 'Forecast', type='scatter', mode='lines')%>%
            add_trace(data = meanProd, x=~month, y=~gas/30.45, color=I('black'), name = 'Average', type='scatter', mode='lines')%>%
            layout(yaxis = list(title='Daily Gas Rate, mcfd', type='log'), xaxis=  list(title = 'Month'))
        } else {
          shinyjs::hide('qiGasS')
          shinyjs::hide('DiGasS')
          shinyjs::hide('bGasS')
          shinyjs::hide('DfGasS')
          shinyjs::hide('curtailGasS')
          shinyjs::show('qiWaterS')
          shinyjs::show('DiWaterS')
          shinyjs::show('bWaterS')
          shinyjs::show('DfWaterS')
          shinyjs::show('curtailWaterS')
          shinyjs::hide('qiOilS')
          shinyjs::hide('DiOilS')
          shinyjs::hide('bOilS')
          shinyjs::hide('DfOilS')
          shinyjs::hide('curtailOilS')
          prod.data <- prod.data %>% filter(water > 0) %>%  mutate(id = paste0(API, date)) %>% filter(!duplicated(id)) %>%
            mutate(water = water/perf*as.numeric(capexValues()$perfSelect1)) %>% arrange(API, date) %>% 
            group_by(API) %>% mutate(month=cumsum(API/API)) %>% ungroup()%>% arrange(API, month)
          prod.data <- prod.data[,c('API', 'month', 'water')]
          meanProd <- prod.data %>% group_by(month) %>% summarise(water = mean(water, na.rm=TRUE), count=n()) %>% ungroup()%>%
            filter(!is.nan(water))
          meanProd <- meanProd %>%
            filter(count >= max(meanProd$count)*0.4)
          
          
          
          p <- plot_ly(prod.data %>% arrange(API, month) %>% group_by(API), x=~month, y=~water/30.45, color=I('gray'), type='scatter', mode='lines', name='Actuals') %>%
            #add_trace(data = fitGas, x=~month, y=~gasFCST/30.45, color=I('red'), name = 'Forecast', type='scatter', mode='lines')%>%
            add_trace(data = meanProd, x=~month, y=~water/30.45, color=I('black'), name = 'Average', type='scatter', mode='lines')%>%
            layout(yaxis = list(title='Daily Water Rate, bwpd', type='log'), xaxis=  list(title = 'Month'))
        }
        
        values$p <- p
      }
    })
    
    output$spPlot1 <- renderPlotly({
      if(is.null(values$p)|is.na(declineValues()$bOilS)|is.na(declineValues()$bGasS)
         |is.na(declineValues()$bWaterS)|is.na(declineValues()$DiOilS)|is.na(declineValues()$DiGasS)
         |is.na(declineValues()$DiWaterS)|is.na(declineValues()$DfOilS)|is.na(declineValues()$DfGasS)
         |is.na(declineValues()$DfWaterS)|is.na(declineValues()$qiOilS)|is.na(declineValues()$qiGasS)
         |is.na(declineValues()$qiWaterS)|is.na(declineValues()$curtailOilS)|is.na(declineValues()$curtailGasS)
         |is.na(declineValues()$curtailWaterS)|is.na(declineValues()$wellLifeS|is.null(input$countyTC))){
        NULL
      } else {
        storeWarn<- getOption("warn")
        options(warn = -1)
        if(input$fluid2 == 'oil'){
          #print(head(prod.data))
         
          
          fitOil <- curtailed.q(arps.decline(
            as.numeric(declineValues()$qiOilS)*365, as.nominal(as.numeric(declineValues()$DiOilS)), as.numeric(declineValues()$bOilS), as.nominal(as.numeric(declineValues()$DfOilS))),
            as.numeric(declineValues()$curtailOilS)/12.0,seq(0, declineValues()$wellLifeS-1/12, by= (1/12)))/12
          fitOil <- as.data.frame(fitOil)
          #rm(fitOil)
          names(fitOil) <- c('oilFCST')
          
          
          if(declineValues()$curtailOilS > 0){
            fitOil$oilFCST[1] <- fitOil$oilFCST[1]/2
          }
          
          fitOil <- fitOil %>% mutate(month = nrow(fitOil)) %>% mutate(month = cumsum(month/month))
          
          fcstOil <- sum(fitOil$oil)
          #values$qiOil <- as.numeric(declineValues()$qiOilS)/(fcstOil/1000)
          #print(values$qiOil)
          title <- paste('Forecast Oil EUR (MBO): ', as.integer(fcstOil/1000), sep='')
          output$eurCalc <- renderText(title)
          fitOil <- fitOil %>% filter(month < (year(today()) - min(as.numeric(input$selectYr)))*12+12)
          
          
          plott <- values$p %>% add_trace(data = fitOil, x=~month, y=~oilFCST/30.45, color=I('green'), name = 'Forecast', type='scatter', mode='lines')
        } else if (input$fluid2 == 'gas'){
          
          
          fitGas <- curtailed.q(arps.decline(
            as.numeric(declineValues()$qiGasS)*365, as.nominal(as.numeric(declineValues()$DiGasS)), as.numeric(declineValues()$bGasS), as.nominal(as.numeric(declineValues()$DfGasS))),
            as.numeric(declineValues()$curtailGasS)/12.0,seq(0, declineValues()$wellLifeS-1/12, by= (1/12)))/12
          
          fitGas <- as.data.frame(fitGas)
          #rm(fitOil)
          names(fitGas) <- c('gasFCST')
          
          if(declineValues()$curtailGasS > 0){
            fitGas$gasFCST[1] <- fitGas$gasFCST[1]/2
          }
          fitGas <- fitGas %>% mutate(month = nrow(fitGas)) %>% mutate(month = cumsum(month/month))
          
          fcstGas <- sum(fitGas$gas)
          #values$qiGas <- as.numeric(declineValues()$qiGasS)/(fcstGas/1000)
          #print(values$qiGas)
          title <- paste('Forecast Gas EUR (MMCF): ', as.integer(fcstGas/1000), sep='')
          output$eurCalc <- renderText(title)
          fitGas <- fitGas %>% filter(month < (year(today()) - min(as.numeric(input$selectYr)))*12+12)
          
          plott <- values$p %>% add_trace(data = fitGas, x=~month, y=~gasFCST/30.45, color=I('red'), name = 'Forecast', type='scatter', mode='lines')
          
        } else {
          
          
          fitWater <- curtailed.q(arps.decline(
            as.numeric(declineValues()$qiWaterS)*365, as.nominal(as.numeric(declineValues()$DiWaterS)), as.numeric(declineValues()$bWaterS), as.nominal(as.numeric(declineValues()$DfWaterS))),
            as.numeric(declineValues()$curtailWaterS)/12.0,seq(0, declineValues()$wellLifeS-1/12, by= (1/12)))/12
          
          fitWater <- as.data.frame(fitWater)
          #rm(fitOil)
          names(fitWater) <- c('waterFCST')
          
          if(declineValues()$curtailWaterS > 0){
            fitWater$waterFCST[1] <- fitWater$waterFCST[1]/2
          }
          fitWater <- fitWater %>% mutate(month = nrow(fitWater)) %>% mutate(month = cumsum(month/month))
          
          fcstWater <- sum(fitWater$water)
          #values$qiGas <- as.numeric(declineValues()$qiGasS)/(fcstGas/1000)
          #print(values$qiGas)
          title <- paste('Forecast Water EUR (MBBL): ', as.integer(fcstWater/1000), sep='')
          output$eurCalc <- renderText(title)
          fitWater <- fitWater%>% filter(month < (year(today()) - min(as.numeric(input$selectYr)))*12+12)
          
          plott <- values$p %>% add_trace(data = fitWater, x=~month, y=~waterFCST/30.45, color=I('blue'), name = 'Forecast', type='scatter', mode='lines')
          
        }
        
        
      
        
        
        #plott <- values$p
        shinyjs::delay(expr =({
          options(warn = storeWarn)
        }) ,ms = 100)
        
        plott
      }

    })

    
    
    
    observe({
      if(input$priceType == 'Strip'){
        
        shinyjs::hide('wti')
        shinyjs::hide('hh')
        
        crude <-'https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=RWTC&f=M'
        webpage <- read_html(crude)
        tbls_ls <- webpage %>%
          html_nodes('table') %>%
          .[5] %>%
          html_table(fill = TRUE)
        wti1 <- tbls_ls[[1]]
        wti1 <- wti1 %>% filter(!is.na(Jan))
        
        wti1 <- wti1 %>% gather(DATE, WTI, -c(Year))
        wti1 <- wti1 %>% mutate(DATE = paste0(DATE,'/01/', Year))
        wti1$DATE <- as.POSIXct(wti1$DATE, format = '%b/%d/%Y')
        wti1 <- wti1 %>% arrange(DATE) %>% select(DATE, WTI)
        
        crude <-'https://www.eia.gov/dnav/ng/hist/rngwhhdm.htm'
        webpage <- read_html(crude)
        tbls_ls <- webpage %>%
          html_nodes('table') %>%
          .[5] %>%
          html_table(fill = TRUE)
        hh1 <- tbls_ls[[1]]
        hh1 <- hh1 %>% filter(!is.na(Jan))
        
        hh1 <- hh1 %>% gather(DATE, HH, -c(Year))
        hh1 <- hh1 %>% mutate(DATE = paste0(DATE,'/01/', Year))
        hh1$DATE <- as.POSIXct(hh1$DATE, format = '%b/%d/%Y')
        hh1 <- hh1 %>% arrange(DATE) %>% select(DATE, HH)
        wti1 <- wti1 %>% filter(DATE >= min(hh1$DATE))
        
        crude = 'https://quotes.wsj.com/futures/CRUDE%20OIL%20-%20ELECTRONIC/contracts'
        webpage <- read_html(crude)
        #tbls <- html_nodes(webpage, 'table')
        
        tbls_ls <- webpage %>%
          html_nodes('table') %>%
          .[1] %>%
          html_table(fill = TRUE)
        
        wti <- tbls_ls[[1]]
        
        crude = 'https://quotes.wsj.com/futures/NATURAL%20GAS/contracts'
        webpage <- read_html(crude)
        #tbls <- html_nodes(webpage, 'table')
        
        tbls_ls <- webpage %>%
          html_nodes('table') %>%
          .[1] %>%
          html_table(fill = TRUE)
        
        hh <- tbls_ls[[1]]
        
        
        rm(crude, webpage, tbls_ls)
        wti <- wti[,c('MONTH', 'SETTLEMENT')]
        hh <- hh[,c('MONTH', 'SETTLEMENT')]
        
        wti <- wti %>% filter(MONTH != 'Front Month')
        hh <- hh %>% filter(MONTH != 'Front Month')
        
        
        
        wti$YEAR <- substr(wti$MONTH, nchar(wti$MONTH)-3, nchar(wti$MONTH))
        wti$MONTH <- substr(wti$MONTH, nchar(wti$MONTH)-7, nchar(wti$MONTH)-5)
        
        
        hh$YEAR <- substr(hh$MONTH, nchar(hh$MONTH)-3, nchar(hh$MONTH))
        hh$MONTH <- substr(hh$MONTH, nchar(hh$MONTH)-7, nchar(hh$MONTH)-5)
        
        
        wti$DATE <- paste(wti$MONTH, '/01/', wti$YEAR, sep='')
        hh$DATE <- paste(hh$MONTH, '/01/', hh$YEAR, sep='')
        
        wti$DATE <- as.POSIXct(wti$DATE, format = '%b/%d/%Y')
        hh$DATE <- as.POSIXct(hh$DATE, format = '%b/%d/%Y')
        
        
        wti <- wti[,c('DATE', 'SETTLEMENT')]
        hh <- hh[,c('DATE', 'SETTLEMENT')]
        
        
        names(wti) <- c('DATE', 'WTI')
        names(hh) <- c('DATE', 'HH')
        
        date1 <- min(wti1$DATE)
        date2 <- max(hh$DATE)
        date3 <- data.frame(DATE = seq(0, 1000, 1))
        date3$DATE <- date1 %m+% months(date3$DATE) 
        date3 <- date3 %>% filter(DATE <= date2)
        wti <- rbind(wti1, wti)
        wti <- merge(date3, wti, by='DATE', all.x=TRUE)
        
        hh <- rbind(hh1, hh)
        
        price <- merge(wti, hh, by='DATE', all.x=TRUE, all.y=TRUE)
        
        rm(wti, hh)
        
        
        if (is.na(price$WTI[1])) {
          price$WTI[1] <- price$WTI[2]
        }
        
        
        
        price$WTI <- na.locf(price$WTI)
        price$HH <- na.locf(price$HH)
        print(head(price))
        values$price <- price
        
        
      } else {
        shinyjs::show('wti')
        shinyjs::show('hh')
        crude <-'https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=RWTC&f=M'
        webpage <- read_html(crude)
        tbls_ls <- webpage %>%
          html_nodes('table') %>%
          .[5] %>%
          html_table(fill = TRUE)
        wti1 <- tbls_ls[[1]]
        wti1 <- wti1 %>% filter(!is.na(Jan))
        
        wti1 <- wti1 %>% gather(DATE, WTI, -c(Year))
        wti1 <- wti1 %>% mutate(DATE = paste0(DATE,'/01/', Year))
        wti1$DATE <- as.POSIXct(wti1$DATE, format = '%b/%d/%Y')
        wti1 <- wti1 %>% arrange(DATE) %>% select(DATE, WTI)
        
        crude <-'https://www.eia.gov/dnav/ng/hist/rngwhhdm.htm'
        webpage <- read_html(crude)
        tbls_ls <- webpage %>%
          html_nodes('table') %>%
          .[5] %>%
          html_table(fill = TRUE)
        hh1 <- tbls_ls[[1]]
        hh1 <- hh1 %>% filter(!is.na(Jan))
        
        hh1 <- hh1 %>% gather(DATE, HH, -c(Year))
        hh1 <- hh1 %>% mutate(DATE = paste0(DATE,'/01/', Year))
        hh1$DATE <- as.POSIXct(hh1$DATE, format = '%b/%d/%Y')
        hh1 <- hh1 %>% arrange(DATE) %>% select(DATE, HH)
        wti1 <- wti1 %>% filter(DATE >= min(hh1$DATE))
        
        crude = 'https://quotes.wsj.com/futures/CRUDE%20OIL%20-%20ELECTRONIC/contracts'
        webpage <- read_html(crude)
        #tbls <- html_nodes(webpage, 'table')
        
        tbls_ls <- webpage %>%
          html_nodes('table') %>%
          .[1] %>%
          html_table(fill = TRUE)
        
        wti <- tbls_ls[[1]]
        
        crude = 'https://quotes.wsj.com/futures/NATURAL%20GAS/contracts'
        webpage <- read_html(crude)
        #tbls <- html_nodes(webpage, 'table')
        
        tbls_ls <- webpage %>%
          html_nodes('table') %>%
          .[1] %>%
          html_table(fill = TRUE)
        
        hh <- tbls_ls[[1]]
        
        
        rm(crude, webpage, tbls_ls)
        wti <- wti[,c('MONTH', 'SETTLEMENT')]
        hh <- hh[,c('MONTH', 'SETTLEMENT')]
        
        wti <- wti %>% filter(MONTH != 'Front Month')
        hh <- hh %>% filter(MONTH != 'Front Month')
        
        
        
        wti$YEAR <- substr(wti$MONTH, nchar(wti$MONTH)-3, nchar(wti$MONTH))
        wti$MONTH <- substr(wti$MONTH, nchar(wti$MONTH)-7, nchar(wti$MONTH)-5)
        
        
        hh$YEAR <- substr(hh$MONTH, nchar(hh$MONTH)-3, nchar(hh$MONTH))
        hh$MONTH <- substr(hh$MONTH, nchar(hh$MONTH)-7, nchar(hh$MONTH)-5)
        
        
        wti$DATE <- paste(wti$MONTH, '/01/', wti$YEAR, sep='')
        hh$DATE <- paste(hh$MONTH, '/01/', hh$YEAR, sep='')
        
        wti$DATE <- as.POSIXct(wti$DATE, format = '%b/%d/%Y')
        hh$DATE <- as.POSIXct(hh$DATE, format = '%b/%d/%Y')
        
        
        wti <- wti[,c('DATE', 'SETTLEMENT')]
        hh <- hh[,c('DATE', 'SETTLEMENT')]
        
        wti$SETTLEMENT <- input$wti
        hh$SETTLEMENT <- input$hh
        
        names(wti) <- c('DATE', 'WTI')
        names(hh) <- c('DATE', 'HH')
        
        date1 <- min(wti1$DATE)
        date2 <- max(hh$DATE)
        date3 <- data.frame(DATE = seq(0, 1000, 1))
        date3$DATE <- date1 %m+% months(date3$DATE) 
        date3 <- date3 %>% filter(DATE <= date2)
        wti <- rbind(wti1, wti)
        wti <- merge(date3, wti, by='DATE', all.x=TRUE)
        
        hh <- rbind(hh1, hh)
        
        price <- merge(wti, hh, by='DATE', all.x=TRUE, all.y=TRUE)
        
        rm(wti, hh)
        
        
        if (is.na(price$WTI[1])) {
          price$WTI[1] <- price$WTI[2]
        }
        
        
        
        price$WTI <- na.locf(price$WTI)
        price$HH <- na.locf(price$HH)
        price <- as.data.frame(price)
        print(head(price))
        values$price <- price
        
      }
      
    })
    
    output$stripPrice <- renderBillboarder({
      df <- values$price
      #df$DATE <- as.character(df$DATE)
      
      if(input$priceType == 'Strip'){
        billboarder(data = df) %>%
          bb_linechart(
            mapping = bbaes(x=DATE, y=WTI), type = 'line', width = 2
          ) %>%
          bb_linechart(
            mapping = bbaes(x=DATE, y=HH), type = 'spline', width = 2
          )  %>% 
          bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE)) %>% 
          bb_data(axes = list(
            "WTI" = "y",
            "HH" = "y2"
          )) %>% 
          bb_colors_manual(
            WTI = 'green',
            HH = 'red'
          )%>%
          bb_axis(
            y2 = list(show = TRUE, label = "Henry Hub Gas Price, $/MCF"),
            y = list(label = "WTI Oil Price, $/BBL")
          )%>%
          bb_labs(title = 'Price Model',
                  caption = 'Data source: EIA (Historical), WSJ (Forward)')
      } else {
        billboarder(data = df) %>%
          bb_linechart(
            mapping = bbaes(x=DATE, y=WTI), type = 'line', width = 2
          ) %>%
          bb_linechart(
            mapping = bbaes(x=DATE, y=HH), type = 'spline', width = 2
          )  %>% 
          bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE)) %>% 
          bb_data(axes = list(
            "WTI" = "y",
            "HH" = "y2"
          )) %>% 
          bb_colors_manual(
            WTI = 'green',
            HH = 'red'
          )%>%
          bb_axis(
            y2 = list(show = TRUE, label = "Henry Hub Gas Price, $/MCF"),
            y = list(label = "WTI Oil Price, $/BBL")
          )%>%
          bb_labs(title = 'Price Model',
                  caption = 'Data source: EIA (Historical)')
      }
      
      
    })
    



    observe({
      updatePickerInput(session, 'operator1', 'Operator', choices = sort(unique(wellData1()$operator)))

    })

    observeEvent(input$operator1, {
      df1 <- wellData1() %>% filter(operator %in% input$operator1)
      updatePickerInput(session, 'county1', 'County', choices = sort(unique(df1$county)))
      values$pdpFcst <- NULL
      values$pudFcst <- NULL
      values$invTable <- NULL
    })
    
    observeEvent(input$county1,{
      values$pdpFcst <- NULL
      values$pudFcst <- NULL
      values$invTable <- NULL
    })

    wellDataX <- reactive({
      if(is.null(input$county1)){
        wellData1() %>% filter(operator %in% input$operator1)
      } else {
        wellData1() %>% filter(operator %in% input$operator1) %>% filter(county %in% input$county1)
      }
    })

    prodSummaryX <- reactive({
      if(is.null(input$county1)){
        NULL
      } else {
        prodSummary %>% filter(API %in% wellDataX()$API)
      }

    })

    observeEvent(input$generatePDP, {
      updateButton(session, 'generatePDP', 'Thinking....', style = 'danger')
      if(is.null(input$county1)){
        values$pdpValue <- NULL
        values$pdpFcst <- NULL


      } else {
        values$pdpValue <- NULL
        values$pdpFcst <- NULL

        price <- values$price
        names(price)[1] <- 'date'

        test1 <- prodSummaryX() %>% merge(wellDataX()[,c('API', 'fp.year')], by='API', all.x=TRUE)
        test1 <- as.data.frame(test1)
        #print(head(test1))
        #
        if(nrow(price) == 0){
          test1$WTI <- 50
          test1$HH <- 2
        } else {
          price <- price %>% group_by(date) %>% summarise_all(mean)
          price <- price %>% mutate(date = paste0(month(date),'/01/',year(date))) %>% mutate(date = as.POSIXct(date, format = '%m/%d/%Y'))
          test1 <- test1 %>% mutate(date = paste0(month(date),'/01/',year(date))) %>% mutate(date = as.POSIXct(date, format = '%m/%d/%Y'))
          test1 <- merge(test1, price, by='date', all.x=TRUE)
          #print(head(test1))
          test1$WTI <- na.locf(test1$WTI)
          test1$HH <- na.locf(test1$HH)
        }
        test1 <- test1[!duplicated(test1),]
        test1 <- test1 %>% arrange(API, date)
        test1 <- as.data.frame(test1)


        econSummary <- lapply(split(test1, test1[,'API']), function (well) tryCatch({

          well <- well %>% arrange(API, date)
          well$ngl <- well$gas*priceValues()$nglYield/1000
          well$netOil <- well$oil*expenseValues()$nri/100
          well$netSalesGas <- well$gas*expenseValues()$nri/100*priceValues()$shrink/100*priceValues()$btu
          well$netNGL <- well$ngl*expenseValues()$nri/100

          well$nglPrice <- priceValues()$nglPrice/100*well$WTI

          well$oilRevenue <- well$netOil*(well$WTI - priceValues()$oilDiff)
          well$gasRevenue <- well$netSalesGas*(well$HH - priceValues()$gasDiff)
          well$nglRevenue <- well$netNGL*well$nglPrice
          well$revenue <- well$oilRevenue+well$gasRevenue+well$nglRevenue

          well$opex <- expenseValues()$finalFixed + expenseValues()$varOilExp*well$oil + expenseValues()$varGasExp*well$gas + expenseValues()$varNGLExp*well$ngl + expenseValues()$varWaterExp*well$water + expenseValues()$wrkExp
          well$opex[1:12] <- well$opex[1:12] - expenseValues()$finalFixed + expenseValues()$yr1Fixed
          well$opex[13:24] <- well$opex[13:24] - expenseValues()$finalFixed + expenseValues()$yr2Fixed

          well$tax <- well$oilRevenue*expenseValues()$sevOil/100 + well$gas*expenseValues()$sevGas + well$revenue*expenseValues()$atx/100
          well$nocf <- well$revenue-well$opex-well$tax

          if(input$econCutoff == 'Yes'){
            limit <- which(well$nocf >= 0)
            limit <- limit[length(limit)]
            well <- well[1:limit,]
          }

          well$capex <- 0
          well$capex[nrow(well)] <- capexValues()$pna

          well <- well %>% filter(date >= input$pdpEffDate)
          #well$month <- seq(0, (nrow(well)-1), 1)
          well$fcf <- well$nocf - well$capex
          #fit$npv10 <- fit$fcf/(1.1^(fit$month/12))


          #well$npv10 <- sum(fit$npv10)
          #well$irr <- IRRcalc(fit$fcf, fit$month)
          well


        },
        error = function(e) {
          e
          NULL
        }))

        test1 <- dplyr::bind_rows(econSummary)
        test1 <- as.data.frame(test1)
        test1 <- subset(test1, select = -c(API))

        test1$oil <- test1$oil*expenseValues()$wi/100
        test1$gas <- test1$gas*expenseValues()$wi/100
        test1$water <- test1$water*expenseValues()$wi/100
        test1$ngl <- test1$ngl*expenseValues()$wi/100
        test1$netOil <- test1$netOil*expenseValues()$wi/100
        test1$netSalesGas <- test1$netSalesGas*expenseValues()$wi/100
        test1$netNGL <- test1$netNGL*expenseValues()$wi/100
        test1$oilRevenue <- test1$oilRevenue*expenseValues()$wi/100
        test1$gasRevenue <- test1$gasRevenue*expenseValues()$wi/100
        test1$nglRevenue <- test1$nglRevenue*expenseValues()$wi/100
        test1$revenue <- test1$revenue*expenseValues()$wi/100
        test1$opex <- test1$opex*expenseValues()$wi/100
        test1$tax <- test1$tax*expenseValues()$wi/100
        test1$nocf <- test1$nocf*expenseValues()$wi/100
        test1$capex <- test1$capex*expenseValues()$wi/100
        test1$fcf <- test1$fcf*expenseValues()$wi/100

        test1 <- test1 %>% group_by(date) %>% summarise(oil = as.integer(sum(oil)), gas = as.integer(sum(gas)),
                                                        water = as.integer(sum(water)), ngl = as.integer(sum(ngl)),
                                                        netOil = as.integer(sum(netOil)), netSalesGas = as.integer(sum(netSalesGas)),
                                                        netNGL = as.integer(sum(netNGL)), WTI = (mean(WTI)), HH = (mean(HH)),
                                                        nglPrice = (mean(nglPrice)), oilRevenue = (as.integer(sum(oilRevenue))),
                                                        gasRevenue = (as.integer(sum(gasRevenue))), nglRevenue = (as.integer(sum(nglRevenue))),
                                                        revenue = (as.integer(sum(revenue))),opex = (as.integer(sum(opex))),
                                                        tax = (as.integer(sum(tax))), nocf =(as.integer(sum(nocf))),
                                                        capex = (as.integer(sum(capex))), fcf = as.integer(sum(fcf))) %>% ungroup() %>%
          arrange(date)

        values$pdpFcst <- test1[,c('date', 'oil', 'gas', 'water', 'ngl', 'netOil', 'netSalesGas', 'netNGL', 'WTI', 'HH', 'nglPrice',
                                   'oilRevenue', 'gasRevenue', 'nglRevenue', 'revenue', 'opex', 'tax', 'nocf', 'capex', 'fcf')]



      }
      updateButton(session, 'generatePDP', 'Calculate', style = 'primary')
    })

    output$pdpStack <- renderBillboarder({
      if(is.null(values$pdpFcst)){
        NULL
      } else {
        storeWarn<- getOption("warn")
        options(warn = -1)
        #print(head(values$pdpFcst))
        df <- as.data.frame(values$pdpFcst) %>% gather(Component, Value, -c(date)) %>% filter(Component %in% input$prodValue) %>% mutate(id = '1PDP') %>% mutate(id = as.factor(id))
        df <- as.data.frame(df)
        #print(head(df))
        # plott <- plotly::plot_ly(df, x=~date, y=~Value,type='area',  mode = 'stack', stackgroup='one',group_by = ~id, fillcolor = ~id, color = ~id)  %>%
        #   layout(title = 'PDP Forecast', yaxis = list(title='Volume in bbl/mcf, Value in $'), xaxis=  list(title = ''))
        
        plott <- billboarder(data = df) %>%
          bb_linechart(
            mapping = bbaes(x=date, y=Value), type = 'area'
          ) %>%
          bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE)) %>% 
          bb_colors_manual(
            Value = 'green'
          )%>%
          bb_legend(show = FALSE) %>%
          # bb_zoom(
          #   enabled = list(type = "drag"),
          #   resetButton = list(text = "Unzoom")
          # ) %>% 
          bb_subchart(show = TRUE, size = list(height = 30)) %>% 
          bb_labs(title = 'PDP Summary',
                  y = 'Volumes in BBLS/MCF, Values in $',
                  caption = 'Data source: North Dakota State Government (https://www.dmr.nd.gov/oilgas/stats/statisticsvw.asp)')
        
        
        shinyjs::delay(expr =({
          options(warn = storeWarn)
        }) ,ms = 100)
        
        plott
      }
    })
    
    
   
    observeEvent(input$wi, {
      values$pdpFcst <- NULL
      values$pudFcst <- NULL
    })
    
    observeEvent(input$wi2, {

      values$pudFcst <- NULL
    })
    observeEvent(input$pdpEffDate, {
      values$pdpFcst <- NULL
      values$pudFcst <- NULL
    })
    
    observeEvent(input$pudStart, {
      values$pudFcst <- NULL
    })
    
    
    sectionInv <- reactive({
      sectionLocs %>% filter(operator %in% input$operator1) 
    })
    
    acreageInv <- reactive({
      acreage%>% filter(operator %in% input$operator1) 
    })
    
    gasFracInv <- reactive(
      gasFrac %>% filter(reservoir %in% input$reservoirSelect)
    )
    
    
    observeEvent(input$calcInv, {
      if(is.null(values$price)||is.null(input$county1)){
        NULL
      } else {
        values$invTable <- NULL
        shinyjs::disable('calcInv')
        updateButton(session,'calcInv', label = 'Calculating....', style = 'danger', size = 'small')
        check1 <- wellData1() %>% filter(operator %in% input$operator1)
        
        df1 <- wellData %>% filter(county %in% input$county1) %>%
          filter(loc %in% check1$loc|surfLoc %in% sectionInv()$TWPRNGSEC|bhLoc %in% sectionInv()$TWPRNGSEC|midLoc %in% sectionInv()$TWPRNGSEC)# %>% filter(reservoir == 'Bakken')
        df1 <- df1 %>% filter(surfLoc %in% acreageInv()$surfLoc|bhLoc %in% acreageInv()$surfLoc|midLoc %in% acreageInv()$surfLoc)
        df1 <- as.data.frame(df1)
        list1 <- append(unique(df1$surfLoc), unique(df1$bhLoc), unique(df1$midLoc))
        list1 <- unique(list1)
        #print(head(list1))
        acreTotal <- as.data.frame(acreageInv()) %>% filter(surfLoc %in% list1)# %>% filter(operator %in% input$operator1)
        #print(head(acreTotal))
        #print(head(acreageInv()))
        acreTotal <- nrow(acreTotal)*640
        #print(head(acreTotal))
        
        df1 <- as.data.frame(df1)
        
        
        df1 <- df1 %>% group_by(loc) %>% summarise(wor = mean(waterEUR, na.rm=TRUE)/(mean(oilEUR, na.rm=TRUE) + mean(gasEUR, na.rm=TRUE)/20)) %>% ungroup()
        
        df3 <- wellData1()  %>% filter(loc %in% df1$loc) %>%
          group_by(loc) %>% summarise(perf = mean(perf, na.rm=TRUE), oldPPF = mean(ppf, na.rm=TRUE), oldFPF = mean(fpf, na.rm=TRUE), wells = n()) %>%
          ungroup()
        
        df1 <- merge(df1, df3, by = 'loc', all.x=TRUE)
        
        df1$perf[is.na(df1$perf)] <- capexValues()$perfSelect1
        df1$oldPPF[is.na(df1$oldPPF)] <- 0
        df1$oldFPF[is.na(df1$oldFPF)] <- 0
        df1$wells[is.na(df1$wells)] <- 0
        
        df3 <- wellData %>% group_by(loc) %>% summarise(avLat = mean(avLat), avLong = mean(avLong)) %>% ungroup()
        df1 <- merge(df1, df3, by = 'loc', all.x=TRUE)
        
        
        k <- unique(df1$avLat)
        
        list1 <- mapply(fnx, k)
        list1 <- as.data.frame(list1)
        names(list1) <- 'distLat'
        list1$avLat <- k
        df1 <- merge(df1, list1, by='avLat', all.x=TRUE)
        df1 <- as.data.frame(df1)
        df1 <- df1 %>%  mutate(distLong = pmap_dbl(list(x = avLong, y = avLat),fn1))
        df1 <- df1 %>% mutate(oneSecLocation = paste(round(distLat/(5280*1), digits=0),',', (round(distLong/(5280*1),digits=0)),sep=''))
        
        df1 <- merge(df1,gasFracInv()[,c('oneSecLocation', 'gasFrac', 'tvd', 'risk')], by='oneSecLocation', all.x=TRUE)
        df1 <- as.data.frame(df1)
        df1 <- df1 %>% filter(!is.na(risk)) %>% filter(!is.na(tvd)) %>% filter(!is.na(gasFrac))
        df1 <- df1 %>% filter(!duplicated(loc))
        df1$ppf <- capexValues()$ppfSelect1
        #df1$fpf <- 1000
        df1$logPPF <- log(df1$ppf)
        #propUplift <- readRDS('./data/propUplift.rds')
        #perfUplift <- readRDS('./data/perfUplift.rds')
        df1$EUR <- exp(predict(propUplift, df1))
        df1$EUR <- df1$EUR*df1$risk
        
        
        
        df1 <- as.data.frame(df1)
        
        df1$perfRisk <- perfUplift$coefficients[1] + perfUplift$coefficients[2]*df1$perf + perfUplift$coefficients[3]*(df1$perf**2)
        df1$EUR <- df1$EUR*df1$perfRisk/1000
        df1$oilEUR <- df1$EUR*(1-df1$gasFrac)
        df1$gasEUR <- (df1$EUR-df1$oilEUR)*20
        df1$waterEUR <- df1$EUR*df1$wor
        df1 <- df1[,c('loc', 'perf', 'oldPPF', 'oldFPF', 'tvd', 'ppf', 'oilEUR', 'gasEUR', 'waterEUR', 'wells')]
        
        df1$remWells <- capexValues()$wellSpacing - df1$wells
        df1 <- df1 %>% mutate(remWells = replace(remWells, remWells < 0, 0))
        
        df1$risk <- 1
        df1$risk[df1$remWells == 1] <- declineValues()$well8Risk/100
        df1$risk[df1$remWells == 2] <- (declineValues()$well8Risk+declineValues()$well7Risk)/200
        df1$risk[df1$remWells == 3] <- (declineValues()$well8Risk +
                                          declineValues()$well7Risk +
                                          declineValues()$well6Risk)/300
        df1$risk[df1$remWells == 4] <- (declineValues()$well8Risk + declineValues()$well7Risk+
                                        declineValues()$well6Risk +declineValues()$well5Risk)/400
        df1$risk[df1$remWells == 5] <- (declineValues()$well8Risk + declineValues()$well7Risk +
                                        declineValues()$well6Risk + declineValues()$well5Risk +
                                        declineValues()$well4Risk)/500
        df1$risk[df1$remWells == 6] <- (declineValues()$well8Risk + declineValues()$well7Risk +
                                        declineValues()$well6Risk + declineValues()$well5Risk +
                                        declineValues()$well4Risk + declineValues()$well3Risk)/600
        df1$risk[df1$remWells == 7] <- (declineValues()$well8Risk + declineValues()$well7Risk +
                                        declineValues()$well6Risk + declineValues()$well5Risk +
                                        declineValues()$well4Risk + declineValues()$well3Risk +
                                        declineValues()$well2Risk)/700
        df1$risk[df1$remWells == 8] <- (declineValues()$well8Risk + declineValues()$well7Risk +
                                        declineValues()$well6Risk + declineValues()$well5Risk+
                                        declineValues()$well4Risk + declineValues()$well3Risk+
                                        declineValues()$well2Risk + declineValues()$well1Risk)/800
        
        df1$oilEUR <- df1$oilEUR*df1$risk
        df1$gasEUR <- df1$gasEUR*df1$risk
        df1$waterEUR <- df1$waterEUR*df1$risk
        
        df1 <- subset(df1, select = -c(risk))
        
        df1$perf <- as.integer(df1$perf)
        df1$oldPPF <- as.integer(df1$oldPPF)
        df1$oldFPF <- as.integer(df1$oldFPF)
        df1$tvd <- as.integer(df1$tvd)
        df1$ppf <- as.integer(df1$ppf)
        df1$oilEUR <- as.integer(df1$oilEUR)
        df1$gasEUR <- as.integer(df1$gasEUR)
        df1$waterEUR <- as.integer(df1$waterEUR)
        df1$wells <- as.integer(df1$wells)
        
        df1 <- df1 %>% filter(wells > 0) %>% filter(oilEUR > 0)
        
        if(nrow(df1) == 0){
          df1 <- data.frame(result = 'No Inventory Remaining')
        } else {
          
          
          qiOil <- declineValues()$qiOilS/(sum(curtailed.q(arps.decline(
            as.numeric(declineValues()$qiOilS)*365, as.nominal(as.numeric(declineValues()$DiOilS)), as.numeric(declineValues()$bOilS), as.nominal(as.numeric(declineValues()$DfOilS))),
            as.numeric(declineValues()$curtailOilS)/12.0,seq(0, declineValues()$wellLifeS*12-1/12, by= (1/12)))/12)/1000)
          
          qiGas <- declineValues()$qiGasS/(sum(curtailed.q(arps.decline(
            as.numeric(declineValues()$qiGasS)*365, as.nominal(as.numeric(declineValues()$DiGasS)), as.numeric(declineValues()$bGasS), as.nominal(as.numeric(declineValues()$DfGasS))),
            as.numeric(declineValues()$curtailGasS)/12.0,seq(0, declineValues()$wellLifeS*12-1/12, by= (1/12)))/12)/1000)
          
          qiWater <- declineValues()$qiWaterS/(sum(curtailed.q(arps.decline(
            as.numeric(declineValues()$qiWaterS)*365, as.nominal(as.numeric(declineValues()$DiWaterS)), as.numeric(declineValues()$bWaterS), as.nominal(as.numeric(declineValues()$DfWaterS))),
            as.numeric(declineValues()$curtailWaterS)/12.0,seq(0, declineValues()$wellLifeS*12-1/12, by= (1/12)))/12)/1000)
          
          df1$qiOil <- as.integer(df1$oilEUR * qiOil)
          df1$qiGas <- as.integer(df1$gasEUR * qiGas)
          df1$qiWater <- as.integer(df1$waterEUR * qiWater)
          
          price <- values$price
          
          price <- price %>% filter(DATE >= today())
          price <- price[capexValues()$spudToProd:nrow(price),]
          
          if(nrow(price) >= declineValues()$wellLifeS*12){
            price <- price[1:declineValues()$wellLifeS*12,]
          }
          
          #print(head(price))
          #well <- df1[230,]
          
          econSummary <- lapply(split(df1, df1[,'loc']), function (well) tryCatch({
            
            fitOil <- curtailed.q(arps.decline(
              as.numeric(well$qiOil)*365, as.nominal(as.numeric(declineValues()$DiOilS)), as.numeric(declineValues()$bOilS), as.nominal(as.numeric(declineValues()$DfOilS))),
              as.numeric(declineValues()$curtailOilS)/12.0,seq(0, declineValues()$wellLifeS-1/12, by= (1/12)))/12
            fitGas <- curtailed.q(arps.decline(
              as.numeric(well$qiGas)*365, as.nominal(as.numeric(declineValues()$DiGasS)), as.numeric(declineValues()$bGasS), as.nominal(as.numeric(declineValues()$DfGasS))),
              as.numeric(declineValues()$curtailGasS)/12.0,seq(0, declineValues()$wellLifeS-1/12, by= (1/12)))/12
            fitWater <- curtailed.q(arps.decline(
              as.numeric(well$qiWater)*365, as.nominal(as.numeric(declineValues()$DiWaterS)), as.numeric(declineValues()$bWaterS), as.nominal(as.numeric(declineValues()$DfWaterS))),
              as.numeric(declineValues()$curtailWaterS)/12.0,seq(0, declineValues()$wellLifeS-1/12, by= (1/12)))/12
            
            fit <- data.frame(month = seq(1, declineValues()$wellLifeS*12, 1), oil = fitOil, gas = fitGas, water=fitWater)
            fit$ngl <- fit$gas*priceValues()$nglYield/1000
            fit$netOil <- fit$oil*expenseValues()$nri/100
            fit$netSalesGas <- fit$gas*expenseValues()$nri/100*priceValues()$shrink/100*priceValues()$btu
            fit$netNGL <- fit$ngl*expenseValues()$nri/100
            
            fit$wti <- NA
            fit$wti[1:nrow(price)] <- price$WTI[1:nrow(price)]
            fit$wti <- na.locf(fit$wti)
            
            fit$hh <- NA
            fit$hh[1:nrow(price)] <- price$HH[1:nrow(price)]
            fit$hh <- na.locf(fit$hh)
            fit$nglPrice <- priceValues()$nglPrice/100*fit$wti
            
            fit$oilRevenue <- fit$netOil*(fit$wti - priceValues()$oilDiff)
            fit$gasRevenue <- fit$netSalesGas*(fit$hh - priceValues()$gasDiff)
            fit$nglRevenue <- fit$netNGL*fit$nglPrice
            fit$revenue <- fit$oilRevenue+fit$gasRevenue+fit$nglRevenue
            
            fit$opex <- expenseValues()$finalFixed + expenseValues()$varOilExp*fit$oil + expenseValues()$varGasExp*fit$gas + expenseValues()$varNGLExp*fit$ngl + expenseValues()$varWaterExp*fit$water + expenseValues()$wrkExp
            fit$opex[1:12] <- fit$opex[1:12] - expenseValues()$finalFixed + expenseValues()$yr1Fixed
            fit$opex[13:24] <- fit$opex[13:24] - expenseValues()$finalFixed + expenseValues()$yr2Fixed
            
            fit$tax <- fit$oilRevenue*expenseValues()$sevOil/100 + fit$gas*expenseValues()$sevGas + fit$revenue*expenseValues()$atx/100
            fit$nocf <- fit$revenue-fit$opex-fit$tax
            
            if(input$econCutoff == 'Yes'){
              limit <- which(fit$nocf >= 0)
              limit <- limit[length(limit)]
              fit <- fit[1:limit,]
            }
            
            fit$capex <- 0
            fit$capex[nrow(fit)] <- capexValues()$pna
            
            fit1 <- fit[1:capexValues()$spudToProd,]
            fit1[1:nrow(fit1), 1:length(fit1)] <- 0
            fit1$capex[1] <- well$perf*capexValues()$drillCapex
            fit1$capex[nrow(fit1)] <- well$perf*capexValues()$compCapex
            fit <- rbind(fit1, fit)
            fit$month <- seq(0, (nrow(fit)-1), 1)
            fit$fcf <- fit$nocf - fit$capex
            fit$npv10 <- fit$fcf/(1.1^(fit$month/12))
            
            
            well$npv10 <- sum(fit$npv10)
            well$irr <- IRRcalc(fit$fcf, fit$month)
            well
          },
          error = function(e) {
            e
            NULL
          }))
          
          df1 <- dplyr::bind_rows(econSummary)
          df1 <- df1 %>% filter(irr >= expenseValues()$irrCutoff/100)
          
          if(nrow(df1) == 0){
            df1 <- data.frame(result = 'No Economic Inventory Remaining')
          } else {
            
            df1 <- df1 %>% mutate(reservoir = input$reservoirSelect) %>% mutate(oilEUR = oilEUR*remWells, qiOil = qiOil*remWells,
                                                                                                            gasEUR = gasEUR*remWells, qiGas = qiGas*remWells,
                                                                                                            waterEUR = waterEUR*remWells, qiWater = qiWater*remWells,
                                                                                                            perf = perf*remWells, tvd = tvd*remWells, ppf=ppf*remWells,
                                                                                                            irr = irr*remWells, npv10 = npv10*remWells) %>%
              group_by(reservoir) %>%
              summarise(remWells = sum(remWells), oilEUR = sum(oilEUR), gasEUR = sum(gasEUR), waterEUR = sum(waterEUR),
                        perf = sum(perf), tvd = sum(tvd), ppf=sum(ppf),
                        irr = sum(irr), npv10 = sum(npv10)) %>% ungroup() %>%
              mutate(oilEUR = as.integer(oilEUR/remWells), gasEUR = as.integer(gasEUR/remWells), waterEUR = as.integer(waterEUR/remWells),
                     perf = as.integer(perf/remWells), tvd = as.integer(tvd/remWells), ppf = as.integer(ppf/remWells), 
                     irr = irr/remWells, npv10 = npv10/remWells)
            
            df1$irr <- percent(df1$irr)
            df1$npv10 <- dollar(df1$npv10/1000)
            
            df1$acres <- acreTotal
            
            df1 <- df1[,c('reservoir', 'acres', 'remWells', 'oilEUR', 'gasEUR', 'waterEUR', 'perf', 'tvd', 'ppf', 'irr', 'npv10')]
            
            names(df1) <- c('RESERVOIR', 'GROSS ACRES',  'REMAINING ECONOMIC INVENTORY', 'GROSS OIL EUR/WELL, MBO', 'GROSS GAS EUR/WELL, BCF', 'GROSS WATER EUR/WELL, MBW',
                            'LATERAL LENGTH, FT', 'AVERAGE TVD, FT', 'PROPPANT LOADING, LB/FT', 'IRR', 'NPV-10, US$thousands')
            
            
            
          }
          updateButton(session, 'calcInv', 'Calculate Inventory', style = 'primary', size = 'small')
          shinyjs::enable('calcInv')
          
        }
        
        #str(df1)
        values$invTable <- as.data.frame(df1)
      }
    })
    
    output$invTable <- DT::renderDataTable({
      if(is.null(values$invTable)){
        NULL
      } else {
        DT::datatable(values$invTable, rownames = FALSE,
                      extensions = c('Buttons', 'Scroller'), 
                      options = list(
                        dom = 'Bfrtip',
                        scrollX = TRUE,
                        scrollY = FALSE,
                        deferRender = TRUE,
                        paging = FALSE,
                        searching = FALSE,
                        buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print')
                      ))  
      }
    })
    
    
    output$pdpValue <- DT::renderDataTable({
      if(is.null(values$pdpFcst)){
        NULL
      } else {
        test1 <- values$pdpFcst

        test1$month <- seq(0, (nrow(test1)-1), 1)
        test1$npv <- test1$fcf/((1+expenseValues()$pdpDiscRate/100)^(test1$month/12))
        npv <- dollar(as.integer(sum(test1$npv)/1000))
        #print(npv)

        output$pdpPV <- renderText(paste0('PDP PV-',expenseValues()$pdpDiscRate,': ', npv,' (in thousands)'))
        if(input$pdpView == 'Monthly'){

          test1 <- test1 %>% mutate(WTI = dollar(WTI), HH = dollar(HH), nglPrice = dollar(nglPrice),
                                    oilRevenue = dollar(oilRevenue), gasRevenue = dollar(gasRevenue),
                                    nglRevenue = dollar(nglRevenue), revenue = dollar(revenue),
                                    opex = dollar(opex), tax = dollar(tax), nocf = dollar(nocf),
                                    capex = dollar(capex), fcf = dollar(fcf))



          test1$date <- as.Date(test1$date)
          test1 <- test1[,c('date', 'oil', 'gas', 'water', 'ngl', 'netOil', 'netSalesGas', 'netNGL', 'WTI', 'HH', 'nglPrice', 'oilRevenue',
                            'gasRevenue', 'nglRevenue', 'revenue', 'opex', 'tax', 'nocf', 'capex', 'fcf')]



          names(test1) <- c('DATE', 'GROSS OIL, BBLS', 'GROSS GAS, MCF', 'GROSS WATER, BBLS', 'GROSS NGL, BBLS',
                            'NET OIL, BBLS', 'NET SALES GAS, MCF', 'NET NGL, BBLS', 'WTI PRICE, $/BBL', 'HENRY HUB PRICE, $/MCF', 'NGL PRICE, $/BBL',
                            'OIL REVENUE', 'GAS REVENUE', 'NGL REVENUE', 'TOTAL REVENUE', 'OPERATING EXPENSES', 'PRODUCTION TAXES', 'NET OPERATING CASH FLOW',
                            'CAPITAL EXPENDITURES', 'FREE CASH FLOW')

          values$pdpValue <- test1
        } else if(input$pdpView == 'Quarterly'){


          test1$quarter <- quarter(test1$date)
          test1$year <- year(test1$date)
          test1 <- test1 %>% group_by(quarter, year) %>% summarise(oil = as.integer(sum(oil)), gas = as.integer(sum(gas)),
                                                                   water = as.integer(sum(water)), ngl = as.integer(sum(ngl)),
                                                                   netOil = as.integer(sum(netOil)), netSalesGas = as.integer(sum(netSalesGas)),
                                                                   netNGL = as.integer(sum(netNGL)), WTI = (mean(WTI)), HH = (mean(HH)),
                                                                   nglPrice = (mean(nglPrice)), oilRevenue = (as.integer(sum(oilRevenue))),
                                                                   gasRevenue = (as.integer(sum(gasRevenue))), nglRevenue = (as.integer(sum(nglRevenue))),
                                                                   revenue = (as.integer(sum(revenue))),opex = (as.integer(sum(opex))),
                                                                   tax = (as.integer(sum(tax))), nocf =(as.integer(sum(nocf))),
                                                                   capex = (as.integer(sum(capex))), fcf = as.integer(sum(fcf))) %>% ungroup() %>%
            arrange(year, quarter)

          test1 <- test1 %>% mutate(WTI = dollar(WTI), HH = dollar(HH), nglPrice = dollar(nglPrice),
                                    oilRevenue = dollar(oilRevenue), gasRevenue = dollar(gasRevenue),
                                    nglRevenue = dollar(nglRevenue), revenue = dollar(revenue),
                                    opex = dollar(opex), tax = dollar(tax), nocf = dollar(nocf),
                                    capex = dollar(capex), fcf = dollar(fcf))

          test1 <- test1[,c('year', 'quarter', 'oil', 'gas', 'water', 'ngl', 'netOil', 'netSalesGas', 'netNGL', 'WTI', 'HH', 'nglPrice', 'oilRevenue',
                            'gasRevenue', 'nglRevenue', 'revenue', 'opex', 'tax', 'nocf', 'capex', 'fcf')]



          names(test1) <- c('YEAR', 'QUARTER', 'GROSS OIL, BBLS', 'GROSS GAS, MCF', 'GROSS WATER, BBLS', 'GROSS NGL, BBLS',
                            'NET OIL, BBLS', 'NET SALES GAS, MCF', 'NET NGL, BBLS', 'WTI PRICE, $/BBL', 'HENRY HUB PRICE, $/MCF', 'NGL PRICE, $/BBL',
                            'OIL REVENUE', 'GAS REVENUE', 'NGL REVENUE', 'TOTAL REVENUE', 'OPERATING EXPENSES', 'PRODUCTION TAXES', 'NET OPERATING CASH FLOW',
                            'CAPITAL EXPENDITURES', 'FREE CASH FLOW')

          values$pdpValue <- test1
        } else {

          test1$year <- year(test1$date)
          test1 <- test1 %>% group_by(year) %>% summarise(oil = as.integer(sum(oil)), gas = as.integer(sum(gas)),
                                                          water = as.integer(sum(water)), ngl = as.integer(sum(ngl)),
                                                          netOil = as.integer(sum(netOil)), netSalesGas = as.integer(sum(netSalesGas)),
                                                          netNGL = as.integer(sum(netNGL)), WTI = (mean(WTI)), HH = (mean(HH)),
                                                          nglPrice = (mean(nglPrice)), oilRevenue = (as.integer(sum(oilRevenue))),
                                                          gasRevenue = (as.integer(sum(gasRevenue))), nglRevenue = (as.integer(sum(nglRevenue))),
                                                          revenue = (as.integer(sum(revenue))),opex = (as.integer(sum(opex))),
                                                          tax = (as.integer(sum(tax))), nocf =(as.integer(sum(nocf))),
                                                          capex = (as.integer(sum(capex))), fcf = as.integer(sum(fcf))) %>% ungroup() %>%
            arrange(year)

          test1 <- test1 %>% mutate(WTI = dollar(WTI), HH = dollar(HH), nglPrice = dollar(nglPrice),
                                    oilRevenue = dollar(oilRevenue), gasRevenue = dollar(gasRevenue),
                                    nglRevenue = dollar(nglRevenue), revenue = dollar(revenue),
                                    opex = dollar(opex), tax = dollar(tax), nocf = dollar(nocf),
                                    capex = dollar(capex), fcf = dollar(fcf))

          test1 <- test1[,c('year',  'oil', 'gas', 'water', 'ngl', 'netOil', 'netSalesGas', 'netNGL', 'WTI', 'HH', 'nglPrice', 'oilRevenue',
                            'gasRevenue', 'nglRevenue', 'revenue', 'opex', 'tax', 'nocf', 'capex', 'fcf')]



          names(test1) <- c('YEAR',  'GROSS OIL, BBLS', 'GROSS GAS, MCF', 'GROSS WATER, BBLS', 'GROSS NGL, BBLS',
                            'NET OIL, BBLS', 'NET SALES GAS, MCF', 'NET NGL, BBLS', 'WTI PRICE, $/BBL', 'HENRY HUB PRICE, $/MCF', 'NGL PRICE, $/BBL',
                            'OIL REVENUE', 'GAS REVENUE', 'NGL REVENUE', 'TOTAL REVENUE', 'OPERATING EXPENSES', 'PRODUCTION TAXES', 'NET OPERATING CASH FLOW',
                            'CAPITAL EXPENDITURES', 'FREE CASH FLOW')

          values$pdpValue <- test1
        }


        DT::datatable(values$pdpValue, rownames = FALSE,
                      extensions = c('Buttons', 'Scroller'),
                      options = list(
                        dom = 'Bfrtip',
                        scrollX = TRUE,
                        scrollY = TRUE,
                        deferRender = TRUE,
                        paging = FALSE,
                        searching = FALSE,
                        buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print')
                      ))
      }
    })
    
    
    observe({
      if(is.null(values$invTable)){
        updateNumericInput(session, 'yr1Dev', value = 0)
        updateNumericInput(session, 'yr2Dev', value = 0)
        updateNumericInput(session, 'yr3Dev', value = 0)
        updateNumericInput(session, 'yr4Dev', value = 0)
        updateNumericInput(session, 'yr5Dev', value = 0)
        updateNumericInput(session, 'yr6Dev', value = 0)
        updateNumericInput(session, 'yr7Dev', value = 0)
        updateNumericInput(session, 'yr8Dev', value = 0)
        updateNumericInput(session, 'yr9Dev', value = 0)
        updateNumericInput(session, 'yr10Dev', value = 0)
      } else {
        val1 <- values$invTable[,3]/10
        updateNumericInput(session, 'yr1Dev', value = val1)
        updateNumericInput(session, 'yr2Dev', value = val1)
        updateNumericInput(session, 'yr3Dev', value = val1)
        updateNumericInput(session, 'yr4Dev', value = val1)
        updateNumericInput(session, 'yr5Dev', value = val1)
        updateNumericInput(session, 'yr6Dev', value = val1)
        updateNumericInput(session, 'yr7Dev', value = val1)
        updateNumericInput(session, 'yr8Dev', value = val1)
        updateNumericInput(session, 'yr9Dev', value = val1)
        updateNumericInput(session, 'yr10Dev', value = val1)
      }
      
      
    })
    
    
    observeEvent(input$yr1Dev, {
      if(is.null(values$invTable)){
        NULL
      } else {
        output$rem1 <- renderText(paste0('Remaining Wells: ',as.integer(values$invTable[,3] - input$yr1Dev)))
      }
    })
    
    observeEvent(input$yr2Dev, {
      if(is.null(values$invTable)){
        NULL
      } else {
        output$rem2 <- renderText(paste0('Remaining Wells: ',as.integer(values$invTable[,3] - input$yr1Dev-input$yr2Dev)))
      }
    })
    
    observeEvent(input$yr3Dev, {
      if(is.null(values$invTable)){
        NULL
      } else {
        output$rem3 <- renderText(paste0('Remaining Wells: ',as.integer(values$invTable[,3] - 
                                                                          input$yr1Dev-input$yr2Dev-input$yr3Dev)))
      }
    })
    
    observeEvent(input$yr4Dev, {
      if(is.null(values$invTable)){
        NULL
      } else {
        output$rem4 <- renderText(paste0('Remaining Wells: ',as.integer(values$invTable[,3] - 
                                                                          input$yr1Dev-input$yr2Dev-
                                                                          input$yr3Dev -
                                                                          input$yr4Dev)))
      }
    })
    
    observeEvent(input$yr5Dev, {
      if(is.null(values$invTable)){
        NULL
      } else {
        output$rem5 <- renderText(paste0('Remaining Wells: ',as.integer(values$invTable[,3] - 
                                                                          input$yr1Dev-input$yr2Dev-
                                                                          input$yr3Dev -
                                                                          input$yr4Dev -
                                                                          input$yr5Dev)))
      }
    })
    
    observeEvent(input$yr6Dev, {
      if(is.null(values$invTable)){
        NULL
      } else {
        output$rem6 <- renderText(paste0('Remaining Wells: ',as.integer(values$invTable[,3] - 
                                                                          input$yr1Dev-input$yr2Dev-
                                                                          input$yr3Dev -
                                                                          input$yr4Dev -
                                                                          input$yr5Dev -
                                                                          input$yr6Dev)))
      }
    })
    
    observeEvent(input$yr7Dev, {
      if(is.null(values$invTable)){
        NULL
      } else {
        output$rem7 <- renderText(paste0('Remaining Wells: ',as.integer(values$invTable[,3] - 
                                                                          input$yr1Dev-input$yr2Dev-
                                                                          input$yr3Dev -
                                                                          input$yr4Dev -
                                                                          input$yr5Dev -
                                                                          input$yr6Dev -
                                                                          input$yr7Dev)))
      }
    })
    
    observeEvent(input$yr8Dev, {
      if(is.null(values$invTable)){
        NULL
      } else {
        output$rem8 <- renderText(paste0('Remaining Wells: ',as.integer(values$invTable[,3] - 
                                                                          input$yr1Dev-input$yr2Dev-
                                                                          input$yr3Dev -
                                                                          input$yr4Dev -
                                                                          input$yr5Dev -
                                                                          input$yr6Dev -
                                                                          input$yr7Dev -
                                                                          input$yr8Dev)))
      }
    })
    
    observeEvent(input$yr9Dev, {
      if(is.null(values$invTable)){
        NULL
      } else {
        output$rem9 <- renderText(paste0('Remaining Wells: ',as.integer(values$invTable[,3] - 
                                                                          input$yr1Dev-input$yr2Dev-
                                                                          input$yr3Dev -
                                                                          input$yr4Dev -
                                                                          input$yr5Dev -
                                                                          input$yr6Dev -
                                                                          input$yr7Dev -
                                                                          input$yr8Dev -
                                                                          input$yr9Dev)))
      }
    })
    
    observeEvent(input$yr10Dev, {
      if(is.null(values$invTable)){
        NULL
      } else {
        
        remWells <- values$invTable[,3] - 
          input$yr1Dev-
          input$yr2Dev-
          input$yr3Dev -
          input$yr4Dev -
          input$yr5Dev -
          input$yr6Dev -
          input$yr7Dev -
          input$yr8Dev -
          input$yr9Dev
        #print(remWells)
        #print(input$yr10Dev)
        output$rem10 <- renderText(paste0('Remaining Months: ', as.integer(12*remWells/input$yr10Dev)))
      }
    })
    
    
    observe({
      if(is.null(values$invTable)){
        shinyjs::disable('addPud')
      } else {
        shinyjs::enable('addPud')
        if(input$yr1Dev >= values$invTable[,3]){
          updateNumericInput(session, 'yr1Dev', value = values$invTable[,3])
          updateNumericInput(session, 'yr2Dev', value = 0)
          updateNumericInput(session, 'yr3Dev', value = 0)
          updateNumericInput(session, 'yr4Dev', value = 0)
          updateNumericInput(session, 'yr5Dev', value = 0)
          updateNumericInput(session, 'yr6Dev', value = 0)
          updateNumericInput(session, 'yr7Dev', value = 0)
          updateNumericInput(session, 'yr8Dev', value = 0)
          updateNumericInput(session, 'yr9Dev', value = 0)
          updateNumericInput(session, 'yr10Dev', value = 0)
          
        } else if(input$yr1Dev + input$yr2Dev >= values$invTable[,3]){
          
          updateNumericInput(session, 'yr2Dev', value = values$invTable[,3] - input$yr1Dev)
          updateNumericInput(session, 'yr3Dev', value = 0)
          updateNumericInput(session, 'yr4Dev', value = 0)
          updateNumericInput(session, 'yr5Dev', value = 0)
          updateNumericInput(session, 'yr6Dev', value = 0)
          updateNumericInput(session, 'yr7Dev', value = 0)
          updateNumericInput(session, 'yr8Dev', value = 0)
          updateNumericInput(session, 'yr9Dev', value = 0)
          updateNumericInput(session, 'yr10Dev', value = 0)
          
        }else if(input$yr1Dev + input$yr2Dev + input$yr3Dev >= values$invTable[,3]){
          
          updateNumericInput(session, 'yr3Dev', value = values$invTable[,3] - input$yr1Dev - input$yr2Dev)
          updateNumericInput(session, 'yr4Dev', value = 0)
          updateNumericInput(session, 'yr5Dev', value = 0)
          updateNumericInput(session, 'yr6Dev', value = 0)
          updateNumericInput(session, 'yr7Dev', value = 0)
          updateNumericInput(session, 'yr8Dev', value = 0)
          updateNumericInput(session, 'yr9Dev', value = 0)
          updateNumericInput(session, 'yr10Dev', value = 0)
          
        }else if(input$yr1Dev + input$yr2Dev + input$yr3Dev+ input$yr4Dev >= values$invTable[,3]){
          
          updateNumericInput(session, 'yr4Dev', value = values$invTable[,3] - input$yr1Dev - input$yr2Dev - input$yr3Dev)
          updateNumericInput(session, 'yr5Dev', value = 0)
          updateNumericInput(session, 'yr6Dev', value = 0)
          updateNumericInput(session, 'yr7Dev', value = 0)
          updateNumericInput(session, 'yr8Dev', value = 0)
          updateNumericInput(session, 'yr9Dev', value = 0)
          updateNumericInput(session, 'yr10Dev', value = 0)
          
        }else if(input$yr1Dev + input$yr2Dev + input$yr3Dev+ input$yr4Dev + input$yr5Dev>= values$invTable[,3]){
          
          updateNumericInput(session, 'yr5Dev', value = values$invTable[,3] - input$yr1Dev - input$yr2Dev - input$yr3Dev - input$yr4Dev)
          updateNumericInput(session, 'yr6Dev', value = 0)
          updateNumericInput(session, 'yr7Dev', value = 0)
          updateNumericInput(session, 'yr8Dev', value = 0)
          updateNumericInput(session, 'yr9Dev', value = 0)
          updateNumericInput(session, 'yr10Dev', value = 0)
          
        }else if(input$yr1Dev + input$yr2Dev + 
                 input$yr3Dev+ input$yr4Dev + 
                 input$yr5Dev + input$yr6Dev>= values$invTable[,3]){
          
          updateNumericInput(session, 'yr6Dev', value = values$invTable[,3] - 
                               input$yr1Dev - input$yr2Dev - 
                               input$yr3Dev - input$yr4Dev -
                               input$yr5Dev)
          updateNumericInput(session, 'yr7Dev', value = 0)
          updateNumericInput(session, 'yr8Dev', value = 0)
          updateNumericInput(session, 'yr9Dev', value = 0)
          updateNumericInput(session, 'yr10Dev', value = 0)
          
        }else if(input$yr1Dev + input$yr2Dev + 
                 input$yr3Dev+ input$yr4Dev + 
                 input$yr5Dev + input$yr6Dev +
                 input$yr7Dev >= values$invTable[,3]){
          
          updateNumericInput(session, 'yr7Dev', value = values$invTable[,3] - 
                               input$yr1Dev - input$yr2Dev - 
                               input$yr3Dev - input$yr4Dev -
                               input$yr5Dev - input$yr6Dev)
          updateNumericInput(session, 'yr8Dev', value = 0)
          updateNumericInput(session, 'yr9Dev', value = 0)
          updateNumericInput(session, 'yr10Dev', value = 0)
          
        }else if(input$yr1Dev + input$yr2Dev + 
                 input$yr3Dev+ input$yr4Dev + 
                 input$yr5Dev + input$yr6Dev +
                 input$yr7Dev + input$yr8Dev >= values$invTable[,3]){
          
          updateNumericInput(session, 'yr8Dev', value = values$invTable[,3] - 
                               input$yr1Dev - input$yr2Dev - 
                               input$yr3Dev - input$yr4Dev -
                               input$yr5Dev - input$yr6Dev -
                               input$yr7Dev)
          updateNumericInput(session, 'yr9Dev', value = 0)
          updateNumericInput(session, 'yr10Dev', value = 0)
          
        }else if(input$yr1Dev + input$yr2Dev + 
                 input$yr3Dev+ input$yr4Dev + 
                 input$yr5Dev + input$yr6Dev +
                 input$yr7Dev + input$yr8Dev +
                 input$yr9Dev >= values$invTable[,3]){
          
          updateNumericInput(session, 'yr9Dev', value = values$invTable[,3] - 
                               input$yr1Dev - input$yr2Dev - 
                               input$yr3Dev - input$yr4Dev -
                               input$yr5Dev - input$yr6Dev -
                               input$yr7Dev - input$yr8Dev)
          updateNumericInput(session, 'yr10Dev', value = 0)
          
        }
        
      }
    })
    
    observeEvent(input$addPud, {
      if(is.null(input$yr1Dev)||is.null(input$yr2Dev)||
         is.null(input$yr3Dev)||is.null(input$yr4Dev)||
         is.null(input$yr5Dev)||is.null(input$yr6Dev)||
         is.null(input$yr7Dev)||is.null(input$yr8Dev)||
         is.null(input$yr9Dev)||is.null(input$yr10Dev)||is.null(values$invTable)){
        NULL
      } else {
        updateButton(session, 'addPud', 'Thinking.....', style = 'danger', size = 'small')
        shinyjs::disable('addPud')
        price <- values$price %>% filter(DATE >= input$pudStart)
        
        #test1 <- test1 %>% mutate(date = paste0(month(date),'/01/',year(date))) %>% mutate(date = as.POSIXct(date, format = '%m/%d/%Y'))
        names(price) <- c('date', 'wti', 'hh')
        price <- price %>% mutate(date = paste0(month(date),'/01/',year(date))) %>% mutate(date = as.POSIXct(date, format = '%m/%d/%Y'))
        price <- price %>% group_by(date) %>% summarise_all(mean)
        df1 <- values$invTable
        #print(input$yr10Dev)
        names(df1) <- c('reservoir', 'acres', 'wells', 'oilEUR', 'gasEUR', 'waterEUR', 'perf', 'tvd', 'ppf', 'irr', 'npv10')
        #print(head(df1))
        if(input$yr10Dev == 0||is.null(input$yr10Dev)){
          totalMonths <- 120
        } else {
          totalMonths <- (df1$wells -
                            input$yr1Dev-
                            input$yr2Dev-
                            input$yr3Dev -
                            input$yr4Dev -
                            input$yr5Dev -
                            input$yr6Dev -
                            input$yr7Dev -
                            input$yr8Dev -
                            input$yr9Dev)/input$yr10Dev*12+120
        }
        #print(totalMonths)
        #print(head(df2))
        df2 <- data.frame(month = seq(0, (totalMonths-1), 1))
        #print(head(df2))
        #print(head(price))
        df2$date <- price$date[1]
        df2 <- as.data.frame(df2)
        #print(head(df2))
        df2$month1 <- as.integer(df2$month)
        df2 <- df2 %>% mutate(date = date %m+% months(month1))
        df2 <- subset(df2, select = -c(month1))
        df2 <- as.data.frame(df2)
        #df2$date <- df2$date %m+% months(df2$month)
        #print(nrow(df2))
        df2$wells <- 0
        df2$wells[1:12] <- input$yr1Dev/12
        df2$wells[13:24] <- input$yr2Dev/12
        df2$wells[25:36] <- input$yr3Dev/12
        df2$wells[37:48] <- input$yr4Dev/12
        df2$wells[49:60] <- input$yr5Dev/12
        df2$wells[61:72] <- input$yr6Dev/12
        df2$wells[73:84] <- input$yr7Dev/12
        df2$wells[85:96] <- input$yr8Dev/12
        df2$wells[97:108] <- input$yr9Dev/12
        df2$wells[109:nrow(df2)] <- input$yr10Dev/12
        #print(head(df2))
        #print(nrow(df2))

        
        qiOil <- declineValues()$qiOilS/(sum(curtailed.q(arps.decline(
          as.numeric(declineValues()$qiOilS)*365, as.nominal(as.numeric(declineValues()$DiOilS)), as.numeric(declineValues()$bOilS), as.nominal(as.numeric(declineValues()$DfOilS))),
          as.numeric(declineValues()$curtailOilS)/12.0,seq(0, declineValues()$wellLifeS*12-1/12, by= (1/12)))/12)/1000)
        
        qiGas <- declineValues()$qiGasS/(sum(curtailed.q(arps.decline(
          as.numeric(declineValues()$qiGasS)*365, as.nominal(as.numeric(declineValues()$DiGasS)), as.numeric(declineValues()$bGasS), as.nominal(as.numeric(declineValues()$DfGasS))),
          as.numeric(declineValues()$curtailGasS)/12.0,seq(0, declineValues()$wellLifeS*12-1/12, by= (1/12)))/12)/1000)
        
        qiWater <- declineValues()$qiWaterS/(sum(curtailed.q(arps.decline(
          as.numeric(declineValues()$qiWaterS)*365, as.nominal(as.numeric(declineValues()$DiWaterS)), as.numeric(declineValues()$bWaterS), as.nominal(as.numeric(declineValues()$DfWaterS))),
          as.numeric(declineValues()$curtailWaterS)/12.0,seq(0, declineValues()$wellLifeS*12-1/12, by= (1/12)))/12)/1000)
        
        df2$qiOil <- as.integer(df1$oilEUR * qiOil)
        df2$qiGas <- as.integer(df1$gasEUR * qiGas)
        df2$qiWater <- as.integer(df1$waterEUR * qiWater)
        #print(head(df2))
        #print(head(df2))
        fitOil <- curtailed.q(arps.decline(
          as.numeric(df2$qiOil[1])*365, as.nominal(as.numeric(declineValues()$DiOilS)), as.numeric(declineValues()$bOilS), as.nominal(as.numeric(declineValues()$DfOilS))),
          as.numeric(declineValues()$curtailOilS)/12.0,seq(0, declineValues()$wellLifeS-1/12, by= (1/12)))/12
        fitGas <- curtailed.q(arps.decline(
          as.numeric(df2$qiGas[1])*365, as.nominal(as.numeric(declineValues()$DiGasS)), as.numeric(declineValues()$bGasS), as.nominal(as.numeric(declineValues()$DfGasS))),
          as.numeric(declineValues()$curtailGasS)/12.0,seq(0, declineValues()$wellLifeS-1/12, by= (1/12)))/12
        fitWater <- curtailed.q(arps.decline(
          as.numeric(df2$qiWater[1])*365, as.nominal(as.numeric(declineValues()$DiWaterS)), as.numeric(declineValues()$bWaterS), as.nominal(as.numeric(declineValues()$DfWaterS))),
          as.numeric(declineValues()$curtailWaterS)/12.0,seq(0, declineValues()$wellLifeS-1/12, by= (1/12)))/12
        
        fit <- data.frame(month = seq(1, declineValues()$wellLifeS*12, 1), oil = fitOil, gas = fitGas, water=fitWater)
        rm(fitOil, fitGas, fitWater)
        fit <- fit[!duplicated(fit),]
        fit$ngl <- fit$gas*priceValues()$nglYield/1000
        fit$netOil <- fit$oil*expenseValues()$nri/100
        fit$netSalesGas <- fit$gas*expenseValues()$nri/100*priceValues()$shrink/100*priceValues()$btu
        fit$netNGL <- fit$ngl*expenseValues()$nri/100
        df2$perf <- df1$perf
        df2 <- as.data.frame(df2)
        df2 <- df2 %>% filter(!is.na(month))
        df2 <- df2[!duplicated(df2$month),]
        
        
        econSummary <- lapply(split(df2, df2[,'month']), function (well) tryCatch({
          
          df <- fit
          df$date <- well$date %m+% months(capexValues()$spudToProd)
          
          df$date <- df$date %m+% months(df$month)
          df <- merge(df, price, by='date', all.x=TRUE)
          df$wti <- na.locf(df$wti)
          df$hh <- na.locf(df$hh)
          df$nglPrice <- priceValues()$nglPrice/100*df$wti
          
          df$oil <- df$oil * well$wells
          df$gas <- df$gas * well$wells
          df$ngl <- df$ngl * well$wells
          df$netOil <- df$netOil * well$wells
          df$netSalesGas <- df$netSalesGas * well$wells
          df$netNGL <- df$netNGL * well$wells
          df$water <- df$water * well$wells
          
          
          df$oilRevenue <- df$netOil*(df$wti - priceValues()$oilDiff)
          df$gasRevenue <- df$netSalesGas*(df$hh - priceValues()$gasDiff)
          df$nglRevenue <- df$netNGL*df$nglPrice
          df$revenue <- df$oilRevenue+df$gasRevenue+df$nglRevenue
          
          df$opex <- expenseValues()$finalFixed*well$wells + expenseValues()$varOilExp*df$oil + expenseValues()$varGasExp*df$gas + expenseValues()$varNGLExp*df$ngl + expenseValues()$varWaterExp*df$water + expenseValues()$wrkExp*well$wells
          df$opex[1:12] <- df$opex[1:12] - expenseValues()$finalFixed*well$wells + expenseValues()$yr1Fixed*well$wells
          df$opex[13:24] <- df$opex[13:24] - expenseValues()$finalFixed*well$wells + expenseValues()$yr2Fixed*well$wells
          
          
          df$tax <- df$oilRevenue*expenseValues()$sevOil/100 + df$gas*expenseValues()$sevGas + df$revenue*expenseValues()$atx/100
          df$nocf <- df$revenue-df$opex-df$tax
          
          if(input$econCutoff == 'Yes'){
            limit <- which(df$nocf >= 0)
            limit <- limit[length(limit)]
            df <- df[1:limit,]
          }
          
          df$capex <- 0
          df$capex[nrow(df)] <- capexValues()$pna*well$wells
          
          fit1 <- df
          monthsOn <- capexValues()$spudToProd
          
          fit1 <- fit1[1:monthsOn,]
          
          fit1[1:(nrow(fit1)), 2:(ncol(fit1))] <- 0
          #print(head(fit1))
          fit1$capex[1] <- well$perf*capexValues()$drillCapex*well$wells
          fit1$capex[nrow(fit1)] <- well$perf*capexValues()$compCapex*well$wells
          fit1$month <- seq(-1*(monthsOn),-1, 1)
          fit1$date <- df$date[1]
          fit1$date <- fit1$date %m+% months(fit1$month)
          
          df <- rbind(fit1, df)
          
          
          df$fcf <- df$nocf - df$capex
          df$wells <- well$wells
          
          df <- subset(df, select = -c(month))
          df <- as.data.frame(df)
          #print(head(df))
          df
        },
        error = function(e) {
          e
          NULL
        }))
        
        df <- dplyr::bind_rows(econSummary)
        
        df <- subset(df, select = -c(wells))
        df <- df[!duplicated(df),]
        df <- df %>% group_by(date) %>% summarise(oil = sum(oil), gas = sum(gas), water = sum(water), ngl = sum(ngl),
                                                  netOil = sum(netOil), netSalesGas=sum(netSalesGas), netNGL = sum(netNGL),
                                                  wti = mean(wti), hh = mean(hh), nglPrice = mean(nglPrice), oilRevenue = sum(oilRevenue),
                                                  gasRevenue = sum(gasRevenue), nglRevenue = sum(nglRevenue), revenue = sum(revenue),
                                                  opex = sum(opex), tax = sum(tax), nocf = sum(nocf), capex = sum(capex), fcf = sum(fcf)) %>% ungroup()
        df <- as.data.frame(df)
        
        df$oil <- df$oil * expenseValues()$wi2/100
        df$gas <- df$gas * expenseValues()$wi2/100
        df$water <- df$water * expenseValues()$wi2/100
        df$ngl <- df$ngl * expenseValues()$wi2/100
        df$netOil <- df$netOil * expenseValues()$wi2/100
        df$netSalesGas <- df$netSalesGas * expenseValues()$wi2/100
        df$netNGL <- df$netNGL * expenseValues()$wi2/100
        
        df$oilRevenue <- df$oilRevenue* expenseValues()$wi2/100
        df$gasRevenue <- df$gasRevenue * expenseValues()$wi2/100
        df$nglRevenue <- df$nglRevenue* expenseValues()$wi2/100
        df$revenue <- df$revenue* expenseValues()$wi2/100
        
        df$opex <- df$opex* expenseValues()$wi2/100
        df$tax <- df$tax* expenseValues()$wi2/100
        df$nocf <- df$nocf* expenseValues()$wi2/100
        df$capex <- df$capex* expenseValues()$wi2/100
        df$fcf <- df$fcf* expenseValues()$wi2/100
        #df$wells <- df$wells* expenseValues()$wi2/100
        
        df <- df %>% filter(date >= input$pdpEffDate)
        #print(head(df))
        values$pudFcst <- df
        updateButton(session, 'addPud', 'Add to Forecast', style = 'primary', size = 'small')
        shinyjs::enable('addPud')
      }
    })
    
    output$pudValue <- DT::renderDataTable({
      if(is.null(values$pudFcst)){
        NULL
      } else {
        test1 <- values$pudFcst
        
        test1$month <- seq(0, (nrow(test1)-1), 1)
        test1$npv <- test1$fcf/((1+expenseValues()$pudDiscRate/100)^(test1$month/12))
        npv <- dollar(as.integer(sum(test1$npv)/1000))
        #print(npv)
        
        output$pudPV <- renderText(paste0('Development PV-',expenseValues()$pudDiscRate,': ', npv,' (in thousands)'))
        if(input$pudView == 'Monthly'){
          
          test1 <- test1 %>% mutate(WTI = dollar(wti), HH = dollar(hh), nglPrice = dollar(nglPrice),
                                    oilRevenue = dollar(oilRevenue), gasRevenue = dollar(gasRevenue),
                                    nglRevenue = dollar(nglRevenue), revenue = dollar(revenue),
                                    opex = dollar(opex), tax = dollar(tax), nocf = dollar(nocf),
                                    capex = dollar(capex), fcf = dollar(fcf))
          
          test1$oil <- as.integer(test1$oil)
          test1$gas <- as.integer(test1$gas)
          test1$water <- as.integer(test1$water)
          test1$ngl <- as.integer(test1$ngl)
          test1$netOil <- as.integer(test1$netOil)
          test1$netSalesGas <- as.integer(test1$netSalesGas)
          test1$netNGL <- as.integer(test1$netNGL)
          
          test1$date <- as.Date(test1$date)
          test1 <- test1[,c('date', 'oil', 'gas', 'water', 'ngl', 'netOil', 'netSalesGas', 'netNGL', 'WTI', 'HH', 'nglPrice', 'oilRevenue',
                            'gasRevenue', 'nglRevenue', 'revenue', 'opex', 'tax', 'nocf', 'capex', 'fcf')]
          
          
          
          names(test1) <- c('DATE', 'GROSS OIL, BBLS', 'GROSS GAS, MCF', 'GROSS WATER, BBLS', 'GROSS NGL, BBLS',
                            'NET OIL, BBLS', 'NET SALES GAS, MCF', 'NET NGL, BBLS', 'WTI PRICE, $/BBL', 'HENRY HUB PRICE, $/MCF', 'NGL PRICE, $/BBL',
                            'OIL REVENUE', 'GAS REVENUE', 'NGL REVENUE', 'TOTAL REVENUE', 'OPERATING EXPENSES', 'PRODUCTION TAXES', 'NET OPERATING CASH FLOW',
                            'CAPITAL EXPENDITURES', 'FREE CASH FLOW')
          
          values$pudValue <- test1
        } else if(input$pudView == 'Quarterly'){
          
          
          test1$quarter <- quarter(test1$date)
          test1$year <- year(test1$date)
          test1 <- test1 %>% group_by(quarter, year) %>% summarise(oil = as.integer(sum(oil)), gas = as.integer(sum(gas)),
                                                                   water = as.integer(sum(water)), ngl = as.integer(sum(ngl)),
                                                                   netOil = as.integer(sum(netOil)), netSalesGas = as.integer(sum(netSalesGas)),
                                                                   netNGL = as.integer(sum(netNGL)), WTI = (mean(wti)), HH = (mean(hh)),
                                                                   nglPrice = (mean(nglPrice)), oilRevenue = (as.integer(sum(oilRevenue))),
                                                                   gasRevenue = (as.integer(sum(gasRevenue))), nglRevenue = (as.integer(sum(nglRevenue))),
                                                                   revenue = (as.integer(sum(revenue))),opex = (as.integer(sum(opex))),
                                                                   tax = (as.integer(sum(tax))), nocf =(as.integer(sum(nocf))),
                                                                   capex = (as.integer(sum(capex))), fcf = as.integer(sum(fcf))) %>% ungroup() %>%
            arrange(year, quarter)
          
          test1 <- test1 %>% mutate(WTI = dollar(WTI), HH = dollar(HH), nglPrice = dollar(nglPrice),
                                    oilRevenue = dollar(oilRevenue), gasRevenue = dollar(gasRevenue),
                                    nglRevenue = dollar(nglRevenue), revenue = dollar(revenue),
                                    opex = dollar(opex), tax = dollar(tax), nocf = dollar(nocf),
                                    capex = dollar(capex), fcf = dollar(fcf))
          
          test1 <- test1[,c('year', 'quarter', 'oil', 'gas', 'water', 'ngl', 'netOil', 'netSalesGas', 'netNGL', 'WTI', 'HH', 'nglPrice', 'oilRevenue',
                            'gasRevenue', 'nglRevenue', 'revenue', 'opex', 'tax', 'nocf', 'capex', 'fcf')]
          
          
          
          names(test1) <- c('YEAR', 'QUARTER', 'GROSS OIL, BBLS', 'GROSS GAS, MCF', 'GROSS WATER, BBLS', 'GROSS NGL, BBLS',
                            'NET OIL, BBLS', 'NET SALES GAS, MCF', 'NET NGL, BBLS', 'WTI PRICE, $/BBL', 'HENRY HUB PRICE, $/MCF', 'NGL PRICE, $/BBL',
                            'OIL REVENUE', 'GAS REVENUE', 'NGL REVENUE', 'TOTAL REVENUE', 'OPERATING EXPENSES', 'PRODUCTION TAXES', 'NET OPERATING CASH FLOW',
                            'CAPITAL EXPENDITURES', 'FREE CASH FLOW')
          
          values$pudValue <- test1
        } else {
          
          test1$year <- year(test1$date)
          test1 <- test1 %>% group_by(year) %>% summarise(oil = as.integer(sum(oil)), gas = as.integer(sum(gas)),
                                                          water = as.integer(sum(water)), ngl = as.integer(sum(ngl)),
                                                          netOil = as.integer(sum(netOil)), netSalesGas = as.integer(sum(netSalesGas)),
                                                          netNGL = as.integer(sum(netNGL)), WTI = (mean(wti)), HH = (mean(hh)),
                                                          nglPrice = (mean(nglPrice)), oilRevenue = (as.integer(sum(oilRevenue))),
                                                          gasRevenue = (as.integer(sum(gasRevenue))), nglRevenue = (as.integer(sum(nglRevenue))),
                                                          revenue = (as.integer(sum(revenue))),opex = (as.integer(sum(opex))),
                                                          tax = (as.integer(sum(tax))), nocf =(as.integer(sum(nocf))),
                                                          capex = (as.integer(sum(capex))), fcf = as.integer(sum(fcf))) %>% ungroup() %>%
            arrange(year)
          
          test1 <- test1 %>% mutate(WTI = dollar(WTI), HH = dollar(HH), nglPrice = dollar(nglPrice),
                                    oilRevenue = dollar(oilRevenue), gasRevenue = dollar(gasRevenue),
                                    nglRevenue = dollar(nglRevenue), revenue = dollar(revenue),
                                    opex = dollar(opex), tax = dollar(tax), nocf = dollar(nocf),
                                    capex = dollar(capex), fcf = dollar(fcf))
          
          test1 <- test1[,c('year',  'oil', 'gas', 'water', 'ngl', 'netOil', 'netSalesGas', 'netNGL', 'WTI', 'HH', 'nglPrice', 'oilRevenue',
                            'gasRevenue', 'nglRevenue', 'revenue', 'opex', 'tax', 'nocf', 'capex', 'fcf')]
          
          
          
          names(test1) <- c('YEAR',  'GROSS OIL, BBLS', 'GROSS GAS, MCF', 'GROSS WATER, BBLS', 'GROSS NGL, BBLS',
                            'NET OIL, BBLS', 'NET SALES GAS, MCF', 'NET NGL, BBLS', 'WTI PRICE, $/BBL', 'HENRY HUB PRICE, $/MCF', 'NGL PRICE, $/BBL',
                            'OIL REVENUE', 'GAS REVENUE', 'NGL REVENUE', 'TOTAL REVENUE', 'OPERATING EXPENSES', 'PRODUCTION TAXES', 'NET OPERATING CASH FLOW',
                            'CAPITAL EXPENDITURES', 'FREE CASH FLOW')
          
          values$pudValue <- test1
        }
        
        
        DT::datatable(values$pudValue, rownames = FALSE,
                      extensions = c('Buttons', 'Scroller'),
                      options = list(
                        dom = 'Bfrtip',
                        scrollX = TRUE,
                        scrollY = TRUE,
                        deferRender = TRUE,
                        paging = FALSE,
                        searching = FALSE,
                        buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print')
                      ))
      }
    })
    
    output$pudStack <- renderBillboarder({
      if(is.null(values$pdpFcst)||is.null(values$pudFcst)){
        NULL
      } else {
        if(is.null(values$pdpFcst)){
          storeWarn<- getOption("warn")
          options(warn = -1)
          #print(head(values$pdpFcst))
          df <- as.data.frame(values$pudFcst) %>% gather(Component, Value, -c(date)) %>% filter(Component %in% input$prodValue2) %>% mutate(id = 'DEV') %>% mutate(id = as.factor(id))
          df <- as.data.frame(df)
          #print(head(df))
          # plott <- plotly::plot_ly(df, x=~date, y=~Value,type='area',  mode = 'stack', stackgroup='one',group_by = ~id, fillcolor = ~id, color = ~id)  %>%
          #   layout(title = 'PDP Forecast', yaxis = list(title='Volume in bbl/mcf, Value in $'), xaxis=  list(title = ''))
          
          
          plott <- billboarder(data = df) %>%
            bb_linechart(
              mapping = bbaes(x=date, y=Value), type = 'area'
            ) %>%
            bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE)) %>% 
            bb_colors_manual(
              Value = 'green'
            )%>%
            bb_legend(show = FALSE) %>%
            # bb_zoom(
            #   enabled = list(type = "drag"),
            #   resetButton = list(text = "Unzoom")
            # ) %>% 
            bb_subchart(show = TRUE, size = list(height = 30)) %>% 
            bb_labs(title = 'Development Summary',
                    y = 'Volumes in BBLS/MCF, Values in $',
                    caption = 'Data source: North Dakota State Government (https://www.dmr.nd.gov/oilgas/stats/statisticsvw.asp)')
          
          
          shinyjs::delay(expr =({
            options(warn = storeWarn)
          }) ,ms = 100)
        } else {
          storeWarn<- getOption("warn")
          options(warn = -1)
          #print(head(values$pdpFcst))
          df1 <- as.data.frame(values$pdpFcst) %>% gather(Component, Value, -c(date)) %>% filter(Component %in% input$prodValue2) %>% mutate(id = '1PDP') %>% mutate(id = as.factor(id))
          df <- as.data.frame(values$pudFcst) %>% gather(Component, Value, -c(date)) %>% filter(Component %in% input$prodValue2) %>% mutate(id = 'DEV') %>% mutate(id = as.factor(id))
          df <- as.data.frame(rbind(df1, df))
          
          df <- subset(df, select = -c(Component))
          df <- df %>% spread(id, Value)
          #print(head(df))
          # plott <- plotly::plot_ly(df, x=~date, y=~Value,type='area',  mode = 'stack', stackgroup='one',group_by = ~id, fillcolor = ~id, color = ~id)  %>%
          #   layout(title = 'PDP Forecast', yaxis = list(title='Volume in bbl/mcf, Value in $'), xaxis=  list(title = ''))
          
          plott <- billboarder() %>%
            bb_linechart(
              data = df, type = 'area'
            ) %>%
            bb_data(
              groups = list(list("DEV", "1PDP")),
              names = list("DEV" = "DEV", "1PDP" = "1PDP")
            ) %>%
            bb_x_axis(tick = list(format = "%Y-%m", fit = FALSE))%>% 
            bb_legend(position = "inset", inset = list(anchor = "top-right")) %>% 
            bb_colors_manual(
              '1PDP' = '#238443', 'DEV' = '#225EA8', opacity = 0.8
            ) %>%
            bb_y_axis(min = 0, padding = 0) %>% 
            # bb_zoom(
            #   enabled = list(type = "drag"),
            #   resetButton = list(text = "Unzoom")
            # ) %>% 
            bb_subchart(show = TRUE, size = list(height = 30)) %>% 
            bb_labs(title = 'PDP + Development Summary',
                    y = 'Volumes in BBLS/MCF, Values in $',
                    caption = 'Data source: North Dakota State Government (https://www.dmr.nd.gov/oilgas/stats/statisticsvw.asp)')
          
          
          shinyjs::delay(expr =({
            options(warn = storeWarn)
          }) ,ms = 100)
        }
       plott
      }
        
      
    
      
    })
  }
)

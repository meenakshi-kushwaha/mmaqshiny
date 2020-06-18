library(shiny)
library(Cairo)
library(DT)
library(plotly)
library(data.table)
library(stringr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(XML)
library(htmltools)
library(shinyjs)
library(lubridate)
library(zoo)
library(caTools)
library(xts)


ui <- fluidPage(
  h1("Explore Mobile Monitoring Air Pollution Data"),
  tags$head(
    tags$style(HTML(".sidebar {
                    height : 10vh; overflow-y : auto; font-size : 14px;
                    }" ,
                    ".shiny-output-error-validation {
                    color : red; font-size : 14px;
                    }"
    ))),
  sidebarLayout(position = "left",
                sidebarPanel(width = 3,
                  conditionalPanel(condition = "input.tabs1 == 6",
                                   tags$hr(),
                                   h4("Alarms! Check for any malfunction."),
                                   tags$hr()),
                  conditionalPanel(condition = "input.tabs1 == 4",
                                   tags$hr(),
                                   selectInput("palleInp", "Map this pollutant",
                                               "Select"),
                                   tags$hr()),
                  conditionalPanel(condition = "input.tabs1 == 3",
                                   tags$hr(),
                                   h4("Summary Statistics"),
                                   tags$hr()),
                  conditionalPanel(condition = "input.tabs1 == 2",
                                   tags$hr(),
                                   h4("Time series plots"),
                                   tags$hr()
                  ),
                  conditionalPanel(condition = "input.tabs1 == 1",
                                   tags$hr(),
                                   helpText("Choose mobile monitoring files."),
                                   tags$hr(),
                                   useShinyjs(),
                                   test <- a("Input Timezone* (link to supported
                                             timezones)",
                                             href = "https://en.wikipedia.org/wiki/List_of_tz_database_time_zones",
                                             style = "font-size:14px; ",
                                             target = "_blank"),
                                   textInput("timezone", "", value = "Asia/Kolkata",
                                             width = NULL),
                                   tags$hr(),
                                   fileInput("file1",
                                             "GPSMAP 64s - location files",
                                             multiple = TRUE,
                                             accept = c('.gpx')),
                                   tags$hr(),
                                   fileInput("file2", "AE51 - BC files",
                                             multiple = TRUE,
                                             accept = c("text/csv",
                                                        "text/comma-separated-values,
                                                        text/plain",
                                                        ".csv")),
                                   tags$hr(),
                                   fileInput("file3", "DT8530 - PM2.5 files",
                                             multiple = TRUE,
                                             accept = c("text/csv",
                                                        "text/comma-separated-values,
                                                        text/plain",
                                                        ".csv")),
                                   h5("Reference Correction for DT8530 - PM2.5"),
                                   numericInput("Slope",
                                                "Slope",
                                                value = 1.0),
                                   numericInput("Intercept",
                                                "Intercept",
                                                value = 0),
                                   tags$hr(),
                                   fileInput("file5", "RH files",
                                             multiple = TRUE,
                                             accept = c("text/csv",
                                                        "text/comma-separated-values,
                                                        text/plain",
                                                        ".csv")),
                                   tags$hr(),
                                   fileInput("file4", "CPC3007 -
                                             Particle Concentration files",
                                             multiple = TRUE,
                                             accept = c("text/csv",
                                                        "text/comma-separated-values,
                                                        text/plain",
                                                        ".csv")),
                                   numericInput("DF",
                                                "Dilution factor for
                                                CPC (1 for no diluter)",
                                                value = 1),
                                   tags$hr(),
                                   fileInput("file6", "LI-COR - CO2 files",
                                             multiple = TRUE,
                                             accept = c("text/csv",
                                                        ".txt",
                                                        "text/comma-separated-values,
                                                        text/plain",
                                                        ".csv")),
                                   helpText("*mandatory"),
                                   tags$hr(),
                                   actionButton("join_button", "JOIN"),
                                   tags$hr(),
                                   downloadButton('download',"Download as csv"),
                                   tags$hr())),
                mainPanel(
                  tags$a(img(src = 'logo.png', align = "right", height = 70,
                             width = 120),
                         href = "https://www.ilklabs.com/", target = "_blank"),
                  tags$head(
                    tags$style(type = 'text/css',
                               ".nav-tabs {
                               font-size: 18px
                               } ")),
                  tabsetPanel(id = "tabs1",
                              tabPanel(value = 1,
                                       title = "Joined File",
                                       dataTableOutput("table1")),
                              tabPanel(
                                value = 3,
                                title = "Summary",
                                dataTableOutput("table")
                              ),
                              tabPanel(
                                value = 2,
                                title = "Plots",
                                plotlyOutput("plot5", width = 800),
                                plotlyOutput("plot6", width = 800),
                                plotlyOutput("plot2", width = 800),
                                plotlyOutput("plot", width = 800),
                                plotlyOutput("plot4", width = 800),
                                plotlyOutput("plot3", width = 800),
                                plotlyOutput("plot7", width = 800)
                              ),
                              tabPanel(
                                value = 4,
                                title = "Map",
                                leafletOutput("map", width = "100%",
                                              height = 800)
                              ),
                              tabPanel(value = 6,
                                       title = "Alarms and Settings",
                                       h5("AE51 Status: "),
                                       dataTableOutput("table5"),
                                       dataTableOutput("table2"),
                                       h5("DT8530 Notes/Alarms: "),
                                       dataTableOutput("table4"),
                                       h5("CPC3007 Notes/Alarms: "),
                                       dataTableOutput("table3")
                              )))
  ))


server <- function(input, output, session) {

  options(shiny.maxRequestSize = 30*1024^2, shiny.launch.browser = TRUE)

  ## date matching

  CPC_f_date <- reactive({
    inFile <- input$file4
    if (is.null(input$file4)) {
      return(NULL)
    }else {
      df_list <- lapply(inFile$datapath, function(y) {
        JSON_csv <- read.delim(y, header = FALSE, sep = ",",
                               row.names = NULL, skip = 4)
        JSON_csv_date <- as.character(JSON_csv[1, 2])
        JSON_csv_date <- as.Date(JSON_csv_date, format = "%m/%d/%y")
        if (is.na(JSON_csv_date)) {
          JSON_csv <- read.delim(y, header = FALSE, sep = ",",
                                 row.names = NULL, skip = 4)
          JSON_csv_date <- as.character(JSON_csv[1, 2])
          JSON_csv_date <- as.Date(JSON_csv_date, format = "%m-%d-%Y")
        }
        as.Date(JSON_csv_date)
        }
      )}
    CPC_f2_date <- df_list[[1]]
    return(CPC_f2_date)
  })

  GPS_f_date <- reactive({
    trial  <- data.frame()
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
    req(input$file1)
    files3 <- lapply(inFile$datapath, function(y) {
      pfile2 <- htmlTreeParse(y, error = function (...) {}, useInternalNodes = T)
      elevations <- as.numeric(xpathSApply(pfile2, path = "//trkpt/ele",
                                           xmlValue))
      times  <- xpathSApply(pfile2, path = "//trkpt/time", xmlValue)
      coords <- xpathSApply(pfile2, path = "//trkpt", xmlAttrs)
      lats <- as.numeric(coords["lat", ])
      lons <- as.numeric(coords["lon", ])
      geodf2 <- data.frame(latitude = lats, longitude = lons,
                           ele = elevations, time = times)
    })
    trial <- do.call(rbind, files3)
    GPS_f <- trial
    GPS_f <- mutate(GPS_f, date = with_tz(ymd_hms(time), input$timezone))
    GPS_f <- GPS_f[!duplicated(as.POSIXct(GPS_f$date)),]
    GPS_f <- GPS_f %>%
      dplyr::select(date, latitude, longitude)
    names(GPS_f) <- c("date", "latitude", "longitude")
    GPS_f_date <- as.Date(GPS_f[1, 1])
    GPS_f_date <- as.Date(GPS_f_date, format="%Y-%m-%d") #"%m/%d/%Y"
    return(GPS_f_date)
  })

  BC_f_date <- reactive({
    inFile <- input$file2
    if (is.null(input$file2)) {
      return(NULL)
    }else {
      completeFun <- function(data, desiredColumns) {
        completeVec <- complete.cases(data[ , desiredColumns])
        return(data[completeVec, ])
      }
      df_list <- lapply(inFile$datapath, function(y) {
        JSON_csv_header <- read.delim(y, header = FALSE, sep = ",",
                                      skip = 15, row.names = NULL,
                                      stringsAsFactors = FALSE)
        JSON_csv_header <-JSON_csv_header[1, ]
        JSON_csv <- read.delim(y, skip = 17, header = FALSE, sep = ",")
        colnames(JSON_csv) <- unlist(JSON_csv_header)
        JSON_csv <- completeFun(JSON_csv, c("BC"))
        JSON_csv$date1 <- with(JSON_csv,
                               as.POSIXct(paste(
                                 as.Date(Date, format = "%Y/%m/%d"), Time),
                                 tz = input$timezone))
        if (is.null(JSON_csv$date1)) {
          JSON_csv$date1 <- with(JSON_csv,
                                 as.POSIXct(paste(
                                   as.Date(Date, format = "%d-%m-%Y"), Time),
                                   tz = input$timezone))
        }
        JSON_csv
      })
      data <- do.call(rbind, df_list)
      BC_f <- data
      BC_f_date1 <- as.character(BC_f[1, 1])
      BC_f_date <- as.Date(BC_f_date1, format = "%d-%m-%Y")
      if (is.na(BC_f_date)) {
        BC_f_date <- as.Date(BC_f_date1, format = "%Y/%m/%d")     #"%d-%m-%Y"; "%Y/%m/%d"
      }
      return(BC_f_date)
    }
  })

  DT_f_date <- reactive({
    inFile <- input$file3
    if (is.null(input$file3)) {
      return(NULL)
    }else {
      df_list <- lapply(inFile$datapath, function(y) {
        JSON_csv <- read.delim(y, header = TRUE, sep = ",",
                               row.names = NULL, skip = 29)
        JSON_csv_date <- as.character(JSON_csv[1, 1])
        JSON_csv_date <- as.Date(JSON_csv_date, format = "%d-%m-%Y")
        if (is.na(JSON_csv_date)) {
          JSON_csv <- read.delim(y, header = TRUE, sep = ",",
                                 row.names = NULL, skip = 29)
          JSON_csv_date <- as.character(JSON_csv[1, 1])
          JSON_csv_date <- as.Date(JSON_csv_date, format = "%m/%d/%Y")
        }
        as.Date(JSON_csv_date)
        })
      DT_f_date <- df_list[[1]]
      return(DT_f_date)
    }
  })

  CO2_f_date <- reactive({
    inFile <- input$file6
    if (is.null(input$file6)) {
      return(NULL)
    }else {
      df_list <- lapply(inFile$datapath, function(y) {
        JSON_csv <-read.delim(y, header = TRUE, sep = ";",
                              row.names = NULL, skip = 2)
        JSON_csv_date <- as.character(JSON_csv[1, 1])
        JSON_csv_date <- as.Date(JSON_csv_date, format = "%d-%m-%Y")
        if (is.na(JSON_csv_date)) {
          JSON_csv <- read.delim(y, header = TRUE, sep = ";",
                                 row.names = NULL, skip = 29)
          JSON_csv_date <- as.character(JSON_csv[1, 1])
          JSON_csv_date <- as.Date(JSON_csv_date, format = "%Y-%m-%d")
        }
        as.Date(JSON_csv_date)
        })
      CO2_f_date <- df_list[[1]]
      return(CO2_f_date)
    }
    })


  ## GPS and pollutant file joining

  CO2_f <- reactive({
    inFile <- input$file6
    if (is.null(input$file6)) {
      return(NULL)
    }else {
      df_list <- lapply(inFile$datapath, function(y) {
        JSON_csv <- read.delim(y, skip = 1, sep = ",",
                               header = TRUE, row.names = NULL,
                               stringsAsFactors = FALSE)
        JSON_csv <- JSON_csv[ ,1:3]
        names(JSON_csv) <- c("Date", "Time", "CO2")
        JSON_csv
      })
      data  <- do.call(rbind, df_list)
      CO2_f <- data
      CO2_f$date <- with(CO2_f,
                         as.POSIXct(paste(
                           as.Date(Date, format = "%d-%m-%Y"), Time),
                                           tz = input$timezone))#"%Y-%m-%d""%d-%m-%Y"
      CO2_f <- CO2_f %>%
        dplyr::select(date, CO2)
      names(CO2_f) <- c("date", "CO2")
      attributes(CO2_f$date)$tzone <- input$timezone
      CO2_f <- data.table(CO2_f)
      setkey(CO2_f, date)
      return(CO2_f)
    }
    })

  GPS_f <- reactive({
    trial  <- data.frame()
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
    req(input$file1)
    files3 <- lapply(inFile$datapath, function(y) {
      pfile2 <- htmlTreeParse(y, error = function (...) {}, useInternalNodes = T)
      elevations <- as.numeric(xpathSApply(pfile2, path = "//trkpt/ele",
                                           xmlValue))
      times  <- xpathSApply(pfile2, path = "//trkpt/time", xmlValue)
      coords <- xpathSApply(pfile2, path = "//trkpt", xmlAttrs)
      lats   <- as.numeric(coords["lat", ])
      lons   <- as.numeric(coords["lon", ])
      geodf2 <- data.frame(latitude = lats, longitude = lons,
                           ele = elevations, time = times)
    })
    trial <- do.call(rbind, files3)
    GPS_f <- trial
    GPS_f <- mutate(GPS_f, date = with_tz(ymd_hms(time), input$timezone))
    GPS_f <- GPS_f[!duplicated(as.POSIXct(GPS_f$date)),]
    GPS_f <- GPS_f %>%
      dplyr::select(date, latitude, longitude)
    names(GPS_f) <- c("date", "latitude", "longitude")
    GPS_f <- data.table(GPS_f)
    setkey(GPS_f, date)
    return(GPS_f)
  })

  BC_f <- reactive({
    inFile <- input$file2
    if (is.null(input$file2)) {
      return(NULL)
    }else {
      completeFun <- function(data, desiredColumns) {
        completeVec <- complete.cases(data[ , desiredColumns])
        return(data[completeVec, ])
      }
      df_list <- lapply(inFile$datapath, function(y){
        JSON_csv_header <- read.delim(y, header = FALSE, sep = ",", skip = 15,
                                      row.names = NULL,
                                      stringsAsFactors = FALSE)
        JSON_csv_header <- JSON_csv_header[1, ]
        JSON_csv <- read.delim(y, skip = 17, header = FALSE, sep = ",")
        colnames( JSON_csv ) <- unlist(JSON_csv_header)
        JSON_csv <- completeFun(JSON_csv, c("BC"))
        JSON_csv$date1 <- with(JSON_csv,
                               as.POSIXct(paste(
                                 as.Date(
                                   Date, format = "%Y/%m/%d"), Time),
                                 tz = input$timezone))
        if (is.null(JSON_csv$date1)) {
          JSON_csv$date1 <- with(JSON_csv,
                                 as.POSIXct(paste(
                                   as.Date(Date, format = "%d-%m-%Y"), Time),
                                   tz = input$timezone))
        }
        JSON_csv
      })
      data <- do.call(rbind, df_list)
      BC_f <- data
      BC_f$Date <- BC_f$date1#"%Y/%m/%d", "%d-%m-%Y"
      ef_file <- data.frame(BC_f)
      ef_file <- ef_file[ef_file$Status == 0,]
      ef_file<-dplyr::select(ef_file, Date, ATN, BC)
      ATN<-ef_file[1,2]
      # ef_file$ATN <- ef_file$ATN-(ATN) # for new filter
      names(ef_file) <- c("Date", "ATN", "BC")
      ef_file$BC1 <- (ef_file$BC / 1000)
      BC_Final <- ef_file
      ef_file$LD   <- ef_file$BC1 - rollapply(ef_file$BC1 , FUN = mean,
                                              width = 30, align = "center",
                                              partial = TRUE)
      ef_file$LD25 <- runquantile(ef_file$LD, 300, 0.25, type = 2,
                                  endrule = c("NA"))
      ef_file$LD75 <- runquantile(ef_file$LD, 300, 0.75, type = 2,
                                  endrule = c("NA"))
      ef_file$BC2  <- ef_file$BC1
      ef_file$BC3  <- ef_file$BC1
      ef_file$BC2[ef_file$BC2 >= 0] <- 0
      ef_file$BC2[ef_file$BC2 < 0] <- 1
      ef_file$BC2 <- rollapply(ef_file$BC2 , FUN = mean, width = 5,
                               align = "center",  partial = TRUE)
      Lower_LD25  <- ((ef_file$LD) > 5 * (ef_file$LD75))
      Higher_LD75 <- ((ef_file$LD) < 5 * (ef_file$LD25))
      ef_file$cev1 <- ifelse(Lower_LD25|Higher_LD75, ef_file$BC1, NA)
      CEV <- data.frame(ef_file$Date, ef_file$cev1)
      CEV$ef_file.cev1[!is.na(CEV$ef_file.cev1)] <- 1
      CEV <- data.frame(CEV)
      date_file <- data.frame(ef_file$Date,  ef_file$BC2, ef_file$BC3)
      completeFun <- function(data, desiredColumns) {
        completeVec <- complete.cases(data[ , desiredColumns])
        return(data[completeVec, ])
      }
      CEV <- completeFun(CEV, c("ef_file.cev1"))
      setDT(CEV)
      setDT(date_file)
      cev_file <- date_file[CEV, on = c('ef_file.Date')]
      cev_file <- cev_file[!(cev_file$ef_file.BC2 == 0),]
      cev_file <- xts(cev_file, order.by = cev_file$ef_file.Date)
      ef_file  <- data.frame(ef_file)
      CE <- data.frame(index(CEV))
      i <- index(cev_file)
      i_old <- index(cev_file)
      i <- index(cev_file) + 1
      j <- index(cev_file) + 2
      k <- index(cev_file) - 2
      l <- index(cev_file) - 1
      i <- cbind(as.character(i), as.character(i_old), as.character(j),
                 as.character(k), as.character(l))
      Date_cev <- data.frame(i)
      remove_cev <- data.frame(Date = unlist(Date_cev, use.names = FALSE))
      Date_Table <- unique(remove_cev[c("Date")])
      e <- nrow(Date_Table)
      if (e == 0) {
        BC <- ef_file
        BC$BC_Factor <- 1
      }else {
        Date_Table$BC_Factor <- 0
        Date_Table$Date <- as.POSIXct(Date_Table$Date)
        setDT(Date_Table)
        setDT(ef_file)
        BC <- Date_Table[ef_file, on = c('Date')]
        BC$BC_Factor[is.na(BC$BC_Factor)] <- 1
      }
      BC$BC_Fi <- BC$BC_Factor * BC$BC1
      BC$BC_Fi[BC$BC_Fi == 0] <- NA
      BC$Tr <- exp(- BC$ATN/  100)
      BC$CF <- 1 / (0.88 * BC$Tr + 0.12)
      BC$BC_Final <- BC$BC_Fi * BC$CF
      BC$BC_Fi[BC$BC_Fi < 0] <- NA
      BC$BC_Fi[is.na(BC$BC_Fi)] <- " "
      BC$BC_Final[BC$BC_Final < 0] <- NA
      BC$BC_Final[is.na(BC$BC_Final)] <- " "
      BC_Final <- dplyr::select(BC, Date, BC3, BC_Fi, BC_Final)
      names(BC_Final) <- c("date", "BC", "BC_NR", "BC_NR_LC")
      BC_Final$BC_NR_LC <- as.numeric(as.character(BC_Final$BC_NR_LC))
      BC_Final$BC_NR <- as.numeric(as.character(BC_Final$BC_NR))
      BC_Final$BC <- as.numeric(as.character(BC_Final$BC))
      BC_Final <- data.table(BC_Final)
      attributes(BC_Final$date)$tzone <- input$timezone
      setkey(BC_Final, date)
      return(BC_Final)
    }
  })

  DT_f<- reactive({
    inFile<-input$file3
    if (is.null(input$file3)) {
      return(NULL)
    }else {
      df_list <- lapply(inFile$datapath, function(y) {
        JSON_csv <- read.delim(y, header = TRUE, sep = ",", row.names = NULL,
                               skip = 28)
        names(JSON_csv) <- c("Date", "Time", "PM2.5")
        JSON_csv
      })
      data <- do.call(rbind, df_list)
      DT_f <- data
      Date1 <- DT_f[1, 1]
      Date1 <- as.Date(Date1, format = "%d-%m-%Y" )
      if (is.na(Date1)) {
        Date1 <- DT_f[1, 1]
        Date1 <- as.Date(Date1, format = "%m/%d/%Y" )
      }
      DT_f$date <- strptime(paste(Date1, DT_f$Time),
                            format = "%Y-%m-%d %H:%M:%S")
      DT_f$date <- as.POSIXct(DT_f$date, format = '%Y-%m-%d %H:%M:%S',
                              tz = input$timezone)
      attributes(DT_f$date)$tzone <- input$timezone
      DT_f$PM2.5 <- as.numeric(as.character(DT_f$PM2.5))
      DT_f$PM2.5 <- DT_f$PM2.5 * 1000
      DT_f <- dplyr::select(DT_f, date,  PM2.5 )
      DT_f <- data.table(DT_f)
      setkey(DT_f, date)
      return(DT_f)
    }
    })


  ## file name matching

  file_name_CPC <- reactive({
    inFile <- input$file4
    if (is.null(inFile)) {
      return(NULL)
    }else {
      z <- sub(".csv$", "", basename(input$file4$name))
      name_CPC <- substr(z, 1, 10)
      return(name_CPC)
    }
  })

  CPC_f <- reactive({
    DF <- input$DF
    inFile <- input$file4
    name_CPC <- file_name_CPC()
    if (is.null(input$file4)) {
      return(NULL)
    }else {
      df_list <- lapply(inFile$datapath, function(y) {
        JSON_csv <- read.delim(y, header = TRUE, sep = ",", row.names = NULL,
                               skip = 17,
                              stringsAsFactors = FALSE, fileEncoding = "latin1")
        JSON_csv
      })
      data  <- do.call(rbind, df_list)
      CPC_f <- data
      u <- stringr::str_extract(name_CPC, "[0-9]{4}\\_[0-9]{2}\\_[0-9]{2}")
      w <- str_replace_all(u, "_", "-")
      CPC_f <- CPC_f[, 1:2]
      names(CPC_f) <- c("Time", "Particle_conc")
      CPC_f$Particle_conc <- CPC_f$Particle_conc * DF
      CPC_f <- mutate(CPC_f, date = ymd_hms(paste(w, Time),
                                            tz = input$timezone))
      CPC_f <- dplyr::select(CPC_f, date, Particle_conc )
      CPC_f <- data.table(CPC_f)
      setkey(CPC_f, date)
      return(CPC_f)
    }
    })

  RH_f<- reactive({
    if (is.null(input$file5)) {
      return(NULL)
    }else {
      RH_f <- data.frame(read.delim(input$file5$datapath, header = TRUE,
                                    sep = ",", skip = 6, row.names = NULL))
      RH_f_Date <- RH_f[ ,2]
      RH_f_Time <- RH_f[ ,3]
      RH <- RH_f[ , grepl( "RH" , names(RH_f ) ) ]
      RH_f <- data.frame(RH_f_Date, RH_f_Time, RH)
      names(RH_f) <- c("LogDate", "LogTime", "RH")
      RH_f$LogTime <- gsub(".", ":", RH_f$LogTime, fixed = TRUE)
      RH_f$LogTime <- gsub("AM", "", RH_f$LogTime, fixed = TRUE)
      RH_f$LogTime <- gsub("PM", "", RH_f$LogTime, fixed = TRUE)
      RH_f$date <- as.POSIXct(paste(RH_f$LogDate, RH_f$LogTime),
                              format = '%d-%m-%Y %H:%M:%S', tz = input$timezone)
      RH_f <- RH_f%>%
        dplyr::select(date, RH)
      names(RH_f) <- c("date", "RH")
      RH_f$RH <- as.numeric(as.character(RH_f$RH))
      RH_f$date <- as.POSIXct(RH_f$date, format = '%Y-%m-%d %H:%M:%S',
                              tz = input$timezone)
      RH_f <- data.table(RH_f)
      setkey(RH_f, date)
      return(RH_f)
    }
  })

  file_name_CO2 <- reactive({
    inFile <- input$file6
    if (is.null(inFile)) {
      return(NULL)
    }else {
      y <- sub(".csv$", "", basename(input$file6$name))
      name_CO2 <- substr(y, 1, 10)
      return(name_CO2)
    }
  })

  file_name_RH <- reactive({
    inFile <- input$file5
    if (is.null(inFile)) {
      return(NULL)
    }else {
      x <- sub(".csv$", "", basename(input$file5$name))
      name_RH <- substr(x, 1, 10)
      return(name_RH)
    }
  })

  file_name_GPS <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
      }else {
      x <- sub(".csv$", "", basename(input$file1$name))
      name_GPS <- substr(x, 1, 10)
      return(name_GPS)
    }
  })

  file_name_BC <- reactive({
    inFile <- input$file2
    if (is.null(inFile)) {
      return(NULL)
      }else {
      y <- sub(".csv$", "", basename(input$file2$name))
      name_BC <- substr(y, 1, 10)
      return(name_BC)
    }
  })

  file_name_DT <- reactive({
    inFile <- input$file3
    if (is.null(inFile)) {
      return(NULL)
    }else {
      w <- sub(".csv$", "", basename(input$file3$name))
      name_DT <- substr(w, 1, 10)
      return(name_DT)
    }
  })


  ## preloaded table

  data_blank <- reactive({
    if (is.null(input$file1) & is.null(input$file2) & is.null(input$file3) &
       is.null(input$file4) & is.null(input$file5) & is.null(input$file6)) {
      pfile2 <- htmlTreeParse("data/GPSMAP64s/2019_09_25_h091000_KAN_Garmin_3.gpx",
                              error = function (...) {}, useInternalNodes = T)
      elevations <- as.numeric(xpathSApply(pfile2, path = "//trkpt/ele",
                                           xmlValue))
      times <- xpathSApply(pfile2, path = "//trkpt/time", xmlValue)
      coords <- xpathSApply(pfile2, path = "//trkpt", xmlAttrs)
      lats <- as.numeric(coords["lat", ])
      lons <- as.numeric(coords["lon", ])
      GPS_f <- data.frame(latitude = lats, longitude = lons, ele = elevations,
                          time = times)
      GPS_f <- mutate(GPS_f, date = with_tz(ymd_hms(time), "Asia/Kolkata"))
      GPS_f <- GPS_f[!duplicated(as.POSIXct(GPS_f$date)), ]
      GPS_f <- GPS_f %>%
        dplyr::select(date, latitude, longitude)
      names(GPS_f) <- c("date", "latitude", "longitude")
      GPS_f <- data.table(GPS_f)
      setkey(GPS_f, date)

      CO2_f <- read.delim("data/LI-COR/2019_09_25_h091000_KAN_LI_COR.csv",
                          skip = 1, sep = ",", header = TRUE, row.names = NULL,
                          stringsAsFactors = FALSE)
      CO2_f <- CO2_f[ ,1:3]
      names(CO2_f) <- c("Date", "Time", "CO2")
      CO2_f$date <- with(CO2_f,
                         as.POSIXct(paste(as.Date(Date, format = "%d-%m-%Y"),
                                                 Time), tz = "Asia/Kolkata"))
      CO2_f <- CO2_f %>%
        dplyr::select(date, CO2)
      names(CO2_f) <- c("date", "CO2")
      attributes(CO2_f$date)$tzone <- "Asia/Kolkata"
      CO2_f <- data.table(CO2_f)
      setkey(CO2_f, date)
      completeFun <- function(data, desiredColumns) {
        completeVec <- complete.cases(data[ , desiredColumns])
        return(data[completeVec, ])
      }

      BC_f_header <- read.delim("data/AE51/2019_09_25_h091000_KAN_AE51.csv",
                                header = FALSE,
                                sep = ",", skip = 15, row.names = NULL,
                                stringsAsFactors = FALSE)
      BC_f_header <- BC_f_header[1, ]
      BC_f<- read.delim("data/AE51/2019_09_25_h091000_KAN_AE51.csv", skip = 17,
                        header = FALSE, sep = ",")
      colnames(BC_f) <- unlist(BC_f_header)
      BC_f<-completeFun(BC_f, c("BC"))
      BC_f$date1<- with(BC_f,
                        as.POSIXct(paste(as.Date(Date, format="%Y/%m/%d"),
                                               Time), tz="Asia/Kolkata"))
      BC_f$Date<- BC_f$date1
      ef_file <- data.frame(BC_f)
      ef_file <- ef_file[ef_file$Status == 0,]
      ef_file<-dplyr::select(ef_file, Date, ATN, BC)
      ATN<-ef_file[1,2]
      names(ef_file) <- c("Date", "ATN", "BC")
      ef_file$BC1 <- (ef_file$BC / 1000)
      BC_Final <- ef_file
      ef_file$LD   <- ef_file$BC1 - rollapply(ef_file$BC1 , FUN = mean,
                                              width = 30, align = "center",
                                              partial = TRUE)
      ef_file$LD25 <- runquantile(ef_file$LD, 300, 0.25, type = 2,
                                  endrule = c("NA"))
      ef_file$LD75 <- runquantile(ef_file$LD, 300, 0.75, type = 2,
                                  endrule = c("NA"))
      ef_file$BC2  <- ef_file$BC1
      ef_file$BC3  <- ef_file$BC1
      ef_file$BC2[ef_file$BC2 >= 0] <- 0
      ef_file$BC2[ef_file$BC2 < 0] <- 1
      ef_file$BC2 <- rollapply(ef_file$BC2 , FUN = mean, width = 5,
                               align = "center",
                               partial = TRUE)
      Lower_LD25  <- ((ef_file$LD) > 5 * (ef_file$LD75))
      Higher_LD75 <- ((ef_file$LD) < 5 * (ef_file$LD25))
      ef_file$cev1 <- ifelse(Lower_LD25|Higher_LD75, ef_file$BC1, NA)
      CEV <- data.frame(ef_file$Date, ef_file$cev1)
      CEV$ef_file.cev1[!is.na(CEV$ef_file.cev1)] <- 1
      CEV <- data.frame(CEV)
      date_file <- data.frame(ef_file$Date,  ef_file$BC2, ef_file$BC3)
      completeFun <- function(data, desiredColumns) {
        completeVec <- complete.cases(data[ , desiredColumns])
        return(data[completeVec, ])
      }
      CEV <- completeFun(CEV, c("ef_file.cev1"))
      setDT(CEV)
      setDT(date_file)
      cev_file <- date_file[CEV, on = c('ef_file.Date')]
      cev_file <- cev_file[!(cev_file$ef_file.BC2 == 0),]
      cev_file <- xts(cev_file, order.by = cev_file$ef_file.Date)
      ef_file  <- data.frame(ef_file)
      CE <- data.frame(index(CEV))
      i <- index(cev_file)
      i_old <- index(cev_file)
      i <- index(cev_file) + 1
      j <- index(cev_file) + 2
      k <- index(cev_file) - 2
      l <- index(cev_file) - 1
      i <- cbind(as.character(i), as.character(i_old), as.character(j),
                 as.character(k), as.character(l))
      Date_cev <- data.frame(i)
      remove_cev <- data.frame(Date = unlist(Date_cev, use.names = FALSE))
      Date_Table <- unique(remove_cev[c("Date")])
      e <- nrow(Date_Table)
      if (e == 0) {
        BC <- ef_file
        BC$BC_Factor <- 1
      }else {
        Date_Table$BC_Factor <- 0
        Date_Table$Date <- as.POSIXct(Date_Table$Date)
        setDT(Date_Table)
        setDT(ef_file)
        BC <- Date_Table[ef_file, on = c('Date')]
        BC$BC_Factor[is.na(BC$BC_Factor)] <- 1
      }
      BC$BC_Fi <- BC$BC_Factor * BC$BC1
      BC$BC_Fi[BC$BC_Fi == 0] <- NA
      BC$Tr <- exp(- BC$ATN/  100)
      BC$CF <- 1 / (0.88 * BC$Tr + 0.12)
      BC$BC_Final <- BC$BC_Fi * BC$CF
      BC$BC_Fi[BC$BC_Fi < 0] <- NA
      BC$BC_Fi[is.na(BC$BC_Fi)] <- " "
      BC$BC_Final[BC$BC_Final < 0] <- NA
      BC$BC_Final[is.na(BC$BC_Final)] <- " "
      BC_Final <- dplyr::select(BC, Date, BC3, BC_Fi, BC_Final)
      names(BC_Final) <- c("date", "BC", "BC_NR", "BC_NR_LC")
      BC_Final$BC_NR_LC <- as.numeric(as.character(BC_Final$BC_NR_LC))
      BC_Final$BC_NR <- as.numeric(as.character(BC_Final$BC_NR))
      BC_Final$BC <- as.numeric(as.character(BC_Final$BC))
      BC_Final <- data.table(BC_Final)
      attributes(BC_Final$date)$tzone <- "Asia/Kolkata"
      setkey(BC_Final, date)

      DT_f <- read.delim("data/DT8530/2019_09_25_h091000_KAN_DT8530.csv",
                         header = TRUE,
                         sep = ",", row.names = NULL, skip = 28)
      names(DT_f) <- c("Date", "Time", "PM2.5")
      Date1 <- DT_f[1, 1]
      Date1 <- as.Date(Date1, format = "%d-%m-%Y" )
      if (is.na(Date1)) {
        Date1 <- DT_f[1, 1]
        Date1 <- as.Date(Date1, format = "%m/%d/%Y" )
      }
      DT_f$date <- strptime(paste(Date1, DT_f$Time),
                            format = "%Y-%m-%d %H:%M:%S")
      DT_f$date <- as.POSIXct(DT_f$date, format = '%Y-%m-%d %H:%M:%S',
                              tz = "Asia/Kolkata")
      attributes(DT_f$date )$tzone <- "Asia/Kolkata"
      DT_f$PM2.5 <- as.numeric(as.character(DT_f$PM2.5))
      DT_f$PM2.5 <-DT_f$PM2.5 * 1000
      DT_f <- dplyr::select(DT_f, date,  PM2.5 )
      DT_f <- data.table(DT_f)
      setkey(DT_f, date)

      CPC_f <- read.delim("data/CPC3007/2019_09_25_h091000_KAN_CPC3007.csv",
                          header = TRUE,
                          sep = ",", row.names = NULL, skip = 17,
                          stringsAsFactors = FALSE,
                          fileEncoding = "latin1")
      CPC_f <- CPC_f[ ,1:2]
      names(CPC_f) <- c("Time", "Particle_conc")
      CPC_f$Particle_conc <- CPC_f$Particle_conc * 5.5
      w <- as.Date("2019-09-25")
      CPC_f <- mutate(CPC_f, date = ymd_hms(paste(w, Time), tz = "Asia/Kolkata"))
      CPC_f <- dplyr::select(CPC_f,date, Particle_conc )
      CPC_f <- data.table(CPC_f)
      setkey(CPC_f, date)

      RH_f <- data.frame(read.delim("data/RH/2019_09_25_h091000_KAN_RH.csv",
                                    header = TRUE, sep = ",", skip = 6,
                                    row.names = NULL))
      RH_f_Date <- RH_f[ ,2]
      RH_f_Time <- RH_f[ ,3]
      RH <- RH_f[ , grepl("RH" , names(RH_f) ) ]
      RH_f <-data.frame(RH_f_Date, RH_f_Time, RH)
      names(RH_f)  <- c("LogDate", "LogTime", "RH")
      RH_f$LogTime <- gsub(".", ":", RH_f$LogTime, fixed = TRUE)
      RH_f$LogTime <- gsub("AM", "", RH_f$LogTime, fixed = TRUE)
      RH_f$LogTime <- gsub("PM", "", RH_f$LogTime, fixed = TRUE)
      RH_f$date <- as.POSIXct(paste(RH_f$LogDate, RH_f$LogTime),
                              format = '%d-%m-%Y %H:%M:%S', tz = "Asia/Kolkata")
      RH_f <- RH_f%>%
        dplyr::select(date, RH)#X2.P..RH, X1.P..RH
      names(RH_f) <- c("date", "RH")
      RH_f$RH <- as.numeric(as.character(RH_f$RH))
      RH_f$date <- as.POSIXct(RH_f$date, format = '%Y-%m-%d %H:%M:%S',
                              tz = "Asia/Kolkata")
      RH_f <- data.table(RH_f)
      setkey(RH_f, date)
      VecFunc <- function(x) {
        if (x > 0.6) {
          return (1 + (0.25 * (((x) * (x)) / (1 - x))))
        } else
        {
          return (1)
        }
      }

      joined_GPS_CO2 <- left_join(GPS_f, CO2_f, by = "date")
      joined_GPS_CO2 <- dplyr::select(joined_GPS_CO2, date, CO2, latitude,
                                      longitude )
      joined_GPS_CO2 <- data.table(joined_GPS_CO2)
      setkey(joined_GPS_CO2, date)
      joined_GPS_DT <- left_join(GPS_f, DT_f, by = "date")
      joined_GPS_DT$PM2.5_RHC <- NA
      joined_GPS_DT <- dplyr::select(joined_GPS_DT, date, PM2.5, PM2.5_RHC,
                                     latitude, longitude )
      joined_GPS_DT <- data.table(joined_GPS_DT)
      setkey(joined_GPS_DT, date)
      DT_f <- RH_f[joined_GPS_DT, roll = "nearest"]
      DT_f$RH <- DT_f$RH / 100
      DT_f <- dplyr::select(DT_f, date, latitude, longitude,  PM2.5, RH )
      names(DT_f) <- c("date", "latitude", "longitude",  "PM2.5", "RH")
      DT_f$CF <- sapply(DT_f$RH, FUN = VecFunc)
      DT_f$PM2.5_RHC     <- DT_f$PM2.5 / DT_f$CF
      DT_f$PM2.5_Ref     <- (DT_f$PM2.5 * 1 ) + 0
      DT_f$PM2.5_RHC_Ref <- (DT_f$PM2.5_RHC * 1) + 0
      DT_f$date <- as.POSIXct(DT_f$date, format = '%Y-%m-%d %H:%M:%S')
      attributes(DT_f$date )$tzone <- "Asia/Kolkata"
      DT_f <- data.table(DT_f)
      setkey(DT_f, date)
      joined_GPS_BC <- left_join(GPS_f, BC_Final, by = "date")
      joined_GPS_BC <- dplyr::select(joined_GPS_BC, date, BC, BC_NR, BC_NR_LC,
                                     latitude, longitude )
      joined_GPS_BC <- data.table(joined_GPS_BC)
      setkey(joined_GPS_BC, date)
      joined_GPS_CPC <- left_join(GPS_f, CPC_f, by = "date")
      joined_GPS_CPC <- dplyr::select(joined_GPS_CPC, date, Particle_conc,
                                      latitude, longitude )
      joined_GPS_CPC <- data.table(joined_GPS_CPC)
      setkey(joined_GPS_CPC, date)
      joined2 <- left_join(joined_GPS_CPC, joined_GPS_CO2, by = "date")
      setDT(joined2)
      setkey(joined2, date)
      joined_1 <- left_join(DT_f, joined2, by = "date")
      setDT(joined_1)
      setkey(joined_1, date)
      joined <- joined_1[joined_GPS_BC, roll = "nearest"]
      joined <- joined %>%
        dplyr::select(date, latitude, longitude, BC, BC_NR, BC_NR_LC, PM2.5,
                      PM2.5_RHC, PM2.5_RHC_Ref, PM2.5_Ref, RH, Particle_conc,
                      CO2)
      names(joined)<-c("date", "Latitude", "Longitude",  "BC", "BC_NR",
                       "BC_NR_LC", "PM2.5","PM2.5_RHC", "PM2.5_RHC_Ref",
                       "PM2.5_Ref", "RH", "Particle_conc", "CO2")
      attributes(joined$date )$tzone <- "Asia/Kolkata"
      joined <- joined[!duplicated(joined$date), ]
      joined
    }
  })

  joined_GPS_CO2 <- reactive({
    name_GPS <- file_name_GPS()
    name_CO2 <- file_name_CO2()
    GPS_f <- GPS_f()
    CO2_f <- CO2_f()
    if (is.null(CO2_f)) {
      joined_GPS_CO2 <- dplyr::select(GPS_f, date,  latitude, longitude)
      joined_GPS_CO2$CO2 <- NA
      joined_GPS_CO2$CO2 <- as.numeric(joined_GPS_CO2$CO2)

    }
    if (!is.null(CO2_f)) {
      validate(
        need(try(file_name_GPS() == file_name_CO2()), "GPS File mandatory! File
        names of GPS and LI-COR do not match, Please select the correct files")
      )
      validate(
        need(try(GPS_f_date() == CO2_f_date()), "The files have different
             date entries in GPS and LI-COR! Please check once again.")
      )
      joined_GPS_CO2 <- left_join(GPS_f, CO2_f, by = "date")
      joined_GPS_CO2 <- dplyr::select(joined_GPS_CO2, date, CO2,
                                      latitude, longitude )
    }
    joined_GPS_CO2 <- data.table(joined_GPS_CO2)
    setkey(joined_GPS_CO2, date)
    return(joined_GPS_CO2)
  })

  joined_GPS_DT <- reactive({
    name_GPS <- file_name_GPS()
    name_DT  <- file_name_DT()
    GPS_f <- GPS_f()
    DT_f <- DT_f()
    if (is.null(DT_f)) {
      joined_GPS_DT <- dplyr::select(GPS_f, date,  latitude, longitude)
      joined_GPS_DT$PM2.5 <- NA
      joined_GPS_DT$PM2.5_RHC <- NA
      joined_GPS_DT$PM2.5 <- as.numeric(joined_GPS_DT$PM2.5)

    }
    if (!is.null(DT_f)) {
      validate(
        need(try(file_name_GPS() == file_name_DT()), "GPS File mandatory!
             File names of GPS and DustTrak-8530 do not match,
             Please select the correct files")
      )
      validate(
        need(try(GPS_f_date() == DT_f_date()), "The files have different date
             entries in GPS and DustTrak-8530! Please check once again.")
      )
      joined_GPS_DT<-left_join(GPS_f, DT_f, by = "date")
      joined_GPS_DT$PM2.5_RHC <- NA
      joined_GPS_DT <- dplyr::select(joined_GPS_DT, date, PM2.5, PM2.5_RHC,
                                     latitude, longitude )
    }
    joined_GPS_DT <- data.table(joined_GPS_DT)
    setkey(joined_GPS_DT, date)
    return(joined_GPS_DT)
  })

  DT_RH <- reactive({
    name_RH <- file_name_RH()
    name_DT <- file_name_DT()
    DT_fi <- DT_f()
    RH_f <- RH_f()
    joined_GPS_DT <- joined_GPS_DT()
    Slope <- input$Slope
    Intercept <- input$Intercept
    VecFunc <- function(x) {
      if (x > 0.6) {
        return (1 + (0.25 * (((x) * (x)) / (1 - x))))
      } else
      {
        return (1)
      }
    }
    if (is.null(RH_f()) | is.null(DT_f())) {
      DT_f <- joined_GPS_DT
      DT_f$RH <- NA
      DT_f$PM2.5_RHC <- NA
      DT_f$PM2.5_Ref <- (DT_f$PM2.5 * Slope) + Intercept
      DT_f$PM2.5_RHC_Ref <- NA
      DT_f$RH <- as.numeric(DT_f$RH)
    }
    if (!is.null(RH_f()) & !is.null(DT_f())) {
      validate(
        need(try(file_name_DT() == file_name_RH()), "File names of DustTrak
             8530 and RH do not match, Please select the correct files")
      )
      setkey(RH_f, date)
      DT_f <- RH_f[joined_GPS_DT, roll = "nearest"]
      DT_f$RH <- DT_f$RH / 100
      DT_f <- dplyr::select(DT_f, date,latitude, longitude,  PM2.5, RH )
      names(DT_f) <- c("date","latitude", "longitude",  "PM2.5", "RH")
      DT_f$CF <- sapply(DT_f$RH, FUN = VecFunc)
      DT_f$PM2.5_RHC <- DT_f$PM2.5 / DT_f$CF
      DT_f$PM2.5_Ref <- (DT_f$PM2.5 * Slope) + Intercept
      DT_f$PM2.5_RHC_Ref <- (DT_f$PM2.5_RHC * Slope) + Intercept
    }
    DT_f$date <- as.POSIXct(DT_f$date, format = '%Y-%m-%d %H:%M:%S')
    attributes(DT_f$date )$tzone <- input$timezone
    DT_f <- data.table(DT_f)
    setkey(DT_f, date)
    return(DT_f)
  })

  joined_GPS_BC <- reactive({
    name_GPS <- file_name_GPS()
    name_BC <- file_name_BC()
    GPS_f <- GPS_f()
    BC_Final <- BC_f()
    if (is.null(BC_f())) {
      joined_GPS_BC <- dplyr::select(GPS_f, date,  latitude, longitude)
      joined_GPS_BC$BC <- NA
      joined_GPS_BC$BC_NR <- NA
      joined_GPS_BC$BC_NR_LC <- NA
      joined_GPS_BC$BC <- as.numeric(as.character(joined_GPS_BC$BC))
      joined_GPS_BC$BC_NR <- as.numeric(as.character(joined_GPS_BC$BC_NR))
      joined_GPS_BC$BC_NR_LC <- as.numeric(as.character(joined_GPS_BC$BC_NR_LC))
    }
    if (!is.null(BC_f())) {
      validate(
        need(try(file_name_GPS() == file_name_BC()), "GPS File mandatory!
             File names of GPS and AE51 do not match, Please select the correct
             files")
      )
      validate(
        need(try(GPS_f_date() == BC_f_date()), "The files have different
        date entries in GPS and AE51! Please check once again.")
      )
      joined_GPS_BC <- left_join(GPS_f, BC_Final, by = "date")
      joined_GPS_BC <- dplyr::select(joined_GPS_BC, date,  BC, BC_NR, BC_NR_LC,
                                     latitude, longitude )
    }
    joined_GPS_BC <- data.table(joined_GPS_BC)
    setkey(joined_GPS_BC, date)
    return(joined_GPS_BC)
  })

  joined_GPS_CPC <- reactive({
    name_GPS <- file_name_GPS()
    name_CPC <- file_name_CPC()
    GPS_f <- GPS_f()
    CPC_f <- CPC_f()
    if (is.null(CPC_f)) {
      joined_GPS_CPC <- dplyr::select(GPS_f, date,  latitude, longitude)
      joined_GPS_CPC$Particle_conc <- NA
      joined_GPS_CPC$Particle_conc <- as.numeric(joined_GPS_CPC$Particle_conc)

    }
    if (!is.null(CPC_f)) {
      validate(
        need(try(file_name_GPS() == file_name_CPC()), "GPS File mandatory!
        File names of GPS and CPC-3007 do not match,
             Please select the correct files")
      )
      validate(
        need(try(GPS_f_date() == CPC_f_date()), "The files have different
        date entries in GPS and CPC-3007! Please check once again.")
      )
      joined_GPS_CPC <- left_join(GPS_f, CPC_f, by = "date")
      joined_GPS_CPC <- dplyr::select(joined_GPS_CPC, date, Particle_conc,
                                      latitude, longitude )
    }
    joined_GPS_CPC <- data.table(joined_GPS_CPC)
    setkey(joined_GPS_CPC, date)
    return(joined_GPS_CPC)
  })

  observeEvent(input$timezone,{
    toggleState(id = "join_button",
                condition = (input$timezone != "" |
                               is.null(input$timezone) | is.na(input$timezone)))
  })

  data_joined <- eventReactive(input$join_button,{

    joined_GPS_CPC <- joined_GPS_CPC()
    joined_GPS_CO2 <- joined_GPS_CO2()
    joined_GPS_BC  <- joined_GPS_BC()

    DT_RH <- DT_RH()
    joined_GPS_DT <- joined_GPS_DT()
    name_GPS <- file_name_GPS()
    name_BC  <- file_name_BC()
    name_CPC <- file_name_CPC()
    name_DT  <- file_name_DT()
    name_RH  <- file_name_RH()
    name_CO2 <- file_name_CO2()

    GPS_f <- GPS_f()
    CPC_f <- CPC_f()
    BC_Final <- BC_f()
    DT_f <- DT_f()
    RH_f <- RH_f()
    CO2_f <- CO2_f()
    joined2 <- left_join(joined_GPS_CPC, joined_GPS_CO2, by = "date")
    setDT(joined2)
    setkey(joined2, date)
    joined_1 <- left_join(DT_RH, joined2, by = "date")
    setDT(joined_1)
    setkey(joined_1, date)
    joined <- joined_1[joined_GPS_BC, roll = "nearest"]
    joined <- joined %>%
      dplyr::select(date,latitude,longitude, BC, BC_NR, BC_NR_LC, PM2.5,
                    PM2.5_RHC, PM2.5_RHC_Ref, PM2.5_Ref, RH, Particle_conc, CO2)
    names(joined) <- c("date", "Latitude", "Longitude",  "BC", "BC_NR",
                       "BC_NR_LC", "PM2.5", "PM2.5_RHC", "PM2.5_RHC_Ref",
                       "PM2.5_Ref", "RH", "Particle_conc", "CO2")
    attributes(joined$date )$tzone <- input$timezone
    joined <- joined[!duplicated(joined$date), ]
    return(joined)
  })


  ## Final corrected, joined table

  output$table1 <- DT::renderDataTable({
    if (is.null(input$file1) & is.null(input$file2) & is.null(input$file3) &
       is.null(input$file4) & is.null(input$file5) & is.null(input$file6)) {
      data_joined <- data_blank()
    }else{
      data_joined <- data_joined()
    }
    data_joined$BC            <- round(as.numeric(data_joined$BC),
                                       digits = 2)
    data_joined$BC_NR         <- round(as.numeric(data_joined$BC_NR),
                                       digits = 2)
    data_joined$BC_NR_LC      <- round(as.numeric(data_joined$BC_NR_LC),
                                       digits = 2)
    data_joined$PM2.5         <- round(as.numeric(data_joined$PM2.5),
                                       digits = 2)
    data_joined$PM2.5_RHC     <- round(as.numeric(data_joined$PM2.5_RHC),
                                       digits = 2)
    data_joined$PM2.5_Ref     <- round(as.numeric(data_joined$PM2.5_Ref),
                                       digits = 2)
    data_joined$PM2.5_RHC_Ref <- round(as.numeric(data_joined$PM2.5_RHC_Ref),
                                       digits = 2)
    data_joined$Longitude     <- round(as.numeric(data_joined$Longitude),
                                       digits = 4)
    data_joined$Latitude      <- round(as.numeric(data_joined$Latitude),
                                       digits = 4)
    data_joined$Particle_conc <- round(as.numeric(data_joined$Particle_conc),
                                       digits = 2)
    data_joined$CO2           <- round(as.numeric(data_joined$CO2),
                                       digits = 2)
    data_joined$RH            <- data_joined$RH * 100
    data_joined$RH            <- round(as.numeric(data_joined$RH),
                                       digits = 2)
    data_joined <- data_joined %>%
      dplyr::select(date,Latitude,Longitude, BC, BC_NR, BC_NR_LC, PM2.5,
                    PM2.5_RHC, PM2.5_RHC_Ref, PM2.5_Ref, RH,
                    Particle_conc, CO2)
    names(data_joined) <- c("date", "Latitude", "Longitude",  "AE51_BC (ug/m3)",
                            "AE51_BC_NR (ug/m3)", "AE51_BC_NR_LC (ug/m3)",
                            "DT8530_PM2.5 (ug/m3)",
                            "DT8530_PM2.5_RHC (ug/m3)",
                            "DT8530_PM2.5_RHC_Ref (ug/m3)",
                            "DT8530_PM2.5_Ref (ug/m3)", "RH(%)",
                            "CPC3007_Particle Concentration (#/cm3)",
                            "LI-COR_CO2 (ppm)")

    datatable(data_joined, options = list("pageLength" = 25))
  })


  ## Download the csv generated

  output$download <- downloadHandler(
    filename <- function() {"joined_file.csv"},
    content <- function(fname) {
      if (is.null(input$file1) & is.null(input$file2) & is.null(input$file3) &
         is.null(input$file4) & is.null(input$file5) & is.null(input$file6)) {
        data_joined <- data_blank()
      }else {
        data_joined <- data_joined()
      }
      data_joined <- data_joined %>%
        dplyr::select(date, Latitude, Longitude, BC, BC_NR, BC_NR_LC, PM2.5,
                      PM2.5_RHC, PM2.5_RHC_Ref, PM2.5_Ref, RH,
                      Particle_conc, CO2)
      names(data_joined) <- c("date(LT)", "Latitude", "Longitude",
                              "AE51_BC (ug/m3)",
                            "AE51_BC_NR (ug/m3)", "AE51_BC_NR_LC (ug/m3)",
                            "DT8530_PM2.5 (ug/m3)", "DT8530_PM2.5_RHC (ug/m3)",
                            "DT8530_PM2.5_RHC_Ref (ug/m3)",
                            "DT8530_PM2.5_Ref (ug/m3)",
                            "RH(%)", "CPC3007_Particle Concentration (#/cm3)",
                            "LI-COR_CO2 (ppm)")
      write.csv(data_joined, fname)
    })


  ## Summary Statistics

  output$table <- DT::renderDataTable({
    if (is.null(input$file1) & is.null(input$file2) & is.null(input$file3) &
       is.null(input$file4) & is.null(input$file5) & is.null(input$file6)) {
      data <- data_blank()
    }else {
      data <- data_joined()
    }
    data <- dplyr::select(data, BC, BC_NR, BC_NR_LC, PM2.5, PM2.5_RHC,
                          PM2.5_RHC_Ref, PM2.5_Ref, RH, Particle_conc,  CO2)
    data[["BC"]] <- as.numeric(as.character(data[["BC"]]))
    data[["BC_NR"]] <- as.numeric(as.character(data[["BC_NR"]]))
    data[["BC_NR_LC"]] <- as.numeric(as.character(data[["BC_NR_LC"]]))
    data$RH <- data$RH * 100
    names(data) <- c("AE51_BC (ug/m3)", "AE51_BC_NR (ug/m3)",
                     "AE51_BC_NR_LC (ug/m3)",
                     "DT8530_PM2.5 (ug/m3)", "DT8530_PM2.5_RHC (ug/m3)",
                     "DT8530_PM2.5_RHC_Ref (ug/m3)",
                     "DT8530_PM2.5_Ref (ug/m3)", "RH(%)",
                     "CPC3007_Particle Concentration (#/cm3)",
                     "LI-COR_CO2 (ppm)")
    columns <- c("AE51_BC (ug/m3)","AE51_BC_NR (ug/m3)","AE51_BC_NR_LC (ug/m3)",
                 "DT8530_PM2.5 (ug/m3)", "DT8530_PM2.5_RHC (ug/m3)",
                 "DT8530_PM2.5_RHC_Ref (ug/m3)", "DT8530_PM2.5_Ref (ug/m3)",
                 "RH(%)", "CPC3007_Particle Concentration (#/cm3)",
                 "LI-COR_CO2 (ppm)")
    data[ , columns] <- lapply(columns,
                               function(x) as.numeric(as.character(data[[x]])))
    tmp1 <- do.call(data.frame,
                    list(Mean = apply(data, 2, mean, na.rm = TRUE),
                         SD = apply(data, 2, sd, na.rm = TRUE),
                         Median = apply(data, 2, median, na.rm = TRUE),
                         IQR = apply(data, 2, IQR, na.rm = TRUE),
                         Min = apply(data, 2, min, na.rm = TRUE),
                         Max = apply(data, 2, max, na.rm = TRUE),
                         p1  = apply(data, 2, quantile, probs = c(0.01),
                                     na.rm = TRUE),
                         p10 = apply(data, 2, quantile, probs = c(0.1),
                                     na.rm = TRUE),
                         p25 = apply(data, 2, quantile, probs = c(0.25),
                                     na.rm = TRUE),
                         p75 = apply(data, 2, quantile, probs = c(0.75),
                                     na.rm = TRUE),
                         p90 = apply(data, 2, quantile, probs = c(0.9),
                                     na.rm = TRUE),
                         p99 = apply(data, 2, quantile, probs = c(0.99),
                                     na.rm = TRUE),
                         Total_non_NA = apply(data, 2,
                                              function(x)
                                                {length(which(!is.na(x)))})))
    tmp <- data.frame(tmp1)
    tmp$Mean   <- round(as.numeric(as.character(tmp$Mean)), digits = 2)
    tmp$IQR    <- round(as.numeric(as.character(tmp$IQR)), digits = 2)
    tmp$Median <- round(as.numeric(as.character(tmp$Median)), digits = 2)
    tmp$Min    <- round(as.numeric(as.character(tmp$Min)), digits = 2)
    tmp$Max    <- round(as.numeric(as.character(tmp$Max)), digits = 2)
    tmp$p10    <- round(as.numeric(as.character(tmp$p10)), digits = 2)
    tmp$SD     <- round(as.numeric(as.character(tmp$SD)), digits = 2)
    tmp$p90    <- round(as.numeric(as.character(tmp$p90)), digits = 2)
    tmp$p75    <- round(as.numeric(as.character(tmp$p75)), digits = 2)
    tmp$p99    <- round(as.numeric(as.character(tmp$p99)), digits = 2)
    tmp$p1     <- round(as.numeric(as.character(tmp$p1)), digits = 2)
    tmp$p25    <- round(as.numeric(as.character(tmp$p25)), digits = 2)
    tmp
    tmp <- t(tmp)
    datatable(tmp, options = list("pageLength" = 13))
  })


  ## Alarms and settings

  output$table4 <- DT::renderDataTable({
    inFile <- input$file3
    if (is.null(GPS_f()) & is.null(BC_f()) & is.null(CPC_f()) & is.null(DT_f()) &
       is.null(RH_f()) & is.null(CO2_f())) {
      DT_f <- read.delim("data/DT8530/2019_09_25_h091000_KAN_DT8530.csv",
                         header = FALSE, sep = ",", row.names = NULL, skip = 2)
      DT_f <- DT_f[1:11, ]
      DT_f <- DT_f[ ,1:2]
      datatable(DT_f, options = list("pageLength" = 11))
    }
    else if (is.null(DT_f())) {}
    else if (!is.null(DT_f())) {
      files3 <- lapply(inFile$datapath, function(y) {
        JSON_csv <- read.delim(y, header = FALSE, sep = ",", row.names = NULL,
                               skip = 2)
        names(JSON_csv) <- c("Setting", "Value")
        JSON_csv <- JSON_csv[1:11, ]
        JSON_csv <- JSON_csv[ ,1:2]
        JSON_csv
      })
      data <- do.call(rbind, files3)
      DT_f <- data
      datatable(DT_f, options = list("pageLength" = 11))
    }
  })

  output$table3 <- DT::renderDataTable({
    inFile <- input$file4
    if (is.null(GPS_f()) & is.null(BC_f()) & is.null(CPC_f()) & is.null(DT_f()) &
       is.null(RH_f()) & is.null(CO2_f())) {
      CPC_f <- read.delim("data/CPC3007/2019_09_25_h091000_KAN_CPC3007.csv",
                          header = FALSE, sep = ",", row.names = NULL, skip = 1)
      names(CPC_f) <- c("Setting", "Value")
      CPC_f <- CPC_f[1:13, ]
      CPC_f <- CPC_f[ ,1:2]
      datatable(CPC_f, options = list("pageLength" = 13))
    }
    else if (is.null(CPC_f())) {}
    else if (!is.null(CPC_f())){
      files3 <- lapply(inFile$datapath, function(y) {
        JSON_csv <- read.delim(y, header = FALSE, sep = ",", row.names = NULL,
                               skip = 1)
        names(JSON_csv)<-c("Setting", "Value")
        JSON_csv <- JSON_csv[1:13, ]
        JSON_csv <- JSON_csv[ ,1:2]
        JSON_csv
      })
      data <- do.call(rbind, files3)
      CPC_f <- data
      datatable(CPC_f, options = list("pageLength" = 13))
    }
  })

  output$table2 <- DT::renderDataTable({
    inFile <- input$file2
    if (is.null(BC_f())) {}
    else if (!is.null(BC_f())){
      files3 <- lapply(inFile$datapath, function(y) {
        JSON_csv_header <- read.delim(y, header = FALSE, sep=",", skip = 15,
                                      row.names = NULL, stringsAsFactors = FALSE)
        JSON_csv_header <- JSON_csv_header[1, ]
        JSON_csv <- read.delim(y, skip = 17, header = FALSE, sep = ",")
        colnames(JSON_csv) <- unlist(JSON_csv_header)
        JSON_csv
      })
      data <- do.call(rbind, files3)
      BC_f <- data
      completeFun <- function(data, desiredColumns) {
        completeVec <- complete.cases(data[ , desiredColumns])
        return(data[completeVec, ])
      }
      BC_f <- completeFun(BC_f, c("BC"))
      BC_f <- BC_f[BC_f$Status != 0, ]
      BC_f
    }
  })

  output$table5 <- DT::renderDataTable({
    inFile <- input$file2
    if (is.null(GPS_f()) & is.null(BC_f()) & is.null(CPC_f()) & is.null(DT_f()) &
       is.null(RH_f()) & is.null(CO2_f())) {
      BC_f <- read.delim("data/AE51/2019_09_25_h091000_KAN_AE51.csv",
                         header = FALSE,
                         sep = " ", skip = 1, row.names = NULL)
      BC_f <- BC_f[1:14, ]
      names(BC_f) <- c("Setting")
      datatable(BC_f, options = list("pageLength" = 14))
    }
    else if (is.null(BC_f())) {"No AE51 files available"}
    else if (!is.null(BC_f())){
      files3 <- lapply(inFile$datapath, function(y){
        JSON_csv <- read.delim(y, header = FALSE, sep = " ",
                               skip = 1, row.names = NULL)
        JSON_csv <- JSON_csv[1:14, ]
        JSON_csv
      })
      data <- do.call(rbind, files3)
      BC_f <- data
      names(BC_f) <- c("Setting")
      datatable(BC_f, options = list("pageLength" = 14))

    }
  })


  ## Raw pollutants/GPS plot

  output$plot5 <- renderPlotly({
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4)
       & is.null(input$file5) & is.null(input$file6)) {
      data <- data_blank()
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(Latitude))) +
                 geom_line(size = 0.6, color = "dodgerblue2") +
                 labs(title = "Latitude (degree)",
                      y = "",
                      x = "") +
                 scale_x_datetime( date_labels  = "%H:%M") +
                 theme_minimal() +
                 theme(legend.text = element_text(size = 18),
                       plot.title = element_text(size = 14, face = "bold"),
                       axis.title = element_text(size = 14),
                       axis.text = element_text(size = 14, face = "bold"),
                       panel.border = element_rect(colour = "black",
                                                   fill = NA, size = 1.2))
               )}
    else if (is.null(GPS_f())) {}
    else if (!is.null(GPS_f())) {
      data <- data_joined()
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(Latitude))) +
                 geom_line(size = 0.6, color = "dodgerblue2") +
                 labs(title = "Latitude (degree)",
                      y = "",
                      x = "") +
                 scale_x_datetime( date_labels  = "%H:%M") +
                 theme_minimal() +
                 theme(legend.text = element_text(size = 18),
                       plot.title = element_text(size = 14, face = "bold"),
                       axis.title = element_text(size = 14),
                       axis.text = element_text(size = 14, face = "bold"),
                       panel.border = element_rect(colour = "black",
                                                   fill = NA, size = 1.2))
               )}
    })

  output$plot6 <- renderPlotly({
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4)
       & is.null(input$file5) & is.null(input$file6)){
      data <- data_blank()
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(Longitude))) +
                 geom_line(size = 0.6, color = "dodgerblue2") +
                 labs(title = "Longitude (degree)",
                      y = "",
                      x = "") +
                 scale_x_datetime(date_labels  = "%H:%M") +
                 theme_minimal() +
                 theme(legend.text = element_text(size = 18),
                       plot.title = element_text(size = 14, face = "bold"),
                       axis.title = element_text(size = 14),
                       axis.text = element_text(size = 14, face = "bold"),
                       panel.border = element_rect(colour = "black",
                                                   fill = NA, size = 1.2))
               )}
    else if (is.null(GPS_f())) {}
    else if (!is.null(GPS_f())) {
      data <- data_joined()
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(Longitude))) +
                 geom_line(size = 0.6, color = "dodgerblue2") +
                 labs(title = "Longitude (degree)",
                      y = "",
                      x = "") +
                 scale_x_datetime(date_labels = "%H:%M") +
                 theme_minimal() +
                 theme(legend.text = element_text(size = 18),
                       plot.title = element_text(size = 14, face = "bold"),
                       axis.title = element_text(size = 14),
                       axis.text = element_text(size = 14, face = "bold"),
                       panel.border = element_rect(colour = "black",
                                                   fill = NA, size = 1.2))
      )}
  })

  output$plot <- renderPlotly({
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4)
       & is.null(input$file5) & is.null(input$file6)) {
      data <- data_blank()
      data$PM2.5 <- as.numeric(as.character(data$PM2.5))
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(PM2.5))) +
                 geom_line(size = 0.6, color = "dodgerblue2") +
                 labs(title = "DT8530_PM2.5 (ug/m3)",
                      y = "",
                      x = "") +
                 scale_x_datetime(date_labels  = "%H:%M") +
                 scale_y_continuous() +
                 theme_minimal() +
                 theme(legend.text = element_text(size = 18),
                       plot.title = element_text(size = 14, face = "bold"),
                       axis.title = element_text(size = 14),
                       axis.text = element_text(size = 14, face = "bold"),
                       panel.border = element_rect(colour = "black",
                                                   fill = NA, size = 1.2))
            )}
    else if (is.null(DT_f())) {}
    else if (!is.null(DT_f())) {
      data <- data_joined()
      data$PM2.5 <- as.numeric(as.character(data$PM2.5))
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(PM2.5))) +
                 geom_line(size = 0.6, color = "dodgerblue2") +
                 labs(title = "DT8530_PM2.5 (ug/m3)",
                      y = "",
                      x = "") +
                 scale_x_datetime(date_labels  = "%H:%M") +
                 scale_y_continuous() +
                 theme_minimal() +
                 theme(legend.text = element_text(size = 18),
                       plot.title = element_text(size = 14, face = "bold"),
                       axis.title = element_text(size = 14),
                       axis.text = element_text(size = 14, face = "bold"),
                       panel.border = element_rect(colour = "black",
                                                   fill = NA, size = 1.2))
               )}
  })

  output$plot4<- renderPlotly({
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4)
       & is.null(input$file5) & is.null(input$file6)) {
      data <- data_blank()
      data$RH <- data$RH * 100
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(RH))) +
                 geom_line(size = 0.6, color = "dodgerblue2")+
                 labs(title = "Relative Humidity (%)",
                      y = "",
                      x = "") +
                 scale_x_datetime(date_labels  = "%H:%M") +
                 scale_y_continuous(limits = c(20,100)) +
                 theme_minimal() +
                 theme(legend.text = element_text(size = 18),
                       plot.title = element_text(size = 14, face = "bold"),
                       axis.title = element_text(size = 14),
                       axis.text = element_text(size = 14, face = "bold"),
                       panel.border = element_rect(colour = "black",
                                                   fill = NA, size = 1.2))
               )}
    else if (is.null(RH_f())) {}
    else if (!is.null(RH_f())) {
      data <- data_joined()
      data$RH <- data$RH * 100
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(RH))) +
                 geom_line(size = 0.6, color = "dodgerblue2") +
                 labs(title = "Relative Humidity (%)",
                      y = "",
                      x = "") +
                 scale_x_datetime(date_labels = "%H:%M") +
                 scale_y_continuous(limits = c(20,100)) +
                 theme_minimal() +
                 theme(legend.text = element_text(size = 18),
                       plot.title = element_text(size = 14, face = "bold"),
                       axis.title = element_text(size = 14),
                       axis.text = element_text(size = 14, face = "bold"),
                       panel.border = element_rect(colour = "black",
                                                   fill = NA, size = 1.2))
               )}
    })

  output$plot2 <- renderPlotly({
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4)
       & is.null(input$file5) & is.null(input$file6)) {
      data <- data_blank()
      data$BC <- as.numeric(as.character(data$BC))
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(BC))) +
                 geom_line(size = 0.6, color = "dodgerblue2")+
                 labs(title = "AE51_BC (ug/m3)",
                      y = "",
                      x = "") +
                 scale_x_datetime(date_labels  = "%H:%M") +
                 scale_y_continuous() +
                 theme_minimal() +
                 theme(legend.text = element_text(size = 18),
                       plot.title = element_text(size = 14, face = "bold"),
                       axis.title = element_text(size = 14),
                       axis.text = element_text(size = 14, face = "bold"),
                       panel.border = element_rect(colour = "black",
                                                   fill = NA, size = 1.2))
               )}
    else if (is.null(BC_f())) {}
    else if (!is.null(BC_f())) {
      data <- data_joined()
      data$BC <- as.numeric(as.character(data$BC))
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(BC))) +
                 geom_line(size = 0.6, color = "dodgerblue2")+
                 labs(title = "AE51_BC (ug/m3)",
                      y = "",
                      x = "") +
                 scale_x_datetime(date_labels  = "%H:%M") +
                 scale_y_continuous() +
                 theme_minimal() +
                 theme(legend.text = element_text(size = 18),
                       plot.title = element_text(size = 14, face = "bold"),
                       axis.title = element_text(size = 14),
                       axis.text = element_text(size = 14, face = "bold"),
                       panel.border = element_rect(colour = "black",
                                                   fill = NA, size = 1.2))
               )}
  })

  output$plot3 <- renderPlotly({
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4)
       & is.null(input$file5) & is.null(input$file6)) {
      data <- data_blank()
      data$Particle_conc <- as.numeric(as.character(data$Particle_conc))
      ggplotly(ggplot(data, aes(as.POSIXct(date), Particle_conc)) +
                 geom_line(size = 0.6, color = "dodgerblue2")+
                 labs(title = "CPC3007_Particle Concentration (#/cm3)",
                      y = "",
                      x = "") +
                 scale_x_datetime(date_labels = "%H:%M") +
                 scale_y_continuous() +
                 theme_minimal() +
                 theme(legend.text = element_text(size = 18),
                       plot.title = element_text(size = 14, face = "bold"),
                       axis.title = element_text(size = 14),
                       axis.text = element_text(size = 14, face = "bold"),
                       panel.border = element_rect(colour = "black",
                                                   fill = NA, size = 1.2))
               )}
    else if (is.null(CPC_f())) {}
    else if (!is.null(CPC_f())) {
      data <- data_joined()
      data$Particle_conc <- as.numeric(as.character(data$Particle_conc))
      ggplotly(ggplot(data, aes(as.POSIXct(date), Particle_conc)) +
                 geom_line(size = 0.6, color = "dodgerblue2")+
                 labs(title = "CPC3007_Particle Concentration (#/cm3)",
                      y = "",
                      x = "") +
                 scale_x_datetime( date_labels  = "%H:%M") +
                 scale_y_continuous() +
                 theme_minimal() +
                 theme(legend.text = element_text(size = 18),
                       plot.title = element_text(size = 14, face = "bold"),
                       axis.title = element_text(size = 14),
                       axis.text = element_text(size = 14, face = "bold"),
                       panel.border = element_rect(colour = "black",
                                                   fill = NA, size = 1.2))
               )}
  })

  output$plot7 <- renderPlotly({
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4)
       & is.null(input$file5) & is.null(input$file6)) {
      data <- data_blank()
      ggplotly(ggplot(data, aes(as.POSIXct(date), CO2)) +
                 geom_line(size = 0.6, color = "dodgerblue2") +
                 labs(title = "LI-COR_CO2 (ppm)",
                      y = "",
                      x = "") +
                 scale_x_datetime( date_labels  = "%H:%M") +
                 scale_y_continuous() +
                 theme_minimal() +
                 theme(legend.text = element_text(size = 18),
                       plot.title = element_text(size = 14, face = "bold"),
                       axis.title = element_text(size = 14),
                       axis.text = element_text(size = 14, face = "bold"),
                       panel.border = element_rect(colour = "black",
                                                   fill = NA, size = 1.2))
               )}
    else if (is.null(CO2_f())) {}
    else if (!is.null(CO2_f())) {
      data <- data_joined()
      ggplotly(ggplot(data, aes(as.POSIXct(date), CO2)) +
                 geom_line(size = 0.6, color = "dodgerblue2") +
                 labs(title = "LI-COR_CO2 (ppm)",
                      y = "",
                      x = "") +
                 scale_x_datetime( date_labels  = "%H:%M") +
                 scale_y_continuous() +
                 theme_minimal() +
                 theme(legend.text = element_text(size = 18),
                       plot.title = element_text(size = 14, face = "bold"),
                       axis.title = element_text(size = 14),
                       axis.text = element_text(size = 14, face = "bold"),
                       panel.border = element_rect(colour = "black",
                                                   fill = NA, size = 1.2))
               )}
  })

  observe({
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4)
       & is.null(input$file5) & is.null(input$file6)) {
      data_joined <- data_blank()
      data_joined$date          <- NULL
      data_joined$Latitude      <- NULL
      data_joined$Longitude     <- NULL
      data_joined$PM2.5_RHC     <- NULL
      data_joined$PM2.5_RHC_Ref <- NULL
    }else {
      data_joined <- data_joined()
      data_joined$date      <- NULL
      data_joined$Latitude  <- NULL
      data_joined$Longitude <- NULL

      if (is.null(CPC_f())) {
        data_joined$Particle_conc <- NULL
      }
      if (!is.null(CPC_f())) {
        data_joined$Particle_conc <- data_joined$Particle_conc
      }
      if (is.null(RH_f())) {
        data_joined$RH <- NULL
      }
      if (!is.null(RH_f())) {
        data_joined$RH <- data_joined$RH
      }
      if (is.null(DT_f())) {
        data_joined$PM2.5         <- NULL
        data_joined$PM2.5_RHC     <- NULL
        data_joined$PM2.5_RHC_Ref <- NULL
        data_joined$PM2.5_Ref     <- NULL
        }
      if (!is.null(DT_f())) {
        data_joined$PM2.5 <- data_joined$PM2.5
        data_joined$PM2.5_RHC <- data_joined$PM2.5_RHC
        data_joined$PM2.5_RHC_Ref <- data_joined$PM2.5_RHC_Ref
        data_joined$PM2.5_Ref <- data_joined$PM2.5_Ref
      }
      if (is.null(BC_f())) {
        data_joined$BC       <- NULL
        data_joined$BC_NR    <- NULL
        data_joined$BC_NR_LC <- NULL
      }
      if (!is.null(BC_f())) {
        data_joined$BC <- data_joined$BC
        data_joined$BC_NR <- data_joined$BC_NR
        data_joined$BC_NR_LC <- data_joined$BC_NR_LC
      }
      if (is.null(CO2_f())) {
        data_joined$CO2 <- NULL
      }
      if (!is.null(CO2_f())) {
        data_joined$CO2 <- data_joined$CO2
      }
    }
    updateSelectInput(session, "palleInp", choices = names(data_joined))
  })


  ## Mapping pollutant

  output$map <- renderLeaflet({
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4)
       & is.null(input$file5) & is.null(input$file6)) {
      data <- data_blank()
    }else {
      data <- data_joined()
    }
    data$RH <- data$RH * 100
    if (input$palleInp == "BC_NR" | input$palleInp == "BC_NR_LC") {
      risk.bins <- c(0, 0.5, 2, 5, 10, 20, 40, 100, 500, 2000, 10000)
      pal <- colorBin( "Spectral", bins = risk.bins, na.color = "#808080",
                       reverse = TRUE)
    } else if (input$palleInp == "BC") {
      risk.bins <- c( -100, -40, -20, 0, 5, 10, 20, 40, 100, 500, 2000)
      pal <- colorBin( "Spectral", bins = risk.bins, na.color = "#808080",
                       reverse = TRUE)
    } else if (input$palleInp == "PM2.5") {
      risk.bins <- c(0, 10, 25, 50, 100, 500, 2000, 5000, 10000)
      pal <- colorBin( "Spectral", bins = risk.bins, na.color = "#808080",
                       reverse = TRUE)
    } else if (input$palleInp == "Particle_conc") {
      risk.bins <- c(0, 5000, 10000, 20000, 40000, 70000, 100000, 150000,
                     200000, 500000)
      pal <- colorBin( "Spectral", bins = risk.bins, na.color = "#808080",
                       reverse = TRUE)
    }else if (input$palleInp == "RH") {
      pal <- colorNumeric("RdYlGn", domain = data$RH, na.color = "#808080",
                          reverse = TRUE)
    }else if (input$palleInp == "CO2") {
      pal <- colorNumeric("RdYlGn", domain = data$CO2, na.color = "#808080",
                          reverse = TRUE)
    }else if (input$palleInp == "Latitude") {
      pal <- colorNumeric("RdYlGn", domain = data$Latitude, na.color = "#808080",
                          reverse = TRUE)
    }else {
      risk.bins <- c(0, 10, 25, 50, 100, 500, 2000, 5000, 10000)
      pal <- colorBin( "Spectral", bins = risk.bins, na.color = "#808080",
                       reverse = TRUE)
    }

    leaflet(data) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      addCircles(data = data,
                 lng = ~Longitude,
                 lat = ~Latitude,
                 popup =  paste("Date:", data$date, "<br>",
                               "AE51_BC (ug/m3):",
                               round(as.numeric(data$BC), digits = 2), "<br>",
                               "DT8530_PM2.5 (ug/m3):",
                               round(as.numeric(data$PM2.5), digits = 2), "<br>",
                               "RH(%):",
                               round(as.numeric(data$RH), digits = 2), "<br>",
                               "CPC3007_Particle Concentration (#/cm3):",
                               round(as.numeric(data$Particle_conc), digits = 2),"<br>",
                               "CO2:", round(as.numeric(data$CO2),digits = 2)),
                 weight = 3, radius = 8,
                 col = ~pal(data[[input$palleInp]]), stroke = TRUE,
                 fillOpacity = 0.8) %>%
      leaflet::addLegend("bottomright", pal = pal, values = ~data[[input$palleInp]],
                         title = paste(input$palleInp))
  })
}
## Run app
shinyApp(ui, server)






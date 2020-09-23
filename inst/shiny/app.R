library(shiny)
library(Cairo)
library(DT)
library(plotly)
library(data.table)
library(tidyverse)
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
                                              selectInput("timezone",
                                                          label = "",
                                                          choices = OlsonNames(),
                                                          selected = "Asia/Kolkata"),
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
    if (is.null(input$file4)) {
      return(NULL)
    } else {
      df_list <- lapply(input$file4$datapath, function(y) {
        JSON_csv <- read.delim(y, header = FALSE, sep = ",",
                               row.names = NULL, skip = 4)
        JSON_csv_date <- as.Date(as.character(JSON_csv[1, 2]), format = "%m/%d/%y")
        if (is.na(JSON_csv_date)) {
          JSON_csv_date <- as.Date(as.character(JSON_csv[1, 2]), format = "%m-%d-%Y")
        }
        as.Date(JSON_csv_date)
      }
      )}
    CPC_f2_date <- df_list[[1]]
    return(CPC_f2_date)
  })

  GPS_f_date <- reactive({
    GPS_f  <- data.frame()
    if (is.null(input$file1)) {
      return(NULL)
    }
    req(input$file1)
    files3 <- lapply(input$file1$datapath, function(y) {
      pfile2 <- XML::htmlTreeParse(y, error = function (...) {},
                                   useInternalNodes = T)
      geodf2 <- data.frame(time = XML::xpathSApply(pfile2,
                                                   path = "//trkpt/time",
                                                   xmlValue))
    })
    GPS_f <- do.call(rbind, files3)
    GPS_f <- GPS_f %>%
      mutate(date = with_tz(ymd_hms(time), input$timezone)) %>%
      distinct(date, .keep_all = TRUE) %>%
      dplyr::select(date)
    GPS_f_date <- as.Date(as.Date(GPS_f[1, 1]), format="%Y-%m-%d") #"%m/%d/%Y"
    return(GPS_f_date)
  })

  BC_f_date <- reactive({
    if (is.null(input$file2)) {
      return(NULL)
    } else {
      df_list <- lapply(input$file2$datapath, function(y) {
        JSON_csv_header <- read.delim(y, header = FALSE, sep = ",",
                                      skip = 15, row.names = NULL,
                                      stringsAsFactors = FALSE)
        JSON_csv_header <- JSON_csv_header[1, ]
        JSON_csv <- read.delim(y, skip = 17, header = FALSE, sep = ",")
        colnames(JSON_csv) <- unlist(JSON_csv_header)
        JSON_csv <- JSON_csv %>%
          drop_na(BC)
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
      BC_f <- do.call(rbind, df_list)
      BC_f_date <- as.Date(as.character(BC_f[1, 1]), format = "%d-%m-%Y")
      if (is.na(BC_f_date)) {
        BC_f_date <- as.Date(as.character(BC_f[1, 1]), format = "%Y/%m/%d")
      }
      return(BC_f_date)
    }
  })

  DT_f_date <- reactive({
    if (is.null(input$file3)) {
      return(NULL)
    } else {
      df_list <- lapply(input$file3$datapath, function(y) {
        JSON_csv <- read.delim(y, header = TRUE, sep = ",",
                               row.names = NULL, skip = 29)
        JSON_csv_date <- as.Date(as.character(JSON_csv[1, 1]),
                                 format = "%d-%m-%Y")
        if (is.na(JSON_csv_date)) {
          JSON_csv_date <- as.Date(as.character(JSON_csv[1, 1]),
                                   format = "%m/%d/%Y")
        }
        as.Date(JSON_csv_date)
      })
      DT_f_date <- df_list[[1]]
      return(DT_f_date)
    }
  })

  CO2_f_date <- reactive({
    if (is.null(input$file6)) {
      return(NULL)
    } else {
      df_list <- lapply(input$file6$datapath, function(y) {
        JSON_csv <- read.delim(y, header = TRUE, sep = ";",
                               row.names = NULL, skip = 2)
        JSON_csv_date <- as.Date(as.character(JSON_csv[1, 1]), format = "%d-%m-%Y")
        if (is.na(JSON_csv_date)) {
          JSON_csv_date <- as.Date(as.character(JSON_csv[1, 1]), format = "%Y-%m-%d")
        }
        as.Date(JSON_csv_date)
      })
      CO2_f_date <- df_list[[1]]
      return(CO2_f_date)
    }
  })

  ## GPS and pollutant file joining

  CO2_f <- reactive({
    if (is.null(input$file6)) {
      return(NULL)
    } else {
      df_list <- lapply(input$file6$datapath, function(y) {
        JSON_csv <- read.delim(y, skip = 1, sep = ",",
                               header = TRUE, row.names = NULL,
                               stringsAsFactors = FALSE)
        JSON_csv <- JSON_csv[ , 1:3]
        names(JSON_csv) <- c("Date", "Time", "CO2")
        JSON_csv
      })
      CO2_f <- do.call(rbind, df_list)
      CO2_f <- CO2_f %>%
        mutate(date = as.POSIXct(paste(as.Date(Date, format = "%d-%m-%Y"), Time),
                           tz = input$timezone)) %>%
        select(date, CO2)
      return(CO2_f)
    }
  })

  GPS_f <- reactive({
    if (is.null(input$file1)) {
      return(NULL)
    }
    req(input$file1)
    files3 <- lapply(input$file1$datapath, function(y) {
      pfile2 <- XML::htmlTreeParse(y, error = function (...) {}, useInternalNodes = T)
      elevations <- as.numeric(XML::xpathSApply(pfile2, path = "//trkpt/ele",
                                           xmlValue))
      times  <- XML::xpathSApply(pfile2, path = "//trkpt/time", xmlValue)
      coords <- XML::xpathSApply(pfile2, path = "//trkpt", xmlAttrs)
      geodf2 <- data.frame(latitude = as.numeric(coords["lat", ]),
                           longitude = as.numeric(coords["lon", ]),
                           ele = elevations, time = times)
    })
    GPS_f <- do.call(rbind, files3)
    GPS_f <- GPS_f %>%
      mutate(date = with_tz(ymd_hms(time), input$timezone)) %>%
      distinct(date, .keep_all = TRUE) %>%
      select(date, latitude, longitude)
    return(GPS_f)
  })

  BC_f <- reactive({
    if (is.null(input$file2)) {
      return(NULL)
    } else {
      df_list <- lapply(input$file2$datapath, function(y){
        JSON_csv_header <- read.delim(y, header = FALSE, sep = ",", skip = 15,
                                      row.names = NULL,
                                      stringsAsFactors = FALSE)
        JSON_csv_header <- JSON_csv_header[1, ]
        JSON_csv <- read.delim(y, skip = 17, header = FALSE, sep = ",")
        colnames(JSON_csv) <- unlist(JSON_csv_header)
        JSON_csv <- JSON_csv %>%
          drop_na(BC)
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
      BC_f <- do.call(rbind, df_list)
      BC_f$Date <- BC_f$date1  #"%Y/%m/%d", "%d-%m-%Y"
      ef_file <- BC_f %>%
        filter(Status == 0) %>%
        select(Date, ATN, BC) %>%
        mutate(BC1 = (BC / 1000), LD = BC1 - rollapply(BC1, FUN = mean,
                                    width = 30, align = "center",
                                    partial = TRUE)) %>%
        mutate(LD75 = runquantile(LD, 300, 0.75, type = 2,
                                  endrule = c("NA")),
               LD25 = runquantile(LD, 300, 0.25, type = 2,
                                  endrule = c("NA")),
               BC2 = BC1, BC3 = BC1) %>%
        mutate(BC2 = ifelse(BC2 >= 0, 0, BC2),
               BC2 = ifelse(BC2 < 0, 1, BC2)) %>%
        mutate(BC2 = rollapply(BC2, FUN = mean, width = 5,
                         align = "center",  partial = TRUE),
               cev1 = ifelse((LD > 5 * LD75) |
                               (LD < 5 * LD25), BC1, NA))
      ATN <- ef_file[1, 2]
      # ef_file$ATN <- ef_file$ATN-(ATN) # for new filter
      BC_Final <- ef_file
      CEV <- data.frame(ef_file$Date, ef_file$cev1)
      CEV$ef_file.cev1[!is.na(CEV$ef_file.cev1)] <- 1
      CEV <- data.frame(CEV)
      date_file <- data.frame(ef_file$Date, ef_file$BC2, ef_file$BC3)
      CEV <- CEV %>%
        drop_na(ef_file.cev1)
      setDT(CEV)
      setDT(date_file)
      cev_file <- date_file[CEV, on = c('ef_file.Date')]
      cev_file <- cev_file[!(cev_file$ef_file.BC2 == 0), ]
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
      if (nrow(Date_Table) == 0 | is.na(nrow(Date_Table))) {
        BC <- ef_file %>%
          mutate(BC_factor = 1)
      } else {
        Date_Table <- Date_Table %>%
          mutate(BC_Factor = 0,
                 Date = as.POSIXct(Date, tz = input$timezone))
        setDT(Date_Table)
        setDT(ef_file)
        BC <- Date_Table[ef_file, on = c('Date')]
        BC$BC_Factor[is.na(BC$BC_Factor)] <- 1
      }
      BC <- BC %>%
        mutate(BC_Fi = BC_Factor * BC1)
      BC$BC_Fi[BC$BC_Fi == 0] <- NA
      BC <- BC %>%
        mutate(Tr = exp(- ATN / 100),
               CF = (1 / (0.88 * Tr + 0.12)),
               BC_Final = BC_Fi * CF)
      BC$BC_Fi[BC$BC_Fi < 0] <- NA
      BC$BC_Fi[is.na(BC$BC_Fi)] <- " "
      BC$BC_Final[BC$BC_Final < 0] <- NA
      BC$BC_Final[is.na(BC$BC_Final)] <- " "
      BC_Final <- BC %>%
        select("date" = Date, "BC" = BC3, "BC_NR" = BC_Fi, "BC_NR_LC" = BC_Final) %>%
        mutate_at(c('BC', 'BC_NR', 'BC_NR_LC'), as.numeric)
      return(BC_Final)
    }
  })

  DT_f <- reactive({
    if (is.null(input$file3)) {
      return(NULL)
    } else {
      df_list <- lapply(input$file3$datapath, function(y) {
        JSON_csv <- read.delim(y, header = TRUE, sep = ",", row.names = NULL,
                               skip = 28)
        names(JSON_csv) <- c("Date", "Time", "PM2.5")
        JSON_csv
      })
      DT_f <- do.call(rbind, df_list)
      Date1 <- as.Date(DT_f[1, 1], format = "%d-%m-%Y",
                       tz = input$timezone)
      if (is.na(Date1)) {
        Date1 <- as.Date(DT_f[1, 1], format = "%m/%d/%Y",
                         tz = input$timezone)
      }
      DT_f$date <- as.POSIXct(strptime(paste(Date1, DT_f$Time),
                                       format = "%Y-%m-%d %H:%M:%S"),
                              format = '%Y-%m-%d %H:%M:%S',
                              tz = input$timezone)
      DT_f <- DT_f %>%
        mutate(PM2.5 = PM2.5 * 1000) %>%
        dplyr::select(date, PM2.5)
      return(DT_f)
    }
  })

  ## file name matching

  file_name_CPC <- reactive({
    if (is.null(input$file4)) {
      return(NULL)
    } else {
      name_CPC <- substr(sub(".csv$", "", basename(input$file4$name)), 1, 10)
      return(name_CPC)
    }
  })

  CPC_f <- reactive({
    DF <- input$DF
    name_CPC <- file_name_CPC()
    if (is.null(input$file4)) {
      return(NULL)
    } else {
      df_list <- lapply(input$file4$datapath, function(y) {
        JSON_csv <- read.delim(y, header = TRUE, sep = ",", row.names = NULL,
                               skip = 17,
                               stringsAsFactors = FALSE, fileEncoding = "latin1")
        JSON_csv
      })
      CPC_f  <- do.call(rbind, df_list)
      u <- stringr::str_extract(name_CPC, "[0-9]{4}\\_[0-9]{2}\\_[0-9]{2}")
      w <- str_replace_all(u, "_", "-")
      CPC_f <- CPC_f[, 1:2]
      names(CPC_f) <- c("Time", "Particle_conc")
      CPC_f$Particle_conc <- CPC_f$Particle_conc * DF
      CPC_f <- CPC_f %>%
        mutate(date = ymd_hms(paste(w, Time), tz = input$timezone)) %>%
        select(date, Particle_conc)
      return(CPC_f)
    }
  })

  RH_f <- reactive({
    if (is.null(input$file5)) {
      return(NULL)
    } else {
      RH_f <- data.frame(read.delim(input$file5$datapath, header = TRUE,
                                    sep = ",", skip = 6, row.names = NULL))
      RH_f_Date <- RH_f[ ,2]
      RH_f_Time <- RH_f[ ,3]
      RH <- RH_f[ , grepl( "RH", names(RH_f ))]
      RH_f <- data.frame(RH_f_Date, RH_f_Time, RH)
      names(RH_f) <- c("LogDate", "LogTime", "RH")
      RH_f$LogTime <- gsub(".", ":", RH_f$LogTime, fixed = TRUE)
      RH_f$LogTime <- gsub("AM", "", RH_f$LogTime, fixed = TRUE)
      RH_f$LogTime <- gsub("PM", "", RH_f$LogTime, fixed = TRUE)
      RH_f$date <- as.POSIXct(paste(RH_f$LogDate, RH_f$LogTime),
                              format = '%d-%m-%Y %H:%M:%S', tz = input$timezone)
      RH_f <- RH_f%>%
        dplyr::select(date, RH) %>%
        mutate_at(c('RH'), as.numeric)
      return(RH_f)
    }
  })

  file_name_CO2 <- reactive({
    if (is.null(input$file6)) {
      return(NULL)
    } else {
      name_CO2 <- substr(sub(".csv$", "", basename(input$file6$name)), 1, 10)
      return(name_CO2)
    }
  })

  file_name_RH <- reactive({
    if (is.null(input$file5)) {
      return(NULL)
    } else {
      name_RH <- substr(sub(".csv$", "", basename(input$file5$name)), 1, 10)
      return(name_RH)
    }
  })

  file_name_GPS <- reactive({
    if (is.null(input$file1)) {
      return(NULL)
    } else {
      name_GPS <- substr(sub(".csv$", "", basename(input$file1$name)), 1, 10)
      return(name_GPS)
    }
  })

  file_name_BC <- reactive({
    if (is.null(input$file2)) {
      return(NULL)
    } else {
      name_BC <- substr(sub(".csv$", "", basename(input$file2$name)), 1, 10)
      return(name_BC)
    }
  })

  file_name_DT <- reactive({
    if (is.null(input$file3)) {
      return(NULL)
    } else {
      name_DT <- substr(sub(".csv$", "", basename(input$file3$name)), 1, 10)
      return(name_DT)
    }
  })

  ## preloaded table

  data_blank <- reactive({
    if (is.null(input$file1) & is.null(input$file2) & is.null(input$file3) &
        is.null(input$file4) & is.null(input$file5) & is.null(input$file6)) {
      joined <- read.delim("data/joined_file.csv", header = TRUE, sep = ",") %>%
        select(- starts_with("X")) %>%
        distinct(date.LT., .keep_all = TRUE)
      names(joined) <- c("date", "Latitude", "Longitude",  "BC", "BC_NR",
                         "BC_NR_LC", "PM2.5", "PM2.5_RHC", "PM2.5_RHC_Ref",
                         "PM2.5_Ref", "RH", "Particle_conc", "CO2")
      joined
    }
  })

  joined_GPS_CO2 <- reactive({
    name_GPS <- file_name_GPS()
    name_CO2 <- file_name_CO2()
    GPS_f <- GPS_f()
    CO2_f <- CO2_f()
    if (is.null(CO2_f)) {
      joined_GPS_CO2 <- dplyr::select(GPS_f, date, latitude, longitude)
      joined_GPS_CO2$CO2 <- NA
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
                                      latitude, longitude)
    }
    return(joined_GPS_CO2)
  })

  joined_GPS_DT <- reactive({
    name_GPS <- file_name_GPS()
    name_DT  <- file_name_DT()
    GPS_f <- GPS_f()
    DT_f <- DT_f()
    if (is.null(DT_f)) {
      joined_GPS_DT <- GPS_f %>%
        dplyr::select(date, latitude, longitude) %>%
        mutate(PM2.5 = NA, PM2.5_RHC = NA) %>%
        mutate_at(c('PM2.5'), as.numeric)
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
      joined_GPS_DT <- left_join(GPS_f, DT_f, by = "date")
      joined_GPS_DT <- joined_GPS_DT %>%
        mutate(PM2.5_RHC = NA) %>%
        dplyr::select(date, PM2.5, PM2.5_RHC, latitude, longitude)
    }
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
        return (1 + (0.25 * ((x * x) / (1 - x))))
      } else {
        return (1)
      }
    }
    if (is.null(RH_f()) | is.null(DT_f())) {
      DT_f <- joined_GPS_DT %>%
        mutate(RH = NA, PM2.5_RHC = NA, PM2.5_Ref = NA, PM2.5_RHC_Ref = NA,
               PM2.5_Ref = ((PM2.5 * Slope) + Intercept)) %>%
        mutate_at(c('RH'), as.numeric)
    }
    if (!is.null(RH_f()) & !is.null(DT_f())) {
      validate(
        need(try(file_name_DT() == file_name_RH()), "File names of DustTrak
             8530 and RH do not match, Please select the correct files")
      )
      setDT(RH_f)
      setkey(RH_f, date)
      DT_f <- RH_f[joined_GPS_DT, roll = "nearest"]
      DT_f$RH <- DT_f$RH / 100
      setDT(DT_f)
      DT_f$CF <- sapply(DT_f$RH, FUN = VecFunc)
      DT_f <- DT_f %>%
        select(date, latitude, longitude, PM2.5, RH, CF) %>%
        mutate(PM2.5_RHC = (PM2.5 / CF),
               PM2.5_Ref = ((PM2.5 * Slope) + Intercept),
               PM2.5_RHC_Ref = ((PM2.5_RHC * Slope) + Intercept),
               date = as.POSIXct(date, format = '%Y-%m-%d %H:%M:%S',
                                 tz = input$timezone))
      }
    return(DT_f)
  })

  joined_GPS_BC <- reactive({
    name_GPS <- file_name_GPS()
    name_BC <- file_name_BC()
    GPS_f <- GPS_f()
    BC_Final <- BC_f()
    if (is.null(BC_f())) {
      joined_GPS_BC <- GPS_f %>%
        dplyr::select(date, latitude, longitude) %>%
        mutate(BC = NA, BC_NR = NA, BC_NR_LC = NA)
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
      joined_GPS_BC <- dplyr::select(joined_GPS_BC, date, BC, BC_NR, BC_NR_LC,
                                     latitude, longitude )
    }
    return(joined_GPS_BC)
  })

  joined_GPS_CPC <- reactive({
    name_GPS <- file_name_GPS()
    name_CPC <- file_name_CPC()
    GPS_f <- GPS_f()
    CPC_f <- CPC_f()
    if (is.null(CPC_f)) {
      joined_GPS_CPC <- dplyr::select(GPS_f, date, latitude, longitude)
      joined_GPS_CPC$Particle_conc <- NA
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
    joined_1 <- left_join(joined_GPS_CPC, joined_GPS_CO2, by = 'date') %>%
      left_join(., DT_RH, by = 'date')
    setDT(joined_1)
    setkey(joined_1, date)
    joined <- joined_1[joined_GPS_BC, roll = "nearest"]
    joined <- joined %>%
      dplyr::select(date, "Latitude" = latitude, "Longitude" = longitude,
                    BC, BC_NR, BC_NR_LC, PM2.5, PM2.5_RHC, PM2.5_RHC_Ref,
                    PM2.5_Ref, RH, Particle_conc, CO2) %>%
      distinct(date, .keep_all = TRUE) %>%
      mutate_at(c('BC', 'BC_NR', 'BC_NR_LC', 'PM2.5', 'PM2.5_RHC', 'PM2.5_RHC_Ref',
                  'PM2.5_Ref', 'RH', 'Particle_conc', 'CO2'), as.numeric)

    return(joined)
  })

  ## Final corrected, joined table

  output$table1 <- DT::renderDataTable({
    if (is.null(input$file1) & is.null(input$file2) & is.null(input$file3) &
        is.null(input$file4) & is.null(input$file5) & is.null(input$file6)) {
      data_joined <- data_blank()
    } else {
      data_joined <- data_joined()
    }
    data_joined <- data_joined %>%
      dplyr::select(date, Latitude, Longitude, BC, BC_NR, BC_NR_LC, PM2.5,
                    PM2.5_RHC, PM2.5_RHC_Ref, PM2.5_Ref, RH,
                    Particle_conc, CO2)
    setDT(data_joined)
    cols <- names(data_joined)[4:ncol(data_joined)]
    data_joined[,(cols) := round(.SD, 2), .SDcols = cols]
    data_joined$Longitude     <- round(as.numeric(data_joined$Longitude),
                                       digits = 4)
    data_joined$Latitude      <- round(as.numeric(data_joined$Latitude),
                                       digits = 4)
    names(data_joined) <- c("date", "Latitude", "Longitude", "AE51_BC (ug/m3)",
                            "AE51_BC_NR (ug/m3)", "AE51_BC_NR_LC (ug/m3)",
                            "DT8530_PM2.5 (ug/m3)", "DT8530_PM2.5_RHC (ug/m3)",
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
      } else {
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
    } else {
      data <- data_joined()
    }
    data <- data %>%
      dplyr::select(BC, BC_NR, BC_NR_LC, PM2.5, PM2.5_RHC,
                          PM2.5_RHC_Ref, PM2.5_Ref, RH, Particle_conc, CO2) %>%
      mutate(RH = (RH * 100))
    names(data) <- c("AE51_BC (ug/m3)", "AE51_BC_NR (ug/m3)",
                     "AE51_BC_NR_LC (ug/m3)",
                     "DT8530_PM2.5 (ug/m3)", "DT8530_PM2.5_RHC (ug/m3)",
                     "DT8530_PM2.5_RHC_Ref (ug/m3)",
                     "DT8530_PM2.5_Ref (ug/m3)", "RH(%)",
                     "CPC3007_Particle Concentration (#/cm3)",
                     "LI-COR_CO2 (ppm)")
    columns <- 1:ncol(data)
    data[ , columns] <- lapply(columns,
                               function(x) as.numeric(as.character(data[[x]])))
    tmp1 <- do.call(data.frame,
                    list(Mean = round(apply(data, 2, mean, na.rm = TRUE), digits = 2),
                         SD = round(apply(data, 2, sd, na.rm = TRUE), digits = 2),
                         Median = round(apply(data, 2, median, na.rm = TRUE), digits = 2),
                         IQR = round(apply(data, 2, IQR, na.rm = TRUE), digits = 2),
                         Min = round(apply(data, 2, min, na.rm = TRUE), digits = 2),
                         Max = round(apply(data, 2, max, na.rm = TRUE), digits = 2),
                         p1  = round(apply(data, 2, quantile, probs = c(0.01),
                                           na.rm = TRUE), digits = 2),
                         p10 = round(apply(data, 2, quantile, probs = c(0.1),
                                           na.rm = TRUE), digits = 2),
                         p25 = round(apply(data, 2, quantile, probs = c(0.25),
                                           na.rm = TRUE), digits = 2),
                         p75 = round(apply(data, 2, quantile, probs = c(0.75),
                                           na.rm = TRUE), digits = 2),
                         p90 = round(apply(data, 2, quantile, probs = c(0.9),
                                           na.rm = TRUE), digits = 2),
                         p99 = round(apply(data, 2, quantile, probs = c(0.99),
                                           na.rm = TRUE), digits = 2),
                         Total_non_NA = round(apply(data, 2,
                                                    function(x)
                                                    {length(which(!is.na(x)))}), digits = 2)))
    tmp <- t(tmp1)
    datatable(tmp, options = list("pageLength" = 13))
  })

  ## Alarms and settings

  output$table4 <- DT::renderDataTable({
    if (is.null(GPS_f()) & is.null(BC_f()) & is.null(CPC_f()) & is.null(DT_f()) &
        is.null(RH_f()) & is.null(CO2_f())) {
      DT_f <- read.delim("data/DT8530/2019_09_25_h091000_KAN_DT8530.csv",
                         header = FALSE, sep = ",", row.names = NULL, skip = 2)
      DT_f <- DT_f[1:11, ]
      DT_f <- DT_f[ , 1:2]
      datatable(DT_f, options = list("pageLength" = 11))
    }
    else if (is.null(DT_f())) {}
    else if (!is.null(DT_f())) {
      files3 <- lapply(input$file3$datapath, function(y) {
        JSON_csv <- read.delim(y, header = FALSE, sep = ",", row.names = NULL,
                               skip = 2)
        names(JSON_csv) <- c("Setting", "Value")
        JSON_csv <- JSON_csv[1:11, ]
        JSON_csv <- JSON_csv[ , 1:2]
        as.data.frame(JSON_csv)
      })
      DT_f <- do.call(rbind, files3)
      datatable(DT_f, options = list("pageLength" = 11))
    }
  })

  output$table3 <- DT::renderDataTable({
    if (is.null(GPS_f()) & is.null(BC_f()) & is.null(CPC_f()) & is.null(DT_f()) &
        is.null(RH_f()) & is.null(CO2_f())) {
      CPC_f <- read.delim("data/CPC3007/2019_09_25_h091000_KAN_CPC3007.csv",
                          header = FALSE, sep = ",", row.names = NULL, skip = 1)
      names(CPC_f) <- c("Setting", "Value")
      CPC_f <- CPC_f[1:13, ]
      CPC_f <- CPC_f[ , 1:2]
      datatable(CPC_f, options = list("pageLength" = 13))
    }
    else if (is.null(CPC_f())) {}
    else if (!is.null(CPC_f())){
      files3 <- lapply(input$file4$datapath, function(y) {
        JSON_csv <- read.delim(y, header = FALSE, sep = ",", row.names = NULL,
                               skip = 1)
        names(JSON_csv) <- c("Setting", "Value")
        JSON_csv <- JSON_csv[1:13, ]
        JSON_csv <- JSON_csv[ , 1:2]
        JSON_csv
      })
      CPC_f <- do.call(rbind, files3)
      datatable(CPC_f, options = list("pageLength" = 13))
    }
  })

  output$table2 <- DT::renderDataTable({
    if (is.null(BC_f())) {}
    else if (!is.null(BC_f())) {
      files3 <- lapply(input$file2$datapath, function(y) {
        JSON_csv_header <- read.delim(y, header = FALSE, sep=",", skip = 15,
                                      row.names = NULL, stringsAsFactors = FALSE)
        JSON_csv_header <- JSON_csv_header[1, ]
        JSON_csv <- read.delim(y, skip = 17, header = FALSE, sep = ",")
        colnames(JSON_csv) <- unlist(JSON_csv_header)
        JSON_csv
      })
      BC_f <- do.call(rbind, files3)
      BC_f <- BC_f %>%
        drop_na(BC)
      BC_f <- BC_f[BC_f$Status != 0, ]
      BC_f
    }
  })

  output$table5 <- DT::renderDataTable({
    if (is.null(GPS_f()) & is.null(BC_f()) & is.null(CPC_f()) & is.null(DT_f()) &
        is.null(RH_f()) & is.null(CO2_f())) {
      BC_f <- read.delim("data/AE51/2019_09_25_h091000_KAN_AE51.csv",
                         header = FALSE,
                         sep = " ", skip = 1, row.names = NULL)
      BC_f <- BC_f[1:14, ]
      names(BC_f) <- c("Setting")
      datatable(BC_f, options = list("pageLength" = 14))
    } else if (is.null(BC_f())) {
      "No AE51 files available"
    } else if (!is.null(BC_f())){
      files3 <- lapply(input$file2$datapath, function(y){
        JSON_csv <- read.delim(y, header = FALSE, sep = " ",
                               skip = 1, row.names = NULL)
        JSON_csv <- JSON_csv[1:14, ]
        JSON_csv
      })
      BC_f <- do.call(rbind, files3)
      names(BC_f) <- c("Setting")
      datatable(BC_f, options = list("pageLength" = 14))
     }
  })

  ## Raw pollutants/GPS plot
  theme1 <- reactive({
    theme1 <- list(geom_line(size = 0.6, color = "dodgerblue2"),
                   scale_x_datetime(date_labels  = "%H:%M"),
                   theme_minimal(),
                   theme(legend.text = element_text(size = 18),
                         plot.title = element_text(size = 14, face = "bold"),
                         axis.title = element_text(size = 14),
                         axis.text = element_text(size = 14, face = "bold"),
                         panel.border = element_rect(colour = "black",
                                                     fill = NA, size = 1.2)))
  })

  output$plot5 <- renderPlotly({
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4)
        & is.null(input$file5) & is.null(input$file6)) {
      data <- data_blank()
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(Latitude))) +
                 geom_line(size = 0.6, color = "dodgerblue2") +
                 labs(title = "Latitude (degree)",
                      y = "",
                      x = "") + theme1()
      )}
    else if (is.null(GPS_f())) {}
    else if (!is.null(GPS_f())) {
      data <- data_joined()
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(Latitude))) +
                 geom_line(size = 0.6, color = "dodgerblue2") +
                 labs(title = "Latitude (degree)",
                      y = "",
                      x = "") + theme1()
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
                      x = "") + theme1()
      )}
    else if (is.null(GPS_f())) {}
    else if (!is.null(GPS_f())) {
      data <- data_joined()
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(Longitude))) +
                 geom_line(size = 0.6, color = "dodgerblue2") +
                 labs(title = "Longitude (degree)",
                      y = "",
                      x = "") + theme1()
      )}
  })

  output$plot <- renderPlotly({
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4)
        & is.null(input$file5) & is.null(input$file6)) {
      data <- data_blank()
      data$PM2.5 <- as.numeric(as.character(data$PM2.5))
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(PM2.5))) +
                 labs(title = "DT8530_PM2.5 (ug/m3)",
                      y = "",
                      x = "") + theme1()
      )}
    else if (is.null(DT_f())) {}
    else if (!is.null(DT_f())) {
      data <- data_joined()
      data$PM2.5 <- as.numeric(as.character(data$PM2.5))
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(PM2.5))) +
                 labs(title = "DT8530_PM2.5 (ug/m3)",
                      y = "",
                      x = "") + theme1()
      )}
  })

  output$plot4<- renderPlotly({
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4)
        & is.null(input$file5) & is.null(input$file6)) {
      data <- data_blank()
      data$RH <- data$RH * 100
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(RH))) +
                 labs(title = "Relative Humidity (%)",
                      y = "",
                      x = "") + theme1()
      )}
    else if (is.null(RH_f())) {}
    else if (!is.null(RH_f())) {
      data <- data_joined()
      data$RH <- data$RH * 100
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(RH))) +
                 labs(title = "Relative Humidity (%)",
                      y = "",
                      x = "") + theme1()
      )}
  })

  output$plot2 <- renderPlotly({
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4)
        & is.null(input$file5) & is.null(input$file6)) {
      data <- data_blank()
      data$BC <- as.numeric(as.character(data$BC))
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(BC))) +
                 labs(title = "AE51_BC (ug/m3)",
                      y = "",
                      x = "") + theme1()
      )}
    else if (is.null(BC_f())) {}
    else if (!is.null(BC_f())) {
      data <- data_joined()
      data$BC <- as.numeric(as.character(data$BC))
      ggplotly(ggplot(data, aes(as.POSIXct(date), as.numeric(BC))) +
                 labs(title = "AE51_BC (ug/m3)",
                      y = "",
                      x = "") + theme1()
      )}
  })

  output$plot3 <- renderPlotly({
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4)
        & is.null(input$file5) & is.null(input$file6)) {
      data <- data_blank()
      data$Particle_conc <- as.numeric(as.character(data$Particle_conc))
      ggplotly(ggplot(data, aes(as.POSIXct(date), Particle_conc)) +
                 labs(title = "CPC3007_Particle Concentration (#/cm3)",
                      y = "",
                      x = "") + theme1()
      )}
    else if (is.null(CPC_f())) {}
    else if (!is.null(CPC_f())) {
      data <- data_joined()
      data$Particle_conc <- as.numeric(as.character(data$Particle_conc))
      ggplotly(ggplot(data, aes(as.POSIXct(date), Particle_conc)) +
                 labs(title = "CPC3007_Particle Concentration (#/cm3)",
                      y = "",
                      x = "") + theme1()
      )}
  })

  output$plot7 <- renderPlotly({
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4)
        & is.null(input$file5) & is.null(input$file6)) {
      data <- data_blank()
      ggplotly(ggplot(data, aes(as.POSIXct(date), CO2)) +
                 labs(title = "LI-COR_CO2 (ppm)",
                      y = "",
                      x = "") + theme1()
      )}
    else if (is.null(CO2_f())) {}
    else if (!is.null(CO2_f())) {
      data <- data_joined()
      ggplotly(ggplot(data, aes(as.POSIXct(date), CO2)) +
                 labs(title = "LI-COR_CO2 (ppm)",
                      y = "",
                      x = "") + theme1()
      )}
  })

  observe({
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4)
        & is.null(input$file5) & is.null(input$file6)) {
      data_joined <- data_blank()
      data_joined <- data_joined %>%
        select(- date, - Latitude, - Longitude, - PM2.5_RHC, - PM2.5_RHC_Ref)
    } else {
      data_joined <- data_joined()
      data_joined <- data_joined %>%
        select(- date, - Latitude, - Longitude)

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
        data_joined <- data_joined %>%
          select(- PM2.5, - PM2.5_Ref, - PM2.5_RHC, - PM2.5_RHC_Ref)
      }
      if (!is.null(DT_f())) {
        data_joined <- data_joined %>%
          select(everything())
      }
      if (is.null(BC_f())) {
        data_joined <- data_joined %>%
          select(- BC, - BC_NR, - BC_NR_LC)
      }
      if (!is.null(BC_f())) {
        data_joined <- data_joined %>%
          select(everything())
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
      risk.bins <- c(-100, -40, -20, 0, 5, 10, 20, 40, 100, 500, 2000)
      pal <- colorBin("Spectral", bins = risk.bins, na.color = "#808080",
                      reverse = TRUE)
    } else if (input$palleInp == "PM2.5") {
      risk.bins <- c(0, 10, 25, 50, 100, 500, 2000, 5000, 10000)
      pal <- colorBin("Spectral", bins = risk.bins, na.color = "#808080",
                      reverse = TRUE)
    } else if (input$palleInp == "Particle_conc") {
      risk.bins <- c(0, 5000, 10000, 20000, 40000, 70000, 100000, 150000,
                     200000, 500000)
      pal <- colorBin("Spectral", bins = risk.bins, na.color = "#808080",
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
      pal <- colorBin("Spectral", bins = risk.bins, na.color = "#808080",
                      reverse = TRUE)
    }

    leaflet(data) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      addCircles(data = data,
                 lng = ~ Longitude,
                 lat = ~ Latitude,
                 popup =  paste("Date:", data$date, "<br>",
                                "AE51_BC (ug/m3):",
                                round(as.numeric(data$BC), digits = 2), "<br>",
                                "DT8530_PM2.5 (ug/m3):",
                                round(as.numeric(data$PM2.5), digits = 2), "<br>",
                                "RH(%):",
                                round(as.numeric(data$RH), digits = 2), "<br>",
                                "CPC3007_Particle Concentration (#/cm3):",
                                round(as.numeric(data$Particle_conc), digits = 2),
                                "<br>",
                                "CO2:", round(as.numeric(data$CO2),digits = 2)),
                 weight = 3, radius = 8,
                 col = ~ pal(data[[input$palleInp]]), stroke = TRUE,
                 fillOpacity = 0.8) %>%
      leaflet::addLegend("bottomright", pal = pal,
                         values = ~data[[input$palleInp]],
                         title = paste(input$palleInp))
  })
}
## Run app
shinyApp(ui, server)







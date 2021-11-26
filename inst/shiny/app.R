library(shiny)
library(Cairo)
library(DT)
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
    tags$style(HTML(
      ".sidebar {
                    height : 10vh; overflow-y : auto; font-size : 14px;
                    }",
      ".shiny-output-error-validation {
                    color : red; font-size : 14px;
                    }"
    ))
  ),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      width = 3,
      conditionalPanel(
        condition = "input.tabs1 == 6",
        tags$hr(),
        h4("Alarms! Check for any malfunction."),
        tags$hr()
      ),
      conditionalPanel(
        condition = "input.tabs1 == 4",
        tags$hr(),
        selectInput("palleInp", "Map this pollutant", "Select"),
        tags$hr()
      ),
      conditionalPanel(
        condition = "input.tabs1 == 3",
        tags$hr(),
        h4("Summary Statistics"),
        tags$hr()
      ),
      conditionalPanel(
        condition = "input.tabs1 == 2",
        tags$hr(),
        h4("Plots"),
        selectInput("palleInp1", "Plot this pollutant", "Select"),
        actionButton("ts", "Time series"),
        actionButton("den", "Density Plot"),
        actionButton("qq", "QQ Plot"),
        tags$hr()
      ),
      conditionalPanel(
        condition = "input.tabs1 == 1",
        tags$hr(),
        helpText("Choose mobile monitoring files."),
        tags$hr(),
        useShinyjs(),
        test <- a("Input Timezone* (link to supported
                                             timezones)",
                  href = "https://en.wikipedia.org/wiki/List_of_tz_database_time_zones",
                  style = "font-size:14px; ",
                  target = "_blank"
        ),
        selectInput("timezone",
                    label = "",
                    choices = OlsonNames(),
                    selected = "Asia/Kolkata"
        ),
        tags$hr(),
        radioButtons(
          "gps_s",
          "GPS file of choice",
          c("Garmin" = "gps",
            "GPS" = "gps_p"),
          selected = "gps"
        ),
        fileInput("file10", "GPS Phone - location files", multiple = TRUE,
                  accept = c(".csv")),
        fileInput("file1", "GPSMAP 64s - location files", multiple = TRUE,
                  accept = c(".gpx")),
        tags$hr(),
        fileInput("file2", "AE51 - BC files", multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values, text/plain", ".csv")),
        tags$hr(),
        fileInput("file3", "DT8530 - PM2.5 files", multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values, text/plain", ".csv")),
        h5("Reference Correction for DT8530 - PM2.5"),
        numericInput("Slope", "Slope", value = 1.0),
        numericInput("Intercept", "Intercept", value = 0),
        tags$hr(),
        fileInput("file7", "DT8533 - PM files", multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values, text/plain", ".csv")),
        tags$hr(),
        fileInput("file8", "RH from Equinox", multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values, text/plain", ".csv")),
        tags$hr(),
        fileInput("file5", "RH from probe", multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values, text/plain", ".csv")),
        tags$hr(),
        radioButtons(
          "rh_c",
          "File to use for RH correction of PM2.5",
          c("RH Equinox" = "equ",
            "RH Probe" = "pro"),
          selected = "equ"
        ),
        tags$hr(),
        fileInput("file4", "CPC3007 - Particle Concentration files", multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values, text/plain", ".csv")),
        numericInput("DF", "Dilution factor for CPC (1 for no diluter)",
                     value = 1),
        tags$hr(),
        fileInput("file6", "LI-COR - CO2 files", multiple = TRUE,
                  accept = c( "text/csv",
                              ".txt", "text/comma-separated-values, text/plain", ".csv")),
        radioButtons(
          "delim",
          "Delimiter for CO2 files",
          c("Tab" = "\t",
            "Comma" = ",",
            "Semicolon" = ";"),
          selected = "\t"
        ),
        tags$hr(),
        helpText("*mandatory"),
        tags$hr(),
        actionButton("join_button", "JOIN"),
        tags$hr(),
        downloadButton("download", "Download as csv"),
        tags$hr()
      )
    ),
    mainPanel(
      tags$a(img(
        src = "logo.png", align = "right", height = 70,
        width = 120
      ),
      href = "https://www.ilklabs.com/", target = "_blank"
      ),
      tags$head(
        tags$style(
          type = "text/css",
          ".nav-tabs { font-size: 18px } ")),
      tabsetPanel(
        id = "tabs1",
        tabPanel(
          value = 1,
          title = "Joined File",
          dataTableOutput("table1")
        ),
        tabPanel(
          value = 3,
          title = "Summary",
          dataTableOutput("table")
        ),
        tabPanel(
          value = 2,
          title = "Plots",
          plotOutput("plot5", width = 800),
          plotOutput("plot6", width = 800),
          plotOutput("plot", width = 800)
        ),
        tabPanel(
          value = 4,
          title = "Map",
          leafletOutput("map", width = "100%", height = 800)
        ),
        tabPanel(
          value = 6,
          title = "Alarms and Settings",
          h5("AE51 Status: "),
          dataTableOutput("table5"),
          dataTableOutput("table2"),
          h5("DT8530 Notes/Alarms: "),
          dataTableOutput("table4"),
          h5("CPC3007 Notes/Alarms: "),
          dataTableOutput("table3"),
          h5("DT8533 Notes/Alarms: "),
          dataTableOutput("table6"),
          h5("RH Equinox Notes/Alarms: "),
          dataTableOutput("table7")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30000 * 1024 ^ 2, shiny.launch.browser = TRUE)

  query_modal <- modalDialog(
    title = "What to expect?",
    HTML("First visit here?<br>"),
    footer = tagList(
      actionButton("switch_tab", "Read Me",
                   onclick = "window.open('https://github.com/adithirgis/mmaqshiny#user-guide',
                   '_blank')"
      ),
      modalButton("Close")
    ),
    easyClose = F
  )
  showModal(query_modal)
  observe({
    shinyjs::toggleState("file10", input$gps_s == "gps_p")
    shinyjs::toggleState("file1", input$gps_s == "gps")
  })
  ## date matching
  ':=' <- function(lhs, rhs) {
    frame <- parent.frame()
    lhs <- as.list(substitute(lhs))
    if (length(lhs) > 1)
      lhs <- lhs[-1]
    if (length(lhs) == 1) {
      do.call(`=`, list(lhs[[1]], rhs), envir = frame)
      return(invisible(NULL))
    }
    if (is.function(rhs) || is(rhs, 'formula'))
      rhs <- list(rhs)
    if (length(lhs) > length(rhs))
      rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
    for (i in 1:length(lhs))
      do.call(`=`, list(lhs[[i]], rhs[[i]]), envir = frame)
    return(invisible(NULL))
  }
  dist_sp <- function(start_lat, start_long, end_lat, end_long) {
    RadE <- 6378.137 # radius of earth in km
    diff_lat_rad <- end_lat * pi / 180 - start_lat * pi / 180
    diff_lon_rad <- end_long * pi / 180 - start_long * pi / 180
    a <- sin(diff_lat_rad / 2) * sin(diff_lat_rad / 2) +
      cos(start_lat * pi / 180) * cos(end_lat * pi / 180) *
      sin(diff_lon_rad / 2) * sin(diff_lon_rad / 2)
    cir <- 2 * atan2(sqrt(a), sqrt(1 - a))
    dist_m <- (RadE * cir) * 1000
  }
  VecFunc <- function(x) {
    x <- as.numeric(as.character(x))
    if (x <= 1 & x > 0.6) {
      return(1 + (0.25 * ((x * x) / (1 - x))))
    } else if (is.na(x) | is.null(x)) {
      return(NA)
    }  else if (x > 0 & x <= 0.6) {
      return(1)
    } else {
      return(NA)
    }
  }
  CPC <- function(path, DF, time_z, file_g) {
    if (is.null(file_g)) {
      CPC_date <- NULL
      CPC_f <- NULL
      CPC_f_error <- NULL
    } else {
      df_list <- lapply(path, function(y) {
        JSON_csv1 <- read.delim(y, header = FALSE, sep = ",", row.names = NULL,
                                skip = 1)
        names(JSON_csv1) <- c("Setting", "Value")
        JSON_csv_date <- as.Date(as.character(subset(JSON_csv1, Setting == "Start Date")$Value), format = "%m/%d/%y",
                                 tz = time_z)
        JSON_csv <- read.delim(y, header = TRUE, sep = ",", row.names = NULL,
                               skip = 17, stringsAsFactors = FALSE, fileEncoding = "latin1")
        if(is.null(JSON_csv_date) | is.na(JSON_csv_date)) {
          JSON_csv_date <- as.Date(as.character(subset(JSON_csv1, Setting == "Start Date")$Value),
                                   format = "%m-%d-%Y", tz = time_z)
        }
        JSON_csv <- JSON_csv[, 1:2]
        names(JSON_csv) <- c("Time", "Particle_conc")
        JSON_csv <- JSON_csv %>%
          mutate(Time = gsub(".", ":", Time, fixed = TRUE)) %>%
          mutate(date = ymd_hms(paste(JSON_csv_date, Time), tz = time_z))
      })
      CPC_f <- do.call(rbind, df_list)
      CPC_f <- CPC_f %>%
        mutate(Particle_conc = Particle_conc * DF) %>%
        filter(!is.na(Time)) %>%
        dplyr::select(date, Particle_conc) %>%
        arrange(date)
      CPC_date <- as.Date(CPC_f[1, "date"], format = "%y-%m-%d", tz = time_z)
      files3 <- lapply(path, function(y) {
        JSON_csv <- read.delim(y, header = FALSE, sep = ",", row.names = NULL,
                               skip = 1)
        names(JSON_csv) <- c("Setting", "Value")
        JSON_csv <- JSON_csv[1:13, 1:2]
      })
      CPC_f_error <- do.call(rbind, files3)
    }
    return(list(CPC_f, CPC_date, CPC_f_error))
  }
  GPS <- function(file_g, path, time_z) {
    if (is.null(file_g)) {
      GPS_f <- NULL
      GPS_date <- NULL
    } else {
      files3 <- lapply(path, function(y) {
        pfile2 <- XML::htmlTreeParse(y, error = function(...) {}, useInternalNodes = T)
        elevations <- as.numeric(XML::xpathSApply(pfile2,
                                                  path = "//trkpt/ele",
                                                  xmlValue))
        times <- XML::xpathSApply(pfile2, path = "//trkpt/time", xmlValue)
        coords <- XML::xpathSApply(pfile2, path = "//trkpt", xmlAttrs)
        geodf2 <- data.frame(
          latitude = as.numeric(coords["lat", ]),
          longitude = as.numeric(coords["lon", ]),
          elevation = elevations, time = times)
        geodf2 <- geodf2 %>%
          arrange(time) %>%
          mutate(speed_m_s = dist_sp(lag(latitude), lag(longitude), latitude, longitude))
      })
      GPS_f <- do.call(rbind, files3)
      GPS_f <- GPS_f %>%
        mutate(date = with_tz(ymd_hms(time), time_z)) %>%
        distinct(date, .keep_all = TRUE) %>%
        dplyr::select(date, latitude, longitude, speed_m_s) %>%
        arrange(date)
      GPS_date <- as.Date(GPS_f[1, "date"], format = "%Y-%m-%d", tz = time_z)
    }
    return(list(GPS_f, GPS_date))
  }
  GPS_phone <- function(file_g, path, time_z) {
    if (is.null(file_g)) {
      GPS_f_p <- NULL
      GPS_date_p <- NULL
    } else {
      df_list <- lapply(path, function(y) {
        JSON_csv <- read.csv(y, sep = ",")
      })
      GPS_f_p <- do.call(rbind, df_list)
      GPS_f_p <- GPS_f_p %>%
        mutate(date = gsub(" 00:", " 12:", date, fixed = TRUE)) %>%
        mutate(date = as.POSIXct(strptime(date, "%m/%d/%Y %I:%M:%OS %p"), format = '%Y-%m-%d %H:%M:%S',
                                 tz = time_z)) %>%
        dplyr::select(date, latitude, longitude, speed, satellites, altitude,
                      accuracy)
      GPS_date_p <- as.Date(GPS_f_p[1, "date"], format = "%Y-%m-%d", tz = time_z)
    }
    return(list(GPS_f_p, GPS_date_p))
  }
  CO2 <- function(file_g, path, time_z, delim) {
    if (is.null(file_g)) {
      CO2_f <- NULL
      CO2_date <- NULL
    } else {
      df_list <- lapply(path, function(y) {
        JSON_csv <- read.delim(y, skip = 1, sep = delim, header = TRUE, row.names = NULL,
                               stringsAsFactors = FALSE
        )
        JSON_csv <- JSON_csv[, 1:4]
        names(JSON_csv) <- c("Date", "Time", "CO2", "H2O")
        JSON_csv
      })
      CO2_f <- do.call(rbind, df_list)
      CO2_f <- CO2_f %>%
        mutate(Time = gsub(".", ":", Time, fixed = TRUE)) %>%
        mutate(date = as.POSIXct(as.character(paste(as.Date(CO2_f$Date,
                                                            tryFormats = c("%Y-%m-%d", "%Y/%m/%d",
                                                                           "%d-%m-%Y")), Time)),
                                 tz = time_z, format = "%Y-%m-%d %H:%M:%S"),
               CO2 = as.numeric(as.character(CO2)), H2O = as.numeric(as.character(H2O)),
               CO2_c = CO2 - min(CO2, na.rm = TRUE)) %>%
        dplyr::select(date, CO2, H2O, CO2_c) %>%
        arrange(date)
      CO2_date <- as.Date(CO2_f[1, "date"], format = "%Y-%m-%d", tz = time_z)
    }
    return(list(CO2_f, CO2_date))
  }
  BC <- function(file_g, path, time_z) {
    if (is.null(file_g)) {
      BC_f <- NULL
      BC_date <- NULL
      BC_f_status <- NULL
      BC_f_error <- NULL
    } else {
      df_list <- lapply(path, function(y) {
        JSON_csv_header <- read.delim(y, header = FALSE, sep = ",", skip = 15,
                                      row.names = NULL,
                                      stringsAsFactors = FALSE)
        JSON_csv_header <- JSON_csv_header[1, ]
        JSON_csv <- read.delim(y, skip = 17, header = FALSE, sep = ",")
        colnames(JSON_csv) <- unlist(JSON_csv_header)
        JSON_csv <- JSON_csv %>%
          drop_na(BC)
        JSON_csv$date1 <- with(JSON_csv,
                               as.POSIXct(paste(as.Date(Date, format = "%Y/%m/%d"), Time),
                                          tz = time_z))
        if (is.null(JSON_csv$date1)) {
          JSON_csv$date1 <- with(JSON_csv,
                                 as.POSIXct(paste(as.Date(Date, format = "%d-%m-%Y"), Time),
                                            tz = time_z))
        }
        BC_f <- JSON_csv
        BC_f$Date <- BC_f$date1 # "%Y/%m/%d", "%d-%m-%Y"
        ef_file <- BC_f %>%
          filter(Status == 0) %>%
          dplyr::select(Date, ATN, BC) %>%
          mutate(BC1 = (BC / 1000), LD = BC1 - rollapply(BC1, FUN = mean,
                                                         width = 30, align = "center",
                                                         partial = TRUE )) %>%
          mutate(LD75 = runquantile(LD, 300, 0.75, type = 2,
                                    endrule = c("NA")),
                 LD25 = runquantile(LD, 300, 0.25, type = 2,
                                    endrule = c("NA")),
                 BC2 = BC1, BC3 = BC1) %>%
          mutate(BC2 = ifelse(BC2 >= 0, 0, BC2),
                 BC2 = ifelse(BC2 < 0, 1, BC2)) %>%
          mutate(BC2 = rollapply(BC2, FUN = mean, width = 5,
                                 align = "center", partial = TRUE),
                 cev1 = ifelse((LD > 5 * LD75) | (LD < 5 * LD25), BC1, NA))
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
        cev_file <- date_file[CEV, on = c("ef_file.Date")]
        cev_file <- cev_file[!(cev_file$ef_file.BC2 == 0), ]
        cev_file <- xts(cev_file, order.by = cev_file$ef_file.Date)
        ef_file <- data.frame(ef_file)
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
            mutate(BC_Factor = 1)
        } else {
          Date_Table <- Date_Table %>%
            mutate(BC_Factor = 0, Date = as.POSIXct(Date, tz = time_z))
          setDT(Date_Table)
          setDT(ef_file)
          BC <- Date_Table[ef_file, on = c("Date")]
          BC$BC_Factor[is.na(BC$BC_Factor)] <- 1
        }
        BC <- BC %>%
          mutate(BC_Fi = BC_Factor * BC1)
        BC$BC_Fi[BC$BC_Fi == 0] <- NA
        BC <- BC %>%
          mutate(Tr = exp(-ATN / 100),
                 CF = (1 / ((0.88 * Tr) + 0.12)),
                 BC_Final = BC_Fi * CF)
        BC$BC_Fi[BC$BC_Fi < 0] <- NA
        BC$BC_Fi[is.na(BC$BC_Fi)] <- " "
        BC$BC_Final[BC$BC_Final < 0] <- NA
        BC$BC_Final[is.na(BC$BC_Final)] <- " "
        BC_f <- BC %>%
          dplyr::select("date" = Date, "BC" = BC3, "BC_NR" = BC_Fi, "BC_NR_LC" = BC_Final) %>%
          mutate_at(c("BC", "BC_NR", "BC_NR_LC"), as.numeric) %>%
          arrange(date)
        BC_f <- as.data.frame(BC_f)
      })
      BC_f <- do.call(rbind, df_list)
      BC_date <- as.Date(BC_f[1, "date"], format = "%Y-%m-%d", tz = time_z)
      files3 <- lapply(path, function(y) {
        JSON_csv_header <- read.delim(y, header = FALSE, sep = ",", skip = 15,
                                      row.names = NULL, stringsAsFactors = FALSE)
        JSON_csv_header <- JSON_csv_header[1, ]
        JSON_csv <- read.delim(y, skip = 17, header = FALSE, sep = ",")
        colnames(JSON_csv) <- unlist(JSON_csv_header)
        JSON_csv
      })
      BC_f_status <- do.call(rbind, files3)
      BC_f_status <- BC_f_status %>%
        drop_na(BC)
      BC_f_status <- BC_f_status[BC_f_status$Status != 0, ]
      files4 <- lapply(path, function(y) {
        JSON_csv <- read.delim(y, header = FALSE, sep = " ",
                               skip = 1, row.names = NULL)
        JSON_csv <- JSON_csv[1:14, ]
      })
      BC_f_error <- do.call(rbind, files4)
      names(BC_f_error) <- c("Setting")
    }
    return(list(BC_f, BC_date, BC_f_status, BC_f_error))
  }
  DT_0 <- function(file_g, path, time_z, Slope, Intercept) {
    if (is.null(file_g)) {
      DT_f_error <- NULL
      DT_0_date <- NULL
      DT_f <- NULL
    } else {
      df_list <- lapply(path, function(y) {
        JSON_csv <- read.delim(y, header = TRUE, sep = ",", row.names = NULL,
                               skip = 28)
        names(JSON_csv) <- c("Date", "Time", "PM2.5")
        Date1 <- as.Date(JSON_csv[1, 1], format = "%d-%m-%Y", tz = time_z)
        if (is.na(Date1)) {
          Date1 <- as.Date(JSON_csv[1, 1], format = "%m/%d/%Y",
                           tz = time_z)
        }
        JSON_csv$date <- as.POSIXct(strptime(paste(Date1, JSON_csv$Time),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                    format = "%Y-%m-%d %H:%M:%S", tz = time_z)
        JSON_csv
      })
      DT_f <- do.call(rbind, df_list)
      DT_f <- DT_f %>%
        mutate(PM2.5 = PM2.5 * 1000) %>%
        mutate(PM2.5_Ref = ((PM2.5 * Slope) + Intercept)) %>%
        dplyr::select(date, PM2.5, PM2.5_Ref) %>%
        arrange(date)
      DT_0_date <- as.Date(DT_f[1, "date"], format = "%y-%m-%d", tz = time_z)
      files3 <- lapply(path, function(y) {
        JSON_csv <- read.delim(y, header = FALSE, sep = ",", row.names = NULL,
                               skip = 2)
        names(JSON_csv) <- c("Setting", "Value")
        JSON_csv <- JSON_csv[1:11, 1:2]
        JSON_csv <- as.data.frame(JSON_csv)
      })
      DT_f_error <- do.call(rbind, files3)
    }
    return(list(DT_f, DT_0_date, DT_f_error))
  }
  RH <- function(file_g, path, time_z) {
    if (is.null(file_g)) {
      RH_f <- NULL
      RH_date <- NULL
    } else {
      df_list <- lapply(path, function(y) {
        JSON_csv <- data.frame(read.delim(y, header = TRUE,
                                          sep = ",", skip = 6, row.names = NULL))
      })
      RH_f <- do.call(rbind, df_list)
      RH_f_Date <- RH_f[, 2]
      RH_f_Time <- RH_f[, 3]
      RH <- RH_f[, grepl("RH", names(RH_f))]
      RH_f <- data.frame(RH_f_Date, RH_f_Time, RH)
      names(RH_f) <- c("LogDate", "LogTime", "RH")
      RH_f$LogTime <- gsub(".", ":", RH_f$LogTime, fixed = TRUE)
      RH_f <- RH_f %>%
        mutate(date = as.POSIXct(paste(LogDate, LogTime),
                                 format = "%d-%m-%Y %I:%M:%S %p", tz = time_z)) %>%
        dplyr::select(LogDate, LogTime, date, RH) %>%
        mutate_at(c("RH"), as.numeric) %>%
        arrange(date)
      if(is.na(RH_f[1, "date"]) | is.null(RH_f[1, "date"])) {
        RH_f <- RH_f %>%
          mutate(date = as.POSIXct(paste(LogDate, LogTime),
                                    format = "%d-%m-%Y %H:%M:%S", tz = time_z)) %>%
          dplyr::select(LogDate, LogTime, date, RH)
      }
      RH_f <- RH_f %>%
        select(date, RH) %>%
        na.omit()
      RH_f$CF <- sapply(as.numeric(as.character(RH_f$RH / 100)), FUN = VecFunc)
      RH_date <- as.Date(RH_f[1, "date"], format = "%Y-%m-%d", tz = time_z)
    }
    return(list(RH_f, RH_date))
  }
  RH_E <- function(file_g, path, time_z) {
    if (is.null(file_g)) {
      RH_Ef <- NULL
      RH_Ef_date <- NULL
      RH_Ef_error <- NULL
    } else {
      df_list <- lapply(path, function(y) {
        JSON_csv <- read.csv(y, header = TRUE, sep = ",", row.names = NULL,
                             skip = 10)
      })
      RH_Ef <- do.call(rbind, df_list)
      RH_Ef <- RH_Ef %>%
        dplyr::select(TIME, RH, Temp) %>%
        mutate(RH = as.numeric(as.character(gsub("%RH", "", RH))),
               Temp = as.numeric(as.character(gsub("C", "", Temp)))) %>%
        mutate(TIME = gsub("/", " ", as.character(TIME))) %>%
        mutate(date = as.POSIXct(TIME, format = '%d-%m-%y %H:%M:%S', tz = time_z)) %>%
        arrange(date) %>%
        na.omit() %>%
        dplyr::select(date, Temp, "RH_E" = RH)
      RH_Ef$CF_E <- sapply(as.numeric(as.character(RH_Ef$RH_E / 100)), FUN = VecFunc)
      RH_Ef_date <- as.Date(RH_Ef[1, "date"], format = "%y-%m-%d", tz = time_z)
      files3 <- lapply(path, function(y) {
        JSON_csv <- read.delim(y, header = FALSE, sep = ",", row.names = NULL)
        JSON_csv <- JSON_csv[1:8, ]
      })
      RH_Ef_error <- do.call(rbind, files3)
    }
    return(list(RH_Ef, RH_Ef_date, RH_Ef_error))
  }
  DT_3 <- function(file_g, path, time_z, Slope, Intercept) {
    if (is.null(file_g)) {
      DT_f3 <- NULL
      DT_3_date <- NULL
      DT_3_error <- NULL
    } else {
      df_list <- lapply(path, function(y) {
        JSON_csv <- read.delim(y, header = TRUE, sep = ",", row.names = NULL,
                               skip = 28)
        names(JSON_csv) <- c("Date", "Time", "PM1", "PM2.5_8533", "PM4", "PM10", "Total")
        Date1 <- as.Date(JSON_csv[1, 1], format = "%d-%m-%Y", tz = time_z)
        if (is.na(Date1)) {
          Date1 <- as.Date(JSON_csv[1, 1], format = "%m/%d/%Y",
                           tz = time_z)
        }
        JSON_csv$date <- as.POSIXct(strptime(paste(Date1, JSON_csv$Time),
                                             format = "%Y-%m-%d %H:%M:%S"),
                                    format = "%Y-%m-%d %H:%M:%S", tz = time_z)
        JSON_csv
      })
      DT_f3 <- do.call(rbind, df_list)
      DT_f3 <- DT_f3 %>%
        dplyr::select(date, everything(), -Date, -Time) %>%
        mutate_if(is.numeric, ~ . * 1000) %>%
        mutate(PM2.5_8533_Ref = ((PM2.5_8533 * Slope) + Intercept)) %>%
        arrange(date)
      DT_3_date <- as.Date(DT_f3[1, "date"], format = "%Y-%m-%d", tz = time_z)
      files3 <- lapply(path, function(y) {
        JSON_csv <- read.delim(y, header = FALSE, sep = ",", row.names = NULL,
                               skip = 2)
        JSON_csv <- JSON_csv[1:11, ]
        names(JSON_csv) <- c("Setting", "Value")
        JSON_csv
      })
      DT_3_error <- do.call(rbind, files3)
    }
    return(list(DT_f3, DT_3_date, DT_3_error))
  }
  theme1 <- reactive({
    theme1 <- list(
      theme_minimal(),
      theme(legend.text = element_text(size = 18),
            plot.title = element_text(size = 14, face = "bold"),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14, face = "bold"),
            panel.border = element_rect(
              colour = "black",
              fill = NA, size = 1.2)
      )
    )
  })
  validate_date <- function(file_g, file_date, name_sensor, GPS_date) {
    if (!is.null(file_g)) {
      validate(
        need(try(GPS_date == file_date), paste0("The files have different date entries in GPS and ",
                                                name_sensor, "! Please check once again."))
      )
    }
  }

  ## preloaded table

  data_blank <- reactive({
    if (is.null(input$file1) & is.null(input$file2) & is.null(input$file3) &
        is.null(input$file4) & is.null(input$file5) & is.null(input$file6)) {
      joined <- read.delim("joined_file.csv", header = TRUE, sep = ",") %>%
        dplyr::select(-starts_with("X")) %>%
        distinct(date.LT., .keep_all = TRUE)
      names(joined) <- c(
        "date", "latitude", "longitude", "BC", "BC_NR",
        "BC_NR_LC", "PM2.5", "PM2.5_RHC", "PM2.5_RHC_Ref",
        "PM2.5_Ref", "RH", "Particle_conc", "CO2"
      )
      joined
    }
  })

  observeEvent(input$timezone, {
    toggleState(
      id = "join_button",
      condition = (input$timezone != "" |
                     is.null(input$timezone) | is.na(input$timezone))
    )
  })
  data_joined <- eventReactive(input$join_button, {
    if(input$gps_s == "gps") {
      c(GPS_f, GPS_date) := GPS(input$file1, input$file1$datapath, input$timezone)
      GPS_f <- data.frame(GPS_f)
      GPS_date <- GPS_date
    } else if(input$gps_s == "gps_p") {
      c(GPS_f_p, GPS_date_p) := GPS_phone(input$file10, input$file10$datapath, input$timezone)
      GPS_f <- data.frame(GPS_f_p)
      GPS_date <- GPS_date_p
    }
    c(CPC_f, CPC_date, CPC_f_error) := CPC(input$file4$datapath, input$DF, input$timezone, input$file4)
    CPC_f <- data.frame(CPC_f)
    CPC_date <- CPC_date
    c(CO2_f, CO2_date) := CO2(input$file6, input$file6$datapath, input$timezone, input$delim)
    CO2_f <- data.frame(CO2_f)
    CO2_date <- CO2_date
    c(BC_f, BC_date, BC_f_status, BC_f_error) := BC(input$file2, input$file2$datapath, input$timezone)
    BC_f <- data.frame(BC_f)
    BC_date <- BC_date
    c(DT_f, DT_0_date, DT_f_error) := DT_0(input$file3, input$file3$datapath, input$timezone, input$Slope, input$Intercept)
    DT_f <- data.frame(DT_f)
    DT_0_date <- DT_0_date
    c(RH_f, RH_date) := RH(input$file5, input$file5$datapath, input$timezone)
    RH_f <- data.frame(RH_f)
    RH_date <- RH_date
    c(RH_Ef, RH_Ef_date, RH_Ef_error) := RH_E(input$file8, input$file8$datapath, input$timezone)
    RH_Ef <- data.frame(RH_Ef)
    RH_Ef_date <- RH_Ef_date
    c(DT_f3, DT_3_date, DT_3_error) := DT_3(input$file7, input$file7$datapath, input$timezone, input$Slope, input$Intercept)
    DT_f3 <- data.frame(DT_f3)
    DT_3_date <- DT_3_date
    validate_date(input$file4, CPC_date, "CPC 3007", GPS_date)
    validate_date(input$file2, BC_date, "AE51", GPS_date)
    validate_date(input$file3, DT_0_date, "DusTrak 8530", GPS_date)
    validate_date(input$file5, RH_date, "RH Probe", GPS_date)
    validate_date(input$file7, DT_3_date, "DusTrak 8533", GPS_date)
    validate_date(input$file8, RH_Ef_date, "RH Equinox", GPS_date)
    data_f <- data.frame(date = GPS_f$date)
    x <- list(GPS_f, BC_f, DT_f, CO2_f, CPC_f, RH_f, DT_f3, RH_Ef)
    for(i in x) {
      if((nrow(i) != 0) & ncol(i) != 0 & !is.null(i)){
        data_f <-  data_f %>%
          left_join(., i, by = "date")
      }
    }
    if (input$rh_c == "pro") {
      if((nrow(RH_f) != 0) & ncol(RH_f) != 0 & !is.null(RH_f) &
         (nrow(DT_f) != 0) & ncol(DT_f) != 0 & !is.null(DT_f)) {
        data_f <- data_f %>%
          mutate(PM2.5_RH_corrected = PM2.5 / CF,
                 PM2.5_Ref_RH_corrected = PM2.5_Ref / CF)
      } else if((nrow(RH_f) != 0) & ncol(RH_f) != 0 & !is.null(RH_f) &
                (nrow(DT_f3) != 0) & ncol(DT_f3) != 0 & !is.null(DT_f3)) {
        data_f <- data_f %>%
          mutate(PM2.5_8533_RH_corrected = PM2.5_8533 / CF,
                 PM2.5_8533_Ref_RH_corrected = PM2.5_8533_Ref / CF)
      }
    } else if(input$rh_c == "equ") {
      if((nrow(RH_Ef) != 0) & ncol(RH_Ef) != 0 & !is.null(RH_Ef) &
         (nrow(DT_f) != 0) & ncol(DT_f) != 0 & !is.null(DT_f)) {
        data_f <- data_f %>%
          mutate(PM2.5_RH_E_corrected = PM2.5 / CF_E,
                 PM2.5_Ref_RH_E_corrected = PM2.5_Ref / CF_E)
      } else if((nrow(RH_Ef) != 0) & ncol(RH_Ef) != 0 & !is.null(RH_Ef) &
                (nrow(DT_f3) != 0) & ncol(DT_f3) != 0 & !is.null(DT_f3)) {
        data_f <- data_f %>%
          mutate(PM2.5_8533_RH_E_corrected = PM2.5_8533 / CF_E,
                 PM2.5_8533_Ref_RH_E_corrected = PM2.5_8533_Ref / CF_E)
      }
    }
    return(data_f)
  })

  data <- reactive({
    if (is.null(input$file1) & is.null(input$file2) & is.null(input$file3) &
        is.null(input$file4) & is.null(input$file5) & is.null(input$file6) &
        is.null(input$file7) & is.null(input$file8)) {
      data <- data_blank()
    } else {
      data <- data_joined()
    }
    return(data)
  })

  data_qq <- eventReactive(input$qq, {
    data <- data()
    return(data)
  })
  data_den <- eventReactive(input$den, {
    data <- data()
    return(data)
  })
  data_ts <- eventReactive(input$ts, {
    data<- data()
    return(data)
  })
  ## Final corrected, joined table

  output$table1 <- DT::renderDataTable({
    data_joined <- data()
    data_joined <- data_joined %>%
      dplyr::select(date, latitude, longitude, everything())
    setDT(data_joined)
    cols <- names(data_joined)[4:ncol(data_joined)]
    data_joined[, (cols) := round(.SD, 2), .SDcols = cols]
    data_joined$longitude <- round(as.numeric(data_joined$longitude),
                                   digits = 4)
    data_joined$latitude <- round(as.numeric(data_joined$latitude),
                                  digits = 4)
    datatable(data_joined, options = list("pageLength" = 25))
  })

  ## Download the csv generated

  output$download <- downloadHandler(
    filename <- function() {
      "joined_file.csv"
    },
    content <- function(fname) {
      data_joined <- data()
      data_joined <- data_joined %>%
        mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = time_z)) %>%
        dplyr::select(date, latitude, longitude, everything())
      write.csv(data_joined, fname)
    }
  )

  ## Summary Statistics

  output$table <- DT::renderDataTable({
    data <- data()
    data <- data %>%
      dplyr::select(everything(), - latitude, - longitude, -date)
    columns <- 1:ncol(data)
    data[, columns] <- lapply(
      columns,
      function(x) as.numeric(as.character(data[[x]]))
    )
    tmp1 <- do.call(
      data.frame,
      list(
        Mean = round(apply(data, 2, mean, na.rm = TRUE), digits = 2),
        SD = round(apply(data, 2, sd, na.rm = TRUE), digits = 2),
        Median = round(apply(data, 2, median, na.rm = TRUE), digits = 2),
        IQR = round(apply(data, 2, IQR, na.rm = TRUE), digits = 2),
        Min = round(apply(data, 2, min, na.rm = TRUE), digits = 2),
        Max = round(apply(data, 2, max, na.rm = TRUE), digits = 2),
        p1 = round(apply(data, 2, quantile, probs = c(0.01),
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
                                   function(x) {length(which(!is.na(x)))}), digits = 2)
      )
    )
    tmp <- t(tmp1)
    datatable(tmp, options = list("pageLength" = 13))
  })

  ## Alarms and settings
  ## DT file
  output$table4 <- DT::renderDataTable({
    if (is.null(input$file1) & is.null(input$file2) & is.null(input$file3) &
        is.null(input$file4) & is.null(input$file5) & is.null(input$file6) &
        is.null(input$file7) & is.null(input$file8)) {
      DT_f_error <- read.delim("data/DT8530/2019_09_25_h091000_KAN_DT8530.csv",
                               header = FALSE, sep = ",", row.names = NULL, skip = 2)
      DT_f_error <- DT_f_error[1:11, 1:2]
      datatable(DT_f_error, options = list("pageLength" = 11))
    }
    else if (is.null(input$file3) & (!is.null(input$file1) | !is.null(input$file2) |
                                     !is.null(input$file4) | !is.null(input$file5) |
                                     !is.null(input$file6) | !is.null(input$file7) |
                                     !is.null(input$file8))) {}
    else if (!is.null(input$file3)) {
      c(DT_f, DT_0_date, DT_f_error) := DT_0(input$file3, input$file3$datapath, input$timezone, input$Slope, input$Intercept)
      DT_f_error <- data.frame(DT_f_error)
      datatable(DT_f_error, options = list("pageLength" = 11))
    }
  })
  ## CPC file
  output$table3 <- DT::renderDataTable({
    if (is.null(input$file1) & is.null(input$file2) & is.null(input$file3) &
        is.null(input$file4) & is.null(input$file5) & is.null(input$file6) &
        is.null(input$file7) & is.null(input$file8)) {
      CPC_f_error <- read.delim("data/CPC3007/2019_09_25_h091000_KAN_CPC3007.csv",
                                header = FALSE, sep = ",", row.names = NULL, skip = 1)
      names(CPC_f_error) <- c("Setting", "Value")
      CPC_f_error <- CPC_f_error[1:13, 1:2]
      datatable(CPC_f_error, options = list("pageLength" = 13))
    }
    else if (is.null(input$file4) & (!is.null(input$file1) | !is.null(input$file2) |
                                     !is.null(input$file3) | !is.null(input$file5) |
                                     !is.null(input$file6) | !is.null(input$file7) |
                                     !is.null(input$file8))) {}
    else if (!is.null(input$file4)) {
      c(CPC_f, CPC_date, CPC_f_error) := CPC(input$file4$datapath, input$DF, input$timezone, input$file4)
      CPC_f_error <- data.frame(CPC_f_error)
      datatable(CPC_f_error, options = list("pageLength" = 13))
    }
  })
  ## BC file status
  output$table2 <- DT::renderDataTable({
    if (is.null(input$file2)) {}
    else if (!is.null(input$file2)) {
      c(BC_f, BC_date, BC_f_status, BC_f_error) := BC(input$file2, input$file2$datapath, input$timezone)
      BC_f_status <- data.frame(BC_f_status)
    }
  })
  ## BC file
  output$table5 <- DT::renderDataTable({
    if (is.null(input$file1) & is.null(input$file2) & is.null(input$file3) &
        is.null(input$file4) & is.null(input$file5) & is.null(input$file6) &
        is.null(input$file7) & is.null(input$file8)) {
      BC_f_error <- read.delim("data/AE51/2019_09_25_h091000_KAN_AE51.csv",
                               header = FALSE, sep = " ", skip = 1, row.names = NULL)
      BC_f_error <- BC_f_error[1:14, ]
      names(BC_f_error) <- c("Setting")
      datatable(BC_f_error, options = list("pageLength" = 14))
    } else if (is.null(input$file2) & (!is.null(input$file1) | !is.null(input$file4) |
                                       !is.null(input$file3) | !is.null(input$file5) |
                                       !is.null(input$file6) | !is.null(input$file7) |
                                       !is.null(input$file8))) {}
    else if (!is.null(input$file2)) {
      c(BC_f, BC_date, BC_f_status, BC_f_error) := BC(input$file2, input$file2$datapath, input$timezone)
      BC_f_error <- data.frame(BC_f_error)
      datatable(BC_f_error, options = list("pageLength" = 14))
    }
  })
  ## DT 8533 files
  output$table6 <- DT::renderDataTable({
    if (is.null(input$file1) & is.null(input$file2) & is.null(input$file3) &
        is.null(input$file4) & is.null(input$file5) & is.null(input$file6) &
        is.null(input$file7) & is.null(input$file8)) {
      DT_3_error <- read.delim("data/DT8530/2019_08_30_h134435_CSTEP_DT602_8533.csv",
                               header = FALSE, sep = ",", row.names = NULL, skip = 2)
      DT_3_error <- DT_3_error[1:11, ]
      names(DT_3_error) <- c("Setting", "Value")
      datatable(DT_3_error, options = list("pageLength" = 11))

    }
    else if (is.null(input$file7) & (!is.null(input$file1) | !is.null(input$file2) |
                                     !is.null(input$file4) | !is.null(input$file5) |
                                     !is.null(input$file6) | !is.null(input$file3) |
                                     !is.null(input$file8))) {}
    else if (!is.null(input$file7)) {
      c(DT_f3, DT_3_date, DT_3_error) := DT_3(input$file7, input$file7$datapath, input$timezone, input$Slope, input$Intercept)
      DT_3_error <- data.frame(DT_3_error)
      datatable(DT_3_error, options = list("pageLength" = 11))
    }
  })
  ## RH equinox files
  output$table7 <- DT::renderDataTable({
    if (is.null(input$file1) & is.null(input$file2) & is.null(input$file3) &
        is.null(input$file4) & is.null(input$file5) & is.null(input$file6) &
        is.null(input$file7) & is.null(input$file8)) {
      RH_Ef_error <- read.delim("data/RH/2019_10_01_h091014_CBD_RH172.csv",
                                header = TRUE, sep = ",", row.names = NULL)
      RH_Ef_error <- RH_Ef_error[1:8, ]
      datatable(RH_Ef_error, options = list("pageLength" = 8))

    }
    else if (is.null(input$file8) & (!is.null(input$file1) | !is.null(input$file2) |
                                     !is.null(input$file4) | !is.null(input$file5) |
                                     !is.null(input$file6) | !is.null(input$file3) |
                                     !is.null(input$file7))) {}
    else if (!is.null(input$file8)) {
      c(RH_Ef, RH_Ef_date, RH_Ef_error) := RH_E(input$file8, input$file8$datapath, input$timezone)
      RH_Ef_error <- data.frame(RH_Ef_error)
      datatable(RH_Ef_error, options = list("pageLength" = 8))
    }
  })

  ## Raw pollutants/GPS plot

  observe({
    data_joined <- data() %>%
      dplyr::select(-date, -latitude, -longitude)
    updateSelectInput(session, "palleInp", choices = names(data_joined))
    updateSelectInput(session, "palleInp1", choices = names(data_joined))
  })

  output$plot5 <- renderPlot({
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4) &
        is.null(input$file5) & is.null(input$file6)) {
      data <- data_ts()
      ggplot(data, aes(as.POSIXct(date), as.numeric(BC))) +
        scale_x_datetime(date_labels = "%H:%M") +
        geom_line(size = 0.6, color = "deepskyblue") +
        labs(y = "BC", x = "") +
        theme1()
    }
    else {
      data <- data_ts()
      y <- as.numeric(as.character(data[[input$palleInp1]]))
      ggplot(data, aes(as.POSIXct(date), y)) +
        scale_x_datetime(date_labels = "%H:%M") +
        labs(y = input$palleInp1, x = "") +
        theme1() +
        geom_line(size = 0.6, color = "deepskyblue")
    }
  })

  output$plot6 <- renderPlot({
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4) &
        is.null(input$file5) & is.null(input$file6)) {
      data <- data_den()
      ggplot(data, aes(x = as.numeric(BC))) +
        geom_density(color = "deepskyblue", fill = "lightblue") +
        labs(y = "density", x = "BC", title = "Density Plot") +
        theme1()
    }
    else {
      data <- data_den()
      y <- as.numeric(as.character(data[[input$palleInp1]]))
      ggplot(data, aes(x = y)) +
        geom_density(color = "deepskyblue", fill = "lightblue") +
        labs(y = "density", x = input$palleInp1, title = "Density Plot") +
        theme1()
    }
  })

  output$plot <- renderPlot({
    ticks <- qnorm(c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
    labels <- c(1, 5, 10, 25, 50, 75, 90, 95, 99)
    if (is.null(input$file1) & is.null(input$file2) &
        is.null(input$file3) & is.null(input$file4) &
        is.null(input$file5) & is.null(input$file6)) {
      data <- data_qq()
      data$BC <- as.numeric(as.character(data$BC))
      ggplot(data, aes(sample = log10(BC))) +
        stat_qq(size = 2, geom = "point", color = "deepskyblue") +
        stat_qq_line(size = 1, linetype = 2) +
        scale_x_continuous(breaks = ticks, labels = labels) +
        labs(
          x = "Emperical percentile",
          y = "BC"
        ) +
        theme1()
    }
    else {
      data <- data_qq()
      y <- as.numeric(as.character(data[[input$palleInp1]]))
      ggplot(data, aes(sample = log10(y))) +
        stat_qq(size = 2, geom = "point", color = "deepskyblue") +
        stat_qq_line(size = 1, linetype = 2) +
        scale_x_continuous(breaks = ticks, labels = labels) +
        labs(
          x = "Emperical percentile",
          y = input$palleInp1
        ) +
        theme1()
    }
  })

  ## Mapping pollutant

  output$map <- renderLeaflet({
    data <- data()
    if (input$palleInp == "BC_NR" | input$palleInp == "BC_NR_LC") {
      risk.bins <- c(0, 0.5, 2, 5, 10, 20, 40, 100, 500, 2000, 10000)
      pal <- colorBin("Spectral",
                      bins = risk.bins, na.color = "#808080",
                      reverse = TRUE
      )
    } else if (input$palleInp == "BC") {
      risk.bins <- c(-100, -40, -20, 0, 5, 10, 20, 40, 100, 500, 2000)
      pal <- colorBin("Spectral",
                      bins = risk.bins, na.color = "#808080",
                      reverse = TRUE
      )
    } else if (input$palleInp == "Particle_conc") {
      risk.bins <- c(
        0, 5000, 10000, 20000, 40000, 70000, 100000, 150000,
        200000, 500000
      )
      pal <- colorBin("Spectral",
                      bins = risk.bins, na.color = "#808080",
                      reverse = TRUE
      )
    } else if (input$palleInp == "RH") {
      pal <- colorNumeric("RdYlGn",
                          domain = data$RH, na.color = "#808080",
                          reverse = TRUE
      )
    }  else if (input$palleInp == "RH_E") {
      pal <- colorNumeric("RdYlGn",
                          domain = data$RH_E, na.color = "#808080",
                          reverse = TRUE
      )
    } else if (input$palleInp == "CO2") {
      pal <- colorNumeric("RdYlGn",
                          domain = data$CO2, na.color = "#808080",
                          reverse = TRUE
      )
    } else if (input$palleInp == "latitude") {
      pal <- colorNumeric("RdYlGn",
                          domain = data$latitude, na.color = "#808080",
                          reverse = TRUE
      )
    } else {
      risk.bins <- c(0, 10, 25, 50, 100, 500, 2000, 5000, 10000)
      pal <- colorBin("Spectral",
                      bins = risk.bins, na.color = "#808080",
                      reverse = TRUE
      )
    }
    leaflet(data) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      addCircles(
        data = data,
        lng = ~longitude,
        lat = ~latitude,
        popup = paste(
          "Date:", data$date, "<br>",
          input$palleInp, ":", data[[input$palleInp]]),
        weight = 3, radius = 8,
        col = ~ pal(data[[input$palleInp]]), stroke = TRUE,
        fillOpacity = 0.8
      ) %>%
      leaflet::addLegend("bottomright",
                         pal = pal,
                         values = ~ data[[input$palleInp]],
                         title = paste(input$palleInp)
      )
  })
}
## Run app
shinyApp(ui, server)


# also dt and RH

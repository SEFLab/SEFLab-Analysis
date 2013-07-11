#######################################################################################################
########################    Constants                                ##################################
#######################################################################################################

.seflab.data.source.class.name <- "SEFLabDataSource"
.labview.epoch.string <- "1904-01-01 00:00:00 UTC"
.synch.pulse.threshold <- -0.4

.dell.server.column.names <- c(
    "TIME",
    "CPU1.current",
    "CPU2.current",
    "HDD1.12V.current",
    "HDD1.5V.current",
    "MEM_plus_FANS.current",
    "MB.5V.current",
    "MB.3V.current",
    "CPU.FANS.current",
    "MB.VSB.3V.current",
    "CASE.FANS.current",
    "HDD2.12V.current",
    "HDD2.5V.current",
    "CPU1.voltage.1",
    "CPU1.voltage.2",
    "CPU2.voltage.1",
    "CPU2.voltage.2",
    "MEM_plus_FANS.voltage.1",
    "MEM_plus_FANS.voltage.2",
    "MB.5V.voltage",
    "MB.VSB.3V.voltage",
    "MB.3V.voltage",
    "CASE.FANS.voltage.1",
    "CASE.FANS.voltage.2",
    "CPU.FANS.voltage.1",
    "CPU.FANS.voltage.2",
    "HDD.12V.voltage.1",
    "HDD.12V.voltage.2",
    "HDD.5V.voltage",
    "GND",
    "RAW.PULSE")
.dell.server.Rsense <- data.frame(
    value =     c( 0.015,  0.015,      0.075,      0.03,            0.01,    0.01,   0.015,      0.015,       0.025,         0.1,        0.25,      0.03), 
    row.names = c("CPU1", "CPU2", "HDD1.12V", "HDD1.5V", "MEM_plus_FANS", "MB.5V", "MB.3V", "CPU.FANS", "MB.VSB.3V", "CASE.FANS", "HDD2.12V", "HDD2.5V"))

.sun.server.column.names <- c(
    "TIME",
    "CPU1.current",
    "CPU2.current",
    "HDD1.12V.current",
    "HDD1.5V.current",
    "PSU.current",
    "MEM1.current",
    "MEM2.current",
    "FAN.current",
    "CPU1.voltage.1",
    "CPU1.voltage.2",
    "CPU2.voltage.1",
    "CPU2.voltage.2",
    "MEM1.voltage",
    "MEM2.voltage",
    "FAN.voltage.1",
    "FAN.voltage.2",
    "HDD1.12V.voltage.1",
    "HDD1.12V.voltage.2",
    "HDD1.5V.voltage",
    "RAW.PULSE")
.sun.server.Rsense <- data.frame(
    value =     c( 0.03,    0.03,       0.25,      0.03,  0.005,  0.005, 0.005), 
    row.names = c("CPU1", "CPU2", "HDD1.12V", "HDD1.5V", "MEM1", "MEM2", "FAN"))


#######################################################################################################
########################    Class Definition                         ##################################
#######################################################################################################

    
setClass(
    Class=.seflab.data.source.class.name,
    representation=representation(
        power.data            = "data.frame",
        synch.start.timestamp = "POSIXt" 
    ),
    prototype=prototype(
        power.data            = data.frame(),
        synch.start.timestamp = Sys.time()
    ),
    validity=function(object) { return(TRUE) },
    contains=.data.source.class.name
)

setMethod(
    "initialize",
    .seflab.data.source.class.name,
    function(.Object, file="", data=data.frame(), power.data=data.frame(),
                      synch.start.timestamp=Sys.time()) {
      .Object <- .initialize.data.source(.Object, file, data)
      .Object@power.data <- power.data
      .Object@synch.start.timestamp <- synch.start.timestamp
      return(.Object)
    }
)

seflab.data.source <- function(file, synch.start.timestamp, 
                               server, 
                               epoch=.labview.epoch.string,
                               pulse.threshold=.synch.pulse.threshold,
                               header=FALSE, sep="\t", dec=",") {
  if(server == "dell") {
    calculate.power.per.second <- calculate.power.per.second.dell.server
    column.names <- .dell.server.column.names
    colClass <- c(rep("numeric",29))
  } else if (server == "sun") {
    calculate.power.per.second <- calculate.power.per.second.sun.server
    column.names <- .sun.server.column.names
    colClass <- c(rep("numeric",21))
  }
  
  if(is.null(column.names) || length(column.names) == 0) {
    column.names <- character(0)
  }
  data <- read.csv(file       = file,
                   header     = header,
                   sep        = sep,
                   dec        = dec,
                   col.names  = column.names,
                   colClasses = colClass)
  data$TIME <- as.POSIXct(data$TIME, origin = epoch)
  cat("Processing SEFLab data:\n")
  data <- process.seflab.data(server, data, pulse.threshold, synch.start.timestamp)
  cat("Calculating power per second\n")
  power.data <- calculate.power.per.second(data)
  cat("Constructing object\n")
  new(.seflab.data.source.class.name, file                  = file, 
                                      data                  = data,
                                      power.data            = power.data,
                                      synch.start.timestamp = synch.start.timestamp)
}

setMethod(
    "print",
    .seflab.data.source.class.name,
    function(x,...){
      cat("SEFLab Data Source\n")
      cat("==================\n")
      .print.data.source(x)
      cat("Power Data:\n")
      cat(capture.output(x@power.data))
      cat("Synch timestamps:\n")
      cat("\tStart: ", capture.output(x@synch.start.timestamp), "\n", sep = "")
      cat("\n")
    }
)

setMethod(
    "show",
    .seflab.data.source.class.name,
    function(object){
      cat("SEFLab Data Source\n")
      cat("===========\n")
      .show.data.source(object)
      nrowData <- nrow(object@power.data)
      nrowShow <- min(10, nrowData)
      limitMsg <- if(nrowShow < nrowData) {paste("(Limited to ", nrowShow, " rows)", sep="")} else {""}
      cat("Power Data: ", limitMsg, "\n", sep="")
      show(object@power.data[1:nrowShow,])
      cat("Synch timestamps:\n")
      cat("\tStart: ", capture.output(object@synch.start.timestamp), "\n", sep = "")
      cat("\n")
    }
)

setGeneric("getPowerData", function(object) {standardGeneric("getPowerData")})

setMethod(
    "getPowerData",
    .seflab.data.source.class.name,
    function(object) {
      return(object@power.data)
    }
)

setGeneric("getStartTimestamp", function(object) {standardGeneric("getStartTimestamp")})

setMethod(
    "getStartTimestamp",
    .seflab.data.source.class.name,
    function(object) {
      return(object@synch.start.timestamp)
    }
)


#######################################################################################################
########################    Data Processing                          ##################################
#######################################################################################################

process.seflab.data <- function(server, data, pulse.threshold, synch.start.timestamp) {
  if(server == "dell") {
    Rsense <- .dell.server.Rsense
    consolidate.voltages <- consolidate.voltages.dell.server
    calculate.currents <- calculate.currents.dell.server
    calculate.power <- calculate.power.dell.server
  } else if(server == "sun"){
    Rsense <- .sun.server.Rsense
    consolidate.voltages <- consolidate.voltages.sun.server
    calculate.currents <- calculate.currents.sun.server
    calculate.power <- calculate.power.sun.server
  }
  
  cat("\t- Consolidating voltages\n")
  data <- consolidate.voltages(data)
  cat("\t- Calculating currents\n")
  data <- calculate.currents(data, Rsense)
  cat("\t- Calculating power\n")
  data <- calculate.power(data)
  cat("\t- Removing voltage and current columns\n")
  data <- remove.voltage.and.current.columns(data)
  cat("\t- Determining work period\n")
  data <- calculate.work.period(data, pulse.threshold)
  cat("\t- Aligning timestamps\n")
  data <- align.seflab.data.time(data, synch.start.timestamp)
  
  return(data)
}

remove.voltage.and.current.columns <- function(data) {
  drops <- c(names(data[,grep("current", names(data))]),
             names(data[,grep("voltage", names(data))]))
  return(data[,!(names(data) %in% drops)])
}

consolidate.voltages.dell.server <- function(data) {
  data$CPU1.voltage <- data$CPU1.voltage.1 - data$CPU1.voltage.2
  data$CPU2.voltage <- data$CPU2.voltage.1 - data$CPU2.voltage.2
  data$MEM_plus_FANS.voltage <- data$MEM_plus_FANS.voltage.1 - data$MEM_plus_FANS.voltage.2
  data$CASE.FANS.voltage <- data$CASE.FANS.voltage.1 - data$CASE.FANS.voltage.2
  data$CPU.FANS.voltage <- data$CPU.FANS.voltage.1 - data$CPU.FANS.voltage.2
  data$HDD.12V.voltage <- data$HDD.12V.voltage.1 - data$HDD.12V.voltage.2
  
  data$CPU1.voltage.1 <- NULL
  data$CPU1.voltage.2 <- NULL
  data$CPU2.voltage.1 <- NULL
  data$CPU2.voltage.2 <- NULL
  data$MEM_plus_FANS.voltage.1 <- NULL
  data$MEM_plus_FANS.voltage.2 <- NULL
  data$CASE.FANS.voltage.1 <- NULL
  data$CASE.FANS.voltage.2 <- NULL
  data$CPU.FANS.voltage.1 <- NULL
  data$CPU.FANS.voltage.2 <- NULL
  data$HDD1.12V.voltage.1 <- NULL
  data$HDD1.12V.voltage.2 <- NULL
  
  return(data)
}

consolidate.voltages.sun.server <- function(data) {
  data$CPU1.voltage <- data$CPU1.voltage.1 - data$CPU1.voltage.2
  data$CPU2.voltage <- data$CPU2.voltage.1 - data$CPU2.voltage.2
  data$HDD.12V.voltage <- data$HDD1.12V.voltage.1 - data$HDD1.12V.voltage.2
  data$FAN.voltage <- data$FAN.voltage.1 - data$FAN.voltage.2
  
  data$CPU1.voltage.1 <- NULL
  data$CPU1.voltage.2 <- NULL
  data$CPU2.voltage.1 <- NULL
  data$CPU2.voltage.2 <- NULL
  data$HDD1.12V.voltage.1 <- NULL
  data$HDD1.12V.voltage.2 <- NULL
  data$FAN.voltage.1 <- NULL
  data$FAN.voltage.2 <- NULL
  
  return(data)
}

calculate.currents.dell.server <- function(data, Rsense) {
  data$CPU1.current <- data$CPU1.current / Rsense["CPU1","value"]
  data$CPU2.current <- data$CPU2.current / Rsense["CPU2","value"]
  data$HDD1.12V.current <- data$HDD1.12V.current / Rsense["HDD1.12V", "value"]
  data$HDD1.5V.current <- data$HDD1.5V.current / Rsense["HDD1.5V", "value"]
  data$HDD2.12V.current <- data$HDD2.12V.current / Rsense["HDD2.12V", "value"]
  data$HDD2.5V.current <- data$HDD2.5V.current / Rsense["HDD2.5V", "value"]
  data$MEM_plus_FANS.current <- data$MEM_plus_FANS.current / Rsense["MEM_plus_FANS", "value"]
  data$MB.5V.current <- data$MB.5V.current / Rsense["MB.5V", "value"]
  data$MB.3V.current <- data$MB.3V.current / Rsense["MB.3V", "value"]
  data$CPU.FANS.current <- data$CPU.FANS.current / Rsense["CPU.FANS", "value"]
  data$MB.VSB.3V.current <- data$MB.VSB.3V.current / Rsense["MB.VSB.3V", "value"]
  data$CASE.FANS.current <- data$CASE.FANS.current / Rsense["CASE.FANS", "value"]
  
  data <- calculate.memory.current.dell.server(data)
  
  return(data)
}

calculate.memory.current.dell.server <- function(data) {
  data$MEM.current <- data$MEM_plus_FANS.current - data$CPU.FANS.current - data$CASE.FANS.current
  
  return(data)
}

calculate.currents.sun.server <- function(data, Rsense) {
  data$CPU1.current <- data$CPU1.current / Rsense["CPU1","value"]
  data$CPU2.current <- data$CPU2.current / Rsense["CPU2","value"]
  data$HDD1.12V.current <- data$HDD1.12V.current / Rsense["HDD1.12V", "value"]
  data$HDD1.5V.current <- data$HDD1.5V.current / Rsense["HDD1.5V", "value"]
  data$MEM1.current <- data$MEM1.current / Rsense["MEM1", "value"]
  data$MEM2.current <- data$MEM2.current / Rsense["MEM2", "value"]
  data$FAN.current <- data$FAN.current / Rsense["FAN", "value"]
  
  return(data)
}



calculate.power.dell.server <- function(data) {
  data$CPU1.power <- data$CPU1.current * data$CPU1.voltage
  data$CPU2.power <- data$CPU2.current * data$CPU2.voltage
  data$HDD1.12V.power <- data$HDD1.12V.current * data$HDD.12V.voltage
  data$HDD1.5V.power <- data$HDD1.5V.current * data$HDD.5V.voltage
  data$HDD2.12V.power <- data$HDD2.12V.current * data$HDD.12V.voltage
  data$HDD2.5V.power <- data$HDD2.5V.current * data$HDD.5V.voltage
  data$MEM_plus_FANS.power <- data$MEM_plus_FANS.current * data$MEM_plus_FANS.voltage
  data$MB.5V.power <- data$MB.5V.current * data$MB.5V.voltage
  data$MB.3V.power <- data$MB.3V.current * data$MB.3V.voltage
  data$CPU.FANS.power <- data$CPU.FANS.current * data$CPU.FANS.voltage
  data$MB.VSB.3V.power <- data$MB.VSB.3V.current * data$MB.VSB.3V.voltage
  data$CASE.FANS.power <- data$CASE.FANS.current * data$CASE.FANS.voltage
  data$MEM.power <- data$MEM.current * data$MEM_plus_FANS.voltage
  
  return(data)
}

calculate.power.sun.server <- function(data) {
  data$CPU1.power <- data$CPU1.current * data$CPU1.voltage
  data$CPU2.power <- data$CPU2.current * data$CPU2.voltage
  data$HDD1.12V.power <- data$HDD1.12V.current * data$HDD.12V.voltage
  data$HDD1.5V.power <- data$HDD1.5V.current * data$HDD1.5V.voltage
  data$FAN.power <- data$FAN.current * data$FAN.voltage
  
  return(data)
}

### Check if changes have impact on performance
calculate.work.period <- function(data, pulse.threshold) {
  nr.data.points <- nrow(data)
  
  discrete.pulse <- rep(NULL, nr.data.points)
  work.period <- rep(FALSE, nr.data.points)
  high <- FALSE
  time <- NULL
  start <- NULL
  mark.work.period <- FALSE
  for(i in 1:nr.data.points) {
    cat(".")
    #     if(i %% 1000 == 0) {
    #       cat("DEBUG: Progress = ", i / nr.data.points * 100, "%\n", sep="")
    #     }
    row.discrete.pulse <- if(data$RAW.PULSE[i] < pulse.threshold) 0 else 1
    discrete.pulse <- c(discrete.pulse, row.discrete.pulse)
    row <- data[i,]

    
    if(mark.work.period) {  # continue to stamp work period
      work.period[i] <- TRUE
    }
    
    if(row.discrete.pulse == 1 && !high) {  # rising edge of the pulse 
      high <- TRUE
      time <- row$TIME
      if(!is.null(start)) { # stop time stamping if was on already
        mark.work.period <- FALSE
        start <- NULL
      }
    } else if(row.discrete.pulse == 0 && high) {  # falling edge of the pulse 
      new.time <- row$TIME
      duration <- new.time - time
      high <- FALSE
      time <- NULL
      if(is.null(start) && duration >= 1) {  # start time stamping
        start <- new.time
        work.period[i] <- TRUE
        mark.work.period <- TRUE
      }
    }
  }
  cat("!\n")
  
  data$DISCRETE.PULSE <- discrete.pulse
  data$WORK.PERIOD <- work.period
  
  return(data)
}

align.seflab.data.time <- function(data, sync.start.timestamp) {
  workperiod.timestamps <- data$TIME[data$WORK.PERIOD == TRUE]
  seflab.start.timestamp <- head(workperiod.timestamps, n = 1)
  diff <- difftime(sync.start.timestamp, seflab.start.timestamp)
  data$TIME <- data$TIME + diff
  
  return(data)
}

calculate.power.per.second.dell.server <- function(data) {
  time.vector <- work.period <- c()
  cpu1.power <- cpu2.power <- c()
  hdd1.12V.power <- hdd1.5V.power <- hdd2.12V.power <- hdd2.5V.power <- c()
  mem_plus_fans.power <- mem.power <- c()
  mb.5V.power <- mb.3V.power <- mb.vsb.3V.power <- c()
  cpu.fans.power <- case.fans.power <- c()
  
  
  rows <- data.frame()
  s <- NULL
  nr.data.points <- nrow(data)
  for(idx in 1:nr.data.points) {
    cat(".")
#     if(idx %% 1000 == 0) {
#       cat("DEBUG: Progress = ", idx / nr.data.points * 100, "%\n", sep="")
#     }
    
    row <- data[idx,]
    time <- as.POSIXlt(row$TIME)
    if(idx == 1) {
      s <- floor(time$sec)
    }
    
    if(s == floor(time$sec)) {
      rows <- rbind(rows, row)
    } else {
      # calculate
      new.time <- time
      new.time$sec <- s
      if(new.time$sec == 59) {
        new.time$min <- new.time$min - 1
      }

      time.vector <- c(time.vector, as.POSIXct(new.time))
      work.period <- c(work.period, rows[1, "WORK.PERIOD"])
      cpu1.power <- c(cpu1.power, mean(rows$CPU1.power))
      cpu2.power <- c(cpu2.power, mean(rows$CPU2.power))
      hdd1.12V.power <- c(hdd1.12V.power, mean(rows$HDD1.12V.power))
      hdd1.5V.power <- c(hdd1.5V.power, mean(rows$HDD1.5V.power))
      hdd2.12V.power <- c(hdd2.12V.power, mean(rows$HDD2.12V.power))
      hdd2.5V.power <- c(hdd2.5V.power, mean(rows$HDD2.5V.power))
      mem_plus_fans.power <- c(mem_plus_fans.power, mean(rows$MEM_plus_FANS.power))
      mb.5V.power <- c(mb.5V.power, mean(rows$MB.5V.power))
      mb.3V.power <- c(mb.3V.power, mean(rows$MB.3V.power))
      mb.vsb.3V.power <- c(mb.vsb.3V.power, mean(rows$MB.VSB.3V.power))
      cpu.fans.power <- c(cpu.fans.power, mean(rows$CPU.FANS.power))
      case.fans.power <- c(case.fans.power, mean(rows$CASE.FANS.power))
      mem.power <- c(mem.power, mean(rows$MEM.power))
      
      rows <- data.frame()
      rows <- rbind(rows, row)
      s <- floor(time$sec)
    }
  }
  cat("!\n")
  
  cpu.power <- cpu1.power + cpu2.power
  hdd.power <- hdd1.12V.power + hdd2.12V.power + hdd1.5V.power + hdd2.5V.power
  
  averaged.data <- data.frame(TIME=time.vector,
                              WORK.PERIOD=work.period,
                              CPU1.power=cpu1.power,
                              CPU2.power=cpu2.power,
                              HDD1.12V.power=hdd1.12V.power,
                              HDD1.5V.power=hdd1.5V.power,
                              HDD2.12V.power=hdd2.12V.power,
                              HDD2.5V.power=hdd2.5V.power,
                              MEM_plus_FANS.power=mem_plus_fans.power,
                              MB.5V.power=mb.5V.power,
                              MB.3V.power=mb.3V.power,
                              MB.VSB.3V.power=mb.vsb.3V.power,
                              CPU.FANS.power=cpu.fans.power,
                              CASE.FANS.power=case.fans.power,
                              MEM.power=mem.power,
                              CPU.power=cpu.power,
                              HDD.power=hdd.power)
  
  return(averaged.data)
}

calculate.power.per.second.sun.server <- function(data) {
  time.vector <- work.period <- c()
  cpu1.power <- cpu2.power <- c()
  hdd1.12V.power <- hdd1.5V.power <- c()
  fan.power <- c()
  
  
  rows <- data.frame()
  s <- NULL
  nr.data.points <- nrow(data)
  for(idx in 1:nr.data.points) {
    cat(".")
#     if(idx %% 1000 == 0) {
#       cat("DEBUG: Progress = ", idx / nr.data.points * 100, "%\n", sep="")
#     }
    
    row <- data[idx,]
    time <- as.POSIXlt(row$TIME)
    if(idx == 1) {
      s <- floor(time$sec)
    }
    
    if(s == floor(time$sec)) {
      rows <- rbind(rows, row)
    } else {
      # calculate
      new.time <- time
      new.time$sec <- s
      if(new.time$sec == 59) {
        new.time$min <- new.time$min - 1
      }

      time.vector <- c(time.vector, as.POSIXct(new.time))
      work.period <- c(work.period, rows[1, "WORK.PERIOD"])
      cpu1.power <- c(cpu1.power, mean(rows$CPU1.power))
      cpu2.power <- c(cpu2.power, mean(rows$CPU2.power))
      hdd1.12V.power <- c(hdd1.12V.power, mean(rows$HDD1.12V.power))
      hdd1.5V.power <- c(hdd1.5V.power, mean(rows$HDD1.5V.power))
      fan.power <- c(fan.power, mean(rows$FAN.power))
      
      rows <- data.frame()
      rows <- rbind(rows, row)
      s <- floor(time$sec)
    }
  }
  cat("!\n")
  
  cpu.power <- cpu1.power + cpu2.power
  hdd.power <- hdd1.12V.power + hdd1.5V.power
  
  averaged.data <- data.frame(TIME=time.vector,
                              WORK.PERIOD=work.period,
                              CPU1.power=cpu1.power,
                              CPU2.power=cpu2.power,
                              HDD1.12V.power=hdd1.12V.power,
                              HDD1.5V.power=hdd1.5V.power,
                              FANS.power=fan.power,
                              CPU.power=cpu.power,
                              HDD.power=hdd.power)
  
  return(averaged.data)
}
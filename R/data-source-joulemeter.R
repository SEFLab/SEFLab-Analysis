#######################################################################################################
########################    Constants                                ##################################
#######################################################################################################

.joulemeter.data.source.class.name <- "JoulemeterDataSource"

#######################################################################################################
########################    Class Definition                         ##################################
#######################################################################################################

.joulemeter.column.names <- c(
    "TIME",
    "Total.Power",
    "CPU.Power",
    "Monitor.Power",
    "Disk.Power",
    "Base.Power",
    "Application.Power")
.joulemeter.epoch.string <- "1-1-1 0:0:0"

setClass(
    Class=.joulemeter.data.source.class.name,
    representation=representation(),
    prototype=prototype(),
    validity=function(object) { return(TRUE) },
    contains=.data.source.class.name
)

setMethod(
    "initialize",
    .joulemeter.data.source.class.name,
    function(.Object, file="", data=data.frame()) {
      .Object <- .initialize.data.source(.Object, file, data)
      return(.Object)
    }
)

joulemeter.data.source <- function(file, column.names=.joulemeter.column.names,
                                   epoch=.joulemeter.epoch.string, header=FALSE, sep=";", dec=",") {
  if(is.null(column.names) || length(column.names) == 0) {
    column.names <- character(0)
  }
  data <- read.csv(file = file, 
                   header = header, 
                   sep = sep,
                   dec = dec,
                   col.names = column.names)
  data$TIME <- as.POSIXct(floor(data$TIME / 1000), origin = epoch, tz = "CET")
  data$Total.Power <- as.numeric(data$Total.Power)
  data$Monitor.Power <- NULL
  data$Disk.Power <- as.numeric(data$Disk.Power)
  data$Base.Power <- as.numeric(data$Base.Power)
  data$Application.Power <- as.character(data$Application.Power)
  for(idx in 1:nrow(data)) {
    x <- data$Application.Power[idx]
    if(grepl("Waiting for", x)) {
      data$Application.Power[idx] <- 0
    } else {
      data$Application.Power[idx] <- as.numeric(sub(",", ".", x))
    }
  }
  new(.joulemeter.data.source.class.name, file = file, data = data)
}

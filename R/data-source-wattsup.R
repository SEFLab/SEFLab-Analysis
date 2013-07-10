#######################################################################################################
########################    Constants                                ##################################
#######################################################################################################

.wattsup.data.source.class.name <- "WattsUpDataSource"

#######################################################################################################
########################    Class Definition                         ##################################
#######################################################################################################

.wattsup.column.names <- c(
    "TIME",
    "Total.Power"
)
.wattsup.clock.standard.deviation <- -1

setClass(
    Class=.wattsup.data.source.class.name,
    representation=representation(),
    prototype=prototype(),
    validity=function(object) { return(TRUE) },
    contains=.data.source.class.name
)

setMethod(
    "initialize",
    .wattsup.data.source.class.name,
    function(.Object, file="", data=data.frame(), clock.deviation.sec) {
      .Object <- .initialize.data.source(.Object, file, data)
      return(.Object)
    }
)

wattsup.data.source <- function(file, clock.deviation.sec, column.names=.wattsup.column.names,
                                header=TRUE, sep=",", dec=".") {
  if(is.null(column.names) || length(column.names) == 0) {
    column.names <- character(0)
  }
  data <- read.csv(file = file,
                   header = header,
                   sep = sep,
                   dec = dec)
  data <- data[-c(3:length(names(data)))]
  names(data) <- column.names
  data$TIME <- as.POSIXct(data$TIME)
  time.diff <- as.difftime(clock.deviation.sec + .wattsup.clock.standard.deviation, units = "secs")
  data$TIME <- data$TIME + time.diff
  new(.wattsup.data.source.class.name, file = file, data = data)
}

setMethod(
    "print",
    .wattsup.data.source.class.name,
    function(x,...){
      cat("WattsUp Data Source\n")
      cat("==================\n")
      .print.data.source(x)
      cat("\n")
    }
)

setMethod(
    "show",
    .wattsup.data.source.class.name,
    function(object){
      cat("WattsUp Data Source\n")
      cat("===========\n")
      .show.data.source(object)
      cat("\n")
    }
)
#######################################################################################################
########################    Constants                                ##################################
#######################################################################################################

.data.source.class.name <- "DataSource"

#######################################################################################################
########################    Class Definition                         ##################################
#######################################################################################################

setClass(
    Class=.data.source.class.name,
    representation=representation(
        file         = "character",
        data         = "data.frame"
    ),
    prototype=prototype(
        file         = "",
        data         = data.frame()
    ),
    validity=function(object) { return(TRUE) }
)

data.source <- function(file, header=TRUE, quote="\"", sep=";", dec=".") {
  data <- read.csv(file, header, sep, quote, dec)
  new(.data.source.class.name, file = file, data = data)
}

.initialize.data.source <- function(.Object, file="", data=data.frame()) {
  .Object@file <- file
  .Object@data <- data
  return(.Object)
}

.print.data.source <- function(x,...){
  cat("Data Source\n")
  cat("===========\n")
  cat("File:         '", x@file, "'\n", sep="")
  cat("Data:\n")
  cat(capture.output(x@data))
  cat("\n")
}

.show.data.source <- function(object){
  cat("Data Source\n")
  cat("===========\n")
  cat("File:         '", object@file, "'\n", sep="")
  nrowData <- nrow(object@data)
  nrowShow <- min(10, nrowData)
  limitMsg <- if(nrowShow < nrowData) {paste("(Limited to ", nrowShow, " rows)", sep="")} else {""}
  cat("Data: ", limitMsg, "\n", sep="")
  show(object@data[1:nrowShow,])
  cat("\n")
}

setMethod(
    "initialize",
    .data.source.class.name,
    .initialize.data.source
)

setMethod(
    "print",
    .data.source.class.name,
    .print.data.source
)


setMethod(
    "show",
    .data.source.class.name,
    .show.data.source
)

setGeneric("getData", function(object) {standardGeneric("getData")})

setMethod(
    "getData",
    .data.source.class.name,
    function(object) {
      return(object@data)
    }
)

setGeneric("setData", function(object, data) {standardGeneric("setData")})

setMethod(
    "setData",
    .data.source.class.name,
    function(object, data) {
      object@data <- data
      return(object)
    }
)

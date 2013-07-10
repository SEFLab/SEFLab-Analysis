#######################################################################################################
########################    Constants                                ##################################
#######################################################################################################

.experiment.class.name <- "Experiment"

#######################################################################################################
########################    Class Definition                         ##################################
#######################################################################################################

setClass(
    Class=.experiment.class.name,
    representation=representation(
        name      = "character",
        date      = "POSIXt",
        sources   = "list",
        directory = "character"
    ),
    prototype=prototype(
        name      = "",
        date      = Sys.time(),
        sources   = list(),
        directory = "."
    ),
    validity=function(object) { return(TRUE) }
)

setMethod(
    "initialize",
    .experiment.class.name,
    function(.Object, name="", date=Sys.time(), sources=new("DataSource", file="", data=data.frame()), directory=".") {
      .Object@name <- name
      .Object@date <- date
      .Object@sources <- sources
      .Object@directory <- directory
      return(.Object)
    }
)

experiment <- function(name, date, sources, directory) {
  new(.experiment.class.name, name = name, date = date, sources = sources, directory = directory)
}

setMethod(
    "print",
    .experiment.class.name,
    function(x,...){
      cat("Experiment\n")
      cat("==========\n")
      cat("Name: ", x@name, "\n", sep = "")
      cat("Date: ", capture.output(x@date), "\n", sep = "")
      cat("Directory: ", x@directory, "\n", sep = "")
      cat("Sources:\n")
      for(source in x@sources) {
        lines <- strsplit(capture.output(print(source)), "\n")
        for(line in lines) {cat("\t", line, "\n", sep = "")}
      }
      cat("\n")
    }
)

setMethod(
    "show",
    .experiment.class.name,
    function(object){
      cat("Experiment\n")
      cat("==========\n")
      cat("Name: ", object@name, "\n", sep = "")
      cat("Date: ", capture.output(object@date), "\n", sep = "")
      cat("Directory: ", object@directory, "\n", sep = "")
      cat("Sources:\n")
      for(source in object@sources) {
        lines <- strsplit(capture.output(show(source)), "\n")
        for(line in lines) {cat("\t", line, "\n", sep = "")}
      }
      cat("\n")
    }
)
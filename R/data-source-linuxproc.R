#######################################################################################################
########################    Constants                                ##################################
#######################################################################################################

.linuxproc.data.source.class.name <- "LinuxProcDataSource"

#######################################################################################################
########################    Class Definition                         ##################################
#######################################################################################################

.linuxproc.column.names <- c(
  "TIME")

setClass(
  Class=.linuxproc.data.source.class.name,
  representation=representation(
    cpu.ticks = "data.frame",
    proc.ticks = "data.frame",
    combined.ticks = "data.frame",
    cpu.utilization = "data.frame",
    proc.utilization = "data.frame",
    aggregated.proc.utilization = "data.frame"
  ),
  prototype=prototype(
    cpu.ticks = data.frame(),
    proc.ticks = data.frame(),
    combined.ticks = data.frame(),
    cpu.utilization = data.frame(),
    proc.utilization = data.frame(),
    aggregated.proc.utilization = data.frame()
  ),
  validity=function(object) { return(TRUE) },
  contains=.data.source.class.name
)

setMethod(
  "initialize",
  .linuxproc.data.source.class.name,
  function(.Object, file="", data=data.frame(), cpu.ticks=data.frame(), proc.ticks=data.frame(), 
           combined.ticks=data.frame(), cpu.utilization=data.frame(), proc.utilization=data.frame(), 
           aggregated.proc.utilization=data.frame()) {
    .Object <- .initialize.data.source(.Object, file, data)
    .Object@cpu.ticks <- cpu.ticks
    .Object@proc.ticks <- proc.ticks
    .Object@combined.ticks <- combined.ticks
    .Object@cpu.utilization <- cpu.utilization
    .Object@proc.utilization <- proc.utilization
    .Object@aggregated.proc.utilization <- aggregated.proc.utilization
    return(.Object)
  }
)

linuxproc.data.source <- function(directory) {
  cat("Parsing files at '", directory, "'...\n", sep="")
  
  ticks <- parse.stat.files(directory)
  cpu.ticks <- ticks[[1]]
  proc.ticks <- ticks[[2]]
  
  cat("Files parsed. Calulating utilizations...\n", sep="")
  
  cpu.utilization <- calculate.cpu.utilization(cpu.ticks)
  cat("CPU utilization calculated:\n", sep="")
  
  combined.ticks <- combine.cpu.and.proc.ticks(cpu.ticks, proc.ticks)
  proc.utilization <- calculate.proc.utilization(combined.ticks)
  cat("Process utilizations calulated\n", sep="")
  
  cat("Aggregating sub-process utlizations to main process...\n")
  aggregated.proc.utilizations <- aggregate.proc.utilizations.to.main.processes(proc.utilization)
  
  new(.linuxproc.data.source.class.name, 
      file = directory, 
      data = data.frame(), 
      cpu.ticks = cpu.ticks,
      proc.ticks = proc.ticks,
      combined.ticks = combined.ticks,
      cpu.utilization = cpu.utilization,
      proc.utilization = proc.utilization,
      aggregated.proc.utilization = aggregated.proc.utilization)
}

setMethod(
  "print",
  .linuxproc.data.source.class.name,
  function(x,...){
    cat("Linux proc Data Source\n")
    cat("==================\n")
    cat("Directory:         '", x@file, "'\n", sep="")
    cat("CPU Utilization Data:\n")
    cat(capture.output(x@cpu.utilization))
    cat("\n")
    cat("Proc Utilization Data:\n")
    cat(capture.output(x@aggregated.proc.utilization))
    cat("\n")
  }
)

setMethod(
  "show",
  .linuxproc.data.source.class.name,
  function(object){
    cat("SEFLab Data Source\n")
    cat("===========\n")
    cat("Linux proc Data Source\n")
    cat("==================\n")
    cat("Directory:         '", x@file, "'\n", sep="")
    nrowData <- nrow(object@cpu.utilization)
    nrowShow <- min(10, nrowData)
    limitMsg <- if(nrowShow < nrowData) {paste("(Limited to ", nrowShow, " rows)", sep="")} else {""}
    cat("Data: ", limitMsg, "\n", sep="")
    cat("CPU Utilization Data: ", limitMsg, "\n", sep="")
    show(object@cpu.utilization[1:nrowShow,])
    cat("\n")
    nrowData <- nrow(object@aggregated.proc.utilization)
    nrowShow <- min(10, nrowData)
    limitMsg <- if(nrowShow < nrowData) {paste("(Limited to ", nrowShow, " rows)", sep="")} else {""}
    cat("Proc Utilization Data: ", limitMsg, "\n", sep="")
    show(object@aggregated.proc.utilization[1:nrowShow,])
    cat("\n")
  }
)

setGeneric("getCPUUtilization", function(object) {standardGeneric("getCPUUtilization")})

setMethod(
  "getCPUUtilization",
  .linuxproc.data.source.class.name,
  function(object) {
    return(object@cpu.utilization)
  }
)


setGeneric("getProcUtilization", function(object) {standardGeneric("getProcUtilization")})

setMethod(
  "getProcUtilization",
  .linuxproc.data.source.class.name,
  function(object) {
    return(object@aggregated.proc.utilization)
  }
)

#######################################################################################################
########################    Data Processing                          ##################################
#######################################################################################################
.cpu.file.regex <- "[0-9]+_[0-9]+_stat"


get.timestamps.from.perfomance.files <- function(directory) {
  files <- list.files(path=directory, include.dirs=FALSE, no..=TRUE)
  timestamps <- sub(pattern="(_[0-9]+_[0-9]+_[a-zA-Z(|]+)?_stat$", replacement="", x=files)
  timestamps <- as.POSIXct(x=timestamps, format="%Y%m%d_%H%M%S", tz="CET")
  timestamps <- unique(timestamps)
  
  return(timestamps)
}

get.stat.files.for.timestamp <- function(base.dir, timestamp) {
  pattern <- paste(format(timestamp, format="%Y%m%d_%H%M%S"), "_*", sep = "")
  files <- list.files(base.dir, pattern)
  
  return(files)
}

get.first.line.from.files <- function(files) {
  lines <- c()
  for(file in files) {
    line <- system(command=paste("head -n 1 ", file, sep=""), intern=TRUE)
    lines <- c(lines, line)
  }
  
  return(lines)
}

get.cpu.stat.file <- function(files) {
  cpu.matches <- grepl(.cpu.file.regex, files)
  cpu.file <- files[cpu.matches]
  
  return(cpu.file)
}

get.proc.stat.files <- function(files) {
  cpu.matches <- grepl(.cpu.file.regex, files)
  proc.files <- files[!cpu.matches]
  
  return(proc.files)
}

parse.cpu.stat.file <- function(file) {
  #   cat("DEBUG: parse.cpu.stat.file: file = ", file, "\n")
  first.line <- get.first.line.from.files(file)
  text.connection <- textConnection(first.line)
  data <- read.csv(file = text.connection, header = FALSE, sep="")
  #   data <- data[, -c(1,6)] # remove cpu tag and iowait cause it is included in idle ticks
  data <- data[, -1] # remove cpu tag and iowait cause it is included in idle ticks
  total.ticks <- sum(data)
  idle.ticks <- data[, 4]
  
  return(data.frame(total.ticks = total.ticks, idle.ticks = idle.ticks))
}

parse.proc.stat.files <- function(files) {
  #   cat("DEBUG: parse.proc.stat.files: files = ", files, "\n")
  ticks <- c()
  pid <- c()
  for(file in files) {
    #     cat("DEBUG: calculate.proc.ticks timastamp = ", timestamp, " file = ", file, "\n")
    file.data <- read.csv(file=file, sep=" ", header=FALSE)
    pid <- c(pid, file.data[, 1])
    ticks <- c(ticks, sum(file.data[, 14], file.data[, 15]))
  }
  
  return(data.frame(pid = pid, ticks = ticks))
}

aggregate.cpu.ticks <- function(cpu.ticks, timestamp, cpu.ticks.previous, cpu.ticks.for.timestamp) {
  return(rbind(cpu.ticks, 
               data.frame(timestamp = timestamp,
                          total.ticks.previous = cpu.ticks.previous[1, "total.ticks"],
                          total.ticks.current = cpu.ticks.for.timestamp[1, "total.ticks"],
                          idle.ticks.previous = cpu.ticks.previous[1, "idle.ticks"],
                          idle.ticks.current = cpu.ticks.for.timestamp[1, "idle.ticks"])))
}

aggregate.proc.ticks <- function(proc.ticks, timestamp, proc.ticks.previous, proc.ticks.for.timestamp) {
  tmp.proc.ticks <- data.frame()
  for(pid in proc.ticks.for.timestamp$pid) {
    pid.ticks.for.timestamp <- proc.ticks.for.timestamp[proc.ticks.for.timestamp$pid == pid,]
    pid.ticks.previous <- NA
    if(pid %in% proc.ticks.previous$pid) {
      pid.ticks.previous <- proc.ticks.previous[proc.ticks.previous$pid == pid, "ticks"]
    }
    #     cat("DEBUG: timestamp = ", capture.output(timestamp), 
    #         " pid.ticks.previous = ", pid.ticks.previous, 
    #         " pid.ticks.current = ", pid.ticks.for.timestamp[1, "ticks"] ,"\n")
    tmp.proc.ticks <- rbind(tmp.proc.ticks,
                            data.frame(timestamp = timestamp,
                                       pid = pid,
                                       ticks.previous = pid.ticks.previous,
                                       ticks.current = pid.ticks.for.timestamp[1, "ticks"]))
  }
  return(rbind(proc.ticks, tmp.proc.ticks))
}

# this could be a lot faster if instead of growing a data frame it would grow independent vectors
# however that would mean maintianing some 10 vectors and it would be necessary for subfunctions 
# to return lists of vectors and that is evil!
parse.stat.files <- function(directory) {
  cpu.ticks.previous <- data.frame(total.ticks = NA, idle.ticks = NA)
  proc.ticks.previous <- data.frame(pid = NA, ticks = NA)
  cpu.ticks <- data.frame()
  proc.ticks <- data.frame()
  timestamps <- get.timestamps.from.perfomance.files(directory)
  timestamps.count <- length(timestamps)
  for(index in 1:timestamps.count) {
    cat(".")
#     if(index %% 500 == 0) {
#       cat((index / timestamps.count * 100), "%\n")
#     }
    
    timestamp <- timestamps[index]
    #     cat("DEBUG: directory = ", directory, "timestamp = ", capture.output(timestamp), "\n")
    files.for.timestamp <- get.stat.files.for.timestamp(directory, timestamp)
    cpu.stat.file <- get.cpu.stat.file(paste(directory, "/", files.for.timestamp, sep=""))
    #     cat("DEBUG: files.for.timestamp = ", files.for.timestamp," cpu.stat.file = ", cpu.stat.file, "\n")
    cpu.ticks.for.timestamp <- parse.cpu.stat.file(cpu.stat.file)
    cpu.ticks <- aggregate.cpu.ticks(cpu.ticks, timestamp, cpu.ticks.previous, cpu.ticks.for.timestamp)
    cpu.ticks.previous <- cpu.ticks.for.timestamp
    
    proc.stat.files <- get.proc.stat.files(files.for.timestamp)
    #     cat("DEBUG: files.for.timestamp = ", files.for.timestamp," proc.stat.files = ", proc.stat.files, "\n")
    proc.ticks.for.timestamp <- parse.proc.stat.files(paste(directory, "/", proc.stat.files, sep=""))
    proc.ticks <- aggregate.proc.ticks(proc.ticks, timestamp, 
                                       proc.ticks.previous, proc.ticks.for.timestamp)
    proc.ticks.previous <- proc.ticks.for.timestamp
  }
  cat("!\n")
  
  return(list(cpu.ticks, proc.ticks))
}

combine.cpu.and.proc.ticks <- function(cpu.ticks, proc.ticks) {
  merged <- merge(x=proc.ticks, y=cpu.ticks, by="timestamp", all=TRUE)
  names(merged)[which(names(merged) == "ticks.previous")] <- "proc.ticks.previous"
  names(merged)[which(names(merged) == "ticks.current")] <- "proc.ticks.current"
  names(merged)[which(names(merged) == "total.ticks.previous")] <- "cpu.total.ticks.previous"
  names(merged)[which(names(merged) == "total.ticks.current")] <- "cpu.total.ticks.current"
  names(merged)[which(names(merged) == "idle.ticks.previous")] <- "cpu.idle.ticks.previous"
  names(merged)[which(names(merged) == "idle.ticks.current")] <- "cpu.idle.ticks.current"
  
  return(merged)
}

calculate.cpu.utilization <- function(cpu.ticks) {
  cpu.total.ticks.delta <- cpu.ticks$total.ticks.current - cpu.ticks$total.ticks.previous
  cpu.idle.ticks.delta <- cpu.ticks$idle.ticks.current - cpu.ticks$idle.ticks.previous
  cpu.ticks$percentage <- (1000 * (cpu.total.ticks.delta - cpu.idle.ticks.delta)/cpu.total.ticks.delta + 5) / 10
  
  cpu.utilization <- cpu.ticks[, -c(2:5)]
  
  return(cpu.utilization)
}


calculate.proc.utilization <- function(combined.ticks) {
  cpu.total.ticks.delta <- combined.ticks$cpu.total.ticks.current - combined.ticks$cpu.total.ticks.previous
  proc.ticks.delta <- combined.ticks$proc.ticks.current - combined.ticks$proc.ticks.previous
  combined.ticks$percentage <- (proc.ticks.delta / cpu.total.ticks.delta) * 100
  
  proc.utilization <- combined.ticks[, -c(3:4, 6:9)]
  
  return(proc.utilization)
}

aggregate.proc.utilizations <- function(proc.utilization) {
  aggregated.utilization <- c()
  timestamps <- unique(proc.utilization$timestamp)
  timestamps.count <- length(timestamps)
  for(index in 1:timestamps.count) {
    cat(".")
#     if(index %% 500 == 0) {
#       cat((index / timestamps.count * 100), "%\n")
#     }
    
    timestamp <- timestamps[index]
    proc.utilization.for.timestamp <- proc.utilization[proc.utilization$timestamp == timestamp, ]
    aggregate.utilization.for.timestamp <- sum(proc.utilization.for.timestamp$percentage, na.rm=TRUE)
    aggregated.utilization <- c(aggregated.utilization, aggregate.utilization.for.timestamp)
  }
  cat("!\n")
  
  return(data.frame(timestamp = timestamps, percentage = aggregated.utilization))
}

parse.stat.files.and.calculate.utilizations <- function(directory, save.directory="/tmp") {
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
  aggregated.proc.utilizations <- aggregate.proc.utilizations(proc.utilization)
  
  save(file=paste(save.directory, "/utilization-data.RData", sep=""), 
       cpu.ticks, proc.ticks, combined.ticks, cpu.utilization, proc.utilization, aggregated.proc.utilizations)
  
  return(list(cpu.utilization, aggregated.proc.utilizations))
}
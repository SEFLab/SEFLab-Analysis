source("linux-performance.R")

create.tmp.dir <- function(base.dir) {
  tmp.dir.name <- paste(base.dir, "/tmpDir", sep="")
  while(file.exists(tmp.dir.name)) {
    tmp.dir.name <- paste(tmp.dir.name, ".1", sep="")
  }
  dir.create(path=tmp.dir.name)
  return(tmp.dir.name)
}

remove.tmp.dir <- function(tmp.dir) {
  files <- list.files(path=tmp.dir, recursive=TRUE, include.dirs=FALSE, full.names=TRUE)
  file.remove(files)
  files <- list.files(path=tmp.dir, recursive=TRUE, include.dirs=TRUE, full.names=TRUE)
  file.remove(files)
  file.remove(tmp.dir)
}

populate.dir.with.files <- function(base.dir, file.names, contents=data.frame()) {
  full.file.names <- paste(base.dir, "/", file.names, sep="")
  file.create(full.file.names)
  if(nrow(contents) > 0) {
    for(file.name in file.names) {
#       cat("DEBUG: file.name = ", file.name, "\n")
      write.to.file(paste(base.dir, "/", file.name, sep=""), 
                    contents[contents$file == file.name, "contents"])
    }
  }
}

write.to.file <- function(file, contents) {
#   cat("DEBUG: file = ", file, " contents = ", contents, "\n")
  fileConn <- file(file)
  write(as.character(contents), fileConn)
  close(fileConn)
}

setup.tmp.dir.with.stat.files <- function(file.contents=data.frame()) {
  tmp.dir <- create.tmp.dir("/tmp")
  if(nrow(file.contents) == 0) {
    file.names <- c("20130618_100753_10577_10577_cron_stat",
                    "20130618_100853_10577_10577_cron_stat", 
                    "20130618_100952_10577_10577_cron_stat",
                    "20130618_100753_10645_10577_cron_stat",
                    "20130618_100853_10645_10577_cron_stat",
                    "20130618_100952_10645_10577_cron_stat",
                    "20130618_100753_stat",
                    "20130618_100853_stat",
                    "20130618_100952_stat")
  } else {
    file.names <- file.contents$file
  }
  populate.dir.with.files(tmp.dir, file.names, file.contents)
  
  return(tmp.dir)
}


checkEquals.vector <- function(v1, v2) {
  checkEquals(length(v1), length(v2))
  for(index in 1:length(v1)) {
    checkEquals(as.character(v1[index]), as.character(v2[index]))
  }
}

test.get.timestamps.from.perfomance.files <- function() {
  tmp.dir <- setup.tmp.dir.with.stat.files()
  
  timestamps <- get.timestamps.from.perfomance.files(tmp.dir)
  remove.tmp.dir(tmp.dir)
  
  checkEquals.vector(timestamps, as.POSIXct(c("2013-06-18 10:07:53 CEST",
                                             "2013-06-18 10:08:53 CEST",
                                             "2013-06-18 10:09:52 CEST")))
}


test.get.stat.files.for.timestamp <- function() {
  tmp.dir <- setup.tmp.dir.with.stat.files()
  
  files <- get.stat.files.for.timestamp(tmp.dir, as.POSIXct("2013/06/18 10:07:53"))
  remove.tmp.dir(tmp.dir)
  
  checkEquals.vector(files, c("20130618_100753_10577_10577_cron_stat",
                               "20130618_100753_10645_10577_cron_stat",
                               "20130618_100753_stat"))
}

test.get.first.lines.from.files <- function() {
  tmp.dir <- create.tmp.dir("/tmp")
  file.names <- c("file1", "file2")
  contents <- data.frame(file = file.names, contents = c("some text with \n more than one line",
                                                         "different text\n also with two lines"))
  populate.dir.with.files(tmp.dir, file.names, contents)
  
  lines <- get.first.line.from.files(paste(tmp.dir, "/", file.names, sep=""))
  remove.tmp.dir(tmp.dir)
  
  checkEquals.vector(lines, c("some text with ", "different text"))
}

test.get.cpu.stat.file <- function() {
  tmp.dir <- setup.tmp.dir.with.stat.files()
  files <- get.stat.files.for.timestamp(tmp.dir, as.POSIXct("2013/06/18 10:07:53"))
  
  file <- get.cpu.stat.file(files)
  remove.tmp.dir(tmp.dir)
  
  checkEquals(file, "20130618_100753_stat")
}

test.get.proc.stat.files <- function() {
  tmp.dir <- setup.tmp.dir.with.stat.files()
  files <- get.stat.files.for.timestamp(tmp.dir, as.POSIXct("2013/06/18 10:07:53"))
  
  files <- get.proc.stat.files(files)
  remove.tmp.dir(tmp.dir)
  
  checkEquals.vector(files, c("20130618_100753_10577_10577_cron_stat",
                              "20130618_100753_10645_10577_cron_stat"))
}

test.parse.cpu.stat.file <- function() {
  file.contents <- data.frame(file = "20130618_100753_stat", 
                              contents = "cpu  38161736 13315 37694797 846498666 54217887 9747 3472885 0 0\nsome other conntents")
  tmp.dir <- setup.tmp.dir.with.stat.files(file.contents)
  files <- get.stat.files.for.timestamp(tmp.dir, as.POSIXct("2013/06/18 10:07:53"))
  file <- get.cpu.stat.file(files)
  
  cpu.ticks <- parse.cpu.stat.file(paste(tmp.dir, "/", file, sep=""))
  remove.tmp.dir(tmp.dir)
  
  checkEquals(cpu.ticks[1, "total.ticks"], 980069033)
  checkEquals(cpu.ticks[1, "idle.ticks"], 846498666)
}

test.parse.proc.files <- function() {
  file.contents <- data.frame(file = c("20130618_100753_10577_10577_cron_stat",
                                       "20130618_100753_10645_10577_cron_stat"),
                              contents = c("10577 (cron) S 1 10577 10577 0 -1 4202560 15980 659139729 0 9574 31 54 18779328 2583841 20 0 1 0 368471 14995456 132 28613353472 1 1 0 0 0 0 0 0 81923 18446744073709551615 0 0 17 1 0 0 9 0 0",
                                           "10645 (cron) S 10577 10577 10577 0 -1 4202816 755 0 0 0 2 6 0 0 20 0 1 0 118836214 84746240 599 28613353472 1 1 0 0 0 0 0 0 16387 18446744073709551615 0 0 17 0 0 0 0 0 0"))
  tmp.dir <- setup.tmp.dir.with.stat.files(file.contents)
  files <- get.stat.files.for.timestamp(tmp.dir, as.POSIXct("2013/06/18 10:07:53"))
  files <- get.proc.stat.files(files)
                           
  proc.ticks <- parse.proc.stat.files(paste(tmp.dir, "/", files, sep=""))
  remove.tmp.dir(tmp.dir)
  
  checkEquals.vector(proc.ticks[, "pid"], c(10577, 10645))
  checkEquals.vector(proc.ticks[, "ticks"], c(85, 8))
}

test.aggregate.cpu.ticks <- function() {
  timestamp1 <- as.POSIXct("2013/06/18 15:36:00")
  timestamp2 <- as.POSIXct("2013/06/18 15:37:00")
  cpu.ticks <- data.frame(timestamp = timestamp1,
                          total.ticks.previous = NA,
                          total.ticks.current = 1,
                          idle.ticks.previous = NA,
                          idle.ticks.current = 2)
  cpu.ticks.previous <- data.frame(total.ticks = 1, idle.ticks = 2)
  cpu.ticks.for.timestamp <- data.frame(total.ticks = 3, idle.ticks = 4)
  
  cpu.ticks <- aggregate.cpu.ticks(cpu.ticks, timestamp2, cpu.ticks.previous, cpu.ticks.for.timestamp)
  
  checkEquals(nrow(cpu.ticks), 2)
  checkEquals.vector(cpu.ticks$timestamp, c(timestamp1, timestamp2))
  checkEquals.vector(cpu.ticks$total.ticks.previous, c(NA, 1))
  checkEquals.vector(cpu.ticks$total.ticks.current, c(1, 3))
  checkEquals.vector(cpu.ticks$idle.ticks.previous, c(NA, 2))
  checkEquals.vector(cpu.ticks$idle.ticks.current, c(2, 4))
}

test.aggregate.proc.ticks <- function() {
  timestamp1 <- as.POSIXct("2013/06/18 15:36:00")
  timestamp2 <- as.POSIXct("2013/06/18 15:37:00")
  proc.ticks <- data.frame(timestamp = rep(timestamp1,2),
                           pid = c(100,200),
                           ticks.previous = c(NA,NA),
                           ticks.current = c(1,2))
  proc.ticks.previous <- data.frame(pid = 100, ticks = 1)
  proc.ticks.for.timestamp <- data.frame(pid = c(100,300),
                                         ticks = c(3,4))
  
  proc.ticks <- aggregate.proc.ticks(proc.ticks, timestamp2, proc.ticks.previous, proc.ticks.for.timestamp)
  
  checkEquals(nrow(proc.ticks), 4)
  checkEquals.vector(proc.ticks$timestamp, c(timestamp1, timestamp1, timestamp2, timestamp2))
  checkEquals.vector(proc.ticks$pid, c(100, 200, 100, 300))
  checkEquals.vector(proc.ticks$ticks.previous, c(NA, NA, 1, NA))
  checkEquals.vector(proc.ticks$ticks.current, c(1, 2, 3, 4))
}

test.process.stat.files <- function() {
  timestamp1 <- as.POSIXct("2013-06-18 15:36:00")
  timestamp2 <- as.POSIXct("2013-06-18 15:37:00")
  file.contents <- data.frame(file = c("20130618_153600_stat",
                                       "20130618_153700_stat",
                                       "20130618_153600_100_100_proc100_stat",
                                       "20130618_153700_100_100_proc100_stat",
                                       "20130618_153600_200_100_proc200_stat",
                                       "20130618_153700_300_100_proc300_stat"),
                              contents = c("cpu  0 0 0 2 1 0 0 0 0",
                                           "cpu  0 0 0 4 2 0 0 0 0",
                                           "100 (proc100) S 1 0 0 0 0 0 0 0 0 0 1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
                                           "100 (proc100) S 1 0 0 0 0 0 0 0 0 0 2 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
                                           "200 (proc200) S 1 0 0 0 0 0 0 0 0 0 4 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0",
                                           "300 (proc300) S 1 0 0 0 0 0 0 0 0 0 8 16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"))
  tmp.dir <- setup.tmp.dir.with.stat.files(file.contents)
  
  ticks <- parse.stat.files(tmp.dir)
  cpu.ticks <- ticks[[1]]
  proc.ticks <- ticks[[2]]
  remove.tmp.dir(tmp.dir)
  
  checkEquals(nrow(cpu.ticks), 2)
  checkEquals.vector(cpu.ticks$timestamp, c(timestamp1, timestamp2))
  checkEquals.vector(cpu.ticks$total.ticks.previous, c(NA, 3))
  checkEquals.vector(cpu.ticks$total.ticks.current, c(3, 6))
  checkEquals.vector(cpu.ticks$idle.ticks.previous, c(NA, 2))
  checkEquals.vector(cpu.ticks$idle.ticks.current, c(2, 4))
  checkEquals(nrow(proc.ticks), 4)
  checkEquals.vector(proc.ticks$timestamp, c(timestamp1, timestamp1, timestamp2, timestamp2))
  checkEquals.vector(proc.ticks$pid, c(100, 200, 100, 300))
  checkEquals.vector(proc.ticks$ticks.previous, c(NA, NA, 3, NA))
  checkEquals.vector(proc.ticks$ticks.current, c(3, 12, 6, 24))
}

test.combine.cpu.and.proc.ticks <- function() {
  timestamp1 <- as.POSIXct("2013-06-18 15:36:00")
  timestamp2 <- as.POSIXct("2013-06-18 15:37:00")
  cpu.ticks <- data.frame(timestamp = c(timestamp1, timestamp2),
                          total.ticks.previous = c(NA, 3),
                          total.ticks.current = c(3, 6),
                          idle.ticks.previous = c(NA, 1),
                          idle.ticks.current = c(1, 2))
  proc.ticks <- data.frame(timestamp = c(timestamp1, timestamp1, timestamp2, timestamp2),
                           pid = c(100, 200, 100, 300),
                           ticks.previous = c(NA, NA, 3, NA),
                           ticks.current = c(3, 12, 6, 24),
                           main.proc = c(TRUE, FALSE, TRUE, FALSE))
  
  combined.ticks <- combine.cpu.and.proc.ticks(cpu.ticks, proc.ticks)
  
  checkEquals.vector(combined.ticks$timestamp, c(timestamp1, timestamp1, timestamp2, timestamp2))       
  checkEquals.vector(combined.ticks$pid, c(100, 200, 100, 300))       
  checkEquals.vector(combined.ticks$proc.ticks.previous,  c(NA, NA, 3, NA))       
  checkEquals.vector(combined.ticks$proc.ticks.current,  c(3, 12, 6, 24))       
  checkEquals.vector(combined.ticks$main.proc,  c(TRUE, FALSE, TRUE, FALSE))       
  checkEquals.vector(combined.ticks$cpu.total.ticks.previous,  c(NA, NA, 3, 3))       
  checkEquals.vector(combined.ticks$cpu.total.ticks.current,  c(3, 3, 6, 6))       
  checkEquals.vector(combined.ticks$cpu.idle.ticks.previous,  c(NA, NA, 1, 1))
  checkEquals.vector(combined.ticks$cpu.idle.ticks.current,  c(1, 1, 2, 2))       
}

test.calculate.cpu.ticks <- function() {
  timestamp1 <- as.POSIXct("2013-06-18 15:36:00")
  timestamp2 <- as.POSIXct("2013-06-18 15:37:00")
  timestamp3 <- as.POSIXct("2013-06-18 15:38:00")
  cpu.ticks <- data.frame(timestamp = c(timestamp1, timestamp2, timestamp3),
                          total.ticks.previous = c(NA, 3, 6),
                          total.ticks.current = c(3, 6, 10),
                          idle.ticks.previous = c(NA, 1, 2),
                          idle.ticks.current = c(1, 2, 5))
  
  cpu.utilization <- calculate.cpu.utilization(cpu.ticks)
  
  checkEquals.vector(cpu.utilization$timestamp, c(timestamp1, timestamp2, timestamp3))
  checkEquals.vector(cpu.utilization$percentage, c(NA, (1000 * 2/3 + 5) / 10, (1000 * 1/4 + 5) / 10))
}

test.calculate.proc.ticks <- function() {
  timestamp1 <- as.POSIXct("2013-06-18 15:36:00")
  timestamp2 <- as.POSIXct("2013-06-18 15:37:00")
  timestamp3 <- as.POSIXct("2013-06-18 15:38:00")
  cpu.ticks <- data.frame(timestamp = c(timestamp1, timestamp2, timestamp3),
                          total.ticks.previous = c(NA, 3, 6),
                          total.ticks.current = c(3, 6, 10),
                          idle.ticks.previous = c(NA, 1, 2),
                          idle.ticks.current = c(1, 2, 5))
  proc.ticks <- data.frame(timestamp = c(timestamp1, timestamp1, timestamp2, timestamp2, timestamp3, timestamp3),
                           pid = c(100, 200, 100, 300, 100, 300),
                           ticks.previous = c(NA, NA, 3, NA, 6, 24),
                           ticks.current = c(3, 12, 6, 24, 10, 50),
                           main.proc = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
  combined.ticks <- combine.cpu.and.proc.ticks(cpu.ticks, proc.ticks)
  
  proc.utilization <- calculate.proc.utilization(combined.ticks)
  
  checkEquals.vector(proc.utilization$timestamp, c(timestamp1, timestamp1, timestamp2, timestamp2, timestamp3, timestamp3))
  checkEquals.vector(proc.utilization$percentage, c(NA, NA, 100, NA, 100, 650))
}

test.aggregate.proc.utilizations <- function() {
  timestamp1 <- as.POSIXct("2013-06-18 15:36:00")
  timestamp2 <- as.POSIXct("2013-06-18 15:37:00")
  timestamp3 <- as.POSIXct("2013-06-18 15:38:00")
  proc.utilization <- data.frame(timestamp = c(timestamp1, timestamp1, timestamp2, timestamp2, timestamp3, timestamp3),
                                 pid = c(100, 200, 100, 300, 100, 300),
                                 percentage = c(10, 20, 30, 40 ,50, 60))
  
  aggregated.proc.utilization <- aggregate.proc.utilizations(proc.utilization)
  
  checkEquals(nrow(aggregated.proc.utilization), 3)
  checkEquals.vector(aggregated.proc.utilization$timestamp, c(timestamp1, timestamp2, timestamp3))
  checkEquals.vector(aggregated.proc.utilization$percentage, c(30, 70, 110))
}
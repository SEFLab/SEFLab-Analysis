suppressPackageStartupMessages(library(ggplot2))

#######################################################################################################
########################    Data Functions                           ##################################
#######################################################################################################

get.common.subset.data <- function(joulemeter.src, seflab.src, wattsup.src) {
  joulemeter.data   <- getData(joulemeter.src)
  seflab.power.data <- getPowerData(seflab.src)
  wattsup.data      <- getData(wattsup.src)
  ndata             <- nrow(joulemeter.data)
  
  #   cat("DEBUG nrow(joulemeter.data) = ", ndata, "\n", sep = "")
  
  time <- work.period <- c()
  wattsup.total <- joulemeter.total <- c()
  diff.total <- diff.total.abs <- c()
  seflab.cpu <- joulemeter.cpu <- c()
  diff.cpu <- diff.cpu.abs <- c()
  seflab.hdd <- joulemeter.hdd <- c()
  diff.hdd <- diff.hdd.abs <- c()
  
  for(idx in 1:ndata) {
    timestamp <- joulemeter.data$TIME[idx]
    
    #     cat("DEBUG idx = ", idx, ", time = ", capture.output(time), "\n", sep = "")
    
    work.period.value      <- seflab.power.data$WORK.PERIOD[seflab.power.data$TIME == timestamp]
    seflab.cpu.value       <- seflab.power.data$CPU.power[seflab.power.data$TIME == timestamp]
    seflab.hdd.value       <- seflab.power.data$HDD.power[seflab.power.data$TIME == timestamp]
    wattsup.total.value    <- mean(wattsup.data$Total.Power[wattsup.data$TIME == timestamp])
    joulemeter.cpu.value   <- joulemeter.data$CPU.Power[joulemeter.data$TIME == timestamp]
    joulemeter.hdd.value   <- joulemeter.data$Disk.Power[joulemeter.data$TIME == timestamp]
    joulemeter.total.value <- joulemeter.data$Total.Power[joulemeter.data$TIME == timestamp]
    
    #        cat("DEBUG time = ", capture.output(timestamp), ", CPU = ", seflab.cpu.value, ", HDD = ", seflab.hdd.value, ", Total = ", wattsup.total.value, "\n")
    
    if(length(seflab.cpu.value) > 0 && length(seflab.hdd.value) > 0 && length(wattsup.total.value) > 0) {
      
      diff.cpu.value <- joulemeter.cpu.value - seflab.cpu.value
      diff.cpu.abs.value <- abs(diff.cpu.value)
      diff.hdd.value <- joulemeter.hdd.value - seflab.hdd.value
      diff.hdd.abs.value <- abs(diff.hdd.value)
      diff.total.value <- joulemeter.total.value - wattsup.total.value
      diff.total.abs.value <- abs(diff.total.value)
      
      time <- c(time, as.character(timestamp))
      work.period <- c(work.period, work.period.value)
      wattsup.total <- c(wattsup.total, wattsup.total.value)
      joulemeter.total <- c(joulemeter.total, joulemeter.total.value)
      diff.total <- c(diff.total, diff.total.value)
      diff.total.abs <- c(diff.total.abs, diff.total.abs.value)
      seflab.cpu <- c(seflab.cpu, seflab.cpu.value)
      joulemeter.cpu <- c(joulemeter.cpu, joulemeter.cpu.value)
      diff.cpu <- c(diff.cpu, diff.cpu.value)
      diff.cpu.abs <- c(diff.cpu.abs, diff.cpu.abs.value)
      seflab.hdd <- c(seflab.hdd, seflab.hdd.value)
      joulemeter.hdd <- c(joulemeter.hdd, joulemeter.hdd.value)
      diff.hdd <- c(diff.hdd, diff.hdd.value)
      diff.hdd.abs <- c(diff.hdd.abs, diff.hdd.abs.value)
    } 
  }
  
  return(data.frame(TIME = as.POSIXct(time),
                    WORK.PERIOD = work.period,
                    seflab.cpu         = seflab.cpu, 
                    joulemeter.cpu     = joulemeter.cpu,
                    diff.cpu           = diff.cpu,
                    diff.cpu.per       = diff.cpu / seflab.cpu,
                    diff.cpu.abs       = diff.cpu.abs,
                    diff.cpu.abs.per   = diff.cpu.abs / seflab.cpu,
                    seflab.hdd         = seflab.hdd,
                    joulemeter.hdd     = joulemeter.hdd,
                    diff.hdd           = diff.hdd,
                    diff.hdd.per       = diff.hdd / seflab.hdd,
                    diff.hdd.abs       = diff.hdd.abs,
                    diff.hdd.abs.per   = diff.hdd.abs / seflab.hdd,
                    wattsup.total = wattsup.total,
                    joulemeter.total = joulemeter.total,
                    diff.total = diff.total,
                    diff.total.per = diff.total / wattsup.total,
                    diff.total.abs = diff.total.abs,
                    diff.total.abs.per = diff.total.abs / wattsup.total))
}

calculate.statistics <- function(common.data) {
  data <- data.frame(label    = NULL,
      Min      = NULL,
      "1st.Qu" = NULL,
      Median   = NULL,
      Mean     = NULL,
      "3rd.Qu" = NULL,
      Max      = NULL)
  
  stats <- summary(common.data$diff.cpu)
  data <- rbind(data, data.frame(label   = "CPU",
          Min     = stats[["Min."]],
          "1st.Qu" = stats[["1st Qu."]],
          Median   = stats[["Median"]],
          Mean     = stats[["Mean"]],
          "3rd.Qu" = stats[["3rd Qu."]],
          Max      = stats[["Max."]]))
  
  stats <- summary(common.data$diff.hdd)
  data <- rbind(data, data.frame(label    = "HDD",
          Min      = stats[["Min."]],
          "1st.Qu" = stats[["1st Qu."]],
          Median   = stats[["Median"]],
          Mean     = stats[["Mean"]],
          "3rd.Qu" = stats[["3rd Qu."]],
          Max      = stats[["Max."]]))
  
  stats <- summary(common.data$diff.total)
  data <- rbind(data, data.frame(label    = "Total",
          Min      = stats[["Min."]],
          "1st.Qu" = stats[["1st Qu."]],
          Median   = stats[["Median"]],
          Mean     = stats[["Mean"]],
          "3rd.Qu" = stats[["3rd Qu."]],
          Max      = stats[["Max."]]))
  
  return(data)
}

#######################################################################################################
########################    Plot Functions                           ##################################
#######################################################################################################

get.work.period.duration <- function(work.period) {
  true.work.period <- subset(work.period, WORK.PERIOD == TRUE)
  start <- true.work.period[1, "TIME"]
  end   <- true.work.period[nrow(true.work.period), "TIME"]
  return(difftime(end, start))
}

# TODO
# work periods are not always long enough
# there are some flase positives
# filter work periods based on length
split.common.data.in.work.periods <- function(data) {
  periods <- .split.data.by.work.period(data)
  work.periods <- build.work.periods(periods)
  return(work.periods)
}

build.work.periods <- function(periods.list) {
  work.periods <- list()
  work.period.tmp <- NULL
  for(idx in 1:length(periods.list)) {
    period <- periods.list[[idx]]
    if(FALSE %in% period$WORK.PERIOD) {
      period.size <- nrow(period)
      first.part <- NULL
      last.part <- NULL
      if(period.size > 1) {
        half.size <- floor(period.size / 2)
        first.part <- period[1:half.size, ]
        last.part <- period[(half.size + 1):period.size, ]
      } else {
        first.part <- period
        last.part <- period
      }
      if(idx > 1) {
        work.period.tmp <- rbind(work.period.tmp, first.part)
        work.periods[[length(work.periods) + 1]] <- work.period.tmp
      }
      work.period.tmp <- last.part
    }
    if(TRUE %in% period$WORK.PERIOD) {
      work.period.tmp <- rbind(work.period.tmp, period)
    }
  }
  
  return(work.periods)
}

.split.data.by.work.period <- function(data) {
  periods <- list()
  in.work.period <- data$WORK.PERIOD[1]
  period.start <- 1
  data.size <- nrow(data)
  for(idx in 1:data.size) {
    row <- data[idx,]
    if(row$WORK.PERIOD != in.work.period) {
      periods[[length(periods) + 1]] <- data[period.start:(idx - 1), ]
      in.work.period <- !in.work.period
      period.start <- idx
    }
  }
  periods[[length(periods) + 1]] <- data[period.start:data.size,]
  return(periods)
}

plot.common.data <- function(common.data, file=NULL) {
  if(!is.null(file)) {
    pdf(file = file, width = 20, height = 10)
  }
  
  print(plot.compare.total(common.data))
  print(plot.compare.cpu(common.data))
  print(plot.compare.hdd(common.data))
  print(plot.differences(common.data))
  
  if(!is.null(file)) {
    dev.off()
  }
}

plot.differences <- function(common.data) {
  p <- ggplot(data = common.data, aes(x = TIME)) 
  p <- p + geom_line(aes(y = diff.cpu, colour = "CPU")) 
  p <- p + geom_line(aes(y = diff.hdd, colour = "HDD")) 
  p <- p + geom_line(aes(y = diff.total, colour = "Total"))
  p <- p + opts(title = "Power differences")
  p <- p + scale_x_datetime("Time")
  p <- p + scale_y_continuous("Power")
  p <- p + opts(legend.title = theme_blank())
  p <- p + geom_hline(xintercept = 0, colour = "gray50")
  return(p)
}

# plot.compare.total <- function(joulemeter.src, wattsup.src, synch.start.timestamp, synch.stop.timestamp) {
#   joulemeter.data <- getData(joulemeter.src)
#   wattsup.data <- getData(wattsup.src)
#   vline.data <- data.frame(position = c(synch.start.timestamp, synch.stop.timestamp),
#                            label    = c("Start", "Stop"))
#   p <- ggplot(data = joulemeter.data, aes(x = TIME))
#   p <- p + geom_line(data = wattsup.data, aes(y = Total.Power, colour = "WattsUp"))
#   p <- p + geom_line(data = joulemeter.data, aes(y = Total.Power, colour = "Joulemeter"))
#   p <- p + geom_vline(data = vline.data, aes(xintercept = position, colour = label), linetype = "dashed")
#   p <- p + opts(title = "Compare Total Power")
#   p <- p + scale_x_datetime("Time")
#   p <- p + scale_y_continuous("Power")
#   p <- p + opts(legend.title = theme_blank())
#   return(p)
# }

plot.compare.total <- function(common.data) {
  p <- ggplot(data = common.data, aes(x = TIME))
  p <- p + geom_line(aes(y = wattsup.total, colour = "WattsUp"))
  p <- p + geom_line(aes(y = joulemeter.total, colour = "Joulemeter"))
  p <- p + opts(title = "Compare Total Power (common data)")
  p <- p + scale_x_datetime("Time")
  p <- p + scale_y_continuous("Power")
  p <- p + opts(legend.title = theme_blank())
  return(p)
}

# plot.compare.cpu <- function(seflab.src, joulemeter.src) {
#   seflab.power.data <- getPowerData(seflab.src)
#   synch.start.timestamp <- getStartTimestamp(seflab.src)
#   synch.stop.timestamp <- getStopTimestamp(seflab.src)
#   joulemeter.data <- getData(joulemeter.src)
#   vline.data <- data.frame(position = c(synch.start.timestamp, synch.stop.timestamp),
#                            label    = c("Start", "Stop"))
#   p <- ggplot(data = seflab.power.data, aes(x = TIME))
#   p <- p + geom_line(aes(y = CPU1.power + CPU2.power, colour = "SEFLab"))
#   p <- p + geom_line(data = joulemeter.data, aes(y = CPU.Power, colour = "Joulemeter"))
#   p <- p + geom_vline(data = vline.data, aes(xintercept = position, colour = label), linetype = "dashed")
#   p <- p + opts(title = "Compare CPU Power")
#   p <- p + scale_x_datetime("Time")
#   p <- p + scale_y_continuous("Power")
#   p <- p + opts(legend.title = theme_blank())
#   return(p)
# }

plot.compare.cpu <- function(common.data) {
  p <- ggplot(data = common.data, aes(x = TIME))
  p <- p + geom_line(aes(y = seflab.cpu, colour = "SEFLab"))
  p <- p + geom_line(aes(y = joulemeter.cpu, colour = "Joulemeter"))
  p <- p + opts(title = "Compare CPU Power (common data)")
  p <- p + scale_x_datetime("Time")
  p <- p + scale_y_continuous("Power")
  p <- p + opts(legend.title = theme_blank())
  return(p)
}

# plot.compare.hdd <- function(seflab.src, joulemeter.src) {
#   seflab.power.data <- getPowerData(seflab.src)
#   synch.start.timestamp <- getStartTimestamp(seflab.src)
#   synch.stop.timestamp <- getStopTimestamp(seflab.src)
#   joulemeter.data <- getData(joulemeter.src)
#   vline.data <- data.frame(position = c(synch.start.timestamp, synch.stop.timestamp),
#                            label    = c("Start", "Stop"))
#   p <- ggplot(data = seflab.power.data, aes(x = TIME))
#   p <- p + geom_line(aes(y = HDD1.12V.power + HDD1.5V.power + HDD2.12V.power + HDD2.5V.power, colour = "SEFLab"))
#   p <- p + geom_line(data = joulemeter.data, aes(y = Disk.Power, colour = "Joulemeter"))
#   p <- p + geom_vline(data = vline.data, aes(xintercept = position, colour = label), linetype = "dashed")
#   p <- p + opts(title = "Compare HDD Power")
#   p <- p + scale_x_datetime("Time")
#   p <- p + scale_y_continuous("Power")
#   p <- p + opts(legend.title = theme_blank())
#   return(p)
# }

plot.compare.hdd <- function(common.data) {
  p <- ggplot(data = common.data, aes(x = TIME))
  p <- p + geom_line(aes(y = seflab.hdd, colour = "SEFLab"))
  p <- p + geom_line(aes(y = joulemeter.hdd, colour = "Joulemeter"))
  p <- p + opts(title = "Compare HDD Power (common data)")
  p <- p + scale_x_datetime("Time")
  p <- p + scale_y_continuous("Power")
  p <- p + opts(legend.title = theme_blank())
  return(p)
}

get.diff.distribution <- function(work.periods, column) {
  diff <- c()
  for(work.period in work.periods) {
    diff <- c(diff, work.period[[column]])
  }
  return(diff)
}

plot.diff.histogram <- function(diff, title, xlabel, binwidth=5) {
  p <- ggplot()
  p <- p + geom_histogram(data=data.frame(diff=diff), aes(x=diff), binwidth=binwidth)
  p <- p + labs(title=title)
  p <- p + xlab(xlabel)
  return(p)
}

plot.diff.distributions <- function(work.periods, file=NULL) {
  diff.total <- get.diff.distribution(work.periods, "diff.total")
  diff.cpu <- get.diff.distribution(work.periods, "diff.cpu")
  diff.hdd <- get.diff.distribution(work.periods, "diff.hdd")
  
  if(!is.null(file)) {
    pdf(file = file, width = 20, height = 10)
  }
  
  print(plot.diff.histogram(diff.total, "Distribution of total power estimation error", "Total power estimation error (W)", 5))
  print(plot.diff.histogram(diff.cpu, "Distribution of CPU power estimation error", "CPU power estimation error (W)", 5))
  print(plot.diff.histogram(diff.hdd, "Distribution of HDD power estimation error", "HDD power estimation error (W)", 1))
  
  if(!is.null(file)) {
    dev.off()
  }
}

plot.diff.abs.distributions <- function(work.periods, file=NULL) {
  diff.total <- get.diff.distribution(work.periods, "diff.total.abs")
  diff.cpu <- get.diff.distribution(work.periods, "diff.cpu.abs")
  diff.hdd <- get.diff.distribution(work.periods, "diff.hdd.abs")
  
  if(!is.null(file)) {
    pdf(file = file, width = 20, height = 10)
  }
  
  print(plot.diff.histogram(diff.total, "Distribution of total power absolute estimation error", "Total power absolute estimation error (W)", 5))
  print(plot.diff.histogram(diff.cpu, "Distribution of CPU power absolute estimation error", "CPU power absolute estimation error (W)", 5))
  print(plot.diff.histogram(diff.hdd, "Distribution of HDD power absolute estimation error", "HDD power absolute estimation error (W)", 1))
  
  if(!is.null(file)) {
    dev.off()
  }
}

plot.diff.per.distributions <- function(work.periods, file=NULL) {
  diff.total <- get.diff.distribution(work.periods, "diff.total.per") * 100
  diff.cpu <- get.diff.distribution(work.periods, "diff.cpu.per") * 100
  diff.hdd <- get.diff.distribution(work.periods, "diff.hdd.per") * 100
  
  if(!is.null(file)) {
    pdf(file = file, width = 20, height = 10)
  }
  
  print(plot.diff.histogram(diff.total, "Distribution of total power percentual estimation error", "Total power percentual estimation error", 5))
  print(plot.diff.histogram(diff.cpu, "Distribution of CPU power percentual estimation error", "CPU power percentual estimation error", 5))
  print(plot.diff.histogram(diff.hdd, "Distribution of HDD power percentual estimation error", "HDD power percentual estimation error", 1))
  
  if(!is.null(file)) {
    dev.off()
  }
}
#######################################################################################################
########################    Analyses                                 ##################################
#######################################################################################################



percentiles <- function(metric) {
  percentiles <- data.frame(metric = sort(metric))
  percentiles <- cbind(percentiles, data.frame(metric.cum = cumsum(percentiles$metric)))
  max.cum <- max(percentiles$metric.cum)
  percentiles <- cbind(percentiles, data.frame(percentile = percentiles$metric.cum / max.cum) * 100)
  
  return(percentiles)
}

percentiles.all <- function(common.data.idle, common.data.hload) {
  percentiles(c(common.data.idle$diff.cpu.abs, 
                common.data.idle$diff.hdd.abs,
                common.data.idle$diff.total.abs,
                common.data.hload$diff.cpu.abs,
                common.data.hload$diff.hdd.abs,
                common.data.hload$diff.total.abs))
}

percentiles.hdd <- function(common.data.idle, common.data.hload) {
  percentiles(c(common.data.idle$diff.hdd.abs, common.data.hload$diff.hdd.abs))
}

percentiles.cpu <- function(common.data.idle, common.data.hload) {
  percentiles(c(common.data.idle$diff.cpu.abs, common.data.hload$diff.cpu.abs))
}

percentiles.total <- function(common.data.idle, common.data.hload) {
  percentiles(c(common.data.idle$diff.total.abs, common.data.hload$diff.total.abs))
}


profile <- function(metric.values, medium.threshold, high.threshold, very.high.threshold) {
  low <- 0;
  medium <- 0;
  high <- 0;
  very.high <- 0;
  for(value in metric.values) {
    if(value < medium.threshold) {
      low <- low + 1
    } else if(value < high.threshold) {
      medium <- medium + 1
    } else if(value < very.high.threshold) {
      high <- high + 1
    } else {
      very.high <- very.high + 1
    }
  }
  
  return(c(low, medium, high, very.high))
}


consolidate.work.periods <- function(work.periods) {
  time <- c()
  seflab.cpu <- joulemeter.cpu <- diff.cpu <- diff.cpu.per <- diff.cpu.abs <- diff.cpu.abs.per <- c()
  seflab.hdd <- joulemeter.hdd <- diff.hdd <- diff.hdd.per <- diff.hdd.abs <- diff.hdd.abs.per <- c()
  wattsup.total <- joulemeter.total <- diff.total <- diff.total.per <- diff.total.abs <- diff.total.abs.per <- c()
  
  for(work.period in work.periods) {
    time <- c(time, format(work.period$TIME))
    
    seflab.cpu <- c(seflab.cpu, work.period$seflab.cpu)
    joulemeter.cpu <- c(joulemeter.cpu, work.period$joulemeter.cpu)
    diff.cpu <- c(diff.cpu, work.period$diff.cpu)
    diff.cpu.per <- c(diff.cpu.per, work.period$diff.cpu.per)
    diff.cpu.abs <- c(diff.cpu.abs, work.period$diff.cpu.abs)
    diff.cpu.abs.per <- c(diff.cpu.abs.per, work.period$diff.cpu.abs.per)
    
    seflab.hdd <- c(seflab.hdd, work.period$seflab.hdd)
    joulemeter.hdd <- c(joulemeter.hdd, work.period$joulemeter.hdd)
    diff.hdd <- c(diff.hdd, work.period$diff.hdd)
    diff.hdd.per <- c(diff.hdd.per, work.period$diff.hdd.per)
    diff.hdd.abs <- c(diff.hdd.abs, work.period$diff.hdd.abs)
    diff.hdd.abs.per <- c(diff.hdd.abs.per, work.period$diff.hdd.abs.per)
    
    wattsup.total <- c(wattsup.total, work.period$wattsup.total)
    joulemeter.total <- c(joulemeter.total, work.period$joulemeter.total)
    diff.total <- c(diff.total, work.period$diff.total)
    diff.total.per <- c(diff.total.per, work.period$diff.total.per)
    diff.total.abs <- c(diff.total.abs, work.period$diff.total.abs)
    diff.total.abs.per <- c(diff.total.abs.per, work.period$diff.total.abs.per)
  }
  
  return(data.frame(TIME=time,
                    wattsup.total=wattsup.total,
                    joulemeter.total=joulemeter.total,
                    diff.total=diff.total,
                    diff.total.per=diff.total.per,
                    diff.total.abs=diff.total.abs,
                    diff.total.abs.per=diff.total.abs.per,
                    seflab.cpu=seflab.cpu,
                    joulemeter.cpu=joulemeter.cpu,
                    diff.cpu=diff.cpu,
                    diff.cpu.per=diff.cpu.per,
                    diff.cpu.abs=diff.cpu.abs,
                    diff.cpu.abs.per=diff.cpu.abs.per,
                    seflab.hdd=seflab.hdd,
                    joulemeter.hdd=joulemeter.hdd,
                    diff.hdd=diff.hdd,
                    diff.hdd.per=diff.hdd.per,
                    diff.hdd.abs=diff.hdd.abs,
                    diff.hdd.abs.per=diff.hdd.abs.per))
}

filter.work.periods <- function(work.periods, duration.difftime, plots.destination="/tmp", plot.file.prefix="") {
  final.work.periods <- list()
  count <- 0
  for(work.period in work.periods) {
    work.period.duration <- get.work.period.duration(work.period)
    cat("Work period duration is ", capture.output(work.period.duration), "\n")
    if(work.period.duration > duration.difftime) {
      cat("Work period duration is acceptable\n")
      count <- count  + 1
      plot.common.data(work.period, file=paste(plots.destination, "/", plot.file.prefix, "-plot-wp-", count, ".pdf", sep=""))
      final.work.periods[[length(final.work.periods) + 1]] <- work.period
    }
  }
  cat("Detected ", count, " work periods\n")
  
  return(final.work.periods)
}
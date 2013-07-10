source('analyses.R')

test.split.data.by.work.period.single.value.periods <- function() {
  common.data <- data.frame(WORK.PERIOD = c(FALSE, TRUE, FALSE), other.col=NA)
  
  periods <- .split.data.by.work.period(common.data)
  checkEquals(length(periods), 3)
  checkEquals(nrow(periods[[1]]), 1)
  checkEquals(periods[[1]]$WORK.PERIOD, FALSE)
  checkEquals(nrow(periods[[2]]), 1)
  checkEquals(periods[[2]]$WORK.PERIOD, TRUE)
  checkEquals(nrow(periods[[3]]), 1)
  checkEquals(periods[[3]]$WORK.PERIOD, FALSE)
}

test.split.data.by.work.period.multiple.value.periods <- function() {
  common.data <- data.frame(WORK.PERIOD = c(FALSE, FALSE,
                                            TRUE, TRUE, 
                                            FALSE, FALSE), 
                            other.col=NA)
  
  periods <- .split.data.by.work.period(common.data)
  checkEquals(length(periods), 3)
  checkEquals(nrow(periods[[1]]), 2)
  checkEquals(periods[[1]]$WORK.PERIOD, c(FALSE, FALSE))
  checkEquals(nrow(periods[[2]]), 2)
  checkEquals(periods[[2]]$WORK.PERIOD, c(TRUE, TRUE))
  checkEquals(nrow(periods[[3]]), 2)
  checkEquals(periods[[3]]$WORK.PERIOD, c(FALSE, FALSE))
}

test.split.data.by.work.period.multiple.periods <- function() {
  common.data <- data.frame(WORK.PERIOD = c(FALSE, FALSE,
                                            TRUE, TRUE, 
                                            FALSE,
                                            TRUE, TRUE, TRUE ,
                                            FALSE, FALSE,
                                            TRUE),
                            other.col=NA)
  
  periods <- .split.data.by.work.period(common.data)
  checkEquals(length(periods), 6)
  checkEquals(nrow(periods[[1]]), 2)
  checkEquals(periods[[1]]$WORK.PERIOD, c(FALSE, FALSE))
  checkEquals(nrow(periods[[2]]), 2)
  checkEquals(periods[[2]]$WORK.PERIOD, c(TRUE, TRUE))
  checkEquals(nrow(periods[[3]]), 1)
  checkEquals(periods[[3]]$WORK.PERIOD, c(FALSE))
  checkEquals(nrow(periods[[4]]), 3)
  checkEquals(periods[[4]]$WORK.PERIOD, c(TRUE, TRUE, TRUE))
  checkEquals(nrow(periods[[5]]), 2)
  checkEquals(periods[[5]]$WORK.PERIOD, c(FALSE, FALSE))
  checkEquals(nrow(periods[[6]]), 1)
  checkEquals(periods[[6]]$WORK.PERIOD, c(TRUE))
}

test.build.work.periods <- function() {
  periods <- list(data.frame(WORK.PERIOD = c(FALSE, FALSE), other.col=NA),
                  data.frame(WORK.PERIOD = c(TRUE, TRUE), other.col=NA),
                  data.frame(WORK.PERIOD = c(FALSE), other.col=NA),
                  data.frame(WORK.PERIOD = c(TRUE, TRUE, TRUE), other.col=NA),
                  data.frame(WORK.PERIOD = c(FALSE, FALSE), other.col=NA))
                    
  work.periods <- build.work.periods(periods)
  checkEquals(length(work.periods), 2)
  checkEquals(nrow(work.periods[[1]]), 4)
  checkEquals(work.periods[[1]]$WORK.PERIOD, c(FALSE, TRUE, TRUE, FALSE))
  checkEquals(nrow(work.periods[[2]]), 5)
  checkEquals(work.periods[[2]]$WORK.PERIOD, c(FALSE, TRUE, TRUE, TRUE, FALSE))
}
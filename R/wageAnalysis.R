pacman::p_load(
  readr,
  ggplot2,
  ggthemes,
  magrittr)


getMeanWages <- function(region = c("ni", "uk")) {
  wage.data <- read_delim(
    file = getwd() %>% 
      paste0("/data/", region,"-wages-2017-gross-all.csv"), 
    delim = "\t", 
    escape_double = FALSE, 
    trim_ws = TRUE)
  
  anomalies <- c("x", ":", "..")
  for (a in anomalies) wage.data[wage.data == a] <- NA
  
  mean.wages <- data.frame(
    role = wage.data$Role, 
    number = wage.data$Number,
    id = wage.data$id, 
    mean = wage.data$Mean, 
    stringsAsFactors = FALSE)
  
  mean.wages <- mean.wages[complete.cases(mean.wages), ]
  
  mean.wages$mean <- mean.wages$mean %>% 
    gsub(pattern = ",", 
         replacement = "") %>% 
    as.integer()
  
  mean.wages$number <- mean.wages$number %>% 
    as.integer()
  
  mean.wages$yearly <- mean.wages$mean * 52
  
  return(mean.wages)
}


underThreshold <- function(wageData, socLevel, threshold) {
  socThresholds <- getSOCThresholds()
  
  soc.occupations <- wageData %>% 
    subset(wageData$id > socThresholds$min[socLevel] &
             wageData$id < socThresholds$max[socLevel])
  
  soc.occupations$pass <- ifelse(
    test = soc.occupations$yearly >= threshold, 
    yes = 1, 
    no = 0)
  
  passed.occupations <- subset(
    soc.occupations, 
    pass == 1)
  
  return(list(passed = sum(passed.occupations$number), 
           total = sum(soc.occupations$number),
           percentage = sum(passed.occupations$number) / sum(soc.occupations$number)))
}


thresholdSensitivity <- function(wageData, socLevel, tFrom, tTo) {
  thresholdLevel <- pper <- c()
  for (i in seq(tFrom, tTo, -1000)) {
    thresholdLevel %<>% append(i)
    pper %<>% append(underThreshold(
      wageData = wageData, 
      socLevel = socLevel, 
      threshold = i)$percentage)
  }
  df <- data.frame(
    threshold = thresholdLevel,
    percentage = pper,
    stringsAsFactors = FALSE)
  ggplot(data = df, aes(x = percentage, y = threshold)) + 
    geom_line() + 
    geom_point() + 
    scale_color_ptol() +
    theme_minimal() +
    xlab("Percentage of workers") + 
    ylab("Threshold")
}

getSOCThresholds <- function() {
  socThresholds <- data.frame(
    min = c(0, 10, 100, 1000), 
    max = c(10, 100, 1000, 10000),
    stringsAsFactors = FALSE)
  return(socThresholds)
}


qualifyingOccupationsPlot <- function(wageData, region, socLevel, threshold) {
  socThresholds <- getSOCThresholds()
  
  soc.occupations <- wageData %>% 
    subset(wageData$id > socThresholds$min[socLevel] &
             wageData$id < socThresholds$max[socLevel])
  
  soc.occupations$pass <- ifelse(
    test = soc.occupations$yearly >= threshold, 
    yes = 1, 
    no = 0)
  
  percentagePass <- ((sum(soc.occupations$pass) / nrow(soc.occupations)) * 100) %>%
    round(digits = 2)
  
  cutoff <- data.frame(
    x = c(-Inf, Inf), 
    y = threshold, 
    cutoff = factor(threshold))

  cutoffPlot <- ggplot(
    data = soc.occupations, 
    aes(x = role, 
        y = yearly, 
        size = number^2,
        colour = as.factor(pass))) + 
    geom_hline(yintercept = threshold) +
    scale_color_ptol() +
    theme_minimal() +
    geom_point() + 
    xlab("") + 
    ylab("Yearly income (gross)") +
    ggtitle(paste0(sum(soc.occupations$pass), " out of ", 
                   nrow(soc.occupations), " (", percentagePass, "%) of SOC", 
                   socLevel," occupations qualify in ", toupper(region))) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(
        angle = 90, 
        hjust = 1))
  
  return(cutoffPlot)
}


# Perform some main analysis
region <- "uk"
socLevel <- 3
threshold <- 30000

wageData <- region %>% 
  getMeanWages()
underThreshold(
  wageData = wageData, 
  socLevel = socLevel, 
  threshold = threshold)
qualifyingOccupationsPlot(
  wageData = wageData, 
  region = "ni", 
  socLevel = socLevel, 
  threshold = threshold)




library(dplyr)
library(stringr)
library(ggplot2)

storm_data <- read.csv("storm_dataset.csv")
mental_health_data <- read.csv("mental_health_dataset.csv")

filtered_mental_health <- mental_health_data[mental_health_data$Group == 'By State', ]
filtered_storm_dataset <- storm_data[storm_data$BEGIN_YEARMONTH == 201510, ]
df <- full_join(filtered_mental_health, filtered_storm_dataset, by=c('State'='STATE'))

df$IsWaterDisaster <- rep(0, nrow(df))

df$IsWaterDisaster <- ifelse(
  !is.na(df$EVENT_TYPE) & !is.null(df$EVENT_TYPE) & str_detect(df$EVENT_TYPE, "flood", negate = FALSE),
  1,
  ifelse(
    !is.na(df$EVENT_TYPE) & !is.null(df$EVENT_TYPE) & str_detect(df$EVENT_TYPE, "rain", negate = FALSE),
    1,
    2
  )
)

untreated_patients <- 0

for (i in 1:length(df$Indicator)) {
  if (!is.na(df$Indicator[i]) && !is.null(df$Indicator[i]) && str_detect(df$Indicator[i], regex("did not get", ignore_case = TRUE))) {
    untreated_patients <- untreated_patients + 1
  }
}

unique(df$Phase)
df$Phase <- as.numeric(df$Phase)
avg_phase <- mean(df$Phase, na.rm = TRUE)
summary(df$Phase)

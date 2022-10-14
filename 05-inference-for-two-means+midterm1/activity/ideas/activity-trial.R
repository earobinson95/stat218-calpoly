library(datapasta)
library(tidyverse)
library(infer)


# ------------------------------------------------------------------------------
# DIET https://www.sjsu.edu/faculty/gerstman/StatPrimer/paired.pdf -------------
# ------------------------------------------------------------------------------
cholestorol_data <- tibble::tribble(
          ~ ID, ~CORNFLK, ~OATBRAN,
                                    "1", "4.61", "3.84",
                                    "2", "6.42", "5.57",
                                    "3", "5.40", "5.85",
                                    "4", "4.54", "4.80",
                                    "5", "3.98", "3.68",
                                    "6", "3.82", "2.96",
                                    "7", "5.01", "4.41",
                                    "8", "4.34", "3.72",
                                    "9", "3.80", "3.49",
                                   "10", "4.56", "3.84",
                                   "11", "5.35", "5.26",
                                   "12", "3.89", "3.73",
                                   "13", "2.25", "1.84",
                                   "14", "4.24", "4.14"
          )

factorCols <- c("ID")
cholestorol_data[,factorCols] <- lapply(cholestorol_data[,factorCols], as.factor)
numericCols <- c("CORNFLK", "OATBRAN")
cholestorol_data[,numericCols] <- lapply(cholestorol_data[,numericCols], as.numeric)

cholestorol_data <- cholestorol_data |> 
  mutate(CholestorolDiff = CORNFLK - OATBRAN)

# Independent
cholestorol_data_long <- cholestorol_data |> 
  pivot_longer(cols = c(CORNFLK, OATBRAN),
               names_to = "Diet",
               values_to = "Cholesterol")
head(cholestorol_data_long)

cholestorol_data_long %>%
  ggplot(aes(x = Diet, y = Cholesterol)) +
  geom_boxplot() +
  coord_flip()

infer::t_test(cholestorol_data_long, Cholesterol ~ Diet, order = c("CORNFLK", "OATBRAN"))

# Paired
head(data)

cholestorol_data %>%
  ggplot(aes(x = CholestorolDiff)) +
  geom_boxplot() +
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed")

t_test(cholestorol_data, response = CholestorolDiff)


# ------------------------------------------------------------------------------
# BIKES ------------------------------------------------------------------------
# ------------------------------------------------------------------------------

bikes_data <- tibble::tribble(
                ~Triathlete, ~ChainRing, ~Time,
                        "A", "circular",  84.8,
                        "A",     "oval",  84.9,
                        "B", "circular",  84.7,
                        "B",     "oval",  80.1,
                        "C", "circular",  77.2,
                        "C",     "oval",  73.5,
                        "D", "circular",  86.5,
                        "D",     "oval",  83.2,
                        "E", "circular",  81.8,
                        "E",     "oval",  82.9,
                        "F", "circular",  88.3,
                        "F",     "oval",  84.5,
                        "G", "circular",  84.3,
                        "G",     "oval",  81.7,
                        "H", "circular",  95.6,
                        "H",     "oval",  90.2
                )

# Independent
bikes_data %>%
  ggplot(aes(x = ChainRing, y = Time)) +
  geom_boxplot() +
  coord_flip()

t_test(bikes_data, Time ~ ChainRing, order = c("circular", "oval"))

# Paired
bikes_data_wide <- bikes_data |> 
  pivot_wider(id_cols = Triathlete,
              names_from = ChainRing,
              values_from = Time) |> 
  mutate(TimeDiff = oval - circular)

bikes_data_wide %>%
  ggplot(aes(x = TimeDiff)) +
  geom_boxplot() +
  geom_vline(xintercept = 0, color = "blue", linetype = "dashed")

t_test(bikes_data_wide, response = TimeDiff)


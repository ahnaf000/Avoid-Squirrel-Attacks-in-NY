library(tidyverse)
library(knitr)
source("../../scripts/ggprob.R")


squirrels = read_csv("2018_Central_Park_Squirrel_Census_-_Squirrel_Data-2.csv") %>%
  select(Shift, Eating, Foraging) %>%
  drop_na()


s_sum = squirrels %>%
  group_by(Shift) %>%
  summarize(Proportion = sum(Eating | Foraging) / n())

ggplot(s_sum, aes(x = Shift, y = Proportion, fill = Shift)) +
  geom_col(position = position_dodge2(preserve = "single"), width = 0.35) +
  scale_y_continuous(limits = c(0, 1)) +
  ylab("Proportion of Hungry Squirrels") +
  ggtitle("Proportion of Squirrels Eating or Foraging to Shift") +
  theme_minimal()


s_sum = squirrels %>%
  group_by(Shift) %>%
  summarize(Eating = sum(Eating) / n(),
            Foraging = sum(Foraging) / n())

s_sum = pivot_longer(s_sum, cols = c(Eating, Foraging), names_to = "Status", values_to = "Proportion")

ggplot(s_sum, aes(x = Status, y = Proportion, fill = Shift)) +
  geom_col(position = position_dodge2(preserve = "single"), width = 0.35) +
  scale_y_continuous(limits = c(0, 1)) +
  ggtitle("Proportion of Squirrels Eating and Foraging to Shift") +
  theme_minimal()



dspl = function(df) {
  
  for(i in 1:ncol(df)) {
   
     df[1, i] = round(df[1, i], digits = 3)
    
  }
  
  kable(df, format = "markdown", align = "l")
  
}

# Proportion of squirrels eating in the morning.
hungry_am = squirrels %>%
  filter(Shift == "AM") %>%
  summarize((sum(Eating | Foraging)) / n()) %>%
  pull()

# Number of squirrels eating in the morning.
n_hungry_am = squirrels %>%
  filter(Shift == "AM" & (Eating | Foraging)) %>%
  nrow()

dspl(data.frame(`Proportion of Squirrels Hungry Before Noon` = hungry_am, check.names = FALSE))


hungry_pm = squirrels %>%
  filter(Shift == "PM") %>%
  summarize((sum(Eating | Foraging)) / n()) %>%
  pull()

n_hungry_pm = squirrels %>%
  filter(Shift == "PM" & (Eating | Foraging)) %>%
  nrow()

dspl(data.frame(`Proportion of Squirrels Hungry After Noon` = hungry_pm, check.names = FALSE))


# Total proportion of squirrels hungry.
hungry0 = squirrels %>%
  summarize((sum(Eating | Foraging)) / n()) %>%
  pull()

dspl(data.frame(`Total Proportion of Squirrels Hungry` = hungry0, check.names = FALSE))



n = n_hungry_am
rs = hungry_am * n
ls = (1 - hungry_am) * n



n = n_hungry_pm
rs = hungry_pm * n
ls = (1 - hungry_pm) * n



diff = hungry_pm - hungry_am

se = sqrt((hungry0 * (1 - hungry0) / n_hungry_am) +
          (hungry0 * (1 - hungry0) / n_hungry_pm))

z = diff / se

p_value = 2 * pnorm(-abs(z))

dspl(data.frame(Difference = diff,
           `Standard Error` = se,
           `Z-Score` = z,
           `P-value` = p_value,
           check.names = FALSE))

remove(se, diff, p_value)



tst = qnorm(0.025)

gnorm(color = "#555555") +
  geom_norm_fill(a = -4, b = -z, fill = "#f53131") +
  geom_norm_fill(a = -z, b = z, fill = "#8a8cff") +
  geom_norm_fill(a = z, b = 4, fill = "#f53131") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = c(-z, z), linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(-tst, tst), linetype = "dashed", color = "black") +
  xlab("Difference (normalized)") +
  ylab("Probability Density") +
  ggtitle("Observed Difference in Proportions Compared to the Normal Distribution") +
  theme_minimal()


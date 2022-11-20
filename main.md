---
title: "Avoiding Squirrel Attacks"
output: html_document
date: '2022-05-02'
---

#### **Authors:  Ahnaf Kabir, Alex Chen, Haotian Deng, Daniel Gholson.**

## Introduction

Squirrels, one of the most commonly seen wildlife animals, may be more dangerous to humans than at first glance. Anyone who has visited New York knows of the prolific squirrels that can be found roaming the streets, of which the primary species that can be found is the Eastern gray squirrel. Note that Eastern grays come in different colors despite their name, including gray, white, black, and brown. While some may find these squirrels endearing, there is always some level of risk when dealing with wild animals. Squirrels are very skittish creatures and can react in a variety of different ways towards humans: some may run, some may beg for food, and some may even attack. Additionally, squirrels may carry diseases and spread them to humans if they get too close or interact with them. Animals will naturally react aggressively when hungry, and after living amongst humans for so long, may look towards humans for a snack. Going off of the assumption that hungry animals act more violently, we believe that there may be a time period when squirrels are less hungry which may prove to be safer compared to alternatives. We believe that squirrels may be hungrier, and by extension more dangerous, in the evening.

## Background

Over the past couple of years, there have been several instances in which the Eastern gray have attacked humans, especially in New York. One such incident occurred in 2021 in Rego Park, a neighborhood in Queens, New York, where at least three people were attacked by a squirrel, most likely due to being "overly used to being fed by people."^[[1]](#link1)^ Another news article reports attacks in 2019 at Rockefeller Park and notes that city officials warn against feeding squirrels as well as the fact that there has yet to be a diagnosis of rabies in a squirrel, making hunger the primary suspect of these attacks.^[[2]](#link2)^ This being said, however, New York isn't the only place where Eastern gray squirrels are attacking people. The UK has had an ongoing problem of the invasive species from America, and around Christmas of 2021, there was a particularly violent period of squirrel attacks leaving at least 18 people injured within a two-day period.^[[3]](#link3)^

While attacks are usually not commonplace, it is always best to err on the side of caution when it comes to safety, especially when it comes to the physical damage and resulting diseases that may threaten human lives. Squirrels are most prone to attack when hungry and/or used to being fed, and in an area so dense with tourism and thrown out food as New York City, attacks are bound to occur. In order to determine whether Eastern gray squirrels pose the highest potential threat to New York citizens and tourists during the morning or evening, we analyze the behavior of thousands of squirrels and try to find a relationship between the time of day and the hungriness of the squirrels.

### Data and Variables

The Squirrel Census^[[4]](#link4)^ is a group composed of hundreds of volunteers and key NYC entities seeking to learn about the behaviors and life of Eastern gray squirrels in America. While there were three censuses, we use the data from 2018^[[5]](#link5)^ since it is the most complete data source that is publicly available. This data was gathered in October 2018 and was made available on October 15, 2019 on [NYC Open Data](https://data.cityofnewyork.us/Environment/2018-Central-Park-Squirrel-Census-Squirrel-Data/vfnx-vebw). This is just a sample from a larger population, since the sample is only based on squirrels in Central Park, and does not consider every Eastern gray squirrel in some larger population such as the entire country. Thus, any conclusions drawn would be applicable solely for Eastern grays in Central Park and potentially the rest of New York, but any further generalizations may compromise the result. The data is solely observational based on squirrels seen around Central Park and notes 31 different variables for 3,023 rows where each row is a squirrel. For our analysis, we will be using the time of day as well as the recorded behavior of the squirrels, namely whether they were seen eating or foraging for food. In this study, we assume that squirrels, when searching for food or in the process of eating, are hungry. Additionally, we will be disregarding the color variables under the assumption that the squirrels of this species will react similarly regardless of fur color. Unfortunately, the only recorded data in terms of time of day is the shift, or "AM" and "PM", meaning a more in-depth analysis of time will not be possible. We will then use the proportion of squirrels engaging in eating or foraging to determine how potentially agressive they may act towards humans.

In the study, we are analyzing whether there is a time of day to dwell the park that is safer from the dangers of encountering aggressive squirrels than other times of day. We will form 2 hypotheses, a null and an alternative, in order to test this question. The null hypothesis is that the time of day simply does not play an influence in squirrel eating activity, and thus the day and evening proportions will be equal. In the alternative, we propose there is a difference between the two proportions, or in other words, the day and evening proportions are not equal.


#### Key Variables from the Squirrel Census Dataset

Name     | Description
---------|--------------
Shift    | Time of day the sighting occurred (AM or PM)
Eating   | Whether the squirrel was seen eating (True or False)
Foraging | Whether the squirrel was seen foraging for food (True or False)

### Reproducability
Our test does not use random resampling, so the only variation between reproduced trials would result from different source data.

#### Knitting

To successfully knit this file, the following are needed:

Place the `2018_Central_Park_Squirrel_Census_-_Squirrel_Data-2.csv` file and this file in the same directory. 
  

### At First Glance

```{r, include = FALSE}

library(tidyverse)
library(knitr)
source("../../scripts/ggprob.R")

```

Lets first create a basic visualization of the squirrels seen eating or foraging, to see if there is any significant qualities.

```{r, echo = FALSE, message = FALSE}

squirrels = read_csv("2018_Central_Park_Squirrel_Census_-_Squirrel_Data-2.csv") %>%
  select(Shift, Eating, Foraging) %>%
  drop_na()

#s_sum = squirrels %>%
#  group_by(Shift) %>%
#  summarize(Eating = sum(Eating) / n(),
#            Foraging = sum(Foraging) / n())

#s_sum = pivot_longer(s_sum, cols = c(Eating, Foraging), names_to = "Status", values_to = "Proportion")

s_sum = squirrels %>%
  group_by(Shift) %>%
  summarize(Proportion = sum(Eating | Foraging) / n())

ggplot(s_sum, aes(x = Shift, y = Proportion, fill = Shift)) +
  geom_col(position = position_dodge2(preserve = "single"), width = 0.35) +
  scale_y_continuous(limits = c(0, 1)) +
  ylab("Proportion of Hungry Squirrels") +
  ggtitle("Proportion of Squirrels Eating or Foraging to Shift") +
  theme_minimal()

```

A quick eyeball can tell us that there is a slightly higher proportion of squirrels hungry at night. But to reduce variability between parameters we will also take a look at eating and foraging separately.

```{r, echo = FALSE}

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

```

There turns out to be a consistent difference between both eating compared to shift and foraging compared to shift. This gives us hope for an answer, but we need to create a formal test to know for sure.

## Testing the Hypothesis
Our null hypothesis is that there is no difference in squirrel eating activity between the two times of day.
The proportion of squirrels hungry in the morning is denoted as $p_{\text{1}}$ while the proportion of squirrels hungry in the evening is $p_{\text{2}}$.

$H_0: p_{\text{1}} = p_{\text{2}}$  
$H_a: p_{\text{1}} ≠ p_{\text{2}}$

For this we will have to compare both day and night sample proportions to a population proportion $p$. Our significance level ($\alpha$) is set to 0.05 or 5%.

First we will calculate the proportion of squirrels seen eating or foraging according to the total observations for that time of day. We use the boolean "or" operator when counting the squirrels hungry so that squirrels seen both eating and foraging won't be counted twice. Here is our proportion for squirrels hungry in the morning.
```{r, echo = FALSE}

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

```

And here is our proportion for squirrels hungry in the evening.

```{r, echo = FALSE}

hungry_pm = squirrels %>%
  filter(Shift == "PM") %>%
  summarize((sum(Eating | Foraging)) / n()) %>%
  pull()

n_hungry_pm = squirrels %>%
  filter(Shift == "PM" & (Eating | Foraging)) %>%
  nrow()

dspl(data.frame(`Proportion of Squirrels Hungry After Noon` = hungry_pm, check.names = FALSE))

```

In this test, we assume that these two proportions are simply sample variations of some larger population proportion $p$. So we calculate the total proportion of all squirrels seen either foraging or eating which yields us the following.

```{r, echo = FALSE}
# Total proportion of squirrels hungry.
hungry0 = squirrels %>%
  summarize((sum(Eating | Foraging)) / n()) %>%
  pull()

dspl(data.frame(`Total Proportion of Squirrels Hungry` = hungry0, check.names = FALSE))

```

#### Checking for Normal Approximation

Now that we have these we calculate the observed difference between the two proportions. But before we go any further we have to check if this difference closely approximates a normal distribution. In order to do this we must simply verify that the two proportions also closely approximate a normal distribution.

The requirements that a binomial distribution closely approximates a normal distribution are:  

$\text{Sample Size:  }n>30$  
$\text{Skew:  }pn, (1-p)n > 5$  

Where n is the size and p is the probability of success, as well as needing randomness and independence. These requirements ensure that the sample size is large enough to follow a normal shape, and not too heavily skewed.

```{r, include = FALSE}

n = n_hungry_am
rs = hungry_am * n
ls = (1 - hungry_am) * n

```

After running the calculations on the daytime data, the results are:  
$n = 759$  
$pn = 439.02$  
$(1 - p)n = 329.98$  

```{r, include = FALSE}

n = n_hungry_pm
rs = hungry_pm * n
ls = (1 - hungry_pm) * n

```

Doing the same thing with the nighttime binomial gives us the following.  
$n = 1071$  
$pn = 684.39$  
$(1 - p)n = 386.61$  

Finally, we can safely assume that one squirrel does not make another squirrel hungry, leading to independence, as well as any given observation comes from a random squirrel. The requirements are thoroughly satisfied for both binomial models, meaning we can say that the difference between the two proportions closely follow a normal distribution.

#### Continuing the Analysis

Afterwards, we calculate the z-score, which represents how our observed difference compares with the expected difference. We then finally use our z-score to retrieve a p-value for our hypothesis test.

```{r, echo = FALSE}

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

```

We turned these results into a visualization showing our observed z-score with the typical distribution of diffence in proportions. The threshold for statistical significance is shown as a black dashed line for reference, and the red dashed line and filled in section denotes our resultant z-score and p-value respectively.

```{r, echo = FALSE}

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

```

Looking at the distribution, our observed difference is in fact quite far off. With a reported two-side p-value of 0.003 our observation is statistically significant. It is incredibly unlikely that our null hypothesis is correct given this result.

### Conclusion of our study
There is strong evidence (p = 0.003, z-test for difference in proportions) that there is a long run difference between the daytime and evening eating activity. Since the evening period is hungrier to these squirrels, they are more active in the PM period and are more likely to attack later in the day. This may be due to the squirrels needing to replenish their energy after running around, or due to a possibility that people may be feeding squirrels more in the PM which leads to more expectant squirrels, a topic which may warrant further study. Nevertheless, in order to avoid squirrel attack incidents, we suggest people enjoy the park during the morning/early afternoon as opposed to the evening.



#### Limitations and Future Improvements
One of the largest underlying vulnerability with our experiment is that the test operates with the assumption that hungry squirrels will directly correlate with agression towards unsuspecting humans. Although it is a reasonable parameter to consider, there are other factors that could incite aggression including caring for their young and attacks from other animals. Our question would best be answered with a dataset directly containing information about squirrel attacks and the context behind them, such as whether the squirrel was provoked or whether the squirrel had children around.

Since every squirrel observation was made within Central Park, the results we have obtained using the data could be dependent on the local ecosystem. Meaning this representation of squirrels in Central Park may not scale to other locations around the country without issue.

We could not find a dataset with observations on squirrel activity with a better breakdown of the time. We recommend more surveys to be conducted in Central Park to gather comprehensive data with a finer breakdown of the hours in each of AM and PM shifts. This could lead to a more reliable model, which can be used to answer our question of interest with more accuracy. 

Further questions we may study could be analyzing times when people most often feed squirrels, as stated before. Additionally, with a bigger dataset spanning across the entire year rather than just October, we may analyze the impact of seasons on squirrel behavior.

#### So, now what?
The first thing we can do with this new discovery is tell people about it. One method could be to put up boards and signs within the park warning people of increased squirrel activity and potential agression from 12:00 to 24:00. This is so parents know to supervise their children more closely in the afternoon and evening, and can help avoid squirrel attacks on smaller children.

Using our results, we can also bring attention from others able to collect more precise data sets with our recommendations. With more people working together we can create even better guidelines for avoiding squirrel attacks in public parks across the country.

### References

[[1]]{#link1} Milman, O. (2021, January 8). 'It was angry, vicious': Spate of squirrel attacks leave NYC neighborhood in Fear. The Guardian. Retrieved April 3, 2022, from https://www.theguardian.com/world/2021/jan/08/squirrel-attacks-nyc-neighborhood-fear  

[[2]]{#link2} Lampen, C. (2019, August 15). NYC park visitors warned about hangry squirrels out for blood. Gothamist. Retrieved April 2, 2022, from https://gothamist.com/news/nyc-park-visitors-warned-about-hangry-squirrels-out-for-blood 

[[3]]{#link3} **[Warning: Graphic]** Cost, B. (2021, December 29). 'Psycho' squirrel's 48-hour christmas rampage terrorizes town, injures 18. New York Post. Retrieved April 2, 2022, from https://nypost.com/2021/12/29/squirrel-from-hell-injures-18-during-48-hour-biting-spree/

[[4]]{#link4} The Squirrel Census. THE SQUIRREL CENSUS. (2022). Retrieved April 2, 2022, from https://www.thesquirrelcensus.com/

[[5]]{#link5} Data set used from The Squirrel Census:  https://data.cityofnewyork.us/Environment/2018-Central-Park-Squirrel-Census-Squirrel-Data/vfnx-vebw (accessed on April 2, 2022)
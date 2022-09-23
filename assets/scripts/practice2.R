
# all of the packages we will need
pkg <- c(
    "here",
    "tidyverse",
    "afex",
    "lme4",
    "broom"
)
# install all of them
install.packages(pgk)

# load all of the packages for the statistics class
library(here)
library(tidyverse)
library(afex)
library(lme4)
library(broom)

# load the data
d_psp_full <- read_csv(
    here("Desktop", "LaTeX", "statistics-summer-school", "assets", "data", "psp-data.csv")
)
# show the data
d_psp_full

# only select the data that we are interested in currently
d_psp <- d_psp_full %>%
    filter(trigger_cat != "appo", stage != "Children") %>%
    select(id, itemid, trigger_cat, issue, judgment)

# show the data again
d_psp

p_1 <- d_psp %>%
    ggplot(aes(
        x = trigger_cat,
        y = judgment,
        color = issue,
        pch = issue,
        group = issue
    )) +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun = mean, geom = "point") +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
    labs(
        x = "Trigger Type",
        y = "Judgment \u00B1 SE",
        color = "At-Issueness",
        pch = "At-Issueness"
    ) +
    coord_cartesian(ylim = c(1, 5))

p_1

# ANOVA by item
# ANOVA by participants

# for the bored people: more complex ANOVA
# (two or all predictors in the full data set)

d_psp <- d_psp %>%
    filter(itemid != "cleft1")
    
# ANOVA by participants
mod <- aov(judgment ~ issue * trigger_cat + Error(id / issue), data = d_psp)
summary(mod)
# ANOVA by item
mod_1 <- aov(judgment ~ issue + Error(itemid / issue), data = d_psp)
summary(mod_1)




# + Error(id / (issue + trigger_cat))
aov_ez(
    id = "id",
    dv = "judgment",
    within = c("issue", "trigger_cat"),
    data = d_psp
)
# "easy"
aov_ez(
    id = "itemid",
    dv = "judgment",
    within = c("issue"),
    between = c("trigger_cat"),
    data = d_psp
)

# The factor ISSUENESS was significant: F(1,32) = 284.12, p < 0.05.
# The factor TRIGGER was significant: F(1,32) = 48.11, p < 0.05.
# The interaction between ISSUENESS and TRIGGER was significant: F(1,32) = 112.50, p < 0.05.
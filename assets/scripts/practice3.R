

# load all of the packages for the statistics class
library(here)
library(tidyverse)
library(afex)
library(lme4)
library(broom)
library(ggeffects)

# load the data
d_psp_full <- read_csv(
    here("assets", "data", "psp-data.csv")
)
# show the data
d_psp_full

# only select the data that we are interested in currently
d_psp <- d_psp_full %>%
    filter(trigger_cat != "appo", stage != "Children") %>%
    select(id, itemid, trigger_cat, issue, judgment) %>%
    mutate(
        issue = as.factor(issue),
        trigger_cat = as.factor(trigger_cat)
    )


d_psp

# intercept-only model
mod_intercept <- lm(judgment ~ 1, data = d_psp)
summary(mod_intercept)

# one-way linear model: at-issueness
mod_issue <- lm(judgment ~ 1 + issue, data = d_psp)
summary(mod_issue)

# this should reming you of the ANOVA
# and it is a bit of foreshadowing for the mixed model cases
# being significant, the more complex model explains more variation in the data
# (almost trivially so: the intercept explains none of the variation)
# obviously, we could also do this for all of the other models and compare them
# note also that the F-value lines up with the one from the at-issueness model
anova(mod_intercept, mod_issue)

# one-way linear model: trigger type
mod_trigger <- lm(judgment ~ 1 + trigger_cat, data = d_psp)
summary(mod_trigger)

# two-way linear model: trigger type and at-issueness
mod_both <- lm(judgment ~ 1 + trigger_cat + issue, data = d_psp)
summary(mod_both)

# two-way linear model plus interaction
mod_interaction <- lm(judgment ~ 1 + trigger_cat * issue, data = d_psp)
summary(mod_interaction)

# treatment coding: 0, 1
# contrasts(d_psp$issue) <- contr.treatment(2)

# sum coding
# this changes the meanings of the intercepts (and the slopes)
# such that the intercept should represent the grand mean of the model
# there is some slight variation with the raw mean because it is the weighted
# average, rather than just a raw mean
contrasts(d_psp$issue) <- contr.sum(2)
contrasts(d_psp$trigger_cat) <- contr.sum(2)

mod_interaction_sum <- lm(judgment ~ 1 + trigger_cat * issue, data = d_psp)
summary(mod_interaction_sum)

# this is the reason why the intercept in the sum-coded models
# does not line up with the grand mean: unbalanced data
# (because some of the clefts are missing)
d_psp %>%
    group_by(trigger_cat, issue) %>%
    summarise(n = n())






# here are some visualizations for the lines that the above models fitted
# I need to load the data again to recode some variables

d_psp <- read_csv(here("assets", "data", "psp-data.csv")) %>%
    filter(trigger_cat != "appo", stage != "Children") %>%
    mutate(
        issue = if_else(issue == "non-at-issue", 0, 1),
        trigger_cat = if_else(trigger_cat == "hard", 1, 0),
    )

# no interaction
psp_both <- lm(judgment ~ 1 + issue + trigger_cat, data = d_psp)
summary(psp_both)

equation1 <- function(x) {
    coef(psp_both)[2] * x + coef(psp_both)[1]
}
equation2 <- function(x) {
    coef(psp_both)[2] * x + coef(psp_both)[1] + coef(psp_both)[3]
}

d_psp %>%
    mutate(issue = factor(issue)) %>%
    ggplot(aes(
        x = trigger_cat,
        y = judgment,
        pch = issue,
        color = issue
    )) +
    stat_function(
        fun = equation1,
        geom = "line",
        size = 1
    ) +
    stat_function(
        fun = equation2,
        geom = "line",
        size = 1
    ) +
    guides(pch = "none") +
    stat_summary(fun = mean, geom = "point", size = 4) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = .01) +
    scale_x_continuous(breaks = c(0, 1), labels = c("Soft Trigger", "Hard Trigger")) +
    scale_color_discrete(labels = c("non-at-issue", "at-issue")) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
        x = "Trigger Category",
        y = "Judgment \u00B1 SE"
    )

# here's an alternative way of plotting this
mydf_both <- ggpredict(psp_both, terms = c("issue", "trigger_cat"))
plot(mydf_both, add.data = TRUE)


# yes interaction
psp_int <- lm(judgment ~ 1 + issue * trigger_cat, data = d_psp)
summary(psp_int)


d_psp %>%
    mutate(issue = factor(issue)) %>%
    ggplot(aes(
        x = trigger_cat,
        y = judgment,
        pch = issue,
        color = issue
    )) +
    guides(pch = "none") +
    stat_summary(fun = mean, geom = "point", size = 4) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = .01) +
    scale_x_continuous(breaks = c(0, 1), labels = c("Soft Trigger", "Hard Trigger")) +
    scale_color_discrete(labels = c("non-at-issue", "at-issue")) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
        x = "Trigger Category",
        y = "Judgment \u00B1 SE"
    ) +
    geom_smooth(method = "lm", se = FALSE)

mydf_int <- ggpredict(psp_int, terms = c("issue", "trigger_cat"))
plot(mydf_int, add.data = TRUE)

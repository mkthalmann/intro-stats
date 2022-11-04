library(equatiomatic)
library(lme4)
library(tidyverse)
library(here)

# load the data
d <- read_csv(
    here("assets", "data", "psp-data.csv")
) %>%
    filter(stage != "Children")

# to see the compiled LaTeX code, see `model-equations.pdf`

# here's the first model
mod() <- lmer(judgment ~ issue + (1 | id), data = d)
extract_eq(mod)

extract_eq(mod, mean_separate = TRUE)


d <- d %>%
    mutate(
        issue = factor(issue)
    )

contrasts(d$issue) <- contr.sum(2)

mod <- lmer(judgment ~ issue + (1 | id), data = d)
extract_eq(mod, mean_separate = TRUE)


mod_both_intercepts <- lmer(judgment ~ issue + (1 | id) + (1 | item), data = d)
extract_eq(mod_both_intercepts, mean_separate = TRUE)

mod_slope <- lmer(judgment ~ issue + (issue | id), data = d)
extract_eq(mod_slope, mean_separate = TRUE)


mod_both_intercepts <- lmer(judgment ~ issue + (1 | id) + (1 | item), data = d)
extract_eq(mod_both_intercepts, mean_separate = TRUE)

full_model <- lmer(judgment ~ issue + (issue | id) + (issue | item), data = d)
extract_eq(full_model, mean_separate = TRUE)

# load all of the packages for the statistics class
library(here)
library(tidyverse)
library(afex)
library(lme4)
library(broom)

# load the data
d_psp <- read_csv(
    here("assets", "data", "psp-data.csv")
)
# show the data
d_psp




# enable parallel processing
library(parallel)
(nc <- detectCores())
cl <- makeCluster(rep("localhost", nc))

afex_lmm <- mixed(
    judgment ~ stage * issue * trigger_cat + (issue + trigger_cat | id) +
        (issue | itemid),
    data = d_psp,
    method = "LRT",
    expand_re = TRUE,
    REML = FALSE,
    check_contrasts = TRUE,
    control = lmerControl(optCtrl = list(maxfun = 1e6), optimizer = "bobyqa"),
    cl = cl
)

stopCluster(cl)

summary(afex_lmm)

anova(afex_lmm)

# post-hoc tests
library(emmeans)

# create a reference grid with all marginal means
(ref_id <-
    emmeans(
        afex_lmm,
        specs = c("stage", "trigger_cat", "issue"),
        lmer.df = "satterthwaite"
    ))

# all pairwise comparisons
pairs(ref_id, adjust = "holm")


# better
# at-issue: NRRC v hard adults
hard_appo_at_a <- c(-1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# at-issue: NRRC v hard kids
hard_appo_at_c <- c(0, -1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)
contrasts_lmm <- contrast(ref_id,
    list(
        hard_appo_at_a = hard_appo_at_a,
        hard_appo_at_c = hard_appo_at_c
    ),
    adjust = "holm"
)

summary(contrasts_lmm)
confint(contrasts_lmm)


ref_id

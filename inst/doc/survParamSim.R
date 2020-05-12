## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(survival)
library(survParamSim)

set.seed(12345)

## ----prep---------------------------------------------------------------------
# ref for dataset https://vincentarelbundock.github.io/Rdatasets/doc/survival/colon.html
colon2 <- 
  as_tibble(colon) %>% 
  # recurrence only and not including Lev alone arm
  filter(rx != "Lev",
         etype == 1) %>% 
  # Same definition as Lin et al, 1994
  mutate(rx = factor(rx, levels = c("Obs", "Lev+5FU")),
         depth = as.numeric(extent <= 2))

## ----plot_raw_data------------------------------------------------------------
survfit.colon <- survfit(Surv(time, status) ~ rx, data = colon2)
survminer::ggsurvplot(survfit.colon)

survfit.colon.censor <- survfit(Surv(time, 1-status) ~ rx, data = colon2)
survminer::ggsurvplot(survfit.colon.censor)

## ----fit----------------------------------------------------------------------
fit.colon <- survreg(Surv(time, status) ~ rx + node4 + depth, 
                     data = colon2, dist = "lognormal")

summary(fit.colon)

## ----sim----------------------------------------------------------------------
sim <- 
  surv_param_sim(object = fit.colon, newdata = colon2, 
                 # Simulate censoring according to the plot above
                 censor.dur = c(1800, 3000),
                 # Simulate only 100 times to make the example go fast
                 n.rep = 100)

## ----simout-------------------------------------------------------------------
sim

## ----km_pi_calc---------------------------------------------------------------
km.pi <- calc_km_pi(sim, trt = "rx")
km.pi

## ----km_pi_plot---------------------------------------------------------------
km.pi
plot_km_pi(km.pi) +
  theme(legend.position = "bottom") +
  labs(y = "Recurrence free rate") +
  expand_limits(y = 0)

## ----km_pi_table--------------------------------------------------------------
extract_median_surv(km.pi)

## ----km_pi_group--------------------------------------------------------------
km.pi <- calc_km_pi(sim, trt = "rx", group = c("node4", "depth"))

plot_km_pi(km.pi) +
  theme(legend.position = "bottom") +
  labs(y = "Recurrence free rate") +
  expand_limits(y = 0)

## ----hr_pi--------------------------------------------------------------------
hr.pi <- calc_hr_pi(sim, trt = "rx", group = c("depth"))

hr.pi
plot_hr_pi(hr.pi)

## ----hr_pi_table--------------------------------------------------------------
extract_hr_pi(hr.pi)


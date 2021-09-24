
ProjectTemplate::reload.project()

# Checking for non-prop hazards -------------------------------------------

checkassfunc <- function(outtime, outvar, outval) {
  mod <- coxph(formula(paste0("Surv(", outtime, ",", outvar, " == '", outval, "') ~ nocom_cat  + ",
                              paste0(modvars, collapse = "+"))),
    data = edata %>% filter(survpop)
  )

  testpat <- cox.zph(mod)
  print(sig <- testpat$table[testpat$table[, 3] < 0.05, ])
}

checkassfunc(outvar = "out_death", outval = 1, outtime = "outtime_death")
checkassfunc(outvar = "out_hosp", outval = 1, outtime = "outtime_hosp")
checkassfunc(outvar = "out_deathcv", outval = 1, outtime = "outtime_death")
checkassfunc(outvar = "out_hosphf", outval = 1, outtime = "outtime_hosphf")
checkassfunc(outvar = "out_deathhosphf", outval = 1, outtime = "outtime_hosphf")


# Checking proportional odds assumption -----------------------------------

edatanm <- edata %>%
  drop_na(!!!syms(modvars))

pc <- pomcheckr::pomcheck(formula(paste0("num_hsNyha ~ nocom_cat")), edatanm)
plot(pc)

pc <- pomcheckr::pomcheck(formula(paste0("num_dcNyha ~ nocom_cat")), edatanm %>% filter(dischargepop))
plot(pc)

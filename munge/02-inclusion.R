

# Primary criteria --------------------------------------------------------

flow <- c(paste0("Number of patients in ESC "), nrow(esc))

edata <- esc %>%
  filter(num_dmPtype == "Hospital")
flow <- rbind(flow, c("Hospitalized", nrow(edata)))

edata <- edata %>%
  filter(!is.na(num_dcEf) | !is.na(num_dmEflp))
flow <- rbind(flow, c("Non-missing EF (either during hospitalization or last known)", nrow(edata)))

edata <- edata %>%
  filter(!is.na(num_dmDiab_c1) & !is.na(num_dmCopd) & !is.na(num_dmHyChol) &
    !is.na(num_dmHepa) & !is.na(num_dmDis) & !is.na(num_dmApn) & !is.na(num_dmPark) &
    !is.na(num_dmDepr) & !is.na(num_dmStroke) & !is.na(num_dmPvd) & !is.na(num_dmRheu) & !is.na(num_hsHb))
flow <- rbind(flow, c("Non-missing information on number of non-cardiac comorbidities", nrow(edata)))

edata <- edata %>%
  mutate(
    dischargepop = num_dcVital == "Alive" & !is.na(num_dcVital)
  )
flow <- rbind(flow, c(". Discharged alive (discharge population)", nrow(edata %>% filter(dischargepop))))

edata <- edata %>%
  mutate(
    enddtm = coalesce(num_f1DeathDt, num_f1contDt),
    startdtm = coalesce(num_dcDischdt, num_dmVisitdt),
    outtime_death = as.numeric(enddtm - startdtm),
    survpop = num_f1lost == "No" & outtime_death >= 0 & !is.na(outtime_death) & dischargepop
  )
flow <- rbind(flow, c(
  ". Discharged alive, not lost to follow-up and not negative follow-up times (long-term outcome population)",
  nrow(edata %>% filter(survpop))
))

colnames(flow) <- c("Criteria", "N")

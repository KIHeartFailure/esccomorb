edata <- edata %>%
  mutate(
    num_Ef = coalesce(num_dcEf, num_dmEflp),
    num_Ef_cat = factor(case_when(
      is.na(num_Ef) ~ NA_real_,
      num_Ef < 41 ~ 1,
      num_Ef <= 49 ~ 2,
      num_Ef >= 50 ~ 3
    ),
    levels = 1:3, labels = c("<41%", "41-49%", ">=50%")
    ),
    num_delayhosp_cat = factor(case_when(
      is.na(num_delayhosp) ~ NA_real_,
      num_delayhosp < 7 ~ 1,
      num_delayhosp >= 7 ~ 2
    ),
    levels = 1:2,
    labels = c("<7 days", ">=7 days")
    ),
    num_age_cat = case_when(
      num_age < 65 ~ "<65",
      num_age >= 65 ~ ">=65"
    ),
    num_dmSmoking_cat = factor(case_when(
      is.na(num_dmSmoking) ~ NA_real_,
      num_dmSmoking == "Current" ~ 2,
      TRUE ~ 1
    ), levels = 1:2, labels = c("Never/Former", "Current")),
    d_HFdiagnosis = case_when(
      num_dmHF == "No" ~ "<12mo",
      num_dmMonth %in% c("< 6 months", "6 - 12 months") ~ "<12mo",
      num_dmMonth %in% c("> 12 months") ~ ">12mo"
    ),
    num_dmEtio_c1 = relevel(num_dmEtio_c1, ref = "Non-ischemic heart disease"),
    num_dmEtio = factor(case_when(
      num_dmEtio == "Ischemic heart disease documented by coronary angiography" ~ "IHD doc by ca",
      num_dmEtio == "Ischemic heart disease not documented by coronary angiography" ~ "IHD not documented by ca",
      TRUE ~ as.character(num_dmEtio)
    )),
    num_dmThy_cat = case_when(
      num_dmThy == "No" ~ "No",
      num_dmThy %in% c("Hypothyroidism", "Hyperthyroidism") ~ "Yes"
    ),
    anemia = case_when(
      is.na(num_hsHb) | is.na(num_dmgender) ~ NA_character_,
      num_hsHb < 13 & num_dmgender == "Male" ~ "Yes",
      num_hsHb < 12 & num_dmgender == "Female" ~ "Yes",
      TRUE ~ "No"
    ),
    num_dmDev_cat = factor(case_when(
      is.na(num_dmDev) ~ NA_real_,
      num_dmDev %in% c("No") ~ 1,
      num_dmDev %in% c("PM") ~ 2,
      num_dmDev %in% c("CRT-P", "CRT-D", "ICD") ~ 3
    ), levels = 1:3, labels = c("No device", "PM", "CT/ICD")),
    dcsystemic_congestion = case_when(
      is.na(num_dcJvp) | is.na(num_dcHep) | is.na(num_dcOed) | is.na(num_dcS3) ~ NA_character_,
      num_dcJvp == "Yes" | num_dcHep == "Yes" | num_dcOed == "Yes" | num_dcS3 == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    hssystemic_congestion = case_when(
      is.na(num_hsJvp) | is.na(num_hsHep) | is.na(num_hsOed) | is.na(num_hsS3) ~ NA_character_,
      num_hsJvp == "Yes" | num_hsHep == "Yes" | num_hsOed == "Yes" | num_hsS3 == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    # pulm_congestion = if_else(num_dcXrn == "Yes", "No", as.character(num_dcXpu)),
    num_dcNyha_cat = factor(case_when(
      num_dcNyha %in% c("NYHA I", "NYHA II") ~ 1,
      num_dcNyha %in% c("NYHA III", "NYHA IV") ~ 2
    ), levels = 1:2, labels = c("I-II", "III-IV")),
    num_hsNyha_cat = factor(case_when(
      num_hsNyha %in% c("NYHA I", "NYHA II") ~ 1,
      num_hsNyha %in% c("NYHA III", "NYHA IV") ~ 2
    ), levels = 1:2, labels = c("I-II", "III-IV")),
    tmp_dcnyha = case_when(
      num_dcNyha == "NYHA I" ~ 1,
      num_dcNyha == "NYHA II" ~ 2,
      num_dcNyha == "NYHA III" ~ 3,
      num_dcNyha == "NYHA IV" ~ 4
    ),
    tmp_hsnyha = case_when(
      num_hsNyha == "NYHA I" ~ 1,
      num_hsNyha == "NYHA II" ~ 2,
      num_hsNyha == "NYHA III" ~ 3,
      num_hsNyha == "NYHA IV" ~ 4
    ),
    decrease_Nyha = if_else(tmp_dcnyha < tmp_hsnyha, "Yes", "No"),
    change_Bp1 = num_dcBp1 - num_dmBp1,
    changepercent_Bp1 = change_Bp1 / num_dmBp1 * 100,
    change_Bp2 = num_dcBp2 - num_dmBp2,
    changepercent_Bp2 = change_Bp2 / num_dmBp2 * 100,
    change_Bpm = num_dcBpm - num_dmBpm,
    changepercent_Bpm = change_Bpm / num_dmBpm * 100,
    change_Weight = num_dcWeight - num_dmWeight,
    changepercent_Weight = change_Weight / num_dmWeight * 100,
    mdloopDiur_admission = case_when(
      is.na(num_mdDiurp_c2) ~ NA_character_,
      num_mdDiurp_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") |
        num_mdDiur2p_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") ~ "Yes",
      TRUE ~ "No"
    ),
    mdloopDiur_discharge = case_when(
      is.na(num_mdDiurd_c2) ~ NA_character_,
      num_mdDiurd_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") |
        num_mdDiur2d_c2 %in% c("Flurosemide", "Torasemide", "Bumetanide") ~ "Yes",
      TRUE ~ "No"
    ),
    RASi_admission = case_when(
      is.na(num_mdACEp) | is.na(num_mdATp) ~ NA_character_,
      num_mdACEp == "Yes" | num_mdATp == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    RASi_discharge = case_when(
      is.na(num_mdACEd) | is.na(num_mdATd) ~ NA_character_,
      num_mdACEd == "Yes" | num_mdATd == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    tmp_ACEdosep = case_when(
      num_mdACEp_c2 == "Ramipril" ~ num_mdACEpdo / 10,
      num_mdACEp_c2 == "Enalapril" ~ num_mdACEpdo / 20,
      # num_mdACEp_c2 == "Perindopril" ~ num_mdACEpdo / 4,
      num_mdACEp_c2 == "Captopril" ~ num_mdACEpdo / 50,
      num_mdACEp_c2 == "Lisinopril" ~ num_mdACEpdo / 35
      # num_mdACEp_c2 == "Fosinopril" ~ num_mdACEpdo / 20
    ),
    tmp_ACEdosed = case_when(
      num_mdACEd_c2 == "Ramipril" ~ num_mdACEddo / 10,
      num_mdACEd_c2 == "Enalapril" ~ num_mdACEddo / 20,
      # num_mdACEd_c2 == "Perindopril" ~ num_mdACEddo / 4,
      num_mdACEd_c2 == "Captopril" ~ num_mdACEddo / 50,
      num_mdACEd_c2 == "Lisinopril" ~ num_mdACEddo / 35
      # num_mdACEd_c2 == "Fosinopril" ~ num_mdACEddo / 20
    ),
    tmp_ATdosep = case_when(
      num_mdATp_c2 == "Candesartan" ~ num_mdATpdo / 32,
      num_mdATp_c2 == "Losartan" ~ num_mdATpdo / 150,
      num_mdATp_c2 == "Valsartan" ~ num_mdATpdo / 160
    ),
    tmp_ATdosed = case_when(
      num_mdATd_c2 == "Candesartan" ~ num_mdATddo / 32,
      num_mdATd_c2 == "Losartan" ~ num_mdATddo / 150,
      num_mdATd_c2 == "Valsartan" ~ num_mdATddo / 160
    ),
    tmp_rasidosep = pmax(tmp_ACEdosep, tmp_ATdosep, na.rm = T),
    tmp_rasidosed = pmax(tmp_ACEdosed, tmp_ATdosed, na.rm = T),
    RASidosetarget_admission = factor(case_when(
      tmp_rasidosep < 1 / 2 ~ 1,
      tmp_rasidosep < 1 ~ 2,
      tmp_rasidosep >= 1 ~ 3
    ), levels = 1:3, labels = c("<50%", "50-<100%", "100%")),
    RASidosetarget_discharge = factor(case_when(
      tmp_rasidosed < 1 / 2 ~ 1,
      tmp_rasidosed < 1 ~ 2,
      tmp_rasidosed >= 1 ~ 3
    ), levels = 1:3, labels = c("<50%", "50-<100%", "100%")),
    tmp_BBdosep = case_when(
      num_mdBBp_c2 == "Bisoprolol" ~ num_mdBBpdo / 10,
      num_mdBBp_c2 == "Metoprolol" ~ num_mdBBpdo / 200,
      num_mdBBp_c2 == "Nebivolol" ~ num_mdBBpdo / 10,
      num_mdBBp_c2 == "Carvedilol" ~ num_mdBBpdo / 25
    ),
    BBdosetarget_admission = factor(case_when(
      tmp_BBdosep < 1 / 2 ~ 1,
      tmp_BBdosep < 1 ~ 2,
      tmp_BBdosep >= 1 ~ 3
    ), levels = 1:3, labels = c("<50%", "50-<100%", "100%")),
    tmp_BBdosed = case_when(
      num_mdBBd_c2 == "Bisoprolol" ~ num_mdBBddo / 10,
      num_mdBBd_c2 == "Metoprolol" ~ num_mdBBddo / 200,
      num_mdBBd_c2 == "Nebivolol" ~ num_mdBBddo / 10,
      num_mdBBd_c2 == "Carvedilol" ~ num_mdBBddo / 25
    ),
    BBdosetarget_discharge = factor(case_when(
      tmp_BBdosed < 1 / 2 ~ 1,
      tmp_BBdosed < 1 ~ 2,
      tmp_BBdosed >= 1 ~ 3
    ), levels = 1:3, labels = c("<50%", "50-<100%", "100%")),
    ALdosetarget_admission = factor(case_when(
      num_mdALpdo < 50 / 2 ~ 1,
      num_mdALpdo < 50 ~ 2,
      num_mdALpdo >= 50 ~ 3
    ), levels = 1:3, labels = c("<50%", "50-<100%", "100%")),
    ALdosetarget_discharge = factor(case_when(
      num_mdALddo < 50 / 2 ~ 1,
      num_mdALddo < 50 ~ 2,
      num_mdALddo >= 50 ~ 3
    ), levels = 1:3, labels = c("<50%", "50-<100%", "100%")),

    # Outcomes
    out_death_inhosp = case_when(
      num_dcVital == "Dead" ~ "Yes",
      num_dcVital == "Alive" ~ "No"
    ),
    enddtm = coalesce(num_f1DeathDt, num_f1contDt),
    startdtm = coalesce(num_dcDischdt, num_dmVisitdt),
    outtime_death = as.numeric(enddtm - startdtm),
    out_death = case_when(
      num_f1vital == "Alive" ~ 0,
      num_f1vital == "Dead" ~ 1
    ),
    out_deathcv = case_when(
      is.na(out_death) ~ NA_real_,
      num_f1DeathCs %in% c("Cardiac", "Vascular") ~ 1,
      TRUE ~ 0
    ), # pats with missing info are NOT included in CV

    # All-cause hosp
    out_hosp = case_when(
      num_f1lost != "No" ~ NA_real_,
      num_f1hosp1 == "Yes" |
        num_f1hosp2 == "Yes" |
        num_f1hosp3 == "Yes" |
        num_f1hosp4 == "Yes" |
        num_f1hosp5 == "Yes" ~ 1,
      TRUE ~ 0
    ),
    out_hospdtm = coalesce(
      num_f1hosp1dt, num_f1hosp2dt, num_f1hosp3dt,
      num_f1hosp4dt, num_f1hosp5dt
    ),
    outtime_hosp = as.numeric(out_hospdtm - startdtm),
    outtime_hospmissing = case_when(
      out_hosp == 1 & is.na(outtime_hosp) ~ 1,
      out_hosp == 1 ~ 0
    ),
    outtime_hosp = ifelse(out_hosp == 1 & is.na(outtime_hosp), outtime_death / 2, outtime_hosp),
    outtime_hosp = pmin(outtime_hosp, outtime_death, na.rm = TRUE),

    # HF hosp
    out_hosphf = case_when(
      num_f1lost != "No" ~ NA_real_,
      num_f1hosp1cs == "HF" |
        num_f1hosp2cs == "HF" |
        num_f1hosp3cs == "HF" |
        num_f1hosp4cs == "HF" |
        num_f1hosp5cs == "HF" ~ 1,
      TRUE ~ 0
    ),
    out_hosphfdtm = case_when(
      num_f1hosp1cs == "HF" ~ num_f1hosp1dt,
      num_f1hosp2cs == "HF" ~ num_f1hosp2dt,
      num_f1hosp3cs == "HF" ~ num_f1hosp3dt,
      num_f1hosp4cs == "HF" ~ num_f1hosp4dt,
      num_f1hosp5cs == "HF" ~ num_f1hosp5dt
    ),
    outtime_hosphf = as.numeric(out_hosphfdtm - startdtm),
    outtime_hosphf = ifelse(out_hosphf == 1 & is.na(outtime_hosphf), outtime_death / 2, outtime_hosphf),
    outtime_hosphf = pmin(outtime_hosphf, outtime_death, na.rm = TRUE),

    # death or hf hosp
    out_deathhosphf = ifelse(out_hosphf == 1, 1, out_death),

    # competing risk
    out_hosp_cr = create_crevent(out_hosp, out_death, c(1, 1)),
    out_hosphf_cr = create_crevent(out_hosphf, out_death, c(1, 1)),
    out_deathcv_cr = create_crevent(out_deathcv, out_death, c(1, 1))
  ) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(num_nation = as.character(num_nation)) %>%
  select(-starts_with("tmp_"))

edata <- edata %>%
  mutate(
    nocom = rowSums(select(., all_of(comorbs)) == "Yes"),
    nocom_cat = factor(case_when(
      is.na(nocom) ~ NA_character_,
      nocom == 0 ~ "0",
      nocom == 1 ~ "1",
      nocom == 2 ~ "2",
      nocom == 3 ~ "3",
      nocom >= 4 ~ "4+",
    ))
  )


# Create variables for comp risk model ------------------------------------

edata <- create_crvar(edata, "nocom_cat")

for (i in seq_along(modvars)) {
  edata <- create_crvar(edata, modvars[i])
}

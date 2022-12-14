tabvars <- c(
  "num_region",
  # Clinical characteristics
  "num_age",
  "num_age_cat",
  "num_dmgender",
  "num_dmSmoking_cat",
  "d_HFdiagnosis",
  "num_hsHFstat",
  "num_dmEtio",
  "num_dmEtio_c1",
  "num_dmHT",
  "num_dmMi",
  "num_dmPci",
  "num_dmCabg",
  "num_dmValve",
  "num_dmStroke",
  "num_dmAfib",
  "num_dmAfib_c1",
  "num_dmPvd",
  "num_dmVte",
  "num_dmDiab_c1",
  "num_dmHyChol",
  "num_dmCopd",
  "num_dmApn",
  "num_dmRheu",
  "num_dmHepa",
  "num_dmThy_cat",
  "num_dmDis",
  "num_dmDepr",
  "num_dmPark",
  "anemia",
  "num_dmDev_cat",
  "nocom_cat",
  "nocom",
  "num_dmBp1",
  "num_dcBp1",
  "change_Bp1",
  "changepercent_Bp1",
  "num_dmBp2",
  "num_dcBp2",
  "change_Bp2",
  "changepercent_Bp2",
  "num_dmBpm",
  "num_dcBpm",
  "change_Bpm",
  "changepercent_Bpm",
  "num_dmWeight",
  "num_dcWeight",
  "change_Weight",
  "changepercent_Weight",
  "num_hsRal",
  "num_dcRal",
  "hssystemic_congestion",
  "dcsystemic_congestion",
  "num_hsHosPresCli",

  # ECG
  "num_dcRyth_c1",
  "num_dcQrsD",
  "num_dcQt",
  "num_dcLbbb",
  "num_dcLvh2",

  # Echo
  "num_Ef",
  "num_Ef_cat",
  "num_dcEf",
  "num_dmEflp",
  "num_dcLvdd",
  "num_dcLvh",
  "num_dcLadim",
  "num_dcRsp",
  "num_dcMitReg",
  "num_dcAorSte",
  "num_dcAorReg",
  "num_dcTriCus",

  # chemistry
  "num_hsNt",
  "num_dcNt",
  "num_hsBnp",
  "num_dcBnp",
  "num_hsSod",
  "num_dcSod",
  "num_hsPot",
  "num_dcPot",
  "num_hsCre",
  "num_dcCre",
  "num_hsHb",
  "num_dcHb",
  "num_hsBun",
  "num_dcBun",
  "num_hsUre",
  "num_dcUre",
  "num_hsBili",
  "num_dcBili",
  "num_hsCrp",
  "num_dcCrp",
  "num_hsWbc",
  "num_dcWbc",
  "num_hsGl",
  "num_dcGl",

  # meds
  "num_mdBBp",
  "BBdosetarget_admission",
  "num_mdBBd",
  "BBdosetarget_discharge",
  "RASi_admission",
  "RASidosetarget_admission",
  "RASi_discharge",
  "RASidosetarget_discharge",
  "num_mdARNIp",
  "num_mdARNId",
  "num_mdALp",
  "ALdosetarget_admission",
  "num_mdALd",
  "ALdosetarget_discharge",
  "mdloopDiur_admission",
  "mdloopDiur_discharge",
  "num_mdDigop",
  "num_mdDigod",
  "num_mdACp",
  "num_mdACd",
  "num_mdIvabp",
  "num_mdIvabd",
  "num_hsIntr",
  "num_hsProDiu",
  "num_hsNitrat",
  "num_dcCora",
  "num_dcPci",
  "num_dcEps",
  "num_dcTransab",
  "num_dcCrt",
  "num_dcIcd",
  "num_dcRhCath",
  "num_dcIapb"
)

medvars <- c(
  "num_mdBBp",
  "BBdosetarget_admission",
  "num_mdBBd",
  "BBdosetarget_discharge",
  "RASi_admission",
  "RASidosetarget_admission",
  "RASi_discharge",
  "RASidosetarget_discharge",
  "num_mdARNIp",
  "num_mdARNId",
  "num_mdALp",
  "ALdosetarget_admission",
  "num_mdALd",
  "ALdosetarget_discharge",
  "mdloopDiur_admission",
  "mdloopDiur_discharge",
  "num_mdDigop",
  "num_mdDigod",
  "num_mdACp",
  "num_mdACd",
  "num_mdIvabp",
  "num_mdIvabd"
)

modvars <- c(
  "num_age",
  "num_dmgender",
  "num_dmEtio_c1",
  "num_dmBp1",
  "num_dmMi",
  "num_dmAfib_c1"
)

nsvars <- c("num_age", "num_dmBp1")
modvarsns <- if_else(modvars %in% nsvars, paste0("ns(", modvars, ", df = 4)"), modvars)


comorbs <- c(
  "num_dmDiab_c1",
  "num_dmCopd",
  "num_dmHyChol",
  "num_dmHepa",
  "num_dmDis",
  "num_dmApn",
  "num_dmPark",
  "num_dmDepr",
  "num_dmStroke",
  "num_dmPvd",
  "num_dmRheu",
  "anemia"
)

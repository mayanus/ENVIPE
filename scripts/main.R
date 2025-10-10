options(survey.lonely.psu = "adjust")
library(dplyr)
library(srvyr)

pervic2 <- data.table::fread("data/envipe_2025_csv/TPer_Vic2.csv")
modvic <- data.table::fread("data/envipe_2025_csv/TMod_Vic.csv")

# En 2024, 29.0% de los hogares en México tuvo al menos una
# o un integrante víctima de delito Tabla 7.1
pervic2 |> 
  mutate(
    POB = if_else(!FAC_HOG %in% 0, 1, 0),
    VIC = if_else((AP6_4_01 == 1 & AP6_5_01 == 1) |
                         (AP6_4_02 == 1 & AP6_5_02 == 1) |
                         (AP6_4_04 == 1) | AP6_7 == 1 , 1, 0)) |> 
  as_survey_design(ids = UPM, strata = EST_DIS, weights = FAC_HOG) |> 
  summarise(survey_ratio(VIC, POB)) |> 
  mutate(across(everything(), ~.x * 100))


# 23.1 millones de personas de 18 años y más fueron víctimas
# de algún delito Tabla 1.16 
pervic2 |> 
  filter(ID_PER %in% modvic[!modvic$BPCOD %in% 3,]$ID_PER) |> 
  as_survey_design(ids = UPM, strata = EST_DIS, weights = FAC_ELE) |> 
  summarise(survey_total())


# La tasa de prevalencia delictiva fue de 24 135 víctimas por
# cada 100 mil habitantes Tabla 1.1 
pervic2 |> 
  mutate(
    POB = if_else(!FAC_ELE %in% 0, 1, 0),
    VIC = if_else(ID_PER %in% modvic[!modvic$BPCOD %in% 3,]$ID_PER, 1, 0)
  ) |> 
  as_survey_design(ids = UPM, strata = EST_DIS, weights = FAC_ELE) |> 
  summarise(survey_ratio(VIC, POB)) |> 
  mutate(across(everything(), ~.x * 100000))

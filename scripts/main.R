options(survey.lonely.psu = "adjust")
library(dplyr)
library(srvyr)
library(data.table)

pervic2 <- fread("data/envipe_2025_csv/TPer_Vic2.csv")
modvic <- fread("data/envipe_2025_csv/TMod_Vic.csv")

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

# Los delitos más frecuentes fueron fraude, robo o asalto
# en calle o transporte público y extorsión Tabla 1.13

tasa_inci <- function(delito) {
  modvic |>
    filter(BPCOD %in% delitos[[delito]]) |>
    mutate(FAC_DEL00 = if_else(!BPCOD %in% 3, FAC_DEL,0)) |>
    _[, .(TDEO00 = sum(FAC_DEL00)), by = .(ID_PER)] |>
    right_join(pervic2, by = "ID_PER") |>
    tidyr::replace_na(list(TDEO00 = 0)) |>
    as_survey_design(ids = UPM_DIS, strata = EST_DIS) |>
    summarise(survey_ratio(TDEO00, FAC_ELE)) |>
    mutate(across(everything(), ~.x * 100000), delito = delito)
}

delitos <- list(
  fraude = c(7,8),
  asalto_transp  = 5,
  extorsion = 9,
  amenazas = 10,
  robo = c(1,2)
)

purrr::map(c("fraude", "asalto_transp", "extorsion", "amenazas", "robo"), tasa_inci) |>
  purrr::list_rbind() |>
  arrange(desc(coef))

# 93.2% de los 33.5 millones de delitos que ocurrieron no se denunció o
# la autoridad no inició una carpeta de investigación.
# Este subregistro se conoce como cifra oculta Tabla 3.3

modvic |>
  mutate(
    DO = if_else(!BPCOD %in% 3, 1, 0),
    DD = if_else(!BPCOD %in% 3 & (BP1_20 %in% 1 | BP1_21 %in% 1), 1, 0),
    DND = if_else(!BPCOD %in% 3 & (!BP1_20 %in% 1 & !BP1_21 %in% c(1,9)), 1, 0),
    DSAP = if_else((DD %in% 1) & (BP1_24 %in% 2 | BP1_24 %in% 9), 1, 0),
    DNE = if_else((DD %in% 1) & (BP1_22 %in% 2 | BP1_24 %in% 9), 1, 0),
    NE = if_else(!BPCOD %in% 3 & BP1_21 %in% 9, 1, 0),
    CO = if_else(DND %in% 1 | DSAP %in% 1 | DNE %in% 1 | NE %in% 1, 1, 0)
  ) |>
  as_survey_design(ids = UPM, strata = EST_DIS, weights = FAC_DEL) |>
  summarise(delitos = survey_total(DO), cifra_oculta = survey_ratio(CO,DO))

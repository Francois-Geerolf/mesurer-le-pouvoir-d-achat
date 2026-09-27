library(tidyverse)
library(readxl)

## Données: https://www.insee.fr/fr/statistiques/2830244#%20tableau-figure1_radio1
# https://www.insee.fr/fr/statistiques/fichier/2830244/econ-gen-revenu-dispo-pouv-achat-2.xlsx

version <- 2830244

temp <- tempfile()
download.file(paste0("https://www.insee.fr/fr/statistiques/fichier/", version,
                     "/econ-gen-revenu-dispo-pouv-achat-2.xlsx"), temp)

pouvoir_achat_annuel <- read_xlsx(temp, sheet = 1, skip = 3) |>
  gather(year, value, -Indicateur) |>
  mutate(year = year |> as.numeric(),
         value = value/100) |>
  filter(year >= 2021,
         Indicateur %in% c("Pouvoir d'achat du revenu disponible brut des ménages1",
                           "Pouvoir d'achat par unité de consommation")) |>
  spread(year, value)

# https://www.insee.fr/fr/statistiques/fichier/2830166/reve-niv-vie-pouv-achat-trim.xlsx

version <- 2830166

temp <- tempfile()
download.file(paste0("https://www.insee.fr/fr/statistiques/fichier/", version,
                     "/reve-niv-vie-pouv-achat-trim.xlsx"), temp)

pouvoir_achat_2024 <- read_xlsx(temp, sheet = 1, skip = 3) |>
  head(1) |>
  gather(Indicateur, value, -Trimestre) |>
  filter(Indicateur %in% c("Pouvoir d'achat du RDB",
                           "Pouvoir d'achat du RDB par UC")) |>
  mutate(value = value |> as.numeric())



table1 <- pouvoir_achat_annuel |>
  mutate(`2024 (acquis)` = c(0.014, 0.011),
         Indicateur = c("... pouvoir d'achat", "... pouvoir d'achat par unité de consommation")) |>
  rename(`Évolution du...` = Indicateur) |>
  gt::gt() |>
  gt::fmt_percent(
    columns = 2:5,
    decimals = 1,
    force_sign = T
  ) |>
  gt::cols_align(
    align = "center",
    columns = everything()
  ) |>
  gt::tab_footnote("Source: Insee, comptes nationaux en base 2014")

table1  |>
  gt::gtsave(filename = "table1.png")

table1  |>
  gt::gtsave(filename = "table1.pdf")

system("pdfcrop table1.pdf table1.pdf")


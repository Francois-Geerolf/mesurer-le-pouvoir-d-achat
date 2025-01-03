library(tidyverse)
library(readxl)

# IPC et IPCH --------
# 001764888: Indice annuel des prix à la consommation - Base 2015 - Ensemble des ménages - France - Nomenclature Coicop : 08 - Communications
# 001763313: Indice des prix à la consommation harmonisé annuel - Base 2015 - Ensemble des ménages - France - Nomenclature Coicop : 08 - Communications

figure2_idbanks <- c("001764888", "001763313")

IPC_IPCH <- paste(figure2_idbanks, collapse = "+") |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", i = _) |>
  rsdmx::readSDMX() |>
  tibble::as_tibble() |>
  transmute(date = as.Date(paste0(TIME_PERIOD, "-01-01")),
            OBS_VALUE = as.numeric(OBS_VALUE),
            variable = case_when(grepl("harmonisé", TITLE_FR) ~ "Indice des Prix à la Consommation Harmonisé (IPCH)",
                                 T ~ "Indice des Prix à la Consommation (IPC)"))

# Déflateur de la consommation -------

temp <- tempfile()

curl::curl_download("https://www.insee.fr/fr/statistiques/fichier/8068592/T_CONSO_EFF_FONCTION.xlsx",
                    destfile = temp)

deflateur <- read_excel(temp, skip = 3, sheet = "IPRIX2020") %>%
  rename(fonction = ...1, variable = ...2) %>%
  gather(year, OBS_VALUE, -fonction, -variable) %>%
  filter(!is.na(OBS_VALUE),
         fonction %in% c("CP08")) %>%
  transmute(date = as.Date(paste0(year, "-01-01")),
            OBS_VALUE,
            variable = "Déflateur de la Consommation")


# Figure 4 ----------

figure2 <- deflateur %>%
  bind_rows(IPC_IPCH) %>%
  filter(date >= as.Date("1996-01-01")) %>%
  group_by(variable) %>%
  arrange(date) %>%
  mutate(OBS_VALUE = 100*OBS_VALUE/OBS_VALUE[1])

figure2 %>%
  ggplot() + ylab("") + xlab("") + theme_minimal() +
  geom_line(aes(x = date, y = OBS_VALUE, color = variable)) +
  
  scale_x_date(breaks = seq(1996, 2100, 2) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  theme(legend.position = c(0.3, 0.3),
        legend.title = element_blank()) +
  scale_y_log10(breaks = seq(0, 500, 10),
                labels = dollar_format(accuracy = 1, prefix = "")) +
  labs(caption = "Source: Insee, calculs de l'auteur")

ggsave("figure2.png", width = 1.25*6, height = 1.25*3.375, bg = "white", dpi = 150)
ggsave("figure2.pdf", width = 1.25*6, height = 1.25*3.375, dpi = 150)


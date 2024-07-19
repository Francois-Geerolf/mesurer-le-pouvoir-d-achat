library(tidyverse)
library(readxl)

# IPC et IPCH --------

figure1_idbanks <- c("001764363", "001762489")

IPC_IPCH <- paste(figure1_idbanks, collapse = "+") |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", i = _) |>
  rsdmx::readSDMX() |>
  tibble::as_tibble() |>
  transmute(date = as.Date(paste0(TIME_PERIOD, "-01-01")),
            OBS_VALUE = as.numeric(OBS_VALUE),
            variable = case_when(grepl("harmonisé", TITLE_FR) ~ "IPCH",
                                        T ~ "IPC"))

# Déflateur de la consommation -------

temp <- tempfile()

curl::curl_download("https://www.insee.fr/fr/statistiques/fichier/8068592/T_CONSO_EFF_FONCTION.xlsx",
              destfile = temp)

deflateur <- read_excel(temp, skip = 3, sheet = "IPRIX2020") %>%
  rename(fonction = ...1, variable = ...2) %>%
  gather(year, OBS_VALUE, -fonction, -variable) %>%
  filter(!is.na(OBS_VALUE),
         variable %in% c("Dépense de consommation des ménages")) %>%
  transmute(date = as.Date(paste0(year, "-01-01")),
            OBS_VALUE,
            variable)

# Figure 1 ----------

figure1 <- deflateur %>%
  bind_rows(IPC_IPCH) %>%
  filter(date >= as.Date("1996-01-01")) %>%
  group_by(variable) %>%
  arrange(date) %>%
  mutate(OBS_VALUE = 100*OBS_VALUE/OBS_VALUE[1])

figure1 %>%
  ggplot() + ylab("") + xlab("") + theme_minimal() +
  geom_line(aes(x = date, y = OBS_VALUE, color = variable)) +
  scale_x_date(breaks = seq(1996, 2100, 4) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  theme(legend.position = c(0.25, 0.85),
        legend.title = element_blank()) +
  scale_y_log10(breaks = seq(0, 500, 5),
                labels = dollar_format(accuracy = 1, prefix = ""))

ggsave("figure1.png", width = 1.25*6, height = 1.25*3.375)
ggsave("figure1.pdf", width = 1.25*6, height = 1.25*3.375)

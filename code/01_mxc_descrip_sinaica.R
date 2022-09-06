# ============================================================================ #
# Proyecto: Proyecto Frontera 2025
# Generar descriptivo de las estaciones de monitoreo de SINAICA
# Versión modificada por Dalia y Tex
# Version 2022-09-06
# ============================================================================ #

### Cargo paquetes requeridos para el análisis
library(tidyverse)
library(janitor)
library(openair)
library(lubridate)
library(naniar)
library(scales)


### Importación de datos  ======================================================
# Importo los datos del 2018
mxc2018 <- 
  read_csv("./data/MXC-PM25-2018.csv", skip = 2) %>% 
  clean_names() %>% 
  print()

# Revisamos rápidamente las descriptivas de la malla
summary(mxc2018)

# Importo los datos del 2019
mxc2019 <- 
  read_csv("./data/MXC-PM25-2019.csv", skip = 2) %>% 
  clean_names() %>% 
  print()

# Revisamos rápidamente las descriptivas de la malla
summary(mxc2019)


### Genero de malla de análisis  ===============================================
# veo los datos de ambas mallas
mxc2018
unique(mxc2018$hora) # no existe hora 24 pero es mejor este formato

mxc2019
unique(mxc2019$hora) # no existe hora 24 pero es mejor este formato

# malla en wide
mxc_w <- 
  bind_rows(mxc2018, mxc2019) %>% 
  mutate(year = year(fecha), 
         mes = month(fecha), 
         dia = day(fecha), 
         hora23 = hora -1, # horas del día de 0 a 23
         .after = fecha) %>% 
  rename(hora24 = hora) %>% # horas del día de 1 a 24
  filter(!(dia == 1 & mes == 1 | 
             dia == 1 & mes == 5 | 
             dia == 16 & mes == 9 | 
             dia == 2 & mes == 11 | 
             dia == 12 & mes == 12 |
             dia == 25 & mes == 12)) %>% 
  print()

mxc_w %>% 
  select(fecha, hora24, cob, uabc) %>% 
  pivot_longer(cols = -c(fecha, hora24), 
               names_to = "site", 
               values_to = "pm25") %>% 
  ggplot(aes(x = factor(hora24), y = pm25)) + 
  geom_boxplot(outlier.size = 0.3) + 
  facet_grid(rows = vars(site)) + 
  labs(x = "Hora", 
       y = expression(paste(PM[2.5], '  (', mu, '/', m^3, ')', sep = ''))) + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 5)), 
        strip.text = element_text(size = 14))

fechas <- 
  tibble(date = seq(from = as.POSIXct("2018-01-01 0:00"), 
                    to = as.POSIXct("2019-12-31 23:00"), 
                    by = "hour")) %>% 
  mutate(year = year(date), 
         mes = month(date), 
         dia = day(date), 
         hora23 = hour(date), 
         .after = date) %>% 
  filter(!(dia == 1 & mes == 1 | 
             dia == 1 & mes == 5 | 
             dia == 16 & mes == 9 | 
             dia == 2 & mes == 11 | 
             dia == 12 & mes == 12 |
             dia == 25 & mes == 12)) %>% 
  print()

mxc_w <- 
  left_join(fechas, mxc_w, by = c("year", "mes", "dia", "hora23")) %>% 
  select(-c(fecha)) %>%
  print()

rm(fechas)

mxc_w %>% select(date, cob, uabc) %>% summary()

mxc_w %>% 
  select(date, cob, uabc) %>% 
  gg_miss_var(show_pct = TRUE) + 
  labs(y = "% porcentaje de datos faltantes") + 
  theme_light() + 
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 5)))

mxc_w %>% 
  select(date, year, cob, uabc) %>% 
  gg_miss_var(show_pct = TRUE, facet = year) + 
  labs(y = "% porcentaje de datos faltantes") + 
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 5)), 
        strip.text = element_text(size = 14))

# malla en long
mxc_l <- 
  mxc_w %>% 
  pivot_longer(cols = -c(date:hora24), 
               names_to = "site", 
               values_to = "pm25") %>% 
  print()

# rm(mxc2018, mxc2019)

timePlot(mxc_l, 
         pollutant = "pm25", 
         type = "site")

ggplot(data = mxc_l, aes(x = date, y = pm25)) + 
  geom_line(colour = "dodgerblue4", size = 0.3) + 
  facet_grid(rows = vars(site)) + 
  scale_x_datetime(breaks = date_breaks("1 month"), 
                   labels = date_format("%b-%Y"), expand = c(0,0)) + 
  labs(y = expression(paste(PM[2.5], '  (', mu, '/', m^3, ')', sep = ''))) + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), 
        axis.title.x = element_blank(), 
        strip.text = element_text(size = 14))

# se puede explorar por día, por mes, por estación climática
# generar gráficos siguiendo las diferentes opciones que están en el manual de 
# openair, sobre todo las del capítulo 11 "Time Series and Trends"
# https://bookdown.org/david_carslaw/openair/time-plot.html

# exploro eliminando valores por arriba del percentil 99
cob_p99 <- 
  mxc_l %>% 
  filter(site == "cob", 
         pm25 < quantile(mxc_l$pm25, 0.99, na.rm = TRUE)) %>% 
  print()

uabc_p99 <- 
  mxc_l %>% 
  filter(site == "uabc", 
         pm25 < quantile(mxc_l$pm25, 0.99, na.rm = TRUE)) %>% 
  print()

mxc_l_p99 <- bind_rows(cob_p99, uabc_p99) %>% print()

rm(cob_p99, uabc_p99)

ggplot(data = mxc_l_p99, aes(x = factor(hora24), y = pm25)) + 
  geom_boxplot(outlier.size = 0.3) + 
  facet_grid(rows = vars(site)) + 
  labs(x = "Hora", 
       y = expression(paste(PM[2.5], '  (', mu, '/', m^3, ')', sep = ''))) + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(), 
        strip.text = element_text(size = 14))

# calculamos promedios diarios al 75 % de suficiencia
mxc_l_24h <- 
  mxc_l %>% 
  select(date, site, pm25) %>% 
  timeAverage(., avg.time = "day", data.thresh = 75, 
              statistic = "mean", type = "site") %>% 
  ungroup() %>% 
  mutate(date = as.Date(date), 
         year = year(date), 
         dia_year = yday(date)) %>% 
  select(date, year, everything()) %>% 
  print()

timePlot(mxc_l_24h, 
         pollutant = "pm25", 
         type = "site")

intervalos <- yday(seq(as.Date("2010-01-01"), length = 12, by = "1 month"))
etiquetas <- month(seq(as.Date("2010-01-01"), length = 12,by = "1 month"), label = TRUE)

ggplot(data = mxc_l_24h, aes(x = dia_year, y = pm25)) +
  geom_line(colour = "dodgerblue4", size = 0.3) + 
  facet_grid(rows = vars(site, year)) + 
  scale_x_continuous(breaks = intervalos, labels = etiquetas) + 
  labs(y = expression(paste(PM[2.5], '  (', mu, '/', m^3, ')', sep = ''))) + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(), 
        strip.text = element_text(size = 14))

# hacer gráficas por día-semana, mes, estación









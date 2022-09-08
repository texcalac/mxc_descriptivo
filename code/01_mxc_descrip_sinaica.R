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


### Importación de datos
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


### Genero de malla de análisis
# veo los datos de ambas mallas
mxc2018
unique(mxc2018$hora) # no existe hora 24 pero es mejor este formato

mxc2019
unique(mxc2019$hora) # no existe hora 24 pero es mejor este formato

# malla en wide
mxc_w <- 
  # pegamos las mallas de datos
  bind_rows(mxc2018, mxc2019) %>% 
  # generamos variables separadas de año, mes, día y hora
  mutate(year = year(fecha), 
         mes = month(fecha), 
         dia = day(fecha), 
         hora23 = hora -1, # horas del día de 0 a 23
         .after = fecha) %>% 
  rename(hora24 = hora) %>% # horas del día de 1 a 24
  # elimino días festivos
  filter(!(dia == 1 & mes == 1 | 
             dia == 1 & mes == 5 | 
             dia == 16 & mes == 9 | 
             dia == 2 & mes == 11 | 
             dia == 12 & mes == 12 |
             dia == 25 & mes == 12)) %>% 
  print()


### Comportamiento horario  ====================================================
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
        axis.text = element_text(size = 14), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0), size = 18), 
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 5), size = 18), 
        strip.text = element_text(size = 18), 
        plot.background = element_rect(fill = "white", color = "black"))

ggsave("./output/g_horarios.jpg", width = 1920, height = 1080, units = 'px', dpi = 128)


# generamos malla de fechas y excluimos los días festivos, podemos ver que 
# en total deberían ser 17232 filas y nuestra malla sólo tiene 17101,
# en consecuencia el resto son datos faltantes
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

# pegamos la malla de fechas a la malla de concentraciones para completar
mxc_w <- 
  left_join(fechas, mxc_w, by = c("year", "mes", "dia", "hora23")) %>% 
  select(-c(fecha)) %>%
  print()

# eliminamos la malla temporal de fechas
rm(fechas)

### Descriptivo por estación  ==================================================
mxc_w %>% 
  select(year, cob, uabc) %>% 
  pivot_longer(cols = -year, 
               names_to = "site", 
               values_to = "pm25") %>% 
  group_by(year, site) %>% 
  summarize(minimo = min(pm25, na.rm = TRUE),
            cuart_1 = quantile(pm25, 0.25, na.rm = TRUE),
            mediana = median(pm25, na.rm = TRUE),
            media = mean(pm25, na.rm = TRUE),
            cuart_3 = quantile(pm25, 0.75, na.rm = TRUE),
            maximo = max(pm25, na.rm = TRUE), 
            faltante = sum(is.na(pm25))) %>% 
  arrange(site, year)


# gráfico de valores faltantes horarios por año
mxc_w %>% 
  select(date, year, cob, uabc) %>% 
  gg_miss_var(show_pct = TRUE, facet = year) + 
  labs(y = "% porcentaje de datos faltantes") + 
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 5)), 
        strip.text = element_text(size = 14))

# gráfico de valores faltantes horarios - todo el periodo
mxc_w %>% 
  select(date, cob, uabc) %>% 
  gg_miss_var(show_pct = TRUE) + 
  labs(y = "% porcentaje de datos faltantes") + 
  theme_light() + 
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 5)))


### Comportamiento horario por mes - COBACH
ggplot(data = mxc_w, aes(x = factor(mes), y = cob)) + 
  geom_jitter(colour = "dodgerblue", size = 0.2, alpha = 0.5, width = 0.2) + 
  geom_violin(colour = "dodgerblue4", fill = NA, draw_quantiles = c(0.5)) + 
  scale_x_discrete(labels = c("ene", "feb", "mar", "abr", "may", "jun", 
                              "jul", "ago", "sep", "oct", "nov", "dic")) + 
  scale_y_continuous(limits = c(0, 450), breaks = seq(0, 450, 50)) + 
  labs(y = "COBACH", 
       x = "") + 
  theme_minimal() + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(size = 14), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0), size = 18), 
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 5), size = 18), 
        plot.background = element_rect(fill = "white", color = "black"))

ggsave("./output/g_mes_horarios_cobach.jpg", width = 1920, height = 1080, units = 'px', dpi = 128)


### Comportamiento horario por mes - UABC
ggplot(data = mxc_w, aes(x = factor(mes), y = uabc)) + 
  geom_jitter(colour = "dodgerblue", size = 0.2, alpha = 0.5, width = 0.2) + 
  geom_violin(colour = "dodgerblue4", fill = NA, draw_quantiles = c(0.5)) + 
  scale_x_discrete(labels = c("ene", "feb", "mar", "abr", "may", "jun", 
                              "jul", "ago", "sep", "oct", "nov", "dic")) + 
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50)) + 
  labs(y = "UABC", 
       x = "") + 
  theme_minimal() + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text = element_text(size = 14), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0), size = 18), 
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 5), size = 18), 
        plot.background = element_rect(fill = "white", color = "black"))

ggsave("./output/g_mes_horarios_uabc.jpg", width = 1920, height = 1080, units = 'px', dpi = 128)


# malla en long
mxc_l <- 
  mxc_w %>% 
  pivot_longer(cols = -c(date:hora24), 
               names_to = "site", 
               values_to = "pm25") %>% 
  print()

ggplot(data = mxc_l, aes(x = factor(mes), y = pm25)) + 
  geom_jitter(colour = "dodgerblue", size = 0.2, alpha = 0.5, width = 0.2) + 
  geom_violin(colour = "dodgerblue4", fill = NA, draw_quantiles = c(0.5)) + 
  scale_x_discrete(labels = c("ene", "feb", "mar", "abr", "may", "jun", 
                              "jul", "ago", "sep", "oct", "nov", "dic")) + 
  labs(x = "", 
       y = expression(paste(PM[2.5], '  (', mu, '/', m^3, ')', sep = ''))) + 
  facet_grid(rows = vars(site)) + 
  theme_minimal() + 
  theme(axis.text = element_text(size = 14), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0), size = 18), 
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 5), size = 18), 
        strip.text = element_text(size = 18), 
        plot.background = element_rect(fill = "white", color = "black"))

ggsave("~/Desktop/g_mes_horarios.jpg", width = 1920, height = 1080, units = 'px', dpi = 128)
ggsave("./output/g_mes_horarios.jpg", width = 1920, height = 1080, units = 'px', dpi = 128)

# rm(mxc2018, mxc2019)


# calculamos promedios diarios al 75 % de suficiencia  =========================
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


### Gráfico de promedios diarios al 75% de suficiencia
dias_num <- yday(seq(as.Date("2018-01-01"), length = 24, by = "1 month"))
dias_lab <- month(seq(as.Date("2018-01-01"), length = 24,by = "1 month"), label = TRUE)

ggplot(data = mxc_l_24h, aes(x = dia_year, y = pm25)) + 
  geom_rect(aes(xmin = 1, xmax = 365, ymin = 41, ymax = 100), fill = "gray80", alpha = 0.05) + 
  geom_line(colour = "dodgerblue4", size = 0.3) + 
  labs(x = "", 
       y = expression(paste(PM[2.5], '  (', mu, '/', m^3, ')', sep = ''))) + 
  scale_x_continuous(breaks = dias_num, labels = dias_lab) + 
  facet_grid(rows = vars(site), 
             cols = vars(year)) + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 5, l = 0), size = 18), 
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 5), size = 18), 
        strip.text = element_text(size = 18), 
        plot.background = element_rect(fill = "white", color = "black"))

ggsave("~/Desktop/g_mes_diarios.jpg", width = 1920, height = 1080, units = 'px', dpi = 128)
ggsave("./output/g_mes_diarios.jpg", width = 1920, height = 1080, units = 'px', dpi = 128)






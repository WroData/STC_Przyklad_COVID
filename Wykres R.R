
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Opis ####

# Opis:
# Kroki:

# Materialy pomocnicze 


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Odlaczeni wszytkich pakietów ####
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

detachAllPackages() #wywolanie funkcji 

#usuniecie zmiennych
gc(reset = TRUE)
rm(list = ls())

#ustalenie seed
set.seed(1)

# bedy po angileku
Sys.setenv(LANGUAGE='en')

#nie notacja naukowa
options("scipen"=100, "digits"=4)

# dodaj mozliwosc drukowania polsich liter w ggplot2
Sys.setlocale("LC_ALL", "Polish")
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Deklaracja bibliotek ####

library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(scales)
library(ggplot2)
library(tidyverse)
# install.packages("viridis")
library(viridis)

library(zoo)
#install.packages("zoo")
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Funkcje ####



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Stele ####
MAIN_PATH <- dirname(rstudioapi::getSourceEditorContext()$path)
OUT_PATH <- paste0(MAIN_PATH, "\\CHARTS\\R\\")

WSKAZNIK <- "NO2_1g"
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Wczytanie danych ####

# Wroclaw
ZANIECZYSZCZENIA <- read.csv(paste0(MAIN_PATH, "\\df_ZANIECZYSZCZENIA.csv")) %>%
  dplyr::mutate(Date = paste0("2020", "-", str_pad(miesiac,2, side = "left", pad = "0"), "-", str_pad(dzien,2, side = "left", pad = "0")),
                Date = as.Date(Date)) %>%
  dplyr::select("Value", "STACJA", "ZANIECZYSZCZENIE", "rok", "Date") %>%
  dplyr::group_by(STACJA, ZANIECZYSZCZENIE, rok, Date) %>%
  dplyr::summarise(Value = mean(Value, na.rm=TRUE))

# lockdown >- 
LOCKDOWN <- read.csv(paste0(MAIN_PATH, "\\df_ZANIECZYSZCZENIA.csv")) %>%
  dplyr::select("Data", "rok", "miesiac", "dzien", "retail_and_recreation", "grocery_and_pharmacy", "parks", "transit_stations", "workplaces", "residential") %>%
  drop_na()
  

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### Kiedy Lockdown ####

LOCKDOWN_gr <- LOCKDOWN %>% 
  # rowwise() %>%
  # mutate(mean_col = mean(c_across(c("retail_and_recreation", "grocery_and_pharmacy", "parks", "transit_stations", "workplaces")), na.rm=TRUE)) %>%
  # ungroup() %>%
  group_by(rok, miesiac) %>%
  summarise(mean_sel = mean((retail_and_recreation + grocery_and_pharmacy + transit_stations) / 3),
            retail_and_recreation = mean(retail_and_recreation),
            grocery_and_pharmacy = mean(grocery_and_pharmacy),
            parks = mean(parks),
            transit_stations = mean(transit_stations),
            workplaces = mean(workplaces),
            residential = mean(residential),
  ) %>%
  mutate(Date = paste0(rok, "-", str_pad(miesiac,2, side = "left", pad = "0"), "-01"),
         Date = as.Date(Date),
         m = format(as.Date(Date), "%B"),
         m = str_to_title(m),
         m = factor(m, levels = c('Styczen', 'Luty', 'Marzec', 'Kwiecien', 'Maj', 'Czerwiec', 'Lipiec', 'Sierpien', 'Wrzesien', 'Pazdziernik', 'Listopad', "Grudzien")),
         connect_date = paste0(rok, "-", m))



# how many select - hoky plot
LOCKDOWN_hokey <- LOCKDOWN_gr %>%
  ungroup() %>%
  arrange(mean_sel) %>%
  mutate(rank = 1:nrow(LOCKDOWN_gr))

ggplot(LOCKDOWN_hokey) +
  geom_line(mapping  = aes(x = rank, y = mean_sel))


# select lowest
nbottom = 5

lockdown_months <- LOCKDOWN_hokey %>% 
  filter(rank <= nbottom) %>%
  select(connect_date)


# plot moility indexes

LOCKDOWN_gr_melt <- LOCKDOWN_gr %>%
  pivot_longer(
    cols = mean_sel:residential, 
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric, levels = c("mean_sel", "retail_and_recreation", "grocery_and_pharmacy", "transit_stations", "parks", "workplaces", "residential")),
    lockdown = ifelse(connect_date %in% lockdown_months$connect_date, 1, 0))


w <- ggplot(LOCKDOWN_gr_melt) +
  geom_col(mapping  = aes(x = Date, y = value, fill = lockdown)) +
  facet_wrap(vars(metric)) 


plot(w)



# save
png(filename = paste0(OUT_PATH, "", "\\Wykres COVID months based on mobility - ", 
                      Sys.Date(), " .png", sep=""),
    bg="#dedfe0", width = 7, height = 5, units = 'in', res = 500)
plot(w)
dev.off() 



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### plot THEME ####

C1 <- "#993404"
C2 <- "#d95f0e"
C3 <- "#fec44f"

FILL_COL <- "#dedfe0"
TEXT_COL <- "#4e4d47"
TEXT_BASE_SIZE <- 10

THEME <-
  theme(
    axis.line = element_blank(),
    axis.text.x = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE,  hjust = 0.5,  color = TEXT_COL),
    axis.text.y = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE,  hjust = 0.0,  color = TEXT_COL),
    axis.ticks = element_blank(),
    axis.title.x = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE, hjust = 0.5,  color = TEXT_COL),
    axis.title.y = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE, hjust = 0.5,  color = TEXT_COL),
    
    
    #panel.border = element_blank(),
    # panel.grid.major=element_blank(),
    #panel.grid.minor = element_blank(),
    
    #tlo
    plot.background  = element_rect(fill = FILL_COL,  color = NA), 
    panel.background = element_rect(fill = FILL_COL,  color = NA),
    text = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE - 2, color = TEXT_COL),
    
    # legenda
    legend.position = "bottom",# "none",
    legend.key.width = unit(0.9, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.title.align = 0.5,
    legend.title = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE, hjust = 0, color = TEXT_COL),#element_blank(),
    legend.background = element_rect(fill = FILL_COL, color = NA),
    legend.key = element_rect(fill = FILL_COL),
    legend.text       = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE, hjust = 0, color = TEXT_COL),
    legend.direction = "horizontal",
    
    # tytuy
    plot.title    = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE + 2, hjust = 0.0,  color = TEXT_COL, face="bold"),
    plot.subtitle = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE,  hjust = 0.01, face = "italic", color = TEXT_COL),
    plot.caption  = element_text(family = "Ubuntu", size = TEXT_BASE_SIZE - 2,  hjust = 0.99, color = TEXT_COL),
  )  


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### FINAL plot - 1 day ####

tmp <- ZANIECZYSZCZENIA %>%
  filter(STACJA == "DsWrocAlWisn") %>%
  filter(ZANIECZYSZCZENIE == WSKAZNIK) %>%
  filter(rok >= 2015)


t_other <- tmp %>% filter(rok != 2020) %>% mutate(typ = 'other')
t_avg <- tmp   %>% filter(rok != 2020) %>% dplyr::group_by(Date) %>% dplyr::summarise(Value = mean(Value, na.rm = TRUE))  %>% mutate(typ = 'avg')
t_2020 <- tmp  %>% filter(rok == 2020) %>% mutate(typ = '2020')
t_wytyczna <- tmp %>% filter(rok == 2020) %>% mutate(typ = 'wytyczna', Value = 25, rok = 1)

t <- rbind(t_other, t_avg, t_2020, t_wytyczna) %>%
  mutate(typ = factor(typ, levels = c('other', 'avg', '2020', 'wytyczna')))

t %>%
  mutate(m = month(Date), d = day(Date)) %>%
  group_by(m) %>%
  summarise(n = n_distinct(d, na.rm = FALSE))

w <- ggplot() +
  geom_line(t, mapping  = aes(x = Date, y = Value, group = rok, color = typ, alpha = typ)) + 
  coord_cartesian (ylim = c(0, 75), xlim = as.Date(c("2020-01-01", "2021-01-01"))) +
  scale_alpha_manual(values = c("other" = 0.1, "avg" = 1, "2020" = 1, "wytyczna" = 1)) +
  scale_color_manual(values = c("other" = "red",  "avg" = "red",         "2020" = "blue", "wytyczna" = "black"),
                     labels = c("", "Srednia dlugoletnia", "Rok 2020",      "Wytyczne WHO (2021)"),
                     limits = c("other",          "avg",                 "2020",          "wytyczna")) + 
  labs(
    title = "Czy wprowadzenie Strefy Czystego Transportu poprawi jakosc powietrza?",
    subtitle = "Porównanie roku pandemicznego z dlugookresowa srednia pokazuje, ze nie moze byc to jedyne dzialanie",
    #title = "Redukcja ruchu samochodowego poprawila jakosc powietrza",
    #subtitle = "Czy tak samo bedzie po wprowadzeniu Strefy Czystego Transportu?",
    caption = "Autor: WroData | Dane: GIOS, stacja pomiarów Aleja Wisniowa, lata 2015-2022",
    x = "",
    y = "Sredniodobowy odczyt NO2 [µg/m3]",
    color = ""
  ) +
  guides(alpha = "none") +
  scale_x_date(labels = date_format("%B"), breaks = as.Date(c("2020-01-01", "2020-03-01", "2020-05-01", "2020-07-01", "2020-09-01", "2020-11-01"))) +
  guides(colour = guide_legend(override.aes = list(alpha = c(0.0, 1, 1, 1)))) +
  
  THEME

plot(w)

# save
png(filename = paste0(OUT_PATH, "", "\\_FINAL_Wykres NO2 1 pow 2015 - ", 
                      Sys.Date(), " .png", sep=""),
    bg=FILL_COL, width = 7, height = 5, units = 'in', res = 500)
plot(w)
dev.off() 


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### !!!! OTHER !!!! ####
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### plot - 1 day ####

tmp <- ZANIECZYSZCZENIA %>%
  filter(STACJA == "DsWrocAlWisn") %>%
  filter(ZANIECZYSZCZENIE == WSKAZNIK) 


t_other <- tmp %>% filter(rok != 2020) %>% mutate(typ = 'other')
t_avg <- tmp   %>% filter(rok != 2020) %>% dplyr::group_by(Date) %>% dplyr::summarise(Value = mean(Value, na.rm = TRUE))  %>% mutate(typ = 'avg')
t_2020 <- tmp  %>% filter(rok == 2020) %>% mutate(typ = '2020')
t_wytyczna <- tmp %>% filter(rok == 2020) %>% mutate(typ = 'wytyczna', Value = 25, rok = 1)

t <- rbind(t_other, t_avg, t_2020, t_wytyczna) %>%
  mutate(typ = factor(typ, levels = c('other', 'avg', '2020', 'wytyczna')))

t %>%
  mutate(m = month(Date), d = day(Date)) %>%
  group_by(m) %>%
  summarise(n = n_distinct(d, na.rm = FALSE))

w <- ggplot() +
  geom_line(t, mapping  = aes(x = Date, y = Value, group = rok, color = typ, alpha = typ)) + 
  coord_cartesian (ylim = c(0, 75), xlim = as.Date(c("2020-01-01", "2021-01-01"))) +
  scale_alpha_manual(values = c("other" = 0.1, "avg" = 1, "2020" = 1, "wytyczna" = 1)) +
  scale_color_manual(values = c("other" = "red",  "avg" = "red",         "2020" = "blue", "wytyczna" = "black"),
                     labels = c("", "Srednia dlugoletnia", "Rok 2020",      "Wytyczne WHO (2021)"),
                     limits = c("other",          "avg",                 "2020",          "wytyczna")) + 
  labs(
    title = "Redukcja ruchu samochodowego poprawila jakosc powietrza",
    subtitle = "Czy tak samo bedzie po wprowadzeniu Strefy Czystego Transportu?",
    caption = "Autor: WroData | Dane: GIOS, stacja pomiarów Aleja Wisniowa",
    x = "",
    y = "Srednodobowy odczyt NO2 [µg/m3]",
    color = ""
  ) +
  guides(alpha = "none") +
  scale_x_date(labels = date_format("%b"), breaks = as.Date(c("2020-01-01", "2020-03-01", "2020-05-01", "2020-07-01", "2020-09-01", "2020-11-01"))) +
  guides(colour = guide_legend(override.aes = list(alpha = c(0.0, 1, 1, 1)))) +
  
  THEME

plot(w)

# save
png(filename = paste0(OUT_PATH, "", "\\Wykres NO2 1 dzien - ", 
                      Sys.Date(), " .png", sep=""),
    bg=FILL_COL, width = 7, height = 5, units = 'in', res = 500)
plot(w)
dev.off() 


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### plot - 1 day - selected years ####

tmp <- ZANIECZYSZCZENIA %>%
  filter(STACJA == "DsWrocAlWisn") %>%
  filter(ZANIECZYSZCZENIE == WSKAZNIK) %>%
  filter(rok %in% c(2018, 2019, 2020, 2021, 2022)) # %>%
  # mutate(rok = factor(toString(rok), levels = c(2018, 2019, 2020, 2021, 2022)))

size_factor = 0.2
alpha_factor = 0.5

t <- tmp %>% ungroup()
t %>%
  mutate(m = month(Date), d = day(Date)) %>%
  group_by(m) %>%
  summarise(n = n_distinct(d, na.rm = FALSE))

w <- ggplot() +
  geom_line(as.data.frame(t), mapping  = aes(x = Date, y = Value, color = factor(rok), alpha = factor(rok), size = factor(rok))) + 
  coord_cartesian (ylim = c(0, 75), xlim = as.Date(c("2020-01-01", "2021-01-01"))) +
  labs(
    title = "Redukcja ruchu samochodowego poprawila jakosc powietrza",
    subtitle = "Czy tak samo bedzie po wprowadzeniu Strefy Czystego Transportu?",
    caption = "Autor: WroData | Dane: GIOS, stacja pomiarów Aleja Wisniowa",
    x = "",
    y = "Srednodobowy odczyt NO2 [µg/m3]",
    color = ""
  ) +
  scale_alpha_manual(values = c("2018" = alpha_factor, "2019" = alpha_factor, "2020" = 1, "2021" = alpha_factor, "2022" = alpha_factor)) +
  scale_size_manual(values = c("2018" = size_factor, "2019" = size_factor, "2020" = 1, "2021" = size_factor, "2022" = size_factor)) +
  # scale_color_manual(values = c("2018" = 0.4, "2019" = 0.4, "2020" = 1, "2021" = 0.4, "2022" = "")) + 
  scale_color_viridis(discrete=TRUE, option="magma" ) +
  guides(alpha = "none") +
  guides(size = "none") +
  scale_x_date(labels = date_format("%b"), breaks = as.Date(c("2020-01-01", "2020-03-01", "2020-05-01", "2020-07-01", "2020-09-01", "2020-11-01"))) +
  
  THEME

plot(w)

# save
png(filename = paste0(OUT_PATH, "", "\\Wykres NO2 1 dzien selected countries - ", 
                      Sys.Date(), " .png", sep=""),
    bg=FILL_COL, width = 7, height = 5, units = 'in', res = 500)
plot(w)
dev.off() 


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### plot - 7 day ####


tmp <- ZANIECZYSZCZENIA %>%
  filter(STACJA == "DsWrocAlWisn") %>%
  filter(ZANIECZYSZCZENIE == WSKAZNIK) %>%
  arrange(rok, Date) %>%
  mutate(r_value = rollapply(Value, 7,FUN=function(x) mean(x, na.rm=TRUE), fill=NA))


t_other <- tmp %>% filter(rok != 2020) %>% mutate(typ = 'other')
t_avg <- tmp   %>% filter(rok != 2020) %>% dplyr::group_by(Date) %>% dplyr::summarise(r_value = mean(r_value, na.rm = TRUE))  %>% mutate(typ = 'avg')
t_2020 <- tmp  %>% filter(rok == 2020) %>% mutate(typ = '2020')
t_wytyczna <- tmp %>% filter(rok == 2020) %>% mutate(typ = 'wytyczna', r_value = 25, rok = 1)

t <- rbind(t_other, t_avg, t_2020, t_wytyczna) %>%
  mutate(typ = factor(typ, levels = c('other', 'avg', '2020', 'wytyczna')))

t %>%
  mutate(m = month(Date), d = day(Date)) %>%
  group_by(m) %>%
  summarise(n = n_distinct(d, na.rm = FALSE))

w <- ggplot() +
  geom_line(t, mapping  = aes(x = Date, y = r_value, group = rok, color = typ, alpha = typ)) + 
  coord_cartesian (ylim = c(0, 75), xlim = as.Date(c("2020-01-01", "2021-01-01"))) +
  scale_alpha_manual(values = c("other" = 0.1, "avg" = 1, "2020" = 1, "wytyczna" = 1)) +
  scale_color_manual(values = c("other" = "red",  "avg" = "red",         "2020" = "blue", "wytyczna" = "black"),
                     labels = c("", "Srednia dlugoletnia", "Rok 2020",      "Wytyczne WHO (2021)"),
                     limits = c("other",          "avg",                 "2020",          "wytyczna")) + 
  labs(
    title = "Redukcja ruchu samochodowego poprawila jakosc powietrza",
    subtitle = "Czy tak samo bedzie po wprowadzeniu Strefy Czystego Transportu?",
    caption = "Autor: WroData | Dane: GIOS, stacja pomiarów Aleja Wisniowa",
    x = "",
    y = "Sredniotygodniowy odczyt NO2 [µg/m3]",
    color = ""
  ) +
  guides(alpha = "none") +
  scale_x_date(labels = date_format("%b"), breaks = as.Date(c("2020-01-01", "2020-03-01", "2020-05-01", "2020-07-01", "2020-09-01", "2020-11-01"))) +
  guides(colour = guide_legend(override.aes = list(alpha = c(0.0, 1, 1, 1)))) +
  
  THEME

plot(w)

# save
png(filename = paste0(OUT_PATH, "", "\\Wykres NO2 7 dzien - ", 
                      Sys.Date(), " .png", sep=""),
    bg=FILL_COL, width = 7, height = 5, units = 'in', res = 500)
plot(w)
dev.off() 





# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### plot - 7 day ####

tmp <- ZANIECZYSZCZENIA %>%
  filter(STACJA == "DsWrocAlWisn") %>%
  filter(ZANIECZYSZCZENIE == WSKAZNIK) %>% 
  arrange(rok, Date) %>%
  mutate(r_value = rollapply(Value, 7,FUN=function(x) mean(x, na.rm=TRUE), fill=NA)) %>%
  filter(rok %in% c(2018, 2019, 2020, 2021, 2022)) # %>%
# mutate(rok = factor(toString(rok), levels = c(2018, 2019, 2020, 2021, 2022)))

size_factor = 0.7
alpha_factor = 0.7

t <- tmp %>% ungroup()
t %>%
  mutate(m = month(Date), d = day(Date)) %>%
  group_by(m) %>%
  summarise(n = n_distinct(d, na.rm = FALSE))

w <- ggplot() +
  geom_line(as.data.frame(t), mapping  = aes(x = Date, y = r_value, color = factor(rok), alpha = factor(rok), size = factor(rok))) + 
  coord_cartesian (ylim = c(0, 75), xlim = as.Date(c("2020-01-01", "2021-01-01"))) +
  labs(
    title = "Redukcja ruchu samochodowego poprawila jakosc powietrza",
    subtitle = "Czy tak samo bedzie po wprowadzeniu Strefy Czystego Transportu?",
    caption = "Autor: WroData | Dane: GIOS, stacja pomiarów Aleja Wisniowa",
    x = "",
    y = "Sredniotygodniowy odczyt NO2 [µg/m3]",
    color = ""
  ) +
  scale_alpha_manual(values = c("2018" = alpha_factor, "2019" = alpha_factor, "2020" = 1, "2021" = alpha_factor, "2022" = alpha_factor)) +
  scale_size_manual(values = c("2018" = size_factor, "2019" = size_factor, "2020" = 1, "2021" = size_factor, "2022" = size_factor)) +
  # scale_color_manual(values = c("2018" = 0.4, "2019" = 0.4, "2020" = 1, "2021" = 0.4, "2022" = "")) + 
  scale_color_viridis(discrete=TRUE, option="magma" ) +
  guides(alpha = "none") +
  guides(size = "none") +
  scale_x_date(labels = date_format("%b"), breaks = as.Date(c("2020-01-01", "2020-03-01", "2020-05-01", "2020-07-01", "2020-09-01", "2020-11-01"))) +
  
  THEME

plot(w)

# save
png(filename = paste0(OUT_PATH, "", "\\Wykres NO2 7 dzien selected countries - ", 
                      Sys.Date(), " .png", sep=""),
    bg=FILL_COL, width = 7, height = 5, units = 'in', res = 500)
plot(w)
dev.off() 



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### plot - 7 day boxplot ####



tmp <- ZANIECZYSZCZENIA %>%
  filter(STACJA == "DsWrocAlWisn") %>%
  filter(ZANIECZYSZCZENIE == WSKAZNIK) 

t_other <- tmp %>% filter(rok != 2020) %>% mutate(typ = 'other')
t_2020 <- tmp  %>% filter(rok == 2020) %>% mutate(typ = '2020')

t <- rbind(t_other, t_2020) %>%
  filter(rok <= 2022) %>%
  mutate(typ = factor(typ, levels = c('other', '2020')),
         m = format(as.Date(Date), "%B"),
         m = str_to_title(m),
         m = factor(m, levels = c('Styczen', 'Luty', 'Marzec', 'Kwiecien', 'Maj', 'Czerwiec', 'Lipiec', 'Sierpien', 'Wrzesien', 'Pazdziernik', 'Listopad', "Grudzien")),
         connect_date = paste0(rok, "-", m),
         lockdown = ifelse(connect_date %in% lockdown_months$connect_date, "Tak", "Nie"))
#%>%
# filter(m %in%  c('Styczen', 'Marzec', 'Kwiecien', 'Maj', 'Listopad'))



w <- ggplot(t, mapping  = aes(x =  rok, y = Value, group = rok, fill = typ)) + 
  facet_wrap(vars(m), ncol  = 4) +
  coord_cartesian (ylim = c(0, 75))   +
  geom_boxplot() +
  geom_hline(yintercept = 25, linetype = 2) +
  scale_fill_manual(values = c("other" = C3,  "2020" = C2),
                    labels = c("Pozostale lata", "Rok 2020")) + 
  labs(
    title = "Redukcja ruchu samochodowego poprawila jakosc powietrza",
    subtitle = "Czy tak samo bedzie po wprowadzeniu Strefy Czystego Transportu?",
    caption = "Autor: WroData | Dane: GIOS, stacja pomiarów Aleja Wisniowa",
    x = "",
    y = "Srednodobowy odczyt NO2 [µg/m3]",
    color = "",
    fill = ""
  ) +
  # scale_x_continuous(breaks = c(2016, 2018, 2020, 2022), labels = c("'16", "'18", "'20", "'22")) +
  # scale_y_continuous(breaks = c(0, 100, 200)) +
  THEME



plot(w)

# save
png(filename = paste0(OUT_PATH, "", "\\Wykres NO2 1 boxplot all years - ", 
                      Sys.Date(), " .png", sep=""),
    bg=FILL_COL, width = 7, height = 5, units = 'in', res = 500)
plot(w)
dev.off() 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### plot - 7 day boxplot ####



tmp <- ZANIECZYSZCZENIA %>%
  filter(STACJA == "DsWrocAlWisn") %>%
  filter(ZANIECZYSZCZENIE == WSKAZNIK) 

t_other <- tmp %>% filter(rok != 2020) %>% mutate(typ = 'other')
t_2020 <- tmp  %>% filter(rok == 2020) %>% mutate(typ = '2020')

t <- rbind(t_other, t_2020) %>%
  filter(rok >= 2015) %>%
  filter(rok <= 2022) %>%
  mutate(typ = factor(typ, levels = c('other', '2020')),
         m = format(as.Date(Date), "%B"),
         m = str_to_title(m),
         m = factor(m, levels = c('Styczen', 'Luty', 'Marzec', 'Kwiecien', 'Maj', 'Czerwiec', 'Lipiec', 'Sierpien', 'Wrzesien', 'Pazdziernik', 'Listopad', "Grudzien")),
         connect_date = paste0(rok, "-", m),
         lockdown = ifelse(connect_date %in% lockdown_months$connect_date, "Tak", "Nie"))
#%>%
# filter(m %in%  c('Styczen', 'Marzec', 'Kwiecien', 'Maj', 'Listopad'))



w <- ggplot(t, mapping  = aes(x =  rok, y = Value, group = rok, fill = typ)) + 
  facet_wrap(vars(m), ncol  = 4) +
  coord_cartesian (ylim = c(0, 75))   +
  geom_boxplot() +
  geom_hline(yintercept = 25, linetype = 2) +
  scale_fill_manual(values = c("other" = C3,  "2020" = C2),
                    labels = c("Pozostale lata", "Rok 2020")) + 
  labs(
    title = "Redukcja ruchu samochodowego poprawila jakosc powietrza",
    subtitle = "Czy tak samo bedzie po wprowadzeniu Strefy Czystego Transportu?",
    caption = "Autor: WroData | Dane: GIOS, stacja pomiarów Aleja Wisniowa",
    x = "",
    y = "Srednodobowy odczyt NO2 [µg/m3]",
    color = "",
    fill = ""
  ) +
  scale_x_continuous(breaks = c(2016, 2018, 2020, 2022), labels = c("'16", "'18", "'20", "'22")) +
  #scale_y_continuous(breaks = c(0, 100, 200)) +
  THEME



plot(w)

# save
png(filename = paste0(OUT_PATH, "", "\\Wykres NO2 1 boxplot - ", 
                      Sys.Date(), " .png", sep=""),
    bg=FILL_COL, width = 7, height = 5, units = 'in', res = 500)
plot(w)
dev.off() 


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#### plot - 7 day boxplot - selected ####


tmp <- ZANIECZYSZCZENIA %>%
  filter(STACJA == "DsWrocAlWisn") %>%
  filter(ZANIECZYSZCZENIE == WSKAZNIK) 

t_other <- tmp %>% filter(rok != 2020) %>% mutate(typ = 'other')
t_2020 <- tmp  %>% filter(rok == 2020) %>% mutate(typ = '2020')

t <- rbind(t_other, t_2020) %>%
  filter(rok >= 2015) %>%
  filter(rok <= 2022) %>%
  mutate(typ = factor(typ, levels = c('other', '2020')),
         m = format(as.Date(Date), "%B"),
         m = str_to_title(m),
         m = factor(m, levels = c('Styczen', 'Luty', 'Marzec', 'Kwiecien', 'Maj', 'Czerwiec', 'Lipiec', 'Sierpien', 'Wrzesien', 'Pazdziernik', 'Listopad', "Grudzien")),
         connect_date = paste0(rok, "-", m),
         lockdown = ifelse(connect_date %in% lockdown_months$connect_date, "Tak", "Nie"),
         lockdown = factor(lockdown, levels = c("Nie", "Tak"))) %>%
  filter(m %in%  c('Styczen', 'Marzec', 'Kwiecien', 'Maj', 'Listopad')) %>%
  ungroup()

w <- ggplot(t, mapping  = aes(x = rok, y = Value, , group = rok, fill = lockdown)) + 
  geom_boxplot() +
  facet_wrap(vars(m), ncol  = 5) +
  coord_cartesian (ylim = c(0, 75))   +
  geom_hline(yintercept = 25, linetype = 2) +
  scale_fill_manual(values = c("Nie" = C3,  "Tak" = C2),
                    labels = c("Nie", "Tak")) + 
  labs(
    title = "Redukcja ruchu samochodowego poprawila jakosc powietrza",
    subtitle = "Czy tak samo bedzie po wprowadzeniu Strefy Czystego Transportu?",
    caption = "Autor: WroData | Dane: GIOS, stacja pomiarów Aleja Wisniowa",
    x = "",
    y = "Srednodobowy odczyt NO2 [µg/m3]",
    color = "",
    fill = "Zamknieta gospodarka?"
  ) +
  scale_x_continuous(breaks = c(2016, 2018, 2020, 2022), labels = c("'16", "'18", "'20", "'22")) +
  #scale_y_continuous(breaks = c(0, 100, 200)) +
  THEME



plot(w)

# save
png(filename = paste0(OUT_PATH, "", "\\Wykres NO2 1 boxplot selected - ", 
                      Sys.Date(), " .png", sep=""),
    bg=FILL_COL, width = 7, height = 5, units = 'in', res = 500)
plot(w)
dev.off() 

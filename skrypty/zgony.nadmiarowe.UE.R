library(eurostat)
library(tidyverse)
library(ggtext) 
library(scales)
library(gridExtra)

load("./dane/ISO2.3.PL.Rda")
ISO <- ISO2.3.PL %>% 
  select(nazwa.pl, ISO2)

ludnosc <- get_eurostat("demo_pjan") %>% 
  filter(age == "TOTAL", 
         sex == "T",
         time == "2019-01-01") %>% 
  select(geo, values)

data <- get_eurostat("demo_r_mwk_10")

dane <- data %>% 
  filter(age == "TOTAL",
         sex == "T") %>% 
  mutate(year = str_sub(time, 1,4),
         tydzien = as.numeric(str_sub(time, 6,7))) %>% 
  filter(year %in% c(2015:2020)) %>% 
  select(4:8) %>% 
  arrange(geo,year, tydzien) %>% 
  group_by(geo, year) %>% 
  mutate(zgony.cum = cumsum(values))

filtr <- dane %>% 
  filter(year==2020) %>% 
  group_by(geo) %>% 
  mutate(id = max(tydzien)) %>% 
  select(geo, id) %>% 
  unique()

dane <- dane %>% 
  left_join(filtr, by="geo") %>% 
  filter(!is.na(id)) 

r.2020 <- dane %>% 
  filter(year == 2020,
         tydzien == max(tydzien)) %>% 
  select(-values)

r.2015 <- dane %>% 
  filter(year == 2015,
         tydzien == id)

reszta <- dane %>% 
  mutate(id = if_else(id == 53, 52, id)) %>% 
  filter(year %in% c(2016:2019),
         tydzien == id)

srednia <- r.2015 %>% 
  bind_rows(reszta) %>% 
  group_by(geo) %>% 
  summarise(srednia = mean(zgony.cum))

a <- r.2020 %>% 
  left_join(srednia, by="geo") %>% 
  left_join(ISO, by = c("geo"="ISO2")) %>% 
  mutate(roznica = zgony.cum - srednia) %>% 
  left_join(ludnosc, by = "geo") %>% 
  mutate(zgony.1mln = roznica*1e6/values,
         proc = -1+zgony.cum/srednia) %>% 
  filter(!geo %in% c("GE", "AM")) %>% 
  unique()

ggplot(a, aes(reorder(nazwa.pl, roznica), roznica))+
  geom_col(fill="steelblue")+
  coord_flip()+
  labs(x=NULL, 
       y="liczba zgonów",
       title = "Różnica w ilości zgonów w 2020 i średnią w latach 2015-2019",
       caption = '')+
  theme_bw()+
  theme(legend.position = "top",
        plot.caption = element_markdown(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size=11),
        plot.subtitle = element_text(hjust = 0.5))  -> p1

ggplot(a, aes(reorder(nazwa.pl, proc), proc))+
  geom_col(fill="steelblue")+
  coord_flip()+
  scale_y_continuous(labels = label_percent(accuracy = 1L))+
  labs(x=NULL,
       y="różnica",
       title = "Procentowa zmiana między zgonami w 2020 i średnią w latach 2015-2019",
       caption = '<span style="color:grey50">*Na podstawie danych Eurostat, baza: demo_r_mwk_10*</span>')+
  theme_bw()+
  theme(legend.position = "top",
        plot.caption = element_markdown(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size=11),
        plot.subtitle = element_text(hjust = 0.5))  -> p2

png("./wykresy/Europa.zgony.png", units="in", width=12, height=7, res=300)
grid.arrange(p1, p2, ncol=2)
dev.off()

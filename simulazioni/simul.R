library(gtools)
library(readxl)
library(dplyr)
library(xlsx)
library(ggplot2)
library(ggridges)
library(ggsci)
theme_set(theme_minimal())


source("D:/Personal Statistics/rcb/function_elo.R")
source("D:/Personal Statistics/rcb/elo totale.R", echo=F)


simul <- read_excel("D:/Personal Statistics/rcb/simul_torneo.xlsx",sheet = "Foglio4") 

simul_torneo(simul,elo_post_mixed)

# empty_tibble = tibble(winner = "A")
  system.time(
for(B in 1:1000){
  empty_tibble <- empty_tibble %>% add_row(winner = simul_torneo(simul,elo_post_mixed))
  print(B)
}
  )

empty_tibble=
  empty_tibble%>% 
  filter(winner != "A") 

empty_tibble %>% 
  group_by(winner) %>% 
  summarize(n=n(),
            perc =round(100*n/nrow(empty_tibble))) %>% 
  filter(perc>1) %>% 
  mutate(nomi = c(
                                            "RBZ Capri Sons",
                                            "I Flickettoni",
                                            "RBZ Tocco Italiano",
                                            "RCB Presidenziale",
                                            "Double Bang"
                  )) %>% 
  mutate(nomi = factor(nomi, levels = c(
    "RBZ Capri Sons",
    "Double Bang",
    "I Flickettoni",
    "RBZ Tocco Italiano",
    "RCB Presidenziale"
    ))) %>% 
  ggplot(aes(x = perc, y = nomi, fill = winner))+
  ylab("")+xlab("Probabilità di Vittoria dei Nationals")+
  geom_bar(stat = "identity",show.legend = F)+
  scale_fill_aaas()+
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 10,angle = 30)
    )





















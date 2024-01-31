
punti_evento <- function(data, pnt_assegnati = 90, pnt_base = 10, coeff =  32, diff_max = 0.92,win_ass = 400){
  library(tidyverse)
  
  potenza <- data %>% 
    pivot_longer(names_to = "squadra",values_to = "giocatore", cols = g1:g4) %>% 
    mutate(win = case_when(
      squadra %in% c("g1","g2") & w1 == 1~ 1,
      squadra %in% c("g3","g4") & w2 == 1~ 1,
      TRUE ~ 0
    ),
    loss = ifelse(win ==1,0,1)) %>% 
    group_by(giocatore) %>% 
    summarize(punt=sum(win), losses = sum(loss)) %>% 
    filter(!giocatore %in% c("giova")) %>% 
    mutate(potenza_del_torneo = scales::rescale(punt/(punt+losses))*90) %>% 
    arrange(-potenza_del_torneo) %>% 
    filter(!giocatore %in% c("marco")) %>% 
    select(giocatore,potenza_del_torneo )
  
  rob <- data[-nrow(data),] %>% 
    mutate(perc1 = 1,perc2 = 3,perc3=4,perc4=5)
  for(i in 1:nrow(data[-nrow(data),])){
    for(j in 1:4){
      a <- data[i,j]
      indent <- which(potenza[,1] == as.character(a))
      
      rob[i,j+6] <- potenza[indent,2] 
    }
  }
  last <-  rob %>% 
    mutate(
      # pot1 = perc1+perc2,
      # pot2 = perc3+perc4,
      pot1 = mean(c(perc1,perc2)),
      pot2 = mean(c(perc3,perc4)),
      # per_rel1 = perc1/pot1,
      # per_rel2 = perc2/pot1,
      # per_rel3 = perc3/pot2,
      # per_rel4 = perc4/pot2,
      per_rel1 = perc1/(2*pot1), # eloA * per_rel1
      per_rel2 = perc2/(2*pot1), # eloA * per_rel2
      per_rel3 = perc3/(2*pot2), # eloB * per_rel3
      per_rel4 = perc4/(2*pot2), # eloB * per_rel4
      expA = 1/(1+10^((pot2-pot1)/win_ass)),
      expB = 1/(1+10^((pot1-pot2)/win_ass)),
      eloA = ifelse((w1-expA)>diff_max,diff_max,coeff*(w1-expA)),
      eloB = ifelse((w1-expA)>diff_max,1-diff_max,coeff*(w2-expB)),
      eloA1 = eloA,
      eloA2 = eloA,
      eloB3 = eloB,
      eloB4 = eloB
    ) %>% 
    select(g1:g4,eloA1:eloB4) %>% 
    pivot_longer(cols = g1:g4) %>% 
    mutate(punt = case_when(
      name == "g1" ~ eloA1,
      name == "g2" ~ eloA2,
      name == "g3" ~ eloB3,
      name == "g4" ~ eloB4,
    )) %>% 
    select(value, punt) %>% 
    group_by(value) %>% 
    summarize(punti_evento = sum(punt)) %>% 
    mutate(punti_evento = plyr::round_any(scales::rescale(punti_evento)*pnt_assegnati+pnt_base,5)) %>% 
    arrange(-punti_evento) %>% 
    rename(giocatore = value)
  
  return(last)
}


nomi_to_rcbs <- function(data,column = "giocatore"){
  data = 
    data %>% 
    as_tibble()
  
  data %>% 
    mutate(giocatore = case_when(
      giocatore == "Andrea Borsotti" ~ "borso",
      giocatore == "Alessandro Zanellato" ~ "zeta",
      giocatore == "Mattia Pareschi" ~ "pare",
      giocatore == "Federico Grazi" ~ "greis",
      giocatore == "Luca Tosatti" ~ "luca",
      giocatore == "fabia canu" ~ "fabia",
      giocatore == "Tobia Sermenghi" ~ "tobs",
      giocatore == "Tommaso Marzocchi" ~ "tommi",
      giocatore == "Jannik Tiemann" ~ "jannik",
      giocatore == "Davide Greco" ~ "greek",
      giocatore == "Irene Stranieri" ~ "irene",
      giocatore == "Luca Pallotta" ~ "pallot",
      giocatore == "Giovanni Calvi" ~ "ciopo",
      giocatore == "Alan Saggiorato" ~ "alan",
      giocatore == "Alberto Amato" ~ "albi",
      giocatore == "Nicole Biondini" ~ "niki",
      giocatore == "Angela Augello" ~ "angi",
      giocatore == "Laura Lucchiari" ~ "laura",
      giocatore == "Laura Lucchiari" ~ "laura",
      giocatore == "Laura Lucchiari" ~ "laura",
      TRUE ~ giocatore
    ))
  
}

rcbs_to_nomi <- function(data){
  data %>% 
    mutate(giocatore = case_when(
      giocatore ==  "borso"~"Andrea Borsotti" ,
      giocatore ==  "zeta"~"Alessandro Zanellato" ,
      giocatore ==  "pare"~"Mattia Pareschi" ,
      giocatore ==  "greis" ~"Federico Grazi" ,
      giocatore ==  "luca"~"Luca Tosatti" ,
      giocatore == "fabia"~"fabia canu" ,
      giocatore ==  "tobs"~"Tobia Sermenghi" ,
      giocatore ==  "tommi"~"Tommaso Marzocchi" ,
      giocatore == "jannik"~"Jannik Tiemann" ,
      giocatore == "greek"~"Davide Greco" ,
      giocatore == "irene"~ "Irene Stranieri",
      giocatore == "Luca Pallotta" ~ "pallot",
      giocatore ==  "ciopo"~"Giovanni Calvi",
      giocatore == "alan"~"Alan Saggiorato" ,
      giocatore == "albi"~"Alberto Amato" ,
      giocatore == "niki"~ "Nicole Biondini" ,
      giocatore ==  "angi"~"Angela Augello",
      giocatore ==  "laura"~"Laura Lucchiari" ,
      giocatore ==  "chellogs"~"Marco Checchi" ,
      giocatore ==  "emil"~"Emil Nargaard Maracci" ,
      TRUE ~ giocatore
    ))
  
}

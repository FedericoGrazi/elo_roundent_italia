elo_match_rcb <- function(data, elo_ratings = NULL, coeff = 32, elo_base = 1000, diff_max = 0.92,win_ass = 400){
  library(tidyverse)

  is.integer0 <- function(x)
  {
    is.integer(x) && length(x) == 0L
  }
  
  if(is.null(elo_ratings)){
    elo_ratings <- data.frame(giocatore = as.character(data[1,1]), nuovo_elo = elo_base)
  }else if(ncol(elo_ratings) == 2){
    elo_ratings <- elo_ratings
  }else if(ncol(elo_ratings)>2){
    elo_ratings <- elo_ratings[,c(1,3)]
  }else{
    print("Controllare dimensioni e formato degli elo inseriti")
    break()
  }
  
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
  
 
  
  c <- matrix(0,nrow(potenza),1)
  for(i in 1:nrow(potenza)){
    
    c[i] <- ifelse(is.integer0(which(elo_ratings[1]==as.character(potenza[i,1])))==T,NA,which(elo_ratings[1]==as.character(potenza[i,1])))
    
  }
  
  potenza_to_add <- potenza[which(is.na(c)),] %>% 
    mutate(nuovo_elo = elo_base) %>% 
    select(giocatore,nuovo_elo)
  
  elo_ratings_full <-  suppressMessages( full_join(potenza_to_add,elo_ratings) ) 
  
  
  rob <- data[-nrow(data),] %>% 
    mutate(perc1 = 1,perc2 = 3,perc3=4,perc4=5)
  

    for(i in 1:nrow(data[-nrow(data),])){
      for(j in 1:4){
        a <- data[i,j]
        indent <- which(elo_ratings_full[,1] == as.character(a))
        
        rob[i,j+6] <- elo_ratings_full$nuovo_elo[indent] 
      }
    }
  # }
 
 last <-  rob %>%
   mutate(game = 1:nrow(rob)) %>% 
   group_by(game) %>% 
    mutate(
      # pot1 = perc1+perc2,
      # pot2 = perc3+perc4,
      pot1 = mean(c(perc1,perc2)),
      pot2 = mean(c(perc3,perc4)),
      # per_rel1 = perc1/pot1,
      # per_rel2 = perc2/pot1,
      # per_rel3 = perc3/pot2,
      # per_rel4 = perc4/pot2,
      per_rel1 = perc1/(2*pot1),
      per_rel2 = perc2/(2*pot1),
      per_rel3 = perc3/(2*pot2),
      per_rel4 = perc4/(2*pot2),
      expA = 1/(1+10^((pot2-pot1)/win_ass)),
      expB = 1/(1+10^((pot1-pot2)/win_ass)),
      diffA = case_when(
        w1-expA > diff_max ~ diff_max,
        w1-expA < 1- diff_max ~ 1- diff_max,
        TRUE ~ w1-expA),
      diffB = case_when(
        w1-expA > diff_max ~ 1-diff_max,
        w1-expA < 1- diff_max ~ diff_max,
        TRUE ~ w2-expB),
      eloA = 2*coeff*diffA,
      eloB = 2*coeff*diffB,
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
    summarize(elo_dell_evento = sum(punt)) %>% 
    arrange(-elo_dell_evento) %>% 
   rename(giocatore = value)
 
 final = suppressMessages( full_join(last,elo_ratings_full) %>% 
                             group_by(giocatore) %>% 
   mutate(elo_dell_evento = ifelse(is.na(elo_dell_evento),0,elo_dell_evento),
          nuovo_elo = nuovo_elo + elo_dell_evento,
          nuovo_elo = ifelse(nuovo_elo<900, 900, nuovo_elo)) %>% 
   arrange(-nuovo_elo) %>% 
     ungroup
 )
 
 
  print(final, n = 100)
 return(final)
 
}

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

elo_tornei <-  function(data, elo_ratings = NULL, coeff = 20, elo_base = 1000, diff_max = 0.92,win_ass = 400, conta_tornei_attivo = TRUE, coeff_nuovi = 40){
  library(tidyverse)

  if(is.null(elo_ratings)){
    elo_ratings <- data.frame(giocatore = as.character(data[1,1]), nuovo_elo = elo_base,conta_tornei = 0)
  }else if(ncol(elo_ratings) == 2){
    elo_ratings <- elo_ratings
  }else if(ncol(elo_ratings)==3){
    elo_ratings <- elo_ratings[,c(1,3)]
  }else if(ncol(elo_ratings)==4){
    elo_ratings <- elo_ratings[,c(1,3,4)]
  }else{
    print("Controllare dimensioni e formato degli elo inseriti")
    break()
  }
  

  data <- data %>% 
    mutate(tot_set = w1+w2,
           w1 = case_when(
             w1/tot_set == 2/3 ~ 0.75,
             w1/tot_set == 1/3 ~ 0.25,
             TRUE ~ w1/tot_set
           ),
           w2 = case_when(
             w2/tot_set == 2/3 ~ 0.75,
             w2/tot_set == 1/3 ~ 0.25,
             TRUE ~ w2/tot_set
           ),
           w = case_when(
             tot_set == 1~.75,
             TRUE ~1
           )) %>% 
    select(-tot_set)
  
  w <- data %>% 
    mutate(game = 1:nrow(data)) %>% 
    select(game, w)
  
  data <- data %>% select(-w)
  
  partecipanti <- data %>% 
    pivot_longer(names_to = "squadra",values_to = "giocatore", cols = g1:g4) %>% 
    group_by(giocatore) %>% 
    summarize(n=n()) %>% 
    mutate(partecipante = 0) %>% 
    select(-n)
  
  elo_ratings_full <- suppressMessages( full_join(elo_ratings,partecipanti) %>% 
    mutate(
      nuovo_elo = ifelse(is.na(nuovo_elo),elo_base, nuovo_elo),
      conta_tornei = ifelse(is.na(conta_tornei),0, conta_tornei),
           ) %>% 
    select(-partecipante) )
  
  rob <- data %>% 
    mutate(perc1 = 1,perc2 = 3,perc3=4,perc4=5,
           ct1 = 1, ct2 = 2, ct3 = 3, ct4 = 4)
  

  for(i in 1:nrow(data)){
    for(j in 1:4){
      a <- data[i,j]
      indent <- which(elo_ratings_full[,1] == as.character(a))
      
      rob[i,j+6] <- elo_ratings_full$nuovo_elo[indent] 
      rob[i,j+10] <- elo_ratings_full$conta_tornei[indent]
      
    }
  }

  last <-  rob %>% 
    mutate(game = 1:nrow(rob),
           w = w$w) %>% 
    group_by(game) %>% 
    mutate(
      pot1 = mean(c(perc1,perc2)),
      pot2 = mean(c(perc3,perc4)),
      expA = 1/(1+10^((pot2-pot1)/win_ass)),
      expB = 1/(1+10^((pot1-pot2)/win_ass)),
      diffA = case_when(
        expA > diff_max ~ w1 - diff_max,
        expA < 1- diff_max ~ w1- (1- diff_max),
        TRUE ~ w1-expA),
      diffB = case_when(
        expB > diff_max ~ w2 - diff_max,
        expB < 1- diff_max ~ w2 - (1-diff_max),
        TRUE ~ w2-expB),
      eloA1 = case_when( ct1<4~ w * coeff_nuovi * diffA, perc1 >2400 ~ w * 10 *diffA, TRUE ~ w * coeff * diffA),
      eloA2 = case_when( ct2<4~ w * coeff_nuovi * diffA, perc2 >2400 ~ w * 10 *diffA, TRUE ~ w * coeff * diffA),
      eloB3 = case_when( ct3<4~ w * coeff_nuovi * diffB, perc3 >2400 ~ w * 10 *diffB, TRUE ~ w * coeff * diffB),
      eloB4 = case_when( ct4<4~ w * coeff_nuovi * diffB, perc4 >2400 ~ w * 10 *diffB, TRUE ~ w * coeff * diffB)
    ) %>% 
    ungroup %>% 
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
    summarize(elo_dell_evento = sum(punt)) %>% 
    arrange(-elo_dell_evento) %>% 
    rename(giocatore = value)
  
  final = suppressMessages( full_join(last,elo_ratings_full) %>% 
    group_by(giocatore) %>% 
    mutate(elo_dell_evento = ifelse(is.na(elo_dell_evento),0,elo_dell_evento),
           nuovo_elo = nuovo_elo + elo_dell_evento,
           nuovo_elo = ifelse(nuovo_elo<900, 900, nuovo_elo)) %>% 
    arrange(-nuovo_elo) %>% 
    mutate(conta_tornei = case_when(
      isFALSE(conta_tornei_attivo) ~ conta_tornei,
      isTRUE(conta_tornei_attivo) & elo_dell_evento != 0 ~ conta_tornei +1, 
      isTRUE(conta_tornei_attivo) & elo_dell_evento == 0 ~ conta_tornei, 
    )
  ) %>% ungroup %>% 
    filter(giocatore != "nessuno")
  )
  
  print(final, n= 100)
  return(final)
}

nomi_to_rcbs <- function(data){
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

merge_elo_ratings <- function(elo1,elo2){
  
  elo1_chg <- elo1 %>% 
    rename(tornei_elo1 = conta_tornei,
           elo1 = nuovo_elo,
           ede1 = elo_dell_evento)
  elo2_chg <- elo2 %>% 
    rename(tornei_elo2 = conta_tornei,
           elo2 = nuovo_elo,
           ede2 = elo_dell_evento)
  
  
  elo_tot <- suppressMessages( full_join(elo1_chg,elo2_chg) %>% 
    mutate(conta_tornei = case_when(
      is.na(tornei_elo2) ~ tornei_elo1,
      is.na(tornei_elo1) ~ tornei_elo2,
      tornei_elo1>tornei_elo2 ~ tornei_elo1,
      tornei_elo2>tornei_elo1 ~ tornei_elo2,
      TRUE ~ tornei_elo1
    ),
    nuovo_elo = case_when(
      is.na(elo1) ~ elo2,
      is.na(elo2) ~ elo1,
      tornei_elo1>tornei_elo2 ~ elo1,
      tornei_elo2>tornei_elo1 ~ elo2,
      TRUE ~ elo1
    ), 
    elo_dell_evento = case_when(
      is.na(elo1) ~ ede2,
      is.na(elo2) ~ ede1,
      tornei_elo1>tornei_elo2 ~ ede1,
      tornei_elo2>tornei_elo1 ~ ede2,
      TRUE ~ elo1
    )) %>% 
    select(giocatore, elo_dell_evento,nuovo_elo, conta_tornei)
  )
  
  return(elo_tot)
}

extract_elo_table <-  function(data, elo_ratings = NULL, coeff = 20, elo_base = 1000, diff_max = 0.92,win_ass = 400, conta_tornei_attivo = TRUE, coeff_nuovi = 40){
  
library(tidyverse)

if(is.null(elo_ratings)){
  elo_ratings <- data.frame(giocatore = as.character(data[1,1]), nuovo_elo = elo_base,conta_tornei = 0)
}else if(ncol(elo_ratings) == 2){
  elo_ratings <- elo_ratings
}else if(ncol(elo_ratings)==3){
  elo_ratings <- elo_ratings[,c(1,3)]
}else if(ncol(elo_ratings)==4){
  elo_ratings <- elo_ratings[,c(1,3,4)]
}else{
  print("Controllare dimensioni e formato degli elo inseriti")
  break()
}


  
  
  data <- data %>% 
    mutate(tot_set = w1+w2,
           w1 = case_when(
             w1/tot_set == 2/3 ~ 0.75,
             w1/tot_set == 1/3 ~ 0.25,
             TRUE ~ w1/tot_set
           ),
           w2 = case_when(
             w2/tot_set == 2/3 ~ 0.75,
             w2/tot_set == 1/3 ~ 0.25,
             TRUE ~ w2/tot_set
           ),
           w = case_when(
             tot_set == 1~.75,
             TRUE ~1
           )) %>% 
    select(-tot_set)
  

w <- data %>% 
  mutate(game = 1:nrow(data)) %>% 
  select(game, w)

data <- data %>% select(-w)

partecipanti <- data %>% 
  pivot_longer(names_to = "squadra",values_to = "giocatore", cols = g1:g4) %>% 
  group_by(giocatore) %>% 
  summarize(n=n()) %>% 
  mutate(partecipante = 0) %>% 
  select(-n)

elo_ratings_full <- suppressMessages( full_join(elo_ratings,partecipanti) %>% 
                                        mutate(
                                          nuovo_elo = ifelse(is.na(nuovo_elo),elo_base, nuovo_elo),
                                          conta_tornei = ifelse(is.na(conta_tornei),0, conta_tornei),
                                        ) %>% 
                                        select(-partecipante) )

rob <- data %>% 
  mutate(perc1 = 1,perc2 = 3,perc3=4,perc4=5,
         ct1 = 1, ct2 = 2, ct3 = 3, ct4 = 4)


for(i in 1:nrow(data)){
  for(j in 1:4){
    a <- data[i,j]
    indent <- which(elo_ratings_full[,1] == as.character(a))
    
    rob[i,j+6] <- elo_ratings_full$nuovo_elo[indent] 
    rob[i,j+10] <- elo_ratings_full$conta_tornei[indent]
    
  }
}

last <-  rob %>% 
  mutate(game = 1:nrow(rob),
         w = w$w) %>% 
  group_by(game) %>% 
  mutate(
    pot1 = mean(c(perc1,perc2)),
    pot2 = mean(c(perc3,perc4)),
    expA = 1/(1+10^((pot2-pot1)/win_ass)),
    expB = 1/(1+10^((pot1-pot2)/win_ass)),
    diffA = case_when(
      expA > diff_max ~ w1 - diff_max,
      expA < 1- diff_max ~ w1- (1- diff_max),
      TRUE ~ w1-expA),
    diffB = case_when(
      expB > diff_max ~ w2 - diff_max,
      expB < 1- diff_max ~ w2 - (1-diff_max),
      TRUE ~ w2-expB),
    eloA1 = case_when( ct1<4~ w * coeff_nuovi * diffA, perc1 >2400 ~ w * 10 *diffA, TRUE ~ w * coeff * diffA),
    eloA2 = case_when( ct2<4~ w * coeff_nuovi * diffA, perc2 >2400 ~ w * 10 *diffA, TRUE ~ w * coeff * diffA),
    eloB3 = case_when( ct3<4~ w * coeff_nuovi * diffB, perc3 >2400 ~ w * 10 *diffB, TRUE ~ w * coeff * diffB),
    eloB4 = case_when( ct4<4~ w * coeff_nuovi * diffB, perc4 >2400 ~ w * 10 *diffB, TRUE ~ w * coeff * diffB)
  ) 

return(View(last))
}

insert_elo <- function(data, elo_ratings, plus = 4 ){
  
  data <- data %>% mutate(game = 1:nrow(data)) 
  
  
  rob <- data %>% 
    mutate(perc1 = 1,perc2 = 3,perc3=4,perc4=5)
  
  
  for(i in 1:nrow(data)){
    for(j in 1:4){
      a <- data[i,j]
      indent <- which(elo_ratings[,1] == as.character(a))
      
      rob[i,j+plus] <- elo_ratings$nuovo_elo[indent] 
      
      
    }
  }
  
  rob <- rob %>% 
    mutate(game = 1:nrow(rob)) %>% 
    group_by(game) %>% 
    mutate(
      pot1 = mean(c(perc1,perc2)),
      pot2 = mean(c(perc3,perc4)),
      expA = case_when(
        1/(1+10^((pot2-pot1)/win_ass)) > diff_max ~diff_max ,
        1/(1+10^((pot2-pot1)/win_ass)) <1- diff_max ~1-diff_max,
        TRUE ~ 1/(1+10^((pot2-pot1)/win_ass))))
  
    return(rob)
  
}
create_players <- function(data){
  data %>% 
  transmute(
    g1 = sub(" &.*", "", V1),
    g2 = sub(".*& ", "", V1),
    g3 = sub(" &.*", "", V2),
    g4 = sub(".*& ", "", V2))
}
create_teams <- function(data){
  data %>% 
    mutate(
      t1 =  paste(g1,g2,sep = " & "),
      t2 =  paste(g3,g4,sep = " & ")
    )}
simul_bracket <- function(new_def){
  for(i in 1:nrow(new_def)){
    w1t = 0
    w2t = 0
    n = 0
    while(n<3){
      w1 = rbinom(1,1,new_def$expA[i])
      w2 = 1-w1
      
      w1t = w1t+w1
      w2t = w2t+w2
      n = n+1
      
    }
    new_def$w[i] <-  case_when(w1t > w2t ~1,
                               TRUE ~2)
  }
  return(new_def)
}
create_bracket <- function(last, g1 = c(1,8,4,5,2,7,3,6),g2 = c(16,9,13,12,15,10,14,11),rank =  c(1,8,4,5,2,7,3,6)){
  
  brack1 <-  suppressMessages( tibble(g1 ,g2 ) %>% 
                                 left_join(last, by = c("g1"= "rank")) %>% 
                                 rename(V1 = "value") %>% 
                                 left_join(last, by = c("g2"= "rank")) %>% 
                                 rename(V2 = "value") %>% 
                                 select(V1,V2) )
  
  new_def <- insert_elo(create_players(brack1),elo_ratings,plus = 5) %>% 
    mutate(w = 0) %>% 
    simul_bracket()
  
  win_br1= tibble(value = "A")
  for(i in 1:nrow(brack1)){
    win_br1= win_br1 %>% add_row(value = as.character(brack1[i,new_def$w[i]])) %>% filter(value !="A")
  }
  win_br1 = win_br1 %>% 
    mutate(rank )
  
  return(win_br1)
  
}

simul_torneo <-  function(data, elo_ratings = NULL,win_ass = 400,diff_max = 0.92){
  library(tidyverse)
  elo_ratings <<- elo_ratings[,c(1,3,4)]
  gir <-
  data %>% 
    mutate(rwo  = 1:nrow(simul)) %>% 
    group_by(rwo)%>%
    mutate(elo = mean(c(e1,e2))) %>% 
    ungroup %>% 
    mutate(era = rank(-elo)) %>% 
    arrange(era) %>% 
    mutate(gironi = rep(c(1:4,4:1),times = 2)) %>% 
    select(g1,g2,elo,gironi) %>% 
    group_by(gironi) %>% 
    mutate(team = paste(g1,g2,sep = " & "))
  
  b = tibble(g1 = "a",g2="a",g3="a",g4="a")
  for(i in 1:4){
    
    sgir = gir %>% filter(gironi == i)
    a = combinations(n = 4, r = 2, v =sgir$team, repeats.allowed = F)  %>% as_tibble %>% create_players
    b = b %>% add_row(a) %>% filter(g1 != "a")
    
  }
  

  
  last <-
    suppressMessages( insert_elo(b,elo_ratings, plus = 5) %>% 
    mutate(
      w1 = rbinom(1,1,expA),
      w2 = 1-w1,
      p = case_when(
        w1 == 1 ~rnbinom(1,21,expA)+21,
        TRUE ~rnbinom(1,21,1-expA)+21
      ),
      p1 = case_when(
        w1 == 1 ~21,
        TRUE ~p-21
      ),
      p2 = case_when(
        w2 == 1 ~21,
        TRUE ~p-21
      )      ) %>% 
    select(-game:-pot2) %>% 
    ungroup %>%  
    mutate(gir = rep(c(1:4),each = 6)) %>% 
    mutate(
      t1 =  paste(g1,g2,sep = " & "),
      t2 =  paste(g3,g4,sep = " & "),
    ) %>% 
    select(t1,t2,w1,w2,p1,p2) %>% 
    pivot_longer(cols = t1:t2) %>% 
    mutate(w = case_when(
      name == "t1" & w1 == 0 ~0,
      name == "t1" & w1 == 1 ~1,
      name == "t2" & w1 == 1 ~0,
      name == "t2" & w1 == 0 ~1
    ),
    diff = case_when(
      name == "t1" ~ p1-p2,
      TRUE ~p2-p1
    )) %>% 
    select(value, w,diff) %>% 
    group_by(value) %>% 
    summarize(wins = sum(w),
              diff = sum(diff)) %>% 
    arrange(-wins,-diff) %>% 
    mutate(rank = 1:16)
  )
  
  br2 = create_bracket(last)
  br3 = create_bracket(br2,g1 = c(1,4,2,3),g2 = c(8,5,7,6),rank = c(1,4,2,3))
  br4 = create_bracket(br3,g1 = c(1,4),g2 = c(2,3),rank = c(1,2))
  br5 = create_bracket(br4,g1 = c(1),g2 = c(2),rank = c(1))
  
  winner = br5$value
  
  return(winner)
}

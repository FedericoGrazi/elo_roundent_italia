
elo_tornei <-  function(data, elo_ratings = NULL, coeff = 20, elo_base = 1000, diff_max = 0.92,win_ass = 400, conta_tornei_attivo = TRUE, coeff_nuovi = 40){
  
  library(tidyverse)

  partecipanti <- sort(unique(c(data$g1,data$g2,data$g3,data$g4)))
   
  
  if (!is.null(elo_ratings)) {
    ckeep <-  c("giocatore","nuovo_elo","conta_tornei")
    elo_ratings = as.data.frame(elo_ratings[,ckeep])
    npadd <- partecipanti[!(partecipanti %in% elo_ratings$giocatore)]
    zv <- rep(0, length(npadd))
    
    new_rank <- data.frame(giocatore = npadd,
                           nuovo_elo = rep(elo_base, length(npadd)),
                           conta_tornei = rep(0, length(npadd)))
    
    if (!("nuovo_elo" %in% names(elo_ratings))) 
      elo_ratings <- cbind(elo_ratings, nuovo_elo = 0)
    if (!("conta_tornei" %in% names(elo_ratings))) 
      elo_ratings <- cbind(elo_ratings, conta_tornei = 0)
    
    elo_ratings <- as_tibble(rbind(elo_ratings, new_rank))
    
  }else {
    elo_ratings <- tibble(
      giocatore = partecipanti,
      nuovo_elo = elo_base,
      conta_tornei = 0
    )
  }
  
  totset = data$w1 + data$w2
  
  data$w1 = case_when(
    data$w1/totset == 2/3 ~.75,
    data$w1/totset == 1/3 ~.25,
    TRUE ~ data$w1/totset)
  
  data$w2 = case_when(
    data$w2/totset == 2/3 ~.75,
    data$w2/totset == 1/3 ~.25,
    TRUE ~ data$w2/totset)

    plindex = match(partecipanti, elo_ratings$giocatore)
    
    onplay = elo_ratings$giocatore[plindex]
    outplay = elo_ratings$giocatore[-plindex]
    onrat = elo_ratings$nuovo_elo[plindex]
    outrat = elo_ratings$nuovo_elo[-plindex]
    onct = elo_ratings$conta_tornei[plindex]
    outct = elo_ratings$conta_tornei[-plindex]
  
    
    g1 = match(data$g1,elo_ratings$giocatore)
    g2 = match(data$g2,elo_ratings$giocatore)
    g3 = match(data$g3,elo_ratings$giocatore)
    g4 = match(data$g4,elo_ratings$giocatore)
    
    data$elo1 = elo_ratings$nuovo_elo[g1]
    data$elo2 = elo_ratings$nuovo_elo[g2]
    data$elo3 = elo_ratings$nuovo_elo[g3]
    data$elo4 = elo_ratings$nuovo_elo[g4]
    
    data$ct1 = elo_ratings$conta_tornei[g4]
    data$ct2 = elo_ratings$conta_tornei[g4]
    data$ct3 = elo_ratings$conta_tornei[g4]
    data$ct4 = elo_ratings$conta_tornei[g4]
    
    data$game = 1:nrow(data)
    data$w = ifelse(totset == 1,.75,1)


    

  last <-  data %>% 
    group_by(game) %>% 
    mutate(
      telo1 = mean(c(elo1,elo2)),
      telo2 = mean(c(elo3,elo4)),
      
      expA = 1/(1+10^((telo2-telo1)/win_ass)),
      expB = 1/(1+10^((telo1-telo2)/win_ass)),
      
      diffA = case_when(
        expA > diff_max ~ w1 - diff_max,
        expA < 1- diff_max ~ w1- (1- diff_max),
        TRUE ~ w1-expA),
      diffB = case_when(
        expB > diff_max ~ w2 - diff_max,
        expB < 1- diff_max ~ w2 - (1-diff_max),
        TRUE ~ w2-expB),
      eloA1 = case_when( ct1<4~ w * coeff_nuovi * diffA, elo1 >2400 ~ w * 10 *diffA, TRUE ~ w * coeff * diffA),
      eloA2 = case_when( ct2<4~ w * coeff_nuovi * diffA, elo2 >2400 ~ w * 10 *diffA, TRUE ~ w * coeff * diffA),
      eloB3 = case_when( ct3<4~ w * coeff_nuovi * diffB, elo3 >2400 ~ w * 10 *diffB, TRUE ~ w * coeff * diffB),
      eloB4 = case_when( ct4<4~ w * coeff_nuovi * diffB, elo4 >2400 ~ w * 10 *diffB, TRUE ~ w * coeff * diffB)
    ) %>% 
    ungroup %>% 
    pivot_longer(cols = g1:g4, values_to = "giocatore") %>% 
    mutate(punt = case_when(
      name == "g1" ~ eloA1,
      name == "g2" ~ eloA2,
      name == "g3" ~ eloB3,
      name == "g4" ~ eloB4,
    )) %>% 
    group_by(giocatore) %>% 
    summarize(elo_dell_evento = sum(punt)) 
  
    last = last[order(last$giocatore),]
    
    if(conta_tornei_attivo == T) onct <- onct +1 
  
    elo_ratings = tibble(
      giocatore = c(onplay,outplay),
      elo_dell_evento = c(round(last$elo_dell_evento,0), rep(0,length(outplay))),
      nuovo_elo = c(onrat+last$elo_dell_evento,outrat),
      conta_tornei = c(onct, outct)
    )
    
    elo_ratings$nuovo_elo = ifelse(elo_ratings$nuovo_elo<900,900,elo_ratings$nuovo_elo)
    elo_ratings = arrange(elo_ratings,-nuovo_elo)
  
  
  print(elo_ratings, n= 100)
  return(elo_ratings)
}

merge_elo_ratings <- function(elo1,elo2){
  
  elo1_chg <- elo1 %>% 
    transmute(
      giocatore = giocatore,
      elo1 = nuovo_elo,
      tornei_elo1 = conta_tornei
      )
  
  elo2_chg <- elo2 %>% 
    transmute(
      giocatore = giocatore,
      elo2 = nuovo_elo,
      tornei_elo2 = conta_tornei
      )
  
  
  elo_tot <- suppressMessages( full_join(elo1_chg,elo2_chg) %>% 
    group_by(giocatore) %>% 
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
    )) %>% 
    select(giocatore,nuovo_elo, conta_tornei)
  )
  
  elo_tot = arrange(elo_tot,-nuovo_elo)
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

return(last)
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
insert_elo <- function(data,elo_ratings, plus = 5,win_ass = 400, diff_max = .92 ){
  
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

simul_torneo <-  function(data, elo_ratings,win_ass = 400,diff_max = 0.92){
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
    suppressMessages( 
      insert_elo(b, elo_ratings,plus = 5) %>% 
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
  rm(elo_ratings)
  
  return(winner)
}


source("D:/Personal Statistics/rcb/elo totale.R", echo=F)


## KOTC (ELO RCB) ####

rcb_players <- c("borso","pare","greis","zeta","niki","laura","fulvs","ciopo",
                 "tobs","angi","alan","albi","fabia","nik","victor","jannik","luca",
                 "greek","irene","tommi")

elo_input_per_rcb1 <-nomi_to_rcbs(elo_post_mixed) %>%
  filter(giocatore %in% rcb_players)

kotc_r <- read_excel("D:/Personal Statistics/rcb/ranking_kotc.xlsx")

elo_post_kotc <- elo_match_rcb(kotc_r,elo_input_per_rcb1,coeff = 10,elo_base = 1000)

punti_evento(kotc_r, pnt_base = 0)

## MATCH PENSATI (ELO RCB) ####

matpens <- read_excel("D:/Personal Statistics/rcb/ranking_matchpens.xlsx")

elo_post_matpens <- elo_match_rcb(matpens,elo_post_kotc,coeff = 10,elo_base = 1000)

punti_evento(matpens, pnt_base = 0)

## MATCH NO SERVES (ELO RCB) ####

torn <- read_excel("D:/Personal Statistics/rcb/rank_torneino_221223.xlsx")

elo_post_torn <- elo_match_rcb(torn,elo_post_matpens,coeff = 10,elo_base = 1000)

punti_evento(torn, 80, 10)

write.xlsx(elo_post_torn,"D:/Personal Statistics/rcb/elo_rcb.xlsx")


inner_join(rename(rcbs_to_nomi(elo_post_torn), elo_rcb = nuovo_elo), elo_post_mixed,by = "giocatore") %>%
  group_by(giocatore) %>%
  transmute( diff_elo = elo_rcb-nuovo_elo)

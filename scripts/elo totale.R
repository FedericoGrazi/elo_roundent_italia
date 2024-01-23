library(readxl)
library(tidyverse)
library(xlsx)

# Punti evento del ranking vanno calcolati sulla base della potenza del torneo stesso

# Punti elo possono essere usati richiamando gli elo già utilizzati, in maniera tale
# da avere un sistema continuativo

## LAURA LUCCHIARI è `laura`
## FULVIO FACCHINI è `fulvs`
## NICOLE BIONDINI è `niki`

## K = 40 per INIZIALIZZAZIONE
## K = 20 per TORNEI BASE
## K = QUELLO CHE VOGLIO IO PER RCB

## ELO BASE 1000 PER WOMEN
## ELO BASE 1200 PER OPEN
## ELO BASE 1300 PER OPEN POWER POOL

source("D:/Personal Statistics/rcb/function_elo.R")
elo_sesso <- read_excel("D:/Personal Statistics/rcb/genere_elo_ir.xlsx") 

ir_open_gir <- read_excel("D:/Personal Statistics/rcb/ir_pt.xlsx", sheet = "open_ir") %>% filter(cat == "gir") %>% select(-cat)
ir_open_bra <- read_excel("D:/Personal Statistics/rcb/ir_pt.xlsx", sheet = "open_ir") %>% filter(cat == "bra") %>% select(-cat)
ir_femm_gir <- read_excel("D:/Personal Statistics/rcb/ir_pt.xlsx", sheet = "femminile_ir") %>% filter(cat == "gir") %>% select(-cat)
ir_femm_bra <- read_excel("D:/Personal Statistics/rcb/ir_pt.xlsx", sheet = "femminile_ir") %>% filter(cat == "bra") %>% select(-cat)
ir_mixed <- read_excel("D:/Personal Statistics/rcb/ir_pt.xlsx", sheet = "misto_ir") 
hot_mil_open <- read_excel("D:/Personal Statistics/rcb/hot_mil.xlsx", sheet = "open_mil") 
hot_mil_femm <- read_excel("D:/Personal Statistics/rcb/hot_mil.xlsx", sheet = "femminile") 
hot_mil_mix <- read_excel("D:/Personal Statistics/rcb/hot_mil.xlsx", sheet = "misto") 
nat_forli_pow <- read_excel("D:/Personal Statistics/rcb/Nationals.xlsx", sheet = "nomi_giusti") %>% filter(sit == "pow") %>% select(-sit)
nat_forli_ene <- read_excel("D:/Personal Statistics/rcb/Nationals.xlsx", sheet = "nomi_giusti") %>% filter(sit == "ene") %>% select(-sit)
nat_forli_bra <- read_excel("D:/Personal Statistics/rcb/Nationals.xlsx", sheet = "nomi_giusti") %>% filter(sit == "bra") %>% select(-sit)
nat_forli_mixed_pow <- read_excel("D:/Personal Statistics/rcb/Nationals.xlsx", sheet = "misto")%>% filter(sit == "pow") %>% select(-sit)
nat_forli_mixed_ene <- read_excel("D:/Personal Statistics/rcb/Nationals.xlsx", sheet = "misto")%>% filter(sit == "ene") %>% select(-sit)
nat_forli_mixed_bra <- read_excel("D:/Personal Statistics/rcb/Nationals.xlsx", sheet = "misto")%>% filter(sit == "bra") %>% select(-sit)
open_fw <-  read_excel("D:/Personal Statistics/rcb/ranking_winterspecial.xlsx", sheet = "nomi_giusti") %>% filter(cat == "Open") %>% select(-cat)
femm_xl <- read_excel("D:/Personal Statistics/rcb/femminile.xlsx", sheet = "nomi_giusti") %>% select(-P1,-P2)
mixed_fw <-  read_excel("D:/Personal Statistics/rcb/ranking_winterspecial.xlsx", sheet = "nomi_giusti") %>% filter(cat == "mixed") %>% select(-cat)


  
## ITALROUNDNET MAGGIO  #####

elo_open_ir_bolo_gir <- elo_tornei(ir_open_gir, elo_base = 1200,conta_tornei_attivo = F)
elo_open_ir_bolo_bra <- elo_tornei(ir_open_bra,elo_open_ir_bolo_gir, elo_base = 1200)
elo_open_ir_bolo_bra <- rcbs_to_nomi(elo_open_ir_bolo_bra)


elo_femm_ir_bolo_gir <- elo_tornei(ir_femm_gir, elo_base = 1000,conta_tornei_attivo = F)
elo_femm_ir_bolo_bra <- elo_tornei(ir_femm_bra,elo_femm_ir_bolo_gir)

# elo_femm_ir_bolo_bra <- elo_femm_ir_bolo_bra %>% filter(giocatore != "Chiara Pernigo")
elo_post_maggio <- merge_elo_ratings(elo_femm_ir_bolo_bra,elo_open_ir_bolo_bra)

elo_post_mix_ir_bolo <- elo_tornei(ir_mixed,elo_post_maggio, elo_base = 1000)

## MILANO LUGLIO

elo_post_milOPEN <- elo_tornei(hot_mil_open, elo_post_mix_ir_bolo, elo_base = 1200)
elo_post_milFEMM <- elo_tornei(hot_mil_femm, elo_post_milOPEN, elo_base = 1000)
elo_post_milMIX <- elo_tornei(hot_mil_mix, elo_post_milFEMM, elo_base = 1000)

## NATIONALS FORLI ####

nat_elo_gir_pow <- elo_tornei(nat_forli_pow,elo_post_milMIX, elo_base = 1250, conta_tornei_attivo = F)
nat_elo_gir_ene <- elo_tornei(nat_forli_ene,elo_post_milMIX, elo_base = 1150, conta_tornei_attivo = F)
nat_elo_bra <- elo_tornei(nat_forli_bra, merge_elo_ratings(nat_elo_gir_pow,nat_elo_gir_ene), elo_base = 1200)

## NATIONALS FORLI MIXED ####

elo_post_nat_mixed_pow <- elo_tornei(nat_forli_mixed_pow,nat_elo_bra, coeff_nuovi = 40, coeff = 20, elo_base = 1000,conta_tornei_attivo = F)
elo_post_nat_mixed_ene <- elo_tornei(nat_forli_mixed_ene,nat_elo_bra, coeff_nuovi = 40, coeff = 20, elo_base = 1000,conta_tornei_attivo = F)
elo_post_nat_mixed_bra <- elo_tornei(nat_forli_mixed_bra,merge_elo_ratings(elo_post_nat_mixed_ene,elo_post_nat_mixed_pow),, coeff_nuovi = 40, coeff = 20, elo_base = 1000)

## WINTER SPECIAL #####
elo_post_open <- elo_tornei(open_fw, elo_post_nat_mixed_bra, elo_base = 1150, diff_max = 0.92) 
elo_post_femminile <- elo_tornei(femm_xl, elo_post_open, elo_base = 1075, coeff_nuovi = 20)
elo_post_mixed <- elo_tornei(mixed_fw,elo_post_femminile, coeff_nuovi = 40, coeff = 20, elo_base = 1000 ) 
  
# 
# write.xlsx(full_join(elo_post_mixed,elo_sesso) %>% filter(conta_tornei >1),"D:/Personal Statistics/rcb/elo_italroundnet.xlsx")
# 
# history <- full_join(elo_open_ir_bolo_bra, elo_femm_ir_bolo_bra, by = c("giocatore", "nuovo_elo")) %>% 
#   rename(`IR Bologna 1` = "nuovo_elo") %>% 
#   full_join(elo_post_mix_ir_bolo, by = "giocatore") %>% 
#   rename(`IR Bologna 2` = "nuovo_elo") %>% 
#   full_join(elo_post_milOPEN, by = "giocatore") %>%
#   full_join(elo_post_milFEMM, by = c("giocatore","nuovo_elo")) %>% 
#   rename(`IR Hot Milano 1` = "nuovo_elo") %>% 
#   full_join(elo_post_milMIX, by = "giocatore") %>% 
#   rename(`IR Hot Milano 2` = "nuovo_elo") %>% 
#   full_join(nat_elo_bra, by = "giocatore") %>% 
#   rename(`IR Nationals Open` = "nuovo_elo") %>% 
#   full_join(elo_post_nat_mixed_bra, by = "giocatore") %>% 
#   rename(`IR Nationals Mixed` = "nuovo_elo") %>% 
#   full_join(elo_post_open, by = "giocatore") %>% 
#   full_join(elo_post_femminile, by = c("giocatore","nuovo_elo")) %>% 
#   rename(`Winter Special 1` = "nuovo_elo") %>% 
#   full_join(elo_post_mixed, by = "giocatore") %>% 
#   rename(`Winter Special 2` = "nuovo_elo") %>% 
#   select(-starts_with("conta"),-starts_with("elo")) %>% 
#   arrange(-`Winter Special 2`)
# 
# write.xlsx(history,"D:/Personal Statistics/rcb/andamento.xlsx")
# ########################################################################################################################################
# 
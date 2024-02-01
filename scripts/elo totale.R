library(readxl)
library(tidyverse)
library(xlsx)

## K = 40 per INIZIALIZZAZIONE
## K = 20 per TORNEI BASE

## ELO BASE 1000 PER WOMEN
## ELO BASE 1200 PER OPEN
## ELO BASE 1300 PER OPEN POWER POOL

source("D:/Personal Statistics/rcb/function_elo.R") # Funzioni per l'Elo
source("D:/Personal Statistics/rcb/load_elo.R") # Mi carica i datasets


  
## IR BOLOGNA MAGGIO
# divisione dell'elo in gironi per maggior adattabilità, soprattutto per il primo torneo

elo_open_ir_bolo_gir <- elo_tornei(ir_open_gir, elo_base = 1200,conta_tornei_attivo = F)
elo_open_ir_bolo_bra <- elo_tornei(ir_open_bra,elo_open_ir_bolo_gir, elo_base = 1200)

elo_femm_ir_bolo_gir <- elo_tornei(ir_femm_gir, elo_base = 1000,conta_tornei_attivo = F)
elo_femm_ir_bolo_bra <- elo_tornei(ir_femm_bra,elo_femm_ir_bolo_gir)

elo_post_maggio <- merge_elo_ratings(elo_femm_ir_bolo_bra,elo_open_ir_bolo_bra)

elo_post_mix_ir_bolo <- elo_tornei(ir_mixed,elo_post_maggio, elo_base = 1000)


# IR MILANO LUGLIO

elo_post_milOPEN <- elo_tornei(hot_mil_open, elo_post_mix_ir_bolo, elo_base = 1200)
elo_post_milFEMM <- elo_tornei(hot_mil_femm, elo_post_milOPEN, elo_base = 1000)
elo_post_milMIX <- elo_tornei(hot_mil_mix, elo_post_milFEMM, elo_base = 1000)


# IR FORLI SETTEMBRE
# divisione dell'elo in gironi per maggior adattabilità

nat_elo_gir_pow <- elo_tornei(nat_forli_pow,elo_post_milMIX, elo_base = 1250, conta_tornei_attivo = F)
nat_elo_gir_ene <- elo_tornei(nat_forli_ene,elo_post_milMIX, elo_base = 1150, conta_tornei_attivo = F)
nat_elo_bra <- elo_tornei(nat_forli_bra, merge_elo_ratings(nat_elo_gir_pow,nat_elo_gir_ene), elo_base = 1200)

# Abbiamo pensato che inizializzare Silvia da 1000, come le altre ragazze, implicasse un forte bias di sottostima. 
nat_elo_bra = rbind(nat_elo_bra, tibble(giocatore = "Silvia Zanella",elo_dell_evento = 0,nuovo_elo = 1200, conta_tornei = 0))

# divisione dell'elo in gironi per maggior adattabilità
elo_post_nat_mixed_pow <- elo_tornei(nat_forli_mixed_pow,nat_elo_bra, coeff_nuovi = 40, coeff = 20, elo_base = 1000,conta_tornei_attivo = F)
elo_post_nat_mixed_ene <- elo_tornei(nat_forli_mixed_ene,nat_elo_bra, coeff_nuovi = 40, coeff = 20, elo_base = 1000,conta_tornei_attivo = F)
elo_post_nat_mixed_bra <- elo_tornei(nat_forli_mixed_bra,merge_elo_ratings(elo_post_nat_mixed_ene,elo_post_nat_mixed_pow), coeff_nuovi = 40, coeff = 20, elo_base = 1000)


# WS BOLOGNA OTTOBRE

elo_post_open <- elo_tornei(open_fw, elo_post_nat_mixed_bra, elo_base = 1150, diff_max = 0.92) 
elo_post_femminile <- elo_tornei(femm_xl, elo_post_open, elo_base = 1075, coeff_nuovi = 20)
elo_post_mixed <- elo_tornei(mixed_fw,elo_post_femminile, coeff_nuovi = 40, coeff = 20, elo_base = 1000 ) 
  

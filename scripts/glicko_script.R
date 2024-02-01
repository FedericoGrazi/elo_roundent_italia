library(PlayerRatings)
library(readxl)
library(tidyverse)


source("D:/Personal Statistics/rcb/function_glicko.R")
source("D:/Personal Statistics/rcb/function_elo.R")

data1 <- read_excel("D:/Personal Statistics/rcb/ir_pt.xlsx", sheet = "open_ir")  %>% select(-cat) %>% from_xl_to_glicko(2023,5,28)
data2 <- read_excel("D:/Personal Statistics/rcb/ir_pt.xlsx", sheet = "femminile_ir")  %>% select(-cat) %>% from_xl_to_glicko(2023,5,28)
data3 <- read_excel("D:/Personal Statistics/rcb/ir_pt.xlsx", sheet = "misto_ir")  %>% from_xl_to_glicko(2023,5,28)

data4 <- read_excel("D:/Personal Statistics/rcb/hot_mil.xlsx", sheet = "open_mil") %>% from_xl_to_glicko(2023,7,14)
data5 <- read_excel("D:/Personal Statistics/rcb/hot_mil.xlsx", sheet = "femminile")  %>% from_xl_to_glicko(2023,7,14)
data6 <- read_excel("D:/Personal Statistics/rcb/hot_mil.xlsx", sheet = "misto")  %>% from_xl_to_glicko(2023,7,14)

data7 <- read_excel("D:/Personal Statistics/rcb/Nationals.xlsx", sheet = "nomi_giusti") %>% select(-sit) %>% from_xl_to_glicko(2023,9,30)
data8 <- read_excel("D:/Personal Statistics/rcb/Nationals.xlsx", sheet = "misto") %>% select(-sit) %>%  from_xl_to_glicko(2023,9,30)

data9 <-  read_excel("D:/Personal Statistics/rcb/ranking_winterspecial.xlsx", sheet = "nomi_giusti") %>% filter(cat == "Open") %>% select(-cat) %>%  from_xl_to_glicko(2023,11,4)
data10 <- read_excel("D:/Personal Statistics/rcb/femminile.xlsx", sheet = "nomi_giusti") %>% select(-P1,-P2)%>%  from_xl_to_glicko(2023,11,4)
data11 <-  read_excel("D:/Personal Statistics/rcb/ranking_winterspecial.xlsx", sheet = "nomi_giusti") %>% filter(cat == "mixed") %>% select(-cat)%>%  from_xl_to_glicko(2023,11,4)






# IR BOLOGNA MAGGIO

elo_1 = glick_elo(data1, whichtourn = 1)
elo_2 = glick_elo(data2,init = c(1000,50), status = elo_1$ratings,whichtourn = 1)
elo_3 = glick_elo(data3,init = c(1000,300), status = elo_2$ratings,whichtourn = 2)

# IR MILANO LUGLIO

elo_4 = glick_elo(data4,status = elo_3$ratings,whichtourn = 2)
elo_5 = glick_elo(data5,init = c(1000,100), status = elo_4$ratings,whichtourn = 2)
elo_6 = glick_elo(data6,init = c(1000,300), status = elo_5$ratings,whichtourn = 3)

# IR FORLI SETTEMBRE
silvia  = data.frame(giocatore = "Silvia Zanella",Rating = 1100,Deviation = 100,Games = 0,Win = 0,Loss = 0,nTourn = 0,LastTourn = 17)

elo_7 = glick_elo(data7,init = c(1050,400),status = elo_6$ratings,whichtourn = 4)
elo_8 = glick_elo(data8,init = c(1000,300), status = rbind(elo_7$ratings,silvia) ,whichtourn = 5)

# WS BOLOGNA OTTOBRE

elo_9 = glick_elo(data9,status = elo_8$ratings,whichtourn = 6)
elo_10 = glick_elo(data10,init = c(1000,150), status = elo_9$ratings,whichtourn = 6)
elo_11 = glick_elo(data11,init = c(1000,300), status = elo_10$ratings,whichtourn = 7)


histr = histry_merge(
  list = list(
    elo_1$ratings,
    elo_2$ratings,
    elo_3$ratings,
    elo_4$ratings,
    elo_5$ratings,
    elo_6$ratings,
    elo_7$ratings,
    elo_8$ratings,
    elo_9$ratings,
    elo_10$ratings,
    elo_11$ratings
  ),
  giocatori = elo_11$ratings$giocatore,
  nmin = 6
)

par(mar = c(5,4,4,13))

plot.ratings(histr,lwd = 2 , x= 12.5, y = 1900)




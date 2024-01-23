nomi <- c("Angi","Fabia",	"Niki","GIUDITTA",	"Lalla",	"Irene",	"Alessia",	"Amanda",	"VERO",	"Marti",	"Mari")

library(AlgDesign)
library(tidyverse)
library(ineq)

femminile <- function(cand.list, nTrials, desiredGames){

cl <- tibble(cand.list) 

ind <- cl %>% 
  mutate_all(function(x) as.character(x)) %>% 
  mutate(int = row_number()) %>% 
  filter(
    G1 != G2 , G1 != G3 , G1 != G4 , G1 != G5 , G1 != G6 , G1 != G7 , G1 != G8 ,
    G2 != G3 , G2 != G4 , G2 != G5 , G2 != G6 , G2 != G7 , G2 != G8 ,
    G3 != G4 , G3 != G5 , G3 != G6 , G3 != G7 , G3 != G8 ,
    G4 != G5 , G4 != G6 , G4 != G7 , G4 != G8 ,
    G5 != G6 , G5 != G7 , G5 != G8 ,
    G6 != G7 , G6 != G8 ,
    G7 != G8) %>% 
  select(int) %>% 
  as_vector()


frac_cl <- cand.list[ind,]

partite <- optFederov( ~ ., data = frac_cl, nTrials = nTrials, approximate = FALSE)
uff <- partite$design[sample(desiredGames),]

return(list(uff = uff,
            distrib = uff %>% as_tibble %>% 
              mutate_all(function(x) as.character(x)) %>% 
              gather(key = partita, value = atleta) %>% 
              group_by(atleta) %>% 
              summarize(n())))

}

###Define factor and level in below code
cand.list = expand.grid(G1 = c("Angi","Fabia","Alessia","Lalla"),
                        G2 = c("Niki","GIUDITTA","Mari","Irene"),
                        G3 = c("Irene","Amanda","Mari","Fabia","GIUDITTA"),
                        G4 = c("VERO","Marti","Lalla","Fabia","Mari"),
                        G5 = c("VERO","Niki","Amanda","Alessia"),
                        G6 = c("Angi","Alessia","Irene","Lalla","Amanda"),
                        G7 = c("Fabia","Marti","Angi","Niki", "Alessia"),
                        G8 = c("Niki","Mari","Marti","VERO","GIUDITTA"))



  cand.list = expand.grid(G1 = nomi[sample(1:11,5)],
                          G2 = nomi[sample(1:11,5)],
                          G3 = nomi[sample(1:11,5)],
                          G4 = nomi[sample(1:11,5)],
                          G5 = nomi[sample(1:11,5)],
                          G6 = nomi[sample(1:11,5)],
                          G7 = nomi[sample(1:11,5)],
                          G8 = nomi[sample(1:11,5)])
  
i <- 1
gini <- mean <- diff <-  array()
gini[1] <- 1
uff <- partita <- list()

while(gini[i] >0.001 ){
  
  uff <- femminile(cand.list, 35, 14)
  partita[[i+1]] <- uff$uff
  gini[i+1] <- Gini(as.integer(as.matrix(uff$distrib)[,2]))
  mean[i+1] <- mean(as.integer(as.matrix(uff$distrib)[,2]))
  diff[i+1] <- round((mean[i+1]-10)/sd(as.integer(as.matrix(uff$distrib)[,2])),3)
  i = i+1
  print(paste("iteration: ",i-1,", Gini = ", round(gini[i],4),", mean = ",mean[i], ", z = ",diff[i], sep = " "))
  
}

plot(density(gini[-1]))

partita[[which.min(gini)]] %>% as_tibble %>% 
  mutate_all(function(x) as.character(x)) %>% 
  gather(key = partita, value = atleta) %>% 
  group_by(atleta) %>% 
  summarize(n())


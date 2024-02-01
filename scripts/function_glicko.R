weeks_passed <- function(year, month, day) {
  input_date <- as.Date(paste(year, month, day, sep = "-"))
  reference_date <- as.Date("2023-05-20")
  weeks <- round(as.numeric(difftime(input_date, reference_date, units = "weeks")),0)
  return(weeks)
}

from_xl_to_glicko <- function(data, year, month, day){
  
  modified_data = 
    data %>% 
    mutate(
      Play1 = paste(g1,"&",g2),
      Play2 = paste(g3,"&",g4),
      Time = weeks_passed(year,month,day)) %>% 
    mutate(tot_set = w1+w2,
           Score = case_when(
             w1/tot_set == 2/3 ~ 0.75,
             w1/tot_set == 1/3 ~ 0.25,
             TRUE ~ w1/tot_set
           ),
           Weight = case_when(
             tot_set == 1 ~.75,
             TRUE ~1)) %>% 
    select(Time, Play1,Play2,Score,Weight) %>% 
    as.data.frame()
  
  
  return(modified_data)
  
}

glick_elo <- function(x, status = NULL, init = c(1200,200),rdmax = 250, cval = 50,whichtourn = 0){
  
  x = separate(x, Play1, into = c("Giocatore1", "Giocatore2"), sep = " & ",remove = T)
  x = separate(x, Play2, into = c("Giocatore3", "Giocatore4"), sep = " & ",remove = T)
  
  time = unique(x$Time)
  giocatori = sort(unique(c(x$Giocatore1, x$Giocatore2, x$Giocatore3, x$Giocatore4)))
  
  ng <- length(giocatori)
  np <- nrow(x)
  
  
  x$Giocatore1 <- match(x$Giocatore1, giocatori)
  x$Giocatore2 <- match(x$Giocatore2, giocatori)
  x$Giocatore3 <- match(x$Giocatore3, giocatori)
  x$Giocatore4 <- match(x$Giocatore4, giocatori)
  
  if (!is.null(status)) {
    npadd <- giocatori[!(giocatori %in% status$giocatore)]
    zv <- rep(0, length(npadd))
    npstatus <- data.frame(giocatore = npadd,
                           Rating = rep(init[1], length(npadd)),
                           Deviation = rep(init[2], length(npadd)), 
                           Games = zv, Win = zv,  Loss = zv,nTourn = zv,
                           LastTourn = rep(time,length(npadd)))
    if (!("Games" %in% names(status))) 
      status <- cbind(status, Games = 0)
    if (!("Win" %in% names(status))) 
      status <- cbind(status, Win = 0)
    if (!("Loss" %in% names(status))) 
      status <- cbind(status, Loss = 0)
    if (!("nTourn" %in% names(status))) 
      status <- cbind(status, nTourn = 0)
    if (!("LastTourn" %in% names(status))) 
      status <- cbind(status, LastTourn = time)
    
    status <- rbind(status[, c("giocatore", "Rating", "Deviation", 
                               "Games", "Win", "Loss", "nTourn","LastTourn")], npstatus)
    rinit <- ifelse(status[[2]]<900,900,status[[2]])
    dinit <- status[[3]]
    ngames <- status[[4]]
    nwin <- status[[5]]
    nloss <- status[[6]]
    ntourn <- status[[7]]
    nlasttourn <- status[[8]]
    names(rinit) <- names(dinit) <- names(ngames) <- status$giocatore
  }else {
    rinit <- rep(init[1], length.out = ng)
    dinit <- rep(init[2], length.out = ng)
    ngames <- nwin <- nloss <- rep(0, length.out = ng)
    ntourn <- rep(0, ng)
    nlasttourn <- rep(time,ng)
    names(rinit) <- names(dinit) <- names(ngames) <- names(ntourn) <- names(nlasttourn) <- names(ngames) <- names(nwin) <- names(nloss)<- giocatori
    
  }
  
  curplay <- match(giocatori, names(rinit))
  
  orats <- rinit[-curplay]
  odevs <- dinit[-curplay]
  ongames <- ngames[-curplay]
  onwin <- nwin[-curplay]
  onloss <- nloss[-curplay]
  ontourn <- ntourn[-curplay]
  olasttourn <- nlasttourn[-curplay]
  
  crats <- rinit[curplay]
  cdevs <- dinit[curplay]
  ngames <- ngames[curplay]
  nwin <- nwin[curplay]
  nloss <- nloss[curplay]
  ntourn <- ntourn[curplay]
  ntourn <- ntourn +1
  nlasttourn <- nlasttourn[curplay]
  
  histry <- array(NA,
                  dim = c(ng, 5),
                  dimnames = list(giocatori,
                                  c("Rating", "Deviation", "Games","Wins","WhichT")))
  
  # This needs to be adapted for having a status
  
  x$elo1 = crats[x$Giocatore1]
  x$elo2 = crats[x$Giocatore2]
  x$elo3 = crats[x$Giocatore3]
  x$elo4 = crats[x$Giocatore4]
  x$dev1 = pmin(sqrt(cdevs[x$Giocatore1]^2+cval^2*(time - nlasttourn[x$Giocatore1]+1)),rdmax)
  x$dev2 = pmin(sqrt(cdevs[x$Giocatore2]^2+cval^2*(time - nlasttourn[x$Giocatore2]+1)),rdmax)
  x$dev3 = pmin(sqrt(cdevs[x$Giocatore3]^2+cval^2*(time - nlasttourn[x$Giocatore3]+1)),rdmax)
  x$dev4 = pmin(sqrt(cdevs[x$Giocatore4]^2+cval^2*(time - nlasttourn[x$Giocatore4]+1)),rdmax)
  x$telo1 = (x$elo1+x$elo2)/2
  x$telo2 = (x$elo3+x$elo4)/2
  x$tdev1 = (x$dev1+x$dev2)/4
  x$tdev2 = (x$dev3+x$dev4)/4
  
  nlasttourn <- rep(time,length(nlasttourn))
  
  q = log(10)/400
  x$gdev1 =  1/sqrt(1+3*(q/pi)^2 * x$tdev1^2)
  x$gdev2 =  1/sqrt(1+3*(q/pi)^2 * x$tdev2^2)
  x$exp1 = 1/(1+10^(-x$gdev1*(x$telo1-x$telo2)/400))
  x$exp2 = 1-x$exp1
  
  
  dscores = 
    x %>% 
    as_tibble %>% 
    pivot_longer(cols = Giocatore1:Giocatore4, names_to = "giocatore") %>% 
    group_by(value) %>% 
    mutate( dscores = case_when(
      giocatore == "Giocatore1" | giocatore == "Giocatore2" ~  gdev2^2 * exp1 * exp2,
      giocatore == "Giocatore3" | giocatore == "Giocatore4" ~  gdev1^2 * exp1 * exp2
    )) %>% 
    summarize(
      dscore = (q^2 * sum(dscores))^(-1)
    ) %>% 
    as.data.frame()
  
  for(i in 1:nrow(x)){
    for(j in 1:4){
      numero_giocatore <- x[i,j+1]
      index <- which(dscores[,1] == numero_giocatore)
      
      x[i,j+23] <- dscores$dscore[index] 
      colnames(x)[j+23] <- paste("dscore",j,sep = "")
      
    }
  }
  
  x$pts1 = x$gdev2*x$Weight*(x$Score - x$exp1) * q/((x$dev1^2)^(-1)+(x$dscore1)^(-1))
  x$pts2 = x$gdev2*x$Weight*(x$Score - x$exp1) * q/((x$dev2^2)^(-1)+(x$dscore2)^(-1))
  x$pts3 = x$gdev1*x$Weight*((1-x$Score) - x$exp2) * q/((x$dev3^2)^(-1)+(x$dscore3)^(-1))
  x$pts4 = x$gdev1*x$Weight*((1-x$Score) - x$exp2) * q/((x$dev4^2)^(-1)+(x$dscore4)^(-1))
  
  
  points = 
    x %>% 
    as_tibble %>% 
    pivot_longer(cols = Giocatore1:Giocatore4, names_to = "giocatore") %>% 
    gather("pts_ref","pts",pts1:pts4) %>% 
    gather("elo_ref","elo",elo1:elo4) %>% 
    gather("dev_ref","dev",dev1:dev4) %>% 
    group_by(value) %>% 
    filter(
      giocatore  =="Giocatore1" & elo_ref =="elo1" & pts_ref =="pts1" & dev_ref == "dev1" |
        giocatore  =="Giocatore2" & elo_ref =="elo2" & pts_ref =="pts2" & dev_ref == "dev2" |
        giocatore  =="Giocatore3" & elo_ref =="elo3" & pts_ref =="pts3" & dev_ref == "dev3" |
        giocatore  =="Giocatore4" & elo_ref =="elo4" & pts_ref =="pts4" & dev_ref == "dev4" 
    ) %>% 
    summarize(new_pts = sum(pts)) %>% 
    as.data.frame()
  
  ngamesi <- tabulate(c(x$Giocatore1,x$Giocatore2,x$Giocatore3,x$Giocatore4), ng)
  trainiplw <- c(x$Giocatore1[x$Score > .5], x$Giocatore2[x$Score > .5],x$Giocatore3[x$Score < .5],x$Giocatore4[x$Score < .5])
  trainipll <- c(x$Giocatore1[x$Score < .5], x$Giocatore2[x$Score < .5],x$Giocatore3[x$Score > .5],x$Giocatore4[x$Score > .5])
  ngames <- ngames + ngamesi
  nwin <- nwin + tabulate(trainiplw, ng)
  nloss <- nloss + tabulate(trainipll, ng)
  
  rownames(points) = giocatori[points$value]
  y = merge(rinit,points,by = "row.names")
  y$elo = round(y$x + y$new_pts,0)
  y = y[,c(1,5)]
  names(y) <- c("giocatore","Rating")
  
  
  rownames(dscores) = giocatori[dscores$value]
  z = merge(dinit,dscores,by = "row.names")
  z$rd = round(sqrt((1/z$x^2+1/z$dscore)^(-1)),0)
  z = z[,c(1,5)]
  names(z) <- c("giocatore","Deviation")
  
  history  = merge(y,z)
  history = history[order(history$giocatore,decreasing = F),]
  history$Games = ngames
  history$Win = nwin
  history$Loss = nloss
  
  player <- suppressWarnings(as.numeric(names(c(crats, orats))))
  if (any(is.na(player))) 
    player <- names(c(crats, orats))
  dfout <- data.frame(giocatore = player,
                      Rating = c(history$Rating, orats), 
                      Deviation = c(history$Deviation, odevs),
                      Games = c(history$Games, ongames),
                      Win = c(history$Win, onwin),
                      Loss = c(history$Loss, onloss),
                      nTourn = c(ntourn, ontourn),
                      LastTourn = c(nlasttourn,olasttourn),
                      stringsAsFactors = FALSE)
  
  dfout <- dfout[order(dfout$Rating, decreasing = TRUE), ]
  
  row.names(dfout) <- 1:nrow(dfout)
  
  histry[,1] <- round(points$new_pts,0)
  histry[,2] <- sqrt(cdevs)
  histry[,3] <- ngamesi
  histry[,4] <- tabulate(trainiplw, ng)
  histry[,5] <- rep(whichtourn, length(points$new_pts))
  
  lout <- list(ratings = dfout, history = histry,type = "Glicko")
  class(lout) <- "rating"
  return(lout)
}

histry_merge <- function(list, giocatori, nmin = 4){
  giocatori = sort(unique(giocatori))
  ng = length(giocatori)
  nt = ncol(sapply(list,dim))
  
  histry =
    array(NA,
          dim = c(ng,nt+1, 3),
          dimnames = list(giocatori,1:(nt+1),
                          c("Rating", "Deviation","Games")))
  
  
  playfilt = list[[nt]]$giocatore[list[[nt]]$nTourn>=nmin]
  playfilt = playfilt[order(playfilt)]
  
  for(i in 2:(nt+1)){
    dflisti = as.data.frame(list[i-1])
    
    
    playmatch = match(dflisti$giocatore,giocatori)
    histry[, i, 1][playmatch] <- dflisti$Rating
    histry[, i, 2][playmatch] <- dflisti$Deviation
    histry[, i, 3][playmatch] <- dflisti$Games
  }
  
  histryfilt =
    array(NA,
          dim = c(length(playfilt),nt+1, 3),
          dimnames = list(playfilt,1:(nt+1),
                          c("Rating", "Deviation","Games")))
  
  histryfilt[,,"Rating"] = histry[,,"Rating"][match(playfilt,rownames(histry[,,"Rating"])),]
  histryfilt[,,"Deviation"] = histry[,,"Deviation"][match(playfilt,rownames(histry[,,"Deviation"])),]
  histryfilt[,,"Games"] = histry[,,"Games"][match(playfilt,rownames(histry[,,"Games"])),]
  histryfilt[,,"Games"] = ifelse(is.na(histryfilt[,,"Games"]),0,histryfilt[,,"Games"])
  histryfilt[,,"Rating"] = ifelse(is.na(histryfilt[,,"Rating"]),1200,histryfilt[,,"Rating"])
  
  
  return(histryfilt)
  
}

plot.ratings <- function(histr, x= 13, y = 1900, which = "Rating", lwd = 1){
  par(xpd=TRUE)
  plot(1:ncol(histr[,,which]), rep(0,ncol(histr[,,which])), type="l",lty = "dashed", ylim = c(min(histr[,,which],na.rm = T)-50,max(histr[,,which],na.rm = T)+50), ylab = which,xlab = "Tournament")
  type = c("dotdash","dotted","solid")
  for(i in 1:nrow(histr[,,"Rating"])){
    lines(1:ncol(histr[,,"Rating"]),  histr[,,which][i,], col=i,type="l",lty = type[i%%3 +1],lwd = lwd)
  }
  legend(x = x, y = y, legend=rownames(histr[,,which]), col=1:nrow(histr[,,which]), lty=type[(1:nrow(histr[,,which])%%3 +1)], cex=0.8)
}

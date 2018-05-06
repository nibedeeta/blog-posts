library(readr)
library(tidyr)
library(R2jags)

# Import data -------------------------------------------------------------

cleague2017 <- read_csv("champions-league-2017-CentralEuropeanStandardTime.csv")


# variable names have spaces (i.e. <Home Team>) and this bothers me IMMENSLY, so:
names(cleague2017) <- c("Round", "Date", "Location", "HomeTeam", 
                        "AwayTeam", "Group", "Result")


# split result column into goals for the home team <HomeGoals> and for the away
# team <AwayGoals>
cleague2017 <- separate(cleague2017, col = "Result", into = c("HomeGoals", "AwayGoals"), sep = " - ", convert = T)


# convert everything we can into factor:
## this we are going to use in jags
cleague2017$HomeTeam <- factor(cleague2017$`HomeTeam`)
cleague2017$AwayTeam <- factor(cleague2017$`AwayTeam`, levels = levels(cleague2017$`HomeTeam`))
 

## this we may use in jags
cleague2017$Round <- factor(cleague2017$Round)

# levels are a mess, there is one different value for each round ARGH!
levels(cleague2017$Round) <- c( "Girone", "Girone", "Girone", "Girone","Girone",  "Girone",
                                 "Qtr Finals", "Qtr Finals",  "Round of 16", "Round of 16", 
                                "Semi Finals", "Semi Finals")
cleague2017$Group <- factor(cleague2017$Group)


#there are 32 teams & 124 games:
K <- length(unique(cleague2017$HomeTeam)) 
n <- nrow(cleague2017)
R <- length(unique(cleague2017$Round)) 

cleague.data <- list(n = n, K = K, R = R,
                     HomeTeam  = cleague2017$HomeTeam,
                     AwayTeam  = cleague2017$AwayTeam,
                     HomeGoals = cleague2017$HomeGoals,
                     AwayGoals = cleague2017$AwayGoals, 
                     Round     = cleague2017$Round)



# Model 1 -----------------------------------------------------------------

# Let us start from the easiest model: no mean effect, no round effect, no time
# effect... nothing basically

model1 = "model{
  for (i in 1:n){ 	
    # stochastic component
    HomeGoals[i]~dpois(lambdaH[i])       
    AwayGoals[i]~dpois(lambdaA[i])       
    
    
    # link and linear predictor
    log(lambdaH[i])<- home + a[ HomeTeam[i] ] + d[ AwayTeam[i] ]
    log(lambdaA[i])<- a[ AwayTeam[i] ] + d[ HomeTeam[i] ]
  }
  # STZ constraints		
  a[1]<-  -sum( a[2:K] )
  d[1]<-  -sum( d[2:K] )

  # prior distributions
  home~dnorm(0,0.001)
  for (i in 2:K){
    a[i]~dnorm(0,0.01)
    d[i]~dnorm(0,0.01)
  }
  
}"


# Initialize
soccer.init = list(list( "home"=0.5, "a"=c(NA, rep(0, K-1)) , 
                         "d"=c(NA, rep(0, K-1)  )),
                   list( "home"=0.5, "a"=c(NA, rep(0, K-1)) , 
                         "d"=c(NA, rep(0, K-1)  )),
                   list( "home"=0.5, "a"=c(NA, rep(0, K-1)) , 
                         "d"=c(NA, rep(0, K-1)  )),
                   list( "home"=0.5, "a"=c(NA, rep(0, K-1)) , 
                         "d"=c(NA, rep(0, K-1)  )))


# parameters that we whish to retrieve
soccer.param = c("a", "d", "home", "HomeGoals", "AwayGoals")
# <a> and <d> are just for interpretation
# <HomeGoals> and <AwayGoals> is for prediction


# Run the model
soccer.jags = jags(textConnection(model1), data = cleague.data, inits = soccer.init,        
                   parameters.to.save = soccer.param,          
                   n.chains = 4,                         
                   n.iter = 100000
                  )   

print(soccer.jags)

Hteams = levels(cleague2017$HomeTeam)
Ateams = levels(cleague2017$AwayTeam)

# semifinals scores prediction: 
pred <- cbind("H-Team" = Hteams[cleague.data$HomeTeam[121:124]],
              "A-Team" = Ateams[cleague.data$AwayTeam[121:124]],
              "H-Goal" = soccer.jags$BUGSoutput$mean$HomeGoals[121:124], 
              "A-Goal" = soccer.jags$BUGSoutput$mean$AwayGoals[121:124])

pred




res = cbind(soccer.jags$BUGSoutput$summary[249:280,1],
            soccer.jags$BUGSoutput$summary[281:312,1])

rownames(res) = Hteams
colnames(res) = c("attack", "defence")
res
save(res, file = "latent.RData")

# the final prediction ----------------------------------------------------

thewinner = "model{
  for (i in 1:n){ 	
  # stochastic component
    HomeGoals[i]~dpois(lambdaH[i])       
    AwayGoals[i]~dpois(lambdaA[i])       
  
  
  # link and linear predictor
    log(lambdaH[i])<- home + a[ HomeTeam[i] ] + d[ AwayTeam[i] ] 
    log(lambdaA[i])<- a[ AwayTeam[i] ] + d[ HomeTeam[i] ] 
  }
  
  # change the model for the finals (there is no home effect, both teams are playing in Kiev)
  GoalsF1~dpois(lambdaF1)       
  GoalsF2~dpois(lambdaF2)

  log(lambdaF1)<-  a[ HomeTeam[n+1] ] + d[ AwayTeam[n+1] ]
  log(lambdaF2)<-  a[ AwayTeam[n+1] ] + d[ HomeTeam[n+1] ] 



  # STZ constraints		
  a[1]<-  -sum( a[2:K] )
  d[1]<-  -sum( d[2:K] )

  # prior distributions
  mu~dnorm(0,0.001)
  home~dnorm(0,0.001)

  for (i in 2:K){
    a[i]~dnorm(0,0.01)
    d[i]~dnorm(0,0.01)
  }

}"

# parameters we wish to retrieve
soccer.param.Final = c("a", "d", "home", "GoalsF1", "GoalsF2")

whoisthewinner = function(team1, team2, data = cleague2017){

  Hteams = levels(cleague2017$HomeTeam)
  Ateams = levels(cleague2017$AwayTeam)
  
  idx1 = which(Hteams == team1)
  idx2 = which(Hteams == team2)
  
  
  cleague.data.Final = list(n = n, K = K,
                             HomeTeam  = c(cleague2017$HomeTeam, idx1),
                             AwayTeam  = c(cleague2017$AwayTeam, idx2),
                             HomeGoals = c(cleague2017$HomeGoals[1:120], 5, 1, 2, 4, NA),
                             AwayGoals = c(cleague2017$AwayGoals[1:120], 2, 2, 2, 2, NA)
                             )
  
  soccer.jags.Final = jags(textConnection(thewinner), data = cleague.data.Final, inits = soccer.init,        
                      parameters.to.save = soccer.param.Final,          
                      n.chains = 4,                         
                      n.iter = 100000) 
  
  

  
  # semifinals scores prediction: 
  predF <- c(soccer.jags.Final$BUGSoutput$mean$GoalsF1, 
             soccer.jags.Final$BUGSoutput$mean$GoalsF2)
  names(predF) = c(team1, team2)
  

  # probability of winning
  m   = apply( (soccer.jags.Final$BUGSoutput$sims.list$GoalsF1 - soccer.jags.Final$BUGSoutput$sims.list$GoalsF2), 2, mean)
  p   = apply( (soccer.jags.Final$BUGSoutput$sims.list$GoalsF1 - soccer.jags.Final$BUGSoutput$sims.list$GoalsF2)>0, 2, mean)
  p2  = apply( (soccer.jags.Final$BUGSoutput$sims.list$GoalsF1 - soccer.jags.Final$BUGSoutput$sims.list$GoalsF2)<0, 2, mean)
  
  probW = cbind(team1, team2, 
                soccer.jags.Final$BUGSoutput$mean$GoalsF1, soccer.jags.Final$BUGSoutput$mean$GoalsF2, 
                m,p, p2)
  
  colnames(probW) = c("Team1", "Team2", "Goal Team1", "Goal Team2",
                      "Mean Difference", "p > 0", "p < 0")
 
  
  
  return(list(jags.out = soccer.jags.Final, pred = predF, probW = probW ))
  
  
}


# Liverpool - Bayern Munich
winnerLB = whoisthewinner( "Liverpool","Bayern Munich")

# Liverpool - Real Madrid
winnerLR = whoisthewinner( "Liverpool","Real Madrid")

# Roma - Real Madrid
winnerRR = whoisthewinner( "Roma","Real Madrid")

# Roma - Bayern Munich
winnerRB = whoisthewinner( "Roma","Bayern Munich")


winner.mat = rbind(winnerLB$pred, winnerLR$pred, winnerRB$pred, winnerRR$pred)
winner.prob = rbind(winnerLB$probW, winnerLR$probW, winnerRB$probW, winnerRR$probW)
save(winner.mat, winner.prob, file = "winnermat.RData")



traceplot(winnerLB$jags.out)
print(winnerLR$jags.out)
print(winner$jags.out)





# Is there a phase effect  ------------------------------------------------

# We know that teams that get closer to the final are better, but do they play
# extra-better because of the pressure? in other words, is there a phase effect
# that makes the scoring intensity higher when the competition becomes more and
# more real?


model.phase = "model{
  for (i in 1:n){ 	
  # stochastic component
  HomeGoals[i]~dpois(lambdaH[i])       
  AwayGoals[i]~dpois(lambdaA[i])       


  # link and linear predictor
  log(lambdaH[i])<- home + a[ HomeTeam[i] ] + d[ AwayTeam[i] ] + r[ Round[i] ]
  log(lambdaA[i])<- a[ AwayTeam[i] ] + d[ HomeTeam[i] ] + r[ Round[i] ]
  }



# STZ constraints		
  a[1]<-  -sum( a[2:K] )
  d[1]<-  -sum( d[2:K] )

# prior distributions
  mu~dnorm(0,0.001)
  home~dnorm(0,0.001)

  for (i in 2:K){
    a[i]~dnorm(0,0.01)
    d[i]~dnorm(0,0.01)
  }

  r[1] <-  -sum(r[2:R])
  for(i in 2:R){
    r[i]~dnorm(0,0.01)
  }

}"

# in this case however we also need the scores for the first round of the
# semifinals, otherwise we cannot estimate the phase effect


cleague.data.phase = list(n = n, K = K, R = R,
                          HomeTeam  = c(cleague2017$HomeTeam),
                          AwayTeam  = c(cleague2017$AwayTeam),
                          HomeGoals = c(cleague2017$HomeGoals[1:120], 5, 1, NA, NA),
                          AwayGoals = c(cleague2017$AwayGoals[1:120], 2, 2, NA, NA),
                          Round     = cleague2017$Round
)


soccer.param = c("a", "d", "home", "HomeGoals", "AwayGoals", "r")
# <a> and <d> are just for interpretation
# <HomeGoals> and <AwayGoals> is for prediction
# <r> is for the "phase" effect


# initialize the phase effect to be 0 as well 
soccer.init.phase = list(list( "home"=0.5, "a"=c(NA, rep(0, K-1)) , 
                         "d"=c(NA, rep(0, K-1)  ), r = c(NA, rep(0, R-1))),
                   list( "home"=0.5, "a"=c(NA, rep(0, K-1)) , 
                         "d"=c(NA, rep(0, K-1)  ), r = c(NA, rep(0, R-1))),
                   list( "home"=0.5, "a"=c(NA, rep(0, K-1)) , 
                         "d"=c(NA, rep(0, K-1)  ), r = c(NA, rep(0, R-1))),
                   list( "home"=0.5, "a"=c(NA, rep(0, K-1)) , 
                         "d"=c(NA, rep(0, K-1)  ), r = c(NA, rep(0, R-1))))




# Run the model
soccer.jags.phase = jags(textConnection(model.phase), data = cleague.data.phase, inits = soccer.init.phase,        
                    parameters.to.save = soccer.param,          
                    n.chains = 4,                         
                    n.iter = 100000 )   


Hteams = levels(cleague2017$HomeTeam)
Ateams = levels(cleague2017$AwayTeam)

# semifinals scores prediction: 
pred.phase <- cbind("H-Team" = Hteams[cleague.data$HomeTeam[121:124]],
               "A-Team" = Ateams[cleague.data$AwayTeam[121:124]],
               "H-Goal" = soccer.jags.phase$BUGSoutput$mean$HomeGoals[121:124], 
               "A-Goal" = soccer.jags.phase$BUGSoutput$mean$AwayGoals[121:124])

pred.phase 
summary(soccer.jags.phase$BUGSoutput$sims.list["r"]$r)
apply(soccer.jags.phase$BUGSoutput$sims.list["r"]$r, 2, quantile, probs = c(0.025, 0.975))

traceplot(soccer.jags.phase)
# well, the results are rather inconclusive, aren't they...





# With semi-final data ----------------------------------------------------


cleague.data.phase = list(n = n, K = K, R = R,
                          HomeTeam  = c(cleague2017$HomeTeam),
                          AwayTeam  = c(cleague2017$AwayTeam),
                          HomeGoals = c(cleague2017$HomeGoals[1:120], 5, 1, NA, NA),
                          AwayGoals = c(cleague2017$AwayGoals[1:120], 2, 2, NA, NA),
                          Round     = cleague2017$Round
)

soccer.init = list(list( "home"=0.5, "a"=c(NA, rep(0, K-1)) , 
                         "d"=c(NA, rep(0, K-1)  )),
                   list( "home"=0.5, "a"=c(NA, rep(0, K-1)) , 
                         "d"=c(NA, rep(0, K-1)  )),
                   list( "home"=0.5, "a"=c(NA, rep(0, K-1)) , 
                         "d"=c(NA, rep(0, K-1)  )),
                   list( "home"=0.5, "a"=c(NA, rep(0, K-1)) , 
                         "d"=c(NA, rep(0, K-1)  )))


# parameters that we whish to retrieve
soccer.param = c("a", "d", "home", "HomeGoals", "AwayGoals")
# <a> and <d> are just for interpretation
# <HomeGoals> and <AwayGoals> is for prediction


# Run the model
soccer.jags2 = jags(textConnection(model1), data = cleague.data.phase, inits = soccer.init,        
                   parameters.to.save = soccer.param,          
                   n.chains = 4,                         
                   n.iter = 100000
)   

print(soccer.jags2)

Hteams = levels(cleague2017$HomeTeam)
Ateams = levels(cleague2017$AwayTeam)


# Winning probs and prob --------------------------------------------------

winning.prob = function(jags.output, game){
  m    = apply( (jags.output$BUGSoutput$sims.list$HomeGoals[,game] - jags.output$BUGSoutput$sims.list$AwayGoals[,game]), 2, mean)
  med  = apply( (jags.output$BUGSoutput$sims.list$HomeGoals[,game] - jags.output$BUGSoutput$sims.list$AwayGoals[,game]), 2, median)
  l    = apply( (jags.output$BUGSoutput$sims.list$HomeGoals[,game] - jags.output$BUGSoutput$sims.list$AwayGoals[,game]), 2, quantile, p =0.025)
  u    = apply( (jags.output$BUGSoutput$sims.list$HomeGoals[,game] - jags.output$BUGSoutput$sims.list$AwayGoals[,game]), 2, quantile, p = 0.975)
  p    = apply( (jags.output$BUGSoutput$sims.list$HomeGoals[,game] - jags.output$BUGSoutput$sims.list$AwayGoals[,game])>0, 2, mean)
  mep  = apply( (jags.output$BUGSoutput$sims.list$HomeGoals[,game] - jags.output$BUGSoutput$sims.list$AwayGoals[,game])>0, 2, median)
  lp   = p - 1.96*sqrt(p*(1-p)/length(jags.output$BUGSoutput$sims.list$HomeGoals[,game]))
  up   = p + 1.96*sqrt(p*(1-p)/length(jags.output$BUGSoutput$sims.list$HomeGoals[,game]))
  p2   = apply( (jags.output$BUGSoutput$sims.list$HomeGoals[,game] - jags.output$BUGSoutput$sims.list$AwayGoals[,game])<0, 2, mean)
  mep2 = apply( (jags.output$BUGSoutput$sims.list$HomeGoals[,game] - jags.output$BUGSoutput$sims.list$AwayGoals[,game])<0, 2, median)
  lp2  = p2 - 1.96*sqrt(p2*(1-p2)/length(jags.output$BUGSoutput$sims.list$HomeGoals[,game]))
  up2  = p2 + 1.96*sqrt(p2*(1-p2)/length(jags.output$BUGSoutput$sims.list$HomeGoals[,game]))
  
  
  out = cbind("H-Team" = Hteams[cleague.data$HomeTeam[game]],
              "A-Team" = Ateams[cleague.data$AwayTeam[game]],
              "H-Goal - Mean" = jags.output$BUGSoutput$mean$HomeGoals[game], 
              "H-Goal - Median" = jags.output$BUGSoutput$median$HomeGoals[game], 
              "H-Goal - Sd" = jags.output$BUGSoutput$sd$HomeGoals[game], 
              "A-Goal - Mean" = jags.output$BUGSoutput$mean$AwayGoals[game], 
              "A-Goal - Median" = jags.output$BUGSoutput$median$AwayGoals[game], 
              "A-Goal - Sd" = jags.output$BUGSoutput$sd$AwayGoals[game], 
              "Mean Difference" = m, 
              "Median Difference" = med,
              "0.025 quantile for the Difference" = l,
              "0.975 quantile for the Difference" = u,
              "p > 0" = p,
              "p > 0 - Median" = mep,
              "0.025 quantile for p > 0" = lp,
              "0.975 quantile for p > 0" = up,
              "p < 0" = p2,
              "p < 0 - Median" = mep2,
              "0.025 quantile for p < 0" = lp2,
              "0.975 quantile for p < 0" = up2
              )
  out
}


# semifinals scores prediction: 
semi.fin  = winning.prob(soccer.jags, 121:124)
semi.fin2 = winning.prob(soccer.jags2, 121:124)
semi.fin.phase = winning.prob(soccer.jags.phase, 121:124)
save(semi.fin, semi.fin2, semi.fin.phase, file = "probmat.RData")






winning.prob2 = function(jags.output, team1, team2){
  m    = apply( (jags.output$BUGSoutput$sims.list$GoalsF1 - jags.output$BUGSoutput$sims.list$GoalsF2), 2, mean)
  med  = apply( (jags.output$BUGSoutput$sims.list$GoalsF1 - jags.output$BUGSoutput$sims.list$GoalsF2), 2, median)
  l    = apply( (jags.output$BUGSoutput$sims.list$GoalsF1 - jags.output$BUGSoutput$sims.list$GoalsF2), 2, quantile, p =0.025)
  u    = apply( (jags.output$BUGSoutput$sims.list$GoalsF1 - jags.output$BUGSoutput$sims.list$GoalsF2), 2, quantile, p = 0.975)
  p    = apply( (jags.output$BUGSoutput$sims.list$GoalsF1 - jags.output$BUGSoutput$sims.list$GoalsF2)>0, 2, mean)
  lp   = p - 1.96*sqrt(p*(1-p)/length(jags.output$BUGSoutput$sims.list$GoalsF1))
  up   = p + 1.96*sqrt(p*(1-p)/length(jags.output$BUGSoutput$sims.list$GoalsF1))
  p2   = apply( (jags.output$BUGSoutput$sims.list$GoalsF1 - jags.output$BUGSoutput$sims.list$GoalsF2)<0, 2, mean)
  lp2  = p2 - 1.96*sqrt(p2*(1-p2)/length(jags.output$BUGSoutput$sims.list$GoalsF1))
  up2  = p2 + 1.96*sqrt(p2*(1-p2)/length(jags.output$BUGSoutput$sims.list$GoalsF1))
  
  
  out = cbind("Team 1" = team1,
              "Team 2" = team2,
              "Goal 1 - Mean" = jags.output$BUGSoutput$mean$GoalsF1, 
              "Goal 1 - Median" = jags.output$BUGSoutput$median$GoalsF1,
              "Goal 1 - Sd" = jags.output$BUGSoutput$sd$GoalsF1,
              "Goal 2 - Mean" = jags.output$BUGSoutput$mean$GoalsF2, 
              "Goal 2 - Median" = jags.output$BUGSoutput$median$GoalsF2,
              "Goal 2 - Sd" = jags.output$BUGSoutput$sd$GoalsF2,
              "Mean Difference" = m, 
              "Median Difference" = med,
              "0.025 quantile for the Difference" = l,
              "0.975 quantile for the Difference" = u,
              "p > 0" = p,
              "0.025 quantile for p > 0" = lp,
              "0.975 quantile for p > 0" = up,
              "p < 0" = p2,
              "0.025 quantile for p < 0" = lp2,
              "0.975 quantile for p < 0" = up2
  )
  out
}



winner.prob = rbind(winning.prob2(winnerLB$jags.out, "Liverpool", "Bayern Munich"),
                    winning.prob2(winnerLR$jags.out, "Liverpool", "Real Madrid"),
                    winning.prob2(winnerRR$jags.out, "Roma", "Real Madrid"), 
                    winning.prob2(winnerRB$jags.out,  "Roma", "Bayern Munich"))


save(winner.prob, file = "winnermat.RData")



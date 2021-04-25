premOG = X200910
premOG$DateShort = gsub("^(.+)(20)([0-9]+)$","\\1\\3", premOG$Date)
premOG$DayNumber = as.numeric(as.Date(premOG$DateShort, format="%d/%m/%y")-as.Date("2000-01-01"))
dayToDo=max(premOG$DayNumber)#Finds the maximum day number and assigns this value to dayToDo
prem=premOG[which(premOG$DayNumber<dayToDo),]#Creates a new data frame "prem", with the data from every day up until 

HomeTeamList=premOG[which(premOG$DayNumber==dayToDo),"HomeTeam"]#Creates a list of each team playing at home on dayToDo
AwayTeamList=premOG[which(premOG$DayNumber==dayToDo),"AwayTeam"]#Same as above, but with the away teams

teams = sort(unique(c(prem$HomeTeam, prem$AwayTeam)))#Sorts the combined vectors, and takes out repeated games

list=c()
for (ii in seq(1, sum(premOG$DayNumber==dayToDo), by=1)) {  #Finds the number of matches taking place on dayToDo
  list[[ii]] = ii  #Creates a list of all numbers up to the number of matches
}
for (xx in seq_along(list)){  #Cycles through each value in the list above: 1,2,3,...
  
  HomeTeam=HomeTeamList[xx]  #Finds the HomeTeam and AwayTeam for match number xx
  AwayTeam=AwayTeamList[xx]

  homegames = rep(NA, length(teams))
  awaygames = rep(NA ,length(teams))
  
  teams = sort(unique(c(prem$HomeTeam, prem$AwayTeam)))  
  homeScoreAverageTeam = c()
  homeAttackStrength = c() 
  awayScoreAverageTeam = c()
  
  for (jj in seq_along(teams)) {
    homerows = which(prem$HomeTeam==teams[jj])
    homegames[jj] = length(homerows)
    awayrows = which(prem$AwayTeam==teams[jj])
    awaygames[jj] = length(awayrows)
  }  #Finds the number of home and away matches that each team has played up until dayToDo

  weighting=0.1
  
  for (kk in seq_along(teams)){
    x=subset.data.frame(prem,HomeTeam==teams[kk])
    y=subset.data.frame(prem,AwayTeam==teams[kk])
    homeScoreAverageTeam[kk] = mean(x$FTHG)
    awayScoreAverageTeam[kk] = mean(y$FTAG)
  }
  
  for (jj in seq_along(teams)) {
    homerows = which(prem$HomeTeam==teams[jj])
    awayrows = which(prem$AwayTeam==teams[jj])
    homegames[jj] = length(homerows)
    awaygames[jj] = length(awayrows)
  }
  
  AverageHomeFor = mean(homeScoreAverageTeam)
  AverageAwayFor = mean(awayScoreAverageTeam)
  
  CurrentHomeTeam=which(teams==HomeTeam)  #Finds the index of the HomeTeam within the teams list
  
  x=subset.data.frame(prem,HomeTeam==teams[CurrentHomeTeam]) #Creates a new dataframe, only containing rows with the current hometeam
  y=subset.data.frame(prem,AwayTeam==teams[CurrentHomeTeam])
  AverageHomeForTeam=(sum(x$FTHG)/homegames[CurrentHomeTeam])
  AverageAwayForTeam=(sum(y$FTAG)/awaygames[CurrentHomeTeam])
  AverageHomeAgainstTeam=(sum(x$FTAG)/homegames[CurrentHomeTeam])
  AverageAwayAgainstTeam=(sum(y$FTHG)/awaygames[CurrentHomeTeam])
  AttackHomeAdvantage=AverageHomeFor/AverageAwayFor
  DefenceHomeAdvantage=AverageAwayAgainstTeam/AverageHomeAgainstTeam

  
  HomeHAS=(((1-weighting)*AverageHomeForTeam)+(weighting*AverageAwayForTeam)) / (((1-weighting)*AverageHomeFor) + (weighting*AverageAwayFor))
  HomeAAS=(((1-weighting)*AverageAwayForTeam)+(weighting*AverageHomeForTeam)) / (((1-weighting)*AverageAwayFor) + (weighting*AverageHomeFor))
  HomeHDW=(((1-weighting)*AverageHomeAgainstTeam)+(weighting*AverageAwayAgainstTeam)) / (((1-weighting)*AverageAwayFor) + (weighting*AverageHomeFor))
  HomeADW=(((1-weighting)*AverageAwayAgainstTeam)+(weighting*AverageHomeAgainstTeam)) / (((1-weighting)*AverageHomeFor) + (weighting*AverageAwayFor))
  #Calculates the attack strength and defense weakness for the home team
  
  
  CurrentAwayTeam=which(teams==AwayTeam) #Finds the index of the AwayTeam within the teams list
  p=subset.data.frame(prem,HomeTeam==teams[CurrentAwayTeam])
  q=subset.data.frame(prem,AwayTeam==teams[CurrentAwayTeam])
  AverageHomeForTeam=(sum(p$FTHG)/homegames[CurrentAwayTeam])
  AverageAwayForTeam=(sum(q$FTAG)/awaygames[CurrentAwayTeam])
  AverageHomeAgainstTeam=(sum(p$FTAG)/homegames[CurrentAwayTeam])
  AverageAwayAgainstTeam=(sum(q$FTHG)/awaygames[CurrentAwayTeam])
  AttackHomeAdvantage=AverageHomeFor/AverageAwayFor
  DefenceHomeAdvantage=AverageAwayAgainstTeam/AverageHomeAgainstTeam
  
  teams = sort(unique(c(prem$HomeTeam, prem$AwayTeam)))  
  homegames = rep(NA, length(teams)) 
  awaygames = rep(NA, length(teams))
  homeScoreAverageTeam = c() 
  homeAttackStrength = c() 
  awayScoreAverageTeam = c()
  
  for (kk in seq_along(teams)){
    x=subset.data.frame(prem,HomeTeam==teams[kk])
    y=subset.data.frame(prem,AwayTeam==teams[kk])
    homeScoreAverageTeam[kk] = mean(x$FTHG)
    awayScoreAverageTeam[kk] = mean(y$FTAG)
  }
  homeScoreAverage = mean(homeScoreAverageTeam)
  awayScoreAverage = mean(awayScoreAverageTeam)
  for (jj in seq_along(teams)) {
    homerows = which(prem$HomeTeam==teams[jj])
    awayrows = which(prem$AwayTeam==teams[jj])
    homegames[jj] = length(homerows)
    awaygames[jj] = length(awayrows)
  }
  
  
  AwayHAS=(((1-weighting)*AverageHomeForTeam)+(weighting*AverageAwayForTeam)) / (((1-weighting)*AverageHomeFor) + (weighting*AverageAwayFor))
  AwayAAS=(((1-weighting)*AverageAwayForTeam)+(weighting*AverageHomeForTeam)) / (((1-weighting)*AverageAwayFor) + (weighting*AverageHomeFor))
  AwayHDW=(((1-weighting)*AverageHomeAgainstTeam)+(weighting*AverageAwayAgainstTeam)) / (((1-weighting)*AverageAwayFor) + (weighting*AverageHomeFor))
  AwayADW=(((1-weighting)*AverageAwayAgainstTeam)+(weighting*AverageHomeAgainstTeam)) / (((1-weighting)*AverageHomeFor) + (weighting*AverageAwayFor))
  
  HomeMean=homeScoreAverage*HomeHAS*AwayADW
  AwayMean=awayScoreAverage*AwayAAS*HomeHDW
  
  CycleList=list(0,1,2,3,4,5)
  CycleTemp=0
  zz=0
  HomeResult=0
  AwayResult=0
  HomeWin=0
  Draw=0
  AwayWin=0
  for (HomeScore in CycleList){
    for (AwayScore in CycleList){  #Cycles through each pair of scores, starting at 0-0, 0-1 etc.
      z=0
      HomeScoreProb=0
      AwayScoreProb=0
      if (HomeScore==5){ HomeScoreProb=((1 - ppois(4, lambda = HomeMean))*100)
      }else{HomeScoreProb=(dpois(HomeScore,HomeMean,log=FALSE))*100}
      
      if (AwayScore==5){ AwayScoreProb=((1 - ppois(4, lambda = AwayMean))*100)
      }else{AwayScoreProb=(dpois(AwayScore,AwayMean,log=FALSE))*100}
      #calculates a poisson distribution, returning the probability of each individual score
      
      zz=(HomeScoreProb*AwayScoreProb/100)  #Calculates a combined probability percentage for the exact score
      
      if (HomeScore>AwayScore){ (HomeWin=HomeWin+zz)
      }else if (HomeScore==AwayScore){ (Draw=Draw+zz)
      }else{(AwayWin=AwayWin+zz)}
      #Updates the probability of the result (either a HomeWin, Draw or AwayWin)
      
      
      if (zz>CycleTemp){  #Checks if the calculated probability is the highest so far, in which case it is stored
        CycleTemp=zz
        HomeResult=HomeScore
        AwayResult=AwayScore
      }
    }
  }
  if (xx==1){
    print("--------------------------------------------")
    }
  print(paste0(HomeTeam,paste("  VS ",AwayTeam))) #Outputs the teams, most likely score and probability of that score
  print(c("Home Score:",HomeResult))
  print(c("Away Score:",AwayResult))
  
  if (HomeResult>AwayResult){print(c("The Probability of this result is:",(paste0(round(HomeWin,digits=2),"%"))))
  }else if (HomeResult==AwayResult){print(c("The Probability of this result is:",(paste0(round(Draw,digits=2),"%"))))
  }else{print(c("The Probability of this result is:",(paste0(round(AwayWin,digits=2),"%"))))}
  #Determines which result matches the exact score, outputting the probability of such result
  
  print(c("The Probability of this exact score is:",(paste0(round(CycleTemp,digits=2),"%"))))
  print("--------------------------------------------")
}

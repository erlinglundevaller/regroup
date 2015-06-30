## Data in: individdata med identitetsnummer, region old, region new. 3 variabler alltså.
## De tre variablerna måste vara organiserade i en data.table: pid, old, new
## Behöver ej vara kontrollerat att bägge obs finns för individerna.
## Exempel: test <- omkodningsmaskin(data=forsamlingsuttag[, c("linneid", "ar", "kommun"), with=F], cutoff=0.05, startar=1980, slutar=1985)
## Exempel: test <- omkodningsmaskin(data=forsamlingsuttag[, c("linneid", "ar", "lan"), with=F], cutoff=0.02, startar=1980, slutar=1985)
## Exempel: test <- omkodningsmaskin(data=forsamlingsuttag[, c("linneid", "ar", "forsamling"), with=F], cutoff=0.40,  startar=1980, slutar=1985)
## data <- forsamlingsuttag[, c("linneid", "ar", "forsamling"), with=F]
## as.data.frame(test)  ## data<-forsamlingsuttag[, c("linneid", "ar", "kommun"), with=F]
## as.data.frame(test)[test["New",]==test["Old",]]
## test[is.na(test$newcode),]                         length(unique(test$newcode))
## data=forsamlingsuttag[, c("linneid", "ar", "lan"), with=F]   ## Kör för att steppa igenom omkodningsmaskinen

## Exempel: test <- omkodningsmaskin(data=forsamlingsuttag[, c("linneid", "ar", "lan"), with=F], cutoff=0.02, startar=1980, slutar=1985)
## setkey(test, Old,New, newcode)#is(test)
## length(unique(test$newcode))
## as.data.frame(test)
## table(test$newcode, test$Old)

#' omkodningsmaskin
#' regroups individuals/items over time based on region/group menbership
#' @param data A data.frame with "pid", "time", "region"
#' @param cutoff how large proportion of pids to merge groups
#' @param startar time to start
#' @param slutar time to stop
#'  @param  djup   for sloving nested knots, not implemented.
#' @return lansdt A data.frame with columns: "New", "Old",  "Freq", "totold", "propold", "totnew", "propnew", "newcode"; a key table.
#' @export
#' @author Erling Haggstrom Lundevaller
#' @details regroups individuals/items over time based on region/group menbership
omkodningsmaskin <- function(data, cutoff = 0.05, startar=NA, slutar=NA, djup=1){
  require(data.table)
setnames(data,  c("pid", "time", "region"))
# data$time <- as.character(data$time)
setkey(data,  pid, time)
  data <- data[time == startar | time == slutar, ]   #
  ## Välj ut  de tidpunkter som är intressanta som start och stop
  # if(!is.na(startar)){data <- data[J("startar","slutar")]}
## Räkna hur många ggr varje linneid det är, jag vill bara ha de med 2 ej 1.
#tabell <- (table(forsamlingsuttag$linneid)) # fungerar för att räkna antaler fall, jag vill bara ha de med båda tidpunkterna
tabell <- (table(data$pid)) # fungerar för att räkna antaler fall, jag vill bara ha de med båda tidpunkterna
## Tar ut de med obs bägge årtalen
## För att få linneid måste vi slå ihop rownames med antalet obs:
tabell <- data.table(cbind(as.numeric(rownames(tabell)), tabell))
## namnge med setnames
#setnames(tabell, old = c("V1", "tabell"), new=c("linneid", "antal"))
setnames(tabell, old = c("V1", "tabell"), new=c("pid", "antal"))
setkey(tabell,  antal) ## obs!! key antal!!
tabell <- tabell[J(2)]
#setkey(tabell, linneid)
setkey(tabell, pid)
urval <-data[tabell]  ## pid time region antal
 #tabell[forsamlingsuttag]
#urval <-
#dim(urval)
#head(urval)

# }## Omkodningsmaskin teststop 1
#############################################################################################
## Grunddatat klart !!!!!!!!!!!!!!
lanstab <- table(urval[time==min(time),]$region, urval[time==max(time)]$region)
lansdf <- as.data.frame(lanstab) # ?
#dim(lanstab)
lansdt <- data.table(lansdf) # sum(lansdt$Freq)
setnames(lansdt, old=c("Var1", "Var2", "Freq"), new=c("Old", "New", "Freq"))
#setnames(lansdt, c("Old", "New", "Freq"))   ######################################### Raden ovan ändrad till denna!!!!!!!!!!!!!!!!!!
setkey(lansdt, Old)
#lansdt[, sum(Freq), by=Old]     dim(lansdt)
# lansdt <- cbind(lansdt, rep(lansdt[, sum(Freq), by=Old]$V1, each=(dim(lanstab)[1])))
lansdt <- lansdt[lansdt[, sum(Freq), by=Old]]  # Rätt smart kod för att lägga till aggregerade frekvenser till datatabellen!!!!
#lansdt[lansdt[, (Freq/V1)]]
setnames(lansdt, old="V1", new="totold")
lansdt$propold <- lansdt$Freq/lansdt$totold #

setkey(lansdt, New)
lansdt <- lansdt[lansdt[, sum(Freq), by=New]]
setnames(lansdt, old="V1", new="totnew")
lansdt$propnew <- lansdt$Freq/lansdt$totnew #
#  lansdt[1:100,]
# }## Omkodningsmaskin teststop 2
################################################################################################
# cutoff <- 0.05 ## Parameter som bestämmer om en proportion är flyttning (F) eller ej (T)
lansdt$samma <- ifelse(test=(lansdt$propold > cutoff | lansdt$propnew > cutoff), yes = TRUE, no = FALSE)  # cutoff=0.02
# }## Omkodningsmaskin teststop 3
###############################################################################################
lansdt <- lansdt[lansdt$samma, ] ## urval av de med TRUE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
###########################################
#library(data.table) ## aktivera paket
#dt <- data.table(testdf) ## gör om till data.table
  ##########################################################################
  ## LOOP
setkey(lansdt, Old, New)  # dim(lansdt)
  # sum(is.na(lansdt$propnew))
#oldunique <- unique(testdf$old)  ## vilka unika församlingsnr som finns i old
  ###############################################################################################################################
  ###############################################################################################################################
  lansdt$neworg <- lansdt$New #  lansdt$New <-  lansdt$neworg


  oldunique <- unique(lansdt$Old)  ## vilka unika församlingsnr som finns i old  # length(oldunique) 2314 # length(unique(lansdt$New)) 2313
# lansdt$newcode <- lansdt$New  ## ?? sum(is.na(lansdt$newcode))
  lansdt$oldcode <-  lansdt$Old ## ?? sum(is.na(lansdt$newcode))
  lansdt$newtemp <- lansdt$New
  lansdt$newprel <- lansdt$New
  lansdt$newcode <-  lansdt$New
  lansdt$oldtemp <- lansdt$Old
  newunique <- unique(lansdt$New) ## loop2
  lansdt$New = lansdt$Old # test
  for(i in 1:length(oldunique)){ #  i=1 + i  i=24
    #lansdt$New[lansdt$newtemp %in% unique(lansdt$newtemp[lansdt$oldtemp == oldunique[i]]) ] <- oldunique[i]  # sum(is.na(oldunique))???????????????
    lansdt$New[lansdt$newtemp %in% unique(lansdt$newtemp[lansdt$New == oldunique[i]]) ] <- oldunique[i]  # sum(is.na(oldunique))???????????????

    }
  #lansdt$New = lansdt$oldtemp
  ########################################################################################################################################
  ###########################################
  #for(j in 1:djup){ # För att lösa nästade knutar
  varv = 0
  villkorold <- FALSE
  villkornew <- FALSE
  repeat{
  for(i in 1:length(oldunique)){ #
    lansdt$oldcode[lansdt$newtemp %in% unique(lansdt$newtemp[lansdt$oldcode == oldunique[i]]) ] <- oldunique[i]  # sum(is.na(oldunique))
  }# Loopen ger samma old värde till alla med gemensam old. Det innebär att den nya new koden är denna!
  oldunique <- unique(lansdt$oldcode)  ##  setkey(lansdt, Old, New)  as.data.frame(lansdt)
  ################################

   lansdt$newcode <- lansdt$oldcode  #################!!!!!!!!!!!!!!!!!!!??
  newunique <- unique(lansdt$newcode)
  for(i in 1:length(newunique)){ # Löser bara bilaterala knutar in order of apperence
    lansdt$newcode[lansdt$oldtemp %in% unique(lansdt$oldtemp[lansdt$newcode == newunique[i]]) ] <- newunique[i]  # sum(is.na(oldunique))
  }#
  #newcodeunique <- unique(lansdt$newcode)
  lansdt$oldcode <- lansdt$newcode  ## !!!!!!!!!!!!!!!!!????????????????
  ## för nästa varv i uppnästningen
  tab.old.df=as.data.frame(table(lansdt$newcode, lansdt$Old))
  tab.new.df=as.data.frame(table(lansdt$newcode, lansdt$neworg)) ## för att New blivit korrupt i förlopen
   ## Stop villkor
  villkorold <- length(unique(lansdt$Old)) == sum(tab.old.df$Freq > 0)
  villkornew <- length(unique(lansdt$neworg)) == sum(tab.new.df$Freq > 0)
  varv <- varv + 1
  if(villkorold == TRUE & villkornew == TRUE & varv < 25) break
  }#j (repeat)

  print(cat("Number of laps to solv the knots were: ", varv) )
  ####################################
#  as.data.frame(table(lansdt$New, lansdt$newcode))   length(oldunique)
  lansdt$New = lansdt$neworg
  lansdt <- lansdt[, c("New", "Old",  "Freq", "totold", "propold", "totnew", "propnew", "newcode" ), with=FALSE]
  lansdt$New <- as.integer(as.character(lansdt$New))
   lansdt$Old <- as.integer(as.character(lansdt$Old))
   lansdt$newcode <- as.integer(as.character(lansdt$newcode))
lansdt  # lansdt[is.na(lansdt$newcode),]        # as.data.frame(lansdt)
}

### Main Data Sets

BioGrad = all.data.set@BioGrad
BioGradWeek5 = all.data.set@BioGradWeek5
BioGradWeek9 = all.data.set@BioGradWeek9
BioGradWeek13 = all.data.set@BioGradWeek13
BioGradFin = all.data.set@BioGradFin
PeakGrad = all.data.set@PeakGrad
PeakGradWeek5 = all.data.set@PeakGradWeek5
PeakGradWeek9 = all.data.set@PeakGradWeek9
PeakGradWeek13 = all.data.set@PeakGradWeek13
PeakGradFin = all.data.set@PeakGradFin

BioPosGrad = all.data.set@BioPosGrad
BioPosGradWeek5 = all.data.set@BioPosGradWeek5
BioPosGradWeek9 = all.data.set@BioPosGradWeek9
BioPosGradWeek13 = all.data.set@BioPosGradWeek13
BioPosGradFin = all.data.set@BioPosGradFin
PeakPosGrad = all.data.set@PeakPosGrad
PeakPosGradWeek5 = all.data.set@PeakPosGradWeek5
PeakPosGradWeek9 = all.data.set@PeakPosGradWeek9
PeakPosGradWeek13 = all.data.set@PeakPosGradWeek13
PeakPosGradFin = all.data.set@PeakPosGradFin

BioTec = all.data.set@BioTec
BioTecWeek5 = all.data.set@BioTecWeek5
BioTecWeek9 = all.data.set@BioTecWeek9
BioTecWeek13 = all.data.set@BioTecWeek13
BioTecFin = all.data.set@BioTecFin
PeakTec = all.data.set@PeakTec
PeakTecWeek5 = all.data.set@PeakTecWeek5
PeakTecWeek9 = all.data.set@PeakTecWeek9
PeakTecWeek13 = all.data.set@PeakTecWeek13
PeakTecFin = all.data.set@PeakTecFin


##### function #######

trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

PossibleAgregations <- function(dataset,field,filter){
  r <- as.data.frame( round( prop.table(table(dataset[,field]))*100,2)   )
  r <- r[order(r$Freq),]
  print(subset(r, Freq > filter))
}

###### Aggregations ########

CityTransformation <- function(city){
  cities <- c("MOSSORÓ","NATAL")
  if( trim(city) %in% cities  ){
    as.character(city)
  }else{
    "OUTRAS"
  }
}

StateTransformation <- function(state){
  group_1 <- c("RIO GRANDE DO NORTE","RN","RIO GRANDE DO SUL")
  if( trim(state) %in% group_1  ){
    "RIO GRANDE"
  }else{
    "OUTRAS"
  }
}

HighSchoolTransformation <- function(highschool){
  group_1 <- c("SEM DEFINIÇAO","UNIVERSIDADE POTIGUAR -  UNP MOSSORÓ","UNIVERSIDADE POTIGUAR - UNP")
  if( trim(highschool) %in%  group_1 ){
    "SEM DEFINIÇAO"
  }else{
    "OUTRAS"
  }
}

HighSchoolToPosGradTransformation <- function(highschool){
  group_1 <- c("UNIVERSIDADE POTIGUAR -  UNP MOSSORÓ","UNIVERSIDADE POTIGUAR - UNP")
  if( trim(highschool) %in%  group_1 ){
    "SEM DEFINIÇAO"
  }else{
    "OUTRAS"
  }
}

FormaIngressoTransformation <- function(formaingresso){
  group_1 <- c("ENEM","ENEM/PROUNI")
  group_2 <- c("VESTIBULAR")

  if( trim(formaingresso) %in% group_1 ){
    "ENEM"
  }else if( trim(formaingresso) %in% group_2 ){
    as.character(formaingresso)
  }else{
    "OUTRAS"
  }

}

FacultyTransformation_Grad <- function(faculty){
  group_1 <- c("ESCOLA DE GESTAO E NEGÓCIOS - MOSSORÓ","ESCOLA DO DIREITO - MOSSORÓ","ESCOLA DE ENGENHARIAS E CIENCIAS EXATAS - MOSSORÓ","ESCOLA DA SAÚDE - MOSSORÓ")
  group_2 <- c("ESCOLA DE EDUCAÇAO","ESCOLA DE COMUNICAÇAO E ARTES","ESCOLA DO DIREITO")

  if( trim(faculty)%in%group_1 ){
    "MOSSORÓ"
  }else if( trim(faculty)%in%group_2 ){
    "OUTRAS"
  }else{
    as.character(faculty)
  }
}

FacultyTransformation_Tec <- function(faculty){
  group_1 <- c("ESCOLA DE ENGENHARIAS E CIENCIAS EXATAS - MOSSORÓ","ESCOLA DE GESTAO E NEGÓCIOS - MOSSORÓ")
  group_2 <- c("ESCOLA DE EDUCAÇAO","ESCOLA DE COMUNICAÇAO E ARTES","ESCOLA DO DIREITO")

  if( trim(faculty)%in%group_1 ){
    "MOSSORÓ"
  }else if( trim(faculty)%in%group_2 ){
    "OUTRAS"
  }else{
    as.character(faculty)
  }
}

CreditosTransformation <- function(creditos,cursos){
  x <- creditos/cursos
  if(is.na(x)){
    0
  }else{
    round(x,2)
  }
}


PayEngagement <- function(dp1,dp2,dp3,dp4,dp5,dp6,week){
  penalty.p1 <- 2.8
  penalty.p2 <- 3.3
  penalty.p3 <- 4.2
  penalty.p4 <- 5.5
  penalty.p5 <- 8.3
  penalty.p6 <- 16.6

  weeks.first.month <- 4
  days.on.week <- 5
  weeks.on.month <- 4

  grade <- 100

  ##Payment 1
  penalty1 <- 0
  dp1 <- if (is.na(dp1)) (week + weeks.first.month) * days.on.week * -1 else dp1
  if( dp1 < 0 ){
    result <- dp1 / days.on.week / weeks.on.month
    penalty1 <- ceiling(result * -1) * penalty.p1

  }

  ##Payment 2
  penalty2 <- 0
  dp2 <- if (is.na(dp2)) (week) * days.on.week * -1 else dp2
  if( dp2 < 0 ){
    result <- dp2 / days.on.week / weeks.on.month
    penalty2 <- ceiling(result * -1) * penalty.p2

  }

  ##Payment 3
  penalty3 <- 0
  dp3 <- if (is.na(dp3)) (week - 4) * days.on.week * -1 else dp3
  if( dp3 < 0 ){
    result <- dp3 / days.on.week / weeks.on.month
    penalty3 <- ceiling(result * -1) * penalty.p3
  }


  ##Payment 4
  penalty4 <- 0
  if(week >= 9){
    dp4 <- if (is.na(dp4)) (week - 8) * days.on.week * -1 else dp4
    if( dp4 < 0 ){
      result <- dp4 / days.on.week / weeks.on.month
      penalty4 <- ceiling(result * -1) * penalty.p4

    }
  }

  ##Payment 5
  penalty5 <- 0
  if(week >= 13){
    dp5 <- if (is.na(dp5)) (week - 12) * days.on.week * -1 else dp5
    if( dp5 < 0 ){
      result <- dp5 / days.on.week / weeks.on.month
      penalty5 <- ceiling(result * -1) * penalty.p5

    }
  }

  ##Payment 6
  Penalty6 <- 0
  if(week >= 19){
    dp6 <- if (is.na(dp6))  -1 else dp6
    if(dp6 < 0){
      Penalty6 <- penalty.p6
    }
  }

  grade - penalty1 - penalty2 - penalty3 - penalty4 - penalty5 - Penalty6


}


PaymentBalance <- function(p1,p2,p3,p4,p5,p6,totalbalance){
  p1 <- if (is.na(p1)) 0 else p1
  p2 <- if (is.na(p2)) 0 else p2
  p3 <- if (is.na(p3)) 0 else p3
  p4 <- if (is.na(p4)) 0 else p4
  p5 <- if (is.na(p5)) 0 else p5
  p6 <- if (is.na(p6)) 0 else p6

  r <- (p1 + p2 + p3 + p4 + p5 + p6)
  if(!is.na(totalbalance)){
    r <- totalbalance - (p1 + p2 + p3 + p4 + p5 + p6)
  }
  r

}

ScholarshipType <- function(value){
  if(is.na(value)) as.factor("WITHOUT SCHOLARSHIP")
  else value
}

yearsToEnter <- function(ac,hy){
  ac <- as.character(ac)
  ac <- substring(ac,1,4)
  ac <- as.numeric(ac)

  hy <- if (is.na(hy)) ac-1 else as.numeric(hy)

  abs(ac-hy)
}


######## city transformations
#
###########################

## GRAD ##

print("city aggregation to Grad novo students")

if(nrow(BioGrad)>1)
  BioGrad$city.aggregation <- mapply(CityTransformation,BioGrad$city)
if(nrow(BioGrad)>1)
  BioGrad <- BioGrad[,!( names(BioGrad) %in% c("city") )]

if(nrow(BioGradWeek5)>1)
  BioGradWeek5$city.aggregation <- mapply(CityTransformation,BioGradWeek5$city)
if(nrow(BioGradWeek5)>1)
  BioGradWeek5 <- BioGradWeek5[,!( names(BioGradWeek5) %in% c("city") )]

if(nrow(BioGradWeek9)>1)
  BioGradWeek9$city.aggregation <- mapply(CityTransformation,BioGradWeek9$city)
if(nrow(BioGradWeek9)>1)
  BioGradWeek9 <- BioGradWeek9[,!( names(BioGradWeek9) %in% c("city") )]

if(nrow(BioGradWeek13)>1)
  BioGradWeek13$city.aggregation <- mapply(CityTransformation,BioGradWeek13$city)
if(nrow(BioGradWeek13)>1)
  BioGradWeek13 <- BioGradWeek13[,!( names(BioGradWeek13) %in% c("city") )]

if(nrow(BioGradFin)>1)
  BioGradFin$city.aggregation <- mapply(CityTransformation,BioGradFin$city)
if(nrow(BioGradFin)>1)
  BioGradFin <- BioGradFin[,!( names(BioGradFin) %in% c("city") )]


print("Grad veteran students have the same city, we are going to drop this attr from the datasets. if you are sending real information please contact to uayeb.caballero@laureate.net to do aggreation in this")
if(nrow(PeakGrad)>1)
  PeakGrad <- PeakGrad[,!( names(PeakGrad) %in% c("city") )]
if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5 <- PeakGradWeek5[,!( names(PeakGradWeek5) %in% c("city") )]
if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9 <- PeakGradWeek9[,!( names(PeakGradWeek9) %in% c("city") )]
if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13 <- PeakGradWeek13[,!( names(PeakGradWeek13) %in% c("city") )]
if(nrow(PeakGradFin)>1)
  PeakGradFin <- PeakGradFin[,!( names(PeakGradFin) %in% c("city") )]

### TEc ###

print("city aggregation to Tec novo students")

if(nrow(BioTec) > 1)
  BioTec$city.aggregation <- mapply(CityTransformation,BioTec$city)
if(nrow(BioTec) > 1)
  BioTec <- BioTec[,!( names(BioTec) %in% c("city") )]

if(nrow(BioTecWeek5) > 1)
  BioTecWeek5$city.aggregation <- mapply(CityTransformation,BioTecWeek5$city)
if(nrow(BioTecWeek5) > 1)
  BioTecWeek5 <- BioTecWeek5[,!( names(BioTecWeek5) %in% c("city") )]

if(nrow(BioTecWeek9) > 1)
  BioTecWeek9$city.aggregation <- mapply(CityTransformation,BioTecWeek9$city)
if(nrow(BioTecWeek9) > 1)
  BioTecWeek9 <- BioTecWeek9[,!( names(BioTecWeek9) %in% c("city") )]

if(nrow(BioTecWeek13) > 1)
  BioTecWeek13$city.aggregation <- mapply(CityTransformation,BioTecWeek13$city)
if(nrow(BioTecWeek13) > 1)
  BioTecWeek13 <- BioTecWeek13[,!( names(BioTecWeek13) %in% c("city") )]

if(nrow(BioTecFin) > 1)
  BioTecFin$city.aggregation <- mapply(CityTransformation,BioTecFin$city)
if(nrow(BioTecFin) > 1)
  BioTecFin <- BioTecFin[,!( names(BioTecFin) %in% c("city") )]

print("Tec veteran students have the same city, we are going to drop this attr from the datasets. if you are sending real information please contact to uayeb.caballero@laureate.net to do aggreation in this")
if(nrow(PeakTec)>1)
  PeakTec <- PeakTec[,!( names(PeakTec) %in% c("city") )]
if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5 <- PeakTecWeek5[,!( names(PeakTecWeek5) %in% c("city") )]
if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9 <- PeakTecWeek9[,!( names(PeakTecWeek9) %in% c("city") )]
if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13 <- PeakTecWeek13[,!( names(PeakTecWeek13) %in% c("city") )]
if(nrow(PeakTecFin)>1)
  PeakTecFin <- PeakTecFin[,!( names(PeakTecFin) %in% c("city") )]


### POSGRAD ###

print("city aggregation to PosGrad novo students")

if(nrow(BioPosGrad) > 1)
  BioPosGrad$city.aggregation <- mapply(CityTransformation,BioPosGrad$city)
if(nrow(BioPosGrad) > 1)
  BioPosGrad <- BioPosGrad[,!( names(BioPosGrad) %in% c("city") )]

if(nrow(BioPosGradWeek5) > 1)
  BioPosGradWeek5$city.aggregation <- mapply(CityTransformation,BioPosGradWeek5$city)
if(nrow(BioPosGradWeek5) > 1)
  BioPosGradWeek5 <- BioPosGradWeek5[,!( names(BioPosGradWeek5) %in% c("city") )]

if(nrow(BioPosGradWeek9) > 1)
  BioPosGradWeek9$city.aggregation <- mapply(CityTransformation,BioPosGradWeek9$city)
if(nrow(BioPosGradWeek9) > 1)
  BioPosGradWeek9 <- BioPosGradWeek9[,!( names(BioPosGradWeek9) %in% c("city") )]

if(nrow(BioPosGradWeek13) > 1)
  BioPosGradWeek13$city.aggregation <- mapply(CityTransformation,BioPosGradWeek13$city)
if(nrow(BioPosGradWeek13) > 1)
  BioPosGradWeek13 <- BioPosGradWeek13[,!( names(BioPosGradWeek13) %in% c("city") )]

if(nrow(BioPosGradFin) > 1)
  BioPosGradFin$city.aggregation <- mapply(CityTransformation,BioPosGradFin$city)
if(nrow(BioPosGradFin) > 1)
  BioPosGradFin <- BioPosGradFin[,!( names(BioPosGradFin) %in% c("city") )]


print("PosGrad veteran students have the same city, we are going to drop this attr from the datasets. if you are sending real information please contact to uayeb.caballero@laureate.net to do aggreation in this")
if(nrow(PeakPosGrad)>1)
  PeakPosGrad <- PeakPosGrad[,!( names(PeakPosGrad) %in% c("city") )]
if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5 <- PeakPosGradWeek5[,!( names(PeakPosGradWeek5) %in% c("city") )]
if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9 <- PeakPosGradWeek9[,!( names(PeakPosGradWeek9) %in% c("city") )]
if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13 <- PeakPosGradWeek13[,!( names(PeakPosGradWeek13) %in% c("city") )]
if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin <- PeakPosGradFin[,!( names(PeakPosGradFin) %in% c("city") )]


######## state transformations
#
###########################


### POSTGRAD ###

print("state aggregation to posgrad novo students")

if(nrow(BioPosGrad)>1)
  BioPosGrad$state.aggregation <- mapply(StateTransformation,BioPosGrad$state)
if(nrow(BioPosGrad)>1)
  BioPosGrad <- BioPosGrad[,!( names(BioPosGrad) %in% c("state") )]

if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5$state.aggregation <- mapply(StateTransformation,BioPosGradWeek5$state)
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5 <- BioPosGradWeek5[,!( names(BioPosGradWeek5) %in% c("state") )]

if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9$state.aggregation <- mapply(StateTransformation,BioPosGradWeek9$state)
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9 <- BioPosGradWeek9[,!( names(BioPosGradWeek9) %in% c("state") )]

if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13$state.aggregation <- mapply(StateTransformation,BioPosGradWeek13$state)
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13 <- BioPosGradWeek13[,!( names(BioPosGradWeek13) %in% c("state") )]

if(nrow(BioPosGradFin)>1)
  BioPosGradFin$state.aggregation <- mapply(StateTransformation,BioPosGradFin$state)
if(nrow(BioPosGradFin)>1)
  BioPosGradFin <- BioPosGradFin[,!( names(BioPosGradFin) %in% c("state") )]


print("PosGrad veteran students have the same state, we are going to drop this attr from the datasets. if you are sending real information please contact to uayeb.caballero@laureate.net to do aggreation in this")
if(nrow(PeakPosGrad)>1)
  PeakPosGrad <- PeakPosGrad[,!( names(PeakPosGrad) %in% c("state") )]
if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5 <- PeakPosGradWeek5[,!( names(PeakPosGradWeek5) %in% c("state") )]
if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9 <- PeakPosGradWeek9[,!( names(PeakPosGradWeek9) %in% c("state") )]
if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13 <- PeakPosGradWeek13[,!( names(PeakPosGradWeek13) %in% c("state") )]
if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin <- PeakPosGradFin[,!( names(PeakPosGradFin) %in% c("state") )]

### GRAD ###

print("state aggregation to posgrad novo students")

if(nrow(BioGrad)>1)
  BioGrad$state.aggregation <- mapply(StateTransformation,BioGrad$state)
if(nrow(BioGrad)>1)
  BioGrad <- BioGrad[,!( names(BioGrad) %in% c("state") )]

if(nrow(BioGradWeek5)>1)
  BioGradWeek5$state.aggregation <- mapply(StateTransformation,BioGradWeek5$state)
if(nrow(BioGradWeek5)>1)
  BioGradWeek5 <- BioGradWeek5[,!( names(BioGradWeek5) %in% c("state") )]

if(nrow(BioGradWeek9)>1)
  BioGradWeek9$state.aggregation <- mapply(StateTransformation,BioGradWeek9$state)
if(nrow(BioGradWeek9)>1)
  BioGradWeek9 <- BioGradWeek9[,!( names(BioGradWeek9) %in% c("state") )]

if(nrow(BioGradWeek13)>1)
  BioGradWeek13$state.aggregation <- mapply(StateTransformation,BioGradWeek13$state)
if(nrow(BioGradWeek13)>1)
  BioGradWeek13 <- BioGradWeek13[,!( names(BioGradWeek13) %in% c("state") )]

if(nrow(BioGradFin)>1)
  BioGradFin$state.aggregation <- mapply(StateTransformation,BioGradFin$state)
if(nrow(BioGradFin)>1)
  BioGradFin <- BioGradFin[,!( names(BioGradFin) %in% c("state") )]

print("Grad veteran students have the same state, we are going to drop this attr from the datasets. if you are sending real information please contact to uayeb.caballero@laureate.net to do aggreation in this")
if(nrow(PeakGrad)>1)
  PeakGrad <- PeakGrad[,!( names(PeakGrad) %in% c("state") )]
if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5 <- PeakGradWeek5[,!( names(PeakGradWeek5) %in% c("state") )]
if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9 <- PeakGradWeek9[,!( names(PeakGradWeek9) %in% c("state") )]
if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13 <- PeakGradWeek13[,!( names(PeakGradWeek13) %in% c("state") )]
if(nrow(PeakGradFin)>1)
  PeakGradFin <- PeakGradFin[,!( names(PeakGradFin) %in% c("state") )]

### TEC ###

print("state aggregation to tec novo students")

if(nrow(BioTec)>1)
  BioTec$state.aggregation <- mapply(StateTransformation,BioTec$state)
if(nrow(BioTec)>1)
  BioTec <- BioTec[,!( names(BioTec) %in% c("state") )]

if(nrow(BioTecWeek5)>1)
  BioTecWeek5$state.aggregation <- mapply(StateTransformation,BioTecWeek5$state)
if(nrow(BioTecWeek5)>1)
  BioTecWeek5 <- BioTecWeek5[,!( names(BioTecWeek5) %in% c("state") )]

if(nrow(BioTecWeek9)>1)
  BioTecWeek9$state.aggregation <- mapply(StateTransformation,BioTecWeek9$state)
if(nrow(BioTecWeek9)>1)
  BioTecWeek9 <- BioTecWeek9[,!( names(BioTecWeek9) %in% c("state") )]

if(nrow(BioTecWeek13)>1)
  BioTecWeek13$state.aggregation <- mapply(StateTransformation,BioTecWeek13$state)
if(nrow(BioTecWeek13)>1)
  BioTecWeek13 <- BioTecWeek13[,!( names(BioTecWeek13) %in% c("state") )]

if(nrow(BioTecFin)>1)
  BioTecFin$state.aggregation <- mapply(StateTransformation,BioTecFin$state)
if(nrow(BioTecFin)>1)
  BioTecFin <- BioTecFin[,!( names(BioTecFin) %in% c("state") )]

print("Tec veteran students have the same state, we are going to drop this attr from the datasets. if you are sending real information please contact to uayeb.caballero@laureate.net to do aggreation in this")
if(nrow(PeakTec)>1)
  PeakTec <- PeakTec[,!( names(PeakTec) %in% c("state") )]
if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5 <- PeakTecWeek5[,!( names(PeakTecWeek5) %in% c("state") )]
if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9 <- PeakTecWeek9[,!( names(PeakTecWeek9) %in% c("state") )]
if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13 <- PeakTecWeek13[,!( names(PeakTecWeek13) %in% c("state") )]
if(nrow(PeakTecFin)>1)
  PeakTecFin <- PeakTecFin[,!( names(PeakTecFin) %in% c("state") )]



######## high school transformations
#
###########################

print("high school aggregation to grad novo students")

if(nrow(BioGrad)>1)
  BioGrad$high.school.agreggation <- mapply(HighSchoolTransformation,BioGrad$high.school)
if(nrow(BioGrad)>1)
  BioGrad <- BioGrad[,!( names(BioGrad) %in% c("high.school") )]

if(nrow(BioGradWeek5)>1)
  BioGradWeek5$high.school.agreggation <- mapply(HighSchoolTransformation,BioGradWeek5$high.school)
if(nrow(BioGradWeek5)>1)
  BioGradWeek5 <- BioGradWeek5[,!( names(BioGradWeek5) %in% c("high.school") )]

if(nrow(BioGradWeek9)>1)
  BioGradWeek9$high.school.agreggation <- mapply(HighSchoolTransformation,BioGradWeek9$high.school)
if(nrow(BioGradWeek9)>1)
  BioGradWeek9 <- BioGradWeek9[,!( names(BioGradWeek9) %in% c("high.school") )]

if(nrow(BioGradWeek13)>1)
  BioGradWeek13$high.school.agreggation <- mapply(HighSchoolTransformation,BioGradWeek13$high.school)
if(nrow(BioGradWeek13)>1)
  BioGradWeek13 <- BioGradWeek13[,!( names(BioGradWeek13) %in% c("high.school") )]

if(nrow(BioGradFin)>1)
  BioGradFin$high.school.agreggation <- mapply(HighSchoolTransformation,BioGradFin$high.school)
if(nrow(BioGradFin)>1)
  BioGradFin <- BioGradFin[,!( names(BioGradFin) %in% c("high.school") )]

print("high school aggregation to Tec novo students")

if(nrow(BioTec)>1)
  BioTec$high.school.agreggation <- mapply(HighSchoolTransformation,BioTec$high.school)
if(nrow(BioTec)>1)
  BioTec <- BioTec[,!( names(BioTec) %in% c("high.school") )]

if(nrow(BioTecWeek5)>1)
  BioTecWeek5$high.school.agreggation <- mapply(HighSchoolTransformation,BioTecWeek5$high.school)
if(nrow(BioTecWeek5)>1)
  BioTecWeek5 <- BioTecWeek5[,!( names(BioTecWeek5) %in% c("high.school") )]

if(nrow(BioTecWeek9)>1)
  BioTecWeek9$high.school.agreggation <- mapply(HighSchoolTransformation,BioTecWeek9$high.school)
if(nrow(BioTecWeek9)>1)
  BioTecWeek9 <- BioTecWeek9[,!( names(BioTecWeek9) %in% c("high.school") )]

if(nrow(BioTecWeek13)>1)
  BioTecWeek13$high.school.agreggation <- mapply(HighSchoolTransformation,BioTecWeek13$high.school)
if(nrow(BioTecWeek13)>1)
  BioTecWeek13 <- BioTecWeek13[,!( names(BioTecWeek13) %in% c("high.school") )]

if(nrow(BioTecFin)>1)
  BioTecFin$high.school.agreggation <- mapply(HighSchoolTransformation,BioTecFin$high.school)
if(nrow(BioTecFin)>1)
  BioTecFin <- BioTecFin[,!( names(BioTecFin) %in% c("high.school") )]

print("high school aggregation to PosGrad novo students")

if(nrow(BioPosGrad)>1)
  BioPosGrad$high.school.agreggation <- mapply(HighSchoolToPosGradTransformation,BioPosGrad$high.school)
if(nrow(BioPosGrad)>1)
  BioPosGrad <- BioPosGrad[,!( names(BioPosGrad) %in% c("high.school") )]

if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5$high.school.agreggation <- mapply(HighSchoolToPosGradTransformation,BioPosGradWeek5$high.school)
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5 <- BioPosGradWeek5[,!( names(BioPosGradWeek5) %in% c("high.school") )]

if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9$high.school.agreggation <- mapply(HighSchoolToPosGradTransformation,BioPosGradWeek9$high.school)
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9 <- BioPosGradWeek9[,!( names(BioPosGradWeek9) %in% c("high.school") )]

if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13$high.school.agreggation <- mapply(HighSchoolToPosGradTransformation,BioPosGradWeek13$high.school)
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13 <- BioPosGradWeek13[,!( names(BioPosGradWeek13) %in% c("high.school") )]

if(nrow(BioPosGradFin)>1)
  BioPosGradFin$high.school.agreggation <- mapply(HighSchoolToPosGradTransformation,BioPosGradFin$high.school)
if(nrow(BioPosGradFin)>1)
  BioPosGradFin <- BioPosGradFin[,!( names(BioPosGradFin) %in% c("high.school") )]

######## Forma Ingresso
#
###########################

drop.rows <- c(
  "EXTENSAO",
  "INTERCÂMBIO/LAUREATE",
  "ANÁLISE DE CURRÍCULO",
  "VESTIBULAR SIA",
  "PROCESSO SELETIVO",
  "VESTIBULAR TIA",
  "CONCESSAO DE VAGA"
)

print("Drop rows on tec and grad, forma de ingresso atypical values")

if(nrow(BioGrad)>1)
  BioGrad <- BioGrad[ !(BioGrad$forma.ingresso %in% drop.rows) ,]
if(nrow(BioGradWeek5)>1)
  BioGradWeek5 <- BioGradWeek5[ !(BioGradWeek5$forma.ingresso %in% drop.rows) ,]
if(nrow(BioGradWeek9)>1)
  BioGradWeek9 <- BioGradWeek9[ !(BioGradWeek9$forma.ingresso %in% drop.rows) ,]
if(nrow(BioGradWeek13)>1)
  BioGradWeek13 <- BioGradWeek13[ !(BioGradWeek13$forma.ingresso %in% drop.rows) ,]
if(nrow(BioGradFin)>1)
  BioGradFin <- BioGradFin[ !(BioGradFin$forma.ingresso %in% drop.rows) ,]

if(nrow(BioTec)>1)
  BioTec <- BioTec[ !(BioTec$forma.ingresso %in% drop.rows) ,]
if(nrow(BioTecWeek5)>1)
  BioTecWeek5 <- BioTecWeek5[ !(BioTecWeek5$forma.ingresso %in% drop.rows) ,]
if(nrow(BioTecWeek9)>1)
  BioTecWeek9 <- BioTecWeek9[ !(BioTecWeek9$forma.ingresso %in% drop.rows) ,]
if(nrow(BioTecWeek13)>1)
  BioTecWeek13 <- BioTecWeek13[ !(BioTecWeek13$forma.ingresso %in% drop.rows) ,]
if(nrow(BioTecFin)>1)
  BioTecFin <- BioTecFin[ !(BioTecFin$forma.ingresso %in% drop.rows) ,]

print("forma ingresso aggregation to grad novo students")

if(nrow(BioGrad)>1)
  BioGrad$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioGrad$forma.ingresso)
if(nrow(BioGrad)>1)
  BioGrad <- BioGrad[,!( names(BioGrad) %in% c("forma.ingresso") )]

if(nrow(BioGradWeek5)>1)
  BioGradWeek5$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioGradWeek5$forma.ingresso)
if(nrow(BioGradWeek5)>1)
  BioGradWeek5 <- BioGradWeek5[,!( names(BioGradWeek5) %in% c("forma.ingresso") )]

if(nrow(BioGradWeek9)>1)
  BioGradWeek9$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioGradWeek9$forma.ingresso)
if(nrow(BioGradWeek9)>1)
  BioGradWeek9 <- BioGradWeek9[,!( names(BioGradWeek9) %in% c("forma.ingresso") )]

if(nrow(BioGradWeek13)>1)
  BioGradWeek13$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioGradWeek13$forma.ingresso)
if(nrow(BioGradWeek13)>1)
  BioGradWeek13 <- BioGradWeek13[,!( names(BioGradWeek13) %in% c("forma.ingresso") )]

if(nrow(BioGradFin)>1)
  BioGradFin$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioGradFin$forma.ingresso)
if(nrow(BioGradFin)>1)
  BioGradFin <- BioGradFin[,!( names(BioGradFin) %in% c("forma.ingresso") )]

print("forma ingresso aggregation to Tec novo students")

if(nrow(BioTec)>1)
  BioTec$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioTec$forma.ingresso)
if(nrow(BioTec)>1)
  BioTec <- BioTec[,!( names(BioTec) %in% c("forma.ingresso") )]

if(nrow(BioTecWeek5)>1)
  BioTecWeek5$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioTecWeek5$forma.ingresso)
if(nrow(BioTecWeek5)>1)
  BioTecWeek5 <- BioTecWeek5[,!( names(BioTecWeek5) %in% c("forma.ingresso") )]

if(nrow(BioTecWeek9)>1)
  BioTecWeek9$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioTecWeek9$forma.ingresso)
if(nrow(BioTecWeek9)>1)
  BioTecWeek9 <- BioTecWeek9[,!( names(BioTecWeek9) %in% c("forma.ingresso") )]

if(nrow(BioTecWeek13)>1)
  BioTecWeek13$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioTecWeek13$forma.ingresso)
if(nrow(BioTecWeek13)>1)
  BioTecWeek13 <- BioTecWeek13[,!( names(BioTecWeek13) %in% c("forma.ingresso") )]

if(nrow(BioTecFin)>1)
  BioTecFin$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioTecFin$forma.ingresso)
if(nrow(BioTecFin)>1)
  BioTecFin <- BioTecFin[,!( names(BioTecFin) %in% c("forma.ingresso") )]

######## Program
#
###########################

print("Dropping program for grad nova students")

if(nrow(BioGrad)>1)
  BioGrad <- BioGrad[, !( names(BioGrad) %in% c("program.of.study") )  ]
if(nrow(BioGradWeek5)>1)
  BioGradWeek5 <- BioGradWeek5[, !( names(BioGradWeek5) %in% c("program.of.study") )  ]
if(nrow(BioGradWeek9)>1)
  BioGradWeek9 <- BioGradWeek9[, !( names(BioGradWeek9) %in% c("program.of.study") )  ]
if(nrow(BioGradWeek13)>1)
  BioGradWeek13 <- BioGradWeek13[, !( names(BioGradWeek13) %in% c("program.of.study") )  ]
if(nrow(BioGradFin)>1)
  BioGradFin <- BioGradFin[, !( names(BioGradFin) %in% c("program.of.study") )  ]

print("Dropping program for grad veteran students")

if(nrow(PeakGrad)>1)
  PeakGrad <- PeakGrad[, !(names(PeakGrad) %in% c("program.of.study") ) ]
if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5 <- PeakGradWeek5[, !(names(PeakGradWeek5) %in% c("program.of.study") ) ]
if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9 <- PeakGradWeek9[, !(names(PeakGradWeek9) %in% c("program.of.study") ) ]
if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13 <- PeakGradWeek13[, !(names(PeakGradWeek13) %in% c("program.of.study") ) ]
if(nrow(PeakGradFin)>1)
  PeakGradFin <- PeakGradFin[, !(names(PeakGradFin) %in% c("program.of.study") ) ]

print("Dropping program for Tec nova students")

if(nrow(BioTec)>1)
  BioTec <- BioTec[, !( names(BioTec) %in% c("program.of.study") )  ]
if(nrow(BioTecWeek5)>1)
  BioTecWeek5 <- BioTecWeek5[, !( names(BioTecWeek5) %in% c("program.of.study") )  ]
if(nrow(BioTecWeek9)>1)
  BioTecWeek9 <- BioTecWeek9[, !( names(BioTecWeek9) %in% c("program.of.study") )  ]
if(nrow(BioTecWeek13)>1)
  BioTecWeek13 <- BioTecWeek13[, !( names(BioTecWeek13) %in% c("program.of.study") )  ]
if(nrow(BioTecFin)>1)
  BioTecFin <- BioTecFin[, !( names(BioTecFin) %in% c("program.of.study") )  ]

print("Dropping program for Tec veteran students")

if(nrow(PeakTec)>1)
  PeakTec <- PeakTec[, !(names(PeakTec) %in% c("program.of.study") ) ]
if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5 <- PeakTecWeek5[, !(names(PeakTecWeek5) %in% c("program.of.study") ) ]
if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9 <- PeakTecWeek9[, !(names(PeakTecWeek9) %in% c("program.of.study") ) ]
if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13 <- PeakTecWeek13[, !(names(PeakTecWeek13) %in% c("program.of.study") ) ]
if(nrow(PeakTecFin)>1)
  PeakTecFin <- PeakTecFin[, !(names(PeakTecFin) %in% c("program.of.study") ) ]

print("Dropping program for PosGrad nova students")

if(nrow(BioPosGrad)>1)
  BioPosGrad <- BioPosGrad[, !( names(BioPosGrad) %in% c("program.of.study") )  ]
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5 <- BioPosGradWeek5[, !( names(BioPosGradWeek5) %in% c("program.of.study") )  ]
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9 <- BioPosGradWeek9[, !( names(BioPosGradWeek9) %in% c("program.of.study") )  ]
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13 <- BioPosGradWeek13[, !( names(BioPosGradWeek13) %in% c("program.of.study") )  ]
if(nrow(BioPosGradFin)>1)
  BioPosGradFin <- BioPosGradFin[, !( names(BioPosGradFin) %in% c("program.of.study") )  ]

print("Dropping program for PosGrad veteran students")

if(nrow(PeakPosGrad)>1)
  PeakPosGrad <- PeakPosGrad[, !(names(PeakPosGrad) %in% c("program.of.study") ) ]
if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5 <- PeakPosGradWeek5[, !(names(PeakPosGradWeek5) %in% c("program.of.study") ) ]
if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9 <- PeakPosGradWeek9[, !(names(PeakPosGradWeek9) %in% c("program.of.study") ) ]
if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13 <- PeakPosGradWeek13[, !(names(PeakPosGradWeek13) %in% c("program.of.study") ) ]
if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin <- PeakPosGradFin[, !(names(PeakPosGradFin) %in% c("program.of.study") ) ]

######## Faculty
#
###########################


## Grad ##

print("Dropping some rows for Grad novo students, dropping atypical values on faculty")

drop.rows <- c(
  "ESCOLA TÉCNICA POTIGUAR",
  "INSTITUCIONAL",
  "ESCOLA DE HOSPITALIDADE"
)

if(nrow(BioGrad)>1)
  BioGrad <- BioGrad[ !(trim(BioGrad$faculty)%in%drop.rows) ,]
if(nrow(BioGradWeek5)>1)
  BioGradWeek5 <- BioGradWeek5[ !(trim(BioGradWeek5$faculty)%in%drop.rows) ,]
if(nrow(BioGradWeek9)>1)
  BioGradWeek9 <- BioGradWeek9[ !(trim(BioGradWeek9$faculty)%in%drop.rows) ,]
if(nrow(BioGradWeek13)>1)
  BioGradWeek13 <- BioGradWeek13[ !(trim(BioGradWeek13$faculty)%in%drop.rows) ,]
if(nrow(BioGradFin)>1)
  BioGradFin <- BioGradFin[ !(trim(BioGradFin$faculty)%in%drop.rows) ,]

print("faculty aggregation to grad novo students")

if(nrow(BioGrad)>1){
  BioGrad$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioGrad$faculty)
  BioGrad <- BioGrad[,!( names(BioGrad) %in% c("faculty") )]
}

if(nrow(BioGradWeek5)>1){
  BioGradWeek5$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioGradWeek5$faculty)
  BioGradWeek5 <- BioGradWeek5[,!( names(BioGradWeek5) %in% c("faculty") )]
}

if(nrow(BioGradWeek9)>1){
  BioGradWeek9$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioGradWeek9$faculty)
  BioGradWeek9 <- BioGradWeek9[,!( names(BioGradWeek9) %in% c("faculty") )]
}

if(nrow(BioGradWeek13)>1){
  BioGradWeek13$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioGradWeek13$faculty)
  BioGradWeek13 <- BioGradWeek13[,!( names(BioGradWeek13) %in% c("faculty") )]
}

if(nrow(BioGradFin)>1){
  BioGradFin$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioGradFin$faculty)
  BioGradFin <- BioGradFin[,!( names(BioGradFin) %in% c("faculty") )]
}

print("Dropping some rows for Grad veteran students, dropping atypical values on faculty")

drop.rows <- c(
  "INSTITUCIONAL"
)

if(nrow(PeakGrad)>1)
  PeakGrad <- PeakGrad[ !(trim(PeakGrad$faculty)%in%drop.rows) ,]
if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5 <- PeakGradWeek5[ !(trim(PeakGradWeek5$faculty)%in%drop.rows) ,]
if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9 <- PeakGradWeek9[ !(trim(PeakGradWeek9$faculty)%in%drop.rows) ,]
if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13 <- PeakGradWeek13[ !(trim(PeakGradWeek13$faculty)%in%drop.rows) ,]
if(nrow(PeakGradFin)>1)
  PeakGradFin <- PeakGradFin[ !(trim(PeakGradFin$faculty)%in%drop.rows) ,]

print("faculty aggregation to grad veteran students")

if(nrow(PeakGrad)>1){
  PeakGrad$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakGrad$faculty)
  PeakGrad <- PeakGrad[,!( names(PeakGrad) %in% c("faculty") )]
}

if(nrow(PeakGradWeek5)>1){
  PeakGradWeek5$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakGradWeek5$faculty)
  PeakGradWeek5 <- PeakGradWeek5[,!( names(PeakGradWeek5) %in% c("faculty") )]
}

if(nrow(PeakGradWeek9)>1){
  PeakGradWeek9$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakGradWeek9$faculty)
  PeakGradWeek9 <- PeakGradWeek9[,!( names(PeakGradWeek9) %in% c("faculty") )]
}

if(nrow(PeakGradWeek13)>1){
  PeakGradWeek13$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakGradWeek13$faculty)
  PeakGradWeek13 <- PeakGradWeek13[,!( names(PeakGradWeek13) %in% c("faculty") )]
}

if(nrow(PeakGradFin)>1){
  PeakGradFin$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakGradFin$faculty)
  PeakGradFin <- PeakGradFin[,!( names(PeakGradFin) %in% c("faculty") )]
}

## Tec ##

print("faculty aggregation to tec novo students")

if(nrow(BioTec)>1){
  BioTec$faculty.agreggation <- mapply(FacultyTransformation_Tec,BioTec$faculty)
  BioTec <- BioTec[,!( names(BioTec) %in% c("faculty") )]
}

if(nrow(BioTecWeek5)>1){
  BioTecWeek5$faculty.agreggation <- mapply(FacultyTransformation_Tec,BioTecWeek5$faculty)
  BioTecWeek5 <- BioTecWeek5[,!( names(BioTecWeek5) %in% c("faculty") )]
}

if(nrow(BioTecWeek9)>1){
  BioTecWeek9$faculty.agreggation <- mapply(FacultyTransformation_Tec,BioTecWeek9$faculty)
  BioTecWeek9 <- BioTecWeek9[,!( names(BioTecWeek9) %in% c("faculty") )]
}

if(nrow(BioTecWeek13)>1){
  BioTecWeek13$faculty.agreggation <- mapply(FacultyTransformation_Tec,BioTecWeek13$faculty)
  BioTecWeek13 <- BioTecWeek13[,!( names(BioTecWeek13) %in% c("faculty") )]
}

if(nrow(BioTecFin)>1){
  BioTecFin$faculty.agreggation <- mapply(FacultyTransformation_Tec,BioTecFin$faculty)
  BioTecFin <- BioTecFin[,!( names(BioTecFin) %in% c("faculty") )]
}

print("faculty aggregation to tec veteran students")

if(nrow(PeakTec)>1){
  PeakTec$faculty.agreggation <- mapply(FacultyTransformation_Tec,PeakTec$faculty)
  PeakTec <- PeakTec[,!( names(PeakTec) %in% c("faculty") )]
}

if(nrow(PeakTecWeek5)>1){
  PeakTecWeek5$faculty.agreggation <- mapply(FacultyTransformation_Tec,PeakTecWeek5$faculty)
  PeakTecWeek5 <- PeakTecWeek5[,!( names(PeakTecWeek5) %in% c("faculty") )]
}

if(nrow(PeakTecWeek9)>1){
  PeakTecWeek9$faculty.agreggation <- mapply(FacultyTransformation_Tec,PeakTecWeek9$faculty)
  PeakTecWeek9 <- PeakTecWeek9[,!( names(PeakTecWeek9) %in% c("faculty") )]
}

if(nrow(PeakTecWeek13)>1){
  PeakTecWeek13$faculty.agreggation <- mapply(FacultyTransformation_Tec,PeakTecWeek13$faculty)
  PeakTecWeek13 <- PeakTecWeek13[,!( names(PeakTecWeek13) %in% c("faculty") )]
}

if(nrow(PeakTecFin)>1){
  PeakTecFin$faculty.agreggation <- mapply(FacultyTransformation_Tec,PeakTecFin$faculty)
  PeakTecFin <- PeakTecFin[,!( names(PeakTecFin) %in% c("faculty") )]
}


## posgrad ##

print("Dropping some rows for PosGrad novo students, dropping atypical values on faculty")

drop.rows <- c(
  "ESCOLA TÉCNICA POTIGUAR",
  "INSTITUCIONAL",
  "ESCOLA DA SAÚDE - MOSSORÓ"
)

if(nrow(BioPosGrad)>1)
  BioPosGrad <- BioPosGrad[ !(trim(BioPosGrad$faculty)%in%drop.rows) ,]
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5 <- BioPosGradWeek5[ !(trim(BioPosGradWeek5$faculty)%in%drop.rows) ,]
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9 <- BioPosGradWeek9[ !(trim(BioPosGradWeek9$faculty)%in%drop.rows) ,]
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13 <- BioPosGradWeek13[ !(trim(BioPosGradWeek13$faculty)%in%drop.rows) ,]
if(nrow(BioPosGradFin)>1)
  BioPosGradFin <- BioPosGradFin[ !(trim(BioPosGradFin$faculty)%in%drop.rows) ,]

print("faculty aggregation to posgrad novo students")

if(nrow(BioPosGrad)>1){
  BioPosGrad$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioPosGrad$faculty)
  BioPosGrad <- BioPosGrad[,!( names(BioPosGrad) %in% c("faculty") )]
}

if(nrow(BioPosGradWeek5)>1){
  BioPosGradWeek5$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioPosGradWeek5$faculty)
  BioPosGradWeek5 <- BioPosGradWeek5[,!( names(BioPosGradWeek5) %in% c("faculty") )]
}

if(nrow(BioPosGradWeek9)>1){
  BioPosGradWeek9$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioPosGradWeek9$faculty)
  BioPosGradWeek9 <- BioPosGradWeek9[,!( names(BioPosGradWeek9) %in% c("faculty") )]
}

if(nrow(BioPosGradWeek13)>1){
  BioPosGradWeek13$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioPosGradWeek13$faculty)
  BioPosGradWeek13 <- BioPosGradWeek13[,!( names(BioPosGradWeek13) %in% c("faculty") )]
}

if(nrow(BioPosGradFin)>1){
  BioPosGradFin$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioPosGradFin$faculty)
  BioPosGradFin <- BioPosGradFin[,!( names(BioPosGradFin) %in% c("faculty") )]
}

print("faculty aggregation to posgrad veteran students")

if(nrow(PeakPosGrad)>1){
  PeakPosGrad$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakPosGrad$faculty)
  PeakPosGrad <- PeakPosGrad[,!( names(PeakPosGrad) %in% c("faculty") )]
}

if(nrow(PeakPosGradWeek5)>1){
  PeakPosGradWeek5$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakPosGradWeek5$faculty)
  PeakPosGradWeek5 <- PeakPosGradWeek5[,!( names(PeakPosGradWeek5) %in% c("faculty") )]
}

if(nrow(PeakPosGradWeek9)>1){
  PeakPosGradWeek9$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakPosGradWeek9$faculty)
  PeakPosGradWeek9 <- PeakPosGradWeek9[,!( names(PeakPosGradWeek9) %in% c("faculty") )]
}

if(nrow(PeakPosGradWeek13)>1){
  PeakPosGradWeek13$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakPosGradWeek13$faculty)
  PeakPosGradWeek13 <- PeakPosGradWeek13[,!( names(PeakPosGradWeek13) %in% c("faculty") )]
}

if(nrow(PeakPosGradFin)>1){
  PeakPosGradFin$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakPosGradFin$faculty)
  PeakPosGradFin <- PeakPosGradFin[,!( names(PeakPosGradFin) %in% c("faculty") )]
}

######## Creditos
#
###########################

print("Calculing new credit variable for PosGrad")

if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5$credits.aggregation <- mapply(CreditosTransformation,BioPosGradWeek5$total.creditos,BioPosGradWeek5$num.disciplinas)
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9$credits.aggregation <- mapply(CreditosTransformation,BioPosGradWeek9$total.creditos,BioPosGradWeek9$num.disciplinas)
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13$credits.aggregation <- mapply(CreditosTransformation,BioPosGradWeek13$total.creditos,BioPosGradWeek13$num.disciplinas)
if(nrow(BioPosGradFin)>1)
  BioPosGradFin$credits.aggregation <- mapply(CreditosTransformation,BioPosGradFin$total.creditos,BioPosGradFin$num.disciplinas)
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5 <- BioPosGradWeek5[,!( names(BioPosGradWeek5) %in% c("num.disciplinas","total.creditos") )]
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9 <- BioPosGradWeek9[,!( names(BioPosGradWeek9) %in% c("num.disciplinas","total.creditos") )]
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13 <- BioPosGradWeek13[,!( names(BioPosGradWeek13) %in% c("num.disciplinas","total.creditos") )]
if(nrow(BioPosGradFin)>1)
  BioPosGradFin <- BioPosGradFin[,!( names(BioPosGradFin) %in% c("num.disciplinas","total.creditos") )]

if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5$credits.aggregation <- mapply(CreditosTransformation,PeakPosGradWeek5$total.creditos.matriculado,PeakPosGradWeek5$num.disciplinas.matriculado)
if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9$credits.aggregation <- mapply(CreditosTransformation,PeakPosGradWeek9$total.creditos.matriculado,PeakPosGradWeek9$num.disciplinas.matriculado)
if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13$credits.aggregation <- mapply(CreditosTransformation,PeakPosGradWeek13$total.creditos.matriculado,PeakPosGradWeek13$num.disciplinas.matriculado)
if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin$credits.aggregation <- mapply(CreditosTransformation,PeakPosGradFin$total.creditos.matriculado,PeakPosGradFin$num.disciplinas.matriculado)
if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5 <- PeakPosGradWeek5[,!( names(PeakPosGradWeek5) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]
if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9 <- PeakPosGradWeek9[,!( names(PeakPosGradWeek9) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]
if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13 <- PeakPosGradWeek13[,!( names(PeakPosGradWeek13) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]
if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin <- PeakPosGradFin[,!( names(PeakPosGradFin) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]

print("Calculing new credit variable for Grad")

if(nrow(BioGradWeek5)>1)
  BioGradWeek5$credits.aggregation <- mapply(CreditosTransformation,BioGradWeek5$total.creditos,BioGradWeek5$num.disciplinas)
if(nrow(BioGradWeek9)>1)
  BioGradWeek9$credits.aggregation <- mapply(CreditosTransformation,BioGradWeek9$total.creditos,BioGradWeek9$num.disciplinas)
if(nrow(BioGradWeek13)>1)
  BioGradWeek13$credits.aggregation <- mapply(CreditosTransformation,BioGradWeek13$total.creditos,BioGradWeek13$num.disciplinas)
if(nrow(BioGradFin)>1)
  BioGradFin$credits.aggregation <- mapply(CreditosTransformation,BioGradFin$total.creditos,BioGradFin$num.disciplinas)
if(nrow(BioGradWeek5)>1)
  BioGradWeek5 <- BioGradWeek5[,!( names(BioGradWeek5) %in% c("num.disciplinas","total.creditos") )]
if(nrow(BioGradWeek9)>1)
  BioGradWeek9 <- BioGradWeek9[,!( names(BioGradWeek9) %in% c("num.disciplinas","total.creditos") )]
if(nrow(BioGradWeek13)>1)
  BioGradWeek13 <- BioGradWeek13[,!( names(BioGradWeek13) %in% c("num.disciplinas","total.creditos") )]
if(nrow(BioGradFin)>1)
  BioGradFin <- BioGradFin[,!( names(BioGradFin) %in% c("num.disciplinas","total.creditos") )]

if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5$credits.aggregation <- mapply(CreditosTransformation,PeakGradWeek5$total.creditos.matriculado,PeakGradWeek5$num.disciplinas.matriculado)
if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9$credits.aggregation <- mapply(CreditosTransformation,PeakGradWeek9$total.creditos.matriculado,PeakGradWeek9$num.disciplinas.matriculado)
if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13$credits.aggregation <- mapply(CreditosTransformation,PeakGradWeek13$total.creditos.matriculado,PeakGradWeek13$num.disciplinas.matriculado)
if(nrow(PeakGradFin)>1)
  PeakGradFin$credits.aggregation <- mapply(CreditosTransformation,PeakGradFin$total.creditos.matriculado,PeakGradFin$num.disciplinas.matriculado)
if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5 <- PeakGradWeek5[,!( names(PeakGradWeek5) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]
if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9 <- PeakGradWeek9[,!( names(PeakGradWeek9) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]
if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13 <- PeakGradWeek13[,!( names(PeakGradWeek13) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]
if(nrow(PeakGradFin)>1)
  PeakGradFin <- PeakGradFin[,!( names(PeakGradFin) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]

print("Calculing new credit variable for Tec")

if(nrow(BioTecWeek5)>1)
  BioTecWeek5$credits.aggregation <- mapply(CreditosTransformation,BioTecWeek5$total.creditos,BioTecWeek5$num.disciplinas)
if(nrow(BioTecWeek9)>1)
  BioTecWeek9$credits.aggregation <- mapply(CreditosTransformation,BioTecWeek9$total.creditos,BioTecWeek9$num.disciplinas)
if(nrow(BioTecWeek13)>1)
  BioTecWeek13$credits.aggregation <- mapply(CreditosTransformation,BioTecWeek13$total.creditos,BioTecWeek13$num.disciplinas)
if(nrow(BioTecFin)>1)
  BioTecFin$credits.aggregation <- mapply(CreditosTransformation,BioTecFin$total.creditos,BioTecFin$num.disciplinas)
if(nrow(BioTecWeek5)>1)
  BioTecWeek5 <- BioTecWeek5[,!( names(BioTecWeek5) %in% c("num.disciplinas","total.creditos") )]
if(nrow(BioTecWeek9)>1)
  BioTecWeek9 <- BioTecWeek9[,!( names(BioTecWeek9) %in% c("num.disciplinas","total.creditos") )]
if(nrow(BioTecWeek13)>1)
  BioTecWeek13 <- BioTecWeek13[,!( names(BioTecWeek13) %in% c("num.disciplinas","total.creditos") )]
if(nrow(BioTecFin)>1)
  BioTecFin <- BioTecFin[,!( names(BioTecFin) %in% c("num.disciplinas","total.creditos") )]

if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5$credits.aggregation <- mapply(CreditosTransformation,PeakTecWeek5$total.creditos.matriculado,PeakTecWeek5$num.disciplinas.matriculado)
if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9$credits.aggregation <- mapply(CreditosTransformation,PeakTecWeek9$total.creditos.matriculado,PeakTecWeek9$num.disciplinas.matriculado)
if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13$credits.aggregation <- mapply(CreditosTransformation,PeakTecWeek13$total.creditos.matriculado,PeakTecWeek13$num.disciplinas.matriculado)
if(nrow(PeakTecFin)>1)
  PeakTecFin$credits.aggregation <- mapply(CreditosTransformation,PeakTecFin$total.creditos.matriculado,PeakTecFin$num.disciplinas.matriculado)
if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5 <- PeakTecWeek5[,!( names(PeakTecWeek5) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]
if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9 <- PeakTecWeek9[,!( names(PeakTecWeek9) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]
if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13 <- PeakTecWeek13[,!( names(PeakTecWeek13) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]
if(nrow(PeakTecFin)>1)
  PeakTecFin <- PeakTecFin[,!( names(PeakTecFin) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]


######## Day Payment
#
###########################


## Grad ##

print("pay Engagementfor grad")

if(nrow(BioGradWeek5)>1){
  BioGradWeek5$pay.engagement <- mapply(PayEngagement
                                        , BioGradWeek5$days.payment.1
                                        , BioGradWeek5$days.payment.2
                                        , BioGradWeek5$days.payment.3
                                        , BioGradWeek5$days.payment.4
                                        , BioGradWeek5$days.payment.5
                                        , BioGradWeek5$days.payment.6
                                        , 5)
}

if(nrow(BioGradWeek9)>1){
  BioGradWeek9$pay.engagement <- mapply(PayEngagement
                                        , BioGradWeek9$days.payment.1
                                        , BioGradWeek9$days.payment.2
                                        , BioGradWeek9$days.payment.3
                                        , BioGradWeek9$days.payment.4
                                        , BioGradWeek9$days.payment.5
                                        , BioGradWeek9$days.payment.6
                                        , 9)
}

if(nrow(BioGradWeek13)>1)
  BioGradWeek13$pay.engagement <- mapply(PayEngagement
                                        , BioGradWeek13$days.payment.1
                                        , BioGradWeek13$days.payment.2
                                        , BioGradWeek13$days.payment.3
                                        , BioGradWeek13$days.payment.4
                                        , BioGradWeek13$days.payment.5
                                        , BioGradWeek13$days.payment.6
                                        , 13)
if(nrow(BioTecFin)>1)
  BioGradFin$pay.engagement <- mapply(PayEngagement
                                         , BioGradFin$days.payment.1
                                         , BioGradFin$days.payment.2
                                         , BioGradFin$days.payment.3
                                         , BioGradFin$days.payment.4
                                         , BioGradFin$days.payment.5
                                         , BioGradFin$days.payment.6
                                         , 19)

if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5$pay.engagement <- mapply(PayEngagement
                                       , PeakGradWeek5$days.payment.1
                                       , PeakGradWeek5$days.payment.2
                                       , PeakGradWeek5$days.payment.3
                                       , PeakGradWeek5$days.payment.4
                                       , PeakGradWeek5$days.payment.5
                                       , PeakGradWeek5$days.payment.6
                                       , 5)

if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9$pay.engagement <- mapply(PayEngagement
                                       , PeakGradWeek9$days.payment.1
                                       , PeakGradWeek9$days.payment.2
                                       , PeakGradWeek9$days.payment.3
                                       , PeakGradWeek9$days.payment.4
                                       , PeakGradWeek9$days.payment.5
                                       , PeakGradWeek9$days.payment.6
                                       , 9)
if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13$pay.engagement <- mapply(PayEngagement
                                       , PeakGradWeek13$days.payment.1
                                       , PeakGradWeek13$days.payment.2
                                       , PeakGradWeek13$days.payment.3
                                       , PeakGradWeek13$days.payment.4
                                       , PeakGradWeek13$days.payment.5
                                       , PeakGradWeek13$days.payment.6
                                       , 13)
if(nrow(PeakGradFin)>1)
  PeakGradFin$pay.engagement <- mapply(PayEngagement
                                        , PeakGradFin$days.payment.1
                                        , PeakGradFin$days.payment.2
                                        , PeakGradFin$days.payment.3
                                        , PeakGradFin$days.payment.4
                                        , PeakGradFin$days.payment.5
                                        , PeakGradFin$days.payment.6
                                        , 19)



if(nrow(BioGradWeek5)>1)
  BioGradWeek5 <- BioGradWeek5[, !(  names(BioGradWeek5) %in% c("days.payment.1",
                                                "days.payment.2",
                                                "days.payment.3",
                                                "days.payment.4",
                                                "days.payment.5",
                                                "days.payment.6")  )  ]

if(nrow(BioGradWeek9)>1)
  BioGradWeek9 <- BioGradWeek9[, !(  names(BioGradWeek9) %in% c("days.payment.1",
                                                "days.payment.2",
                                                "days.payment.3",
                                                "days.payment.4",
                                                "days.payment.5",
                                                "days.payment.6")  )  ]

if(nrow(BioGradWeek13)>1)
  BioGradWeek13 <- BioGradWeek13[, !(  names(BioGradWeek13) %in% c("days.payment.1",
                                                "days.payment.2",
                                                "days.payment.3",
                                                "days.payment.4",
                                                "days.payment.5",
                                                "days.payment.6")  )  ]

if(nrow(BioGradFin)>1)
  BioGradFin <- BioGradFin[, !(  names(BioGradFin) %in% c("days.payment.1",
                                                "days.payment.2",
                                                "days.payment.3",
                                                "days.payment.4",
                                                "days.payment.5",
                                                "days.payment.6")  )  ]

if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5 <- PeakGradWeek5[, !(  names(PeakGradWeek5) %in% c("days.payment.1",
                                                "days.payment.2",
                                                "days.payment.3",
                                                "days.payment.4",
                                                "days.payment.5",
                                                "days.payment.6")  )  ]

if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9 <- PeakGradWeek9[, !(  names(PeakGradWeek9) %in% c("days.payment.1",
                                                "days.payment.2",
                                                "days.payment.3",
                                                "days.payment.4",
                                                "days.payment.5",
                                                "days.payment.6")  )  ]

if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13 <- PeakGradWeek13[, !(  names(PeakGradWeek13) %in% c("days.payment.1",
                                                "days.payment.2",
                                                "days.payment.3",
                                                "days.payment.4",
                                                "days.payment.5",
                                                "days.payment.6")  )  ]

if(nrow(PeakGradFin)>1)
  PeakGradFin <- PeakGradFin[, !(  names(PeakGradFin) %in% c("days.payment.1",
                                                "days.payment.2",
                                                "days.payment.3",
                                                "days.payment.4",
                                                "days.payment.5",
                                                "days.payment.6")  )  ]

## PosGrad ##

print("pay Engagementfor PosGrad")

if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5$pay.engagement <- mapply(PayEngagement
                                        , BioPosGradWeek5$days.payment.1
                                        , BioPosGradWeek5$days.payment.2
                                        , BioPosGradWeek5$days.payment.3
                                        , BioPosGradWeek5$days.payment.4
                                        , BioPosGradWeek5$days.payment.5
                                        , BioPosGradWeek5$days.payment.6
                                        , 5)

if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9$pay.engagement <- mapply(PayEngagement
                                        , BioPosGradWeek9$days.payment.1
                                        , BioPosGradWeek9$days.payment.2
                                        , BioPosGradWeek9$days.payment.3
                                        , BioPosGradWeek9$days.payment.4
                                        , BioPosGradWeek9$days.payment.5
                                        , BioPosGradWeek9$days.payment.6
                                        , 9)

if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13$pay.engagement <- mapply(PayEngagement
                                         , BioPosGradWeek13$days.payment.1
                                         , BioPosGradWeek13$days.payment.2
                                         , BioPosGradWeek13$days.payment.3
                                         , BioPosGradWeek13$days.payment.4
                                         , BioPosGradWeek13$days.payment.5
                                         , BioPosGradWeek13$days.payment.6
                                         , 13)

if(nrow(BioPosGradFin)>1)
  BioPosGradFin$pay.engagement <- mapply(PayEngagement
                                      , BioPosGradFin$days.payment.1
                                      , BioPosGradFin$days.payment.2
                                      , BioPosGradFin$days.payment.3
                                      , BioPosGradFin$days.payment.4
                                      , BioPosGradFin$days.payment.5
                                      , BioPosGradFin$days.payment.6
                                      , 19)

if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5$pay.engagement <- mapply(PayEngagement
                                         , PeakPosGradWeek5$days.payment.1
                                         , PeakPosGradWeek5$days.payment.2
                                         , PeakPosGradWeek5$days.payment.3
                                         , PeakPosGradWeek5$days.payment.4
                                         , PeakPosGradWeek5$days.payment.5
                                         , PeakPosGradWeek5$days.payment.6
                                         , 5)

if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9$pay.engagement <- mapply(PayEngagement
                                         , PeakPosGradWeek9$days.payment.1
                                         , PeakPosGradWeek9$days.payment.2
                                         , PeakPosGradWeek9$days.payment.3
                                         , PeakPosGradWeek9$days.payment.4
                                         , PeakPosGradWeek9$days.payment.5
                                         , PeakPosGradWeek9$days.payment.6
                                         , 9)

if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13$pay.engagement <- mapply(PayEngagement
                                          , PeakPosGradWeek13$days.payment.1
                                          , PeakPosGradWeek13$days.payment.2
                                          , PeakPosGradWeek13$days.payment.3
                                          , PeakPosGradWeek13$days.payment.4
                                          , PeakPosGradWeek13$days.payment.5
                                          , PeakPosGradWeek13$days.payment.6
                                          , 13)

if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin$pay.engagement <- mapply(PayEngagement
                                       , PeakPosGradFin$days.payment.1
                                       , PeakPosGradFin$days.payment.2
                                       , PeakPosGradFin$days.payment.3
                                       , PeakPosGradFin$days.payment.4
                                       , PeakPosGradFin$days.payment.5
                                       , PeakPosGradFin$days.payment.6
                                       , 19)


if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5 <- BioPosGradWeek5[, !(  names(BioPosGradWeek5) %in% c("days.payment.1",
                                                "days.payment.2",
                                                "days.payment.3",
                                                "days.payment.4",
                                                "days.payment.5",
                                                "days.payment.6")  )  ]

if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9 <- BioPosGradWeek9[, !(  names(BioPosGradWeek9) %in% c("days.payment.1",
                                                "days.payment.2",
                                                "days.payment.3",
                                                "days.payment.4",
                                                "days.payment.5",
                                                "days.payment.6")  )  ]

if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13 <- BioPosGradWeek13[, !(  names(BioPosGradWeek13) %in% c("days.payment.1",
                                                  "days.payment.2",
                                                  "days.payment.3",
                                                  "days.payment.4",
                                                  "days.payment.5",
                                                  "days.payment.6")  )  ]

if(nrow(BioPosGradFin)>1)
  BioPosGradFin <- BioPosGradFin[, !(  names(BioPosGradFin) %in% c("days.payment.1",
                                            "days.payment.2",
                                            "days.payment.3",
                                            "days.payment.4",
                                            "days.payment.5",
                                            "days.payment.6")  )  ]

if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5 <- PeakPosGradWeek5[, !(  names(PeakPosGradWeek5) %in% c("days.payment.1",
                                                  "days.payment.2",
                                                  "days.payment.3",
                                                  "days.payment.4",
                                                  "days.payment.5",
                                                  "days.payment.6")  )  ]

if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9 <- PeakPosGradWeek9[, !(  names(PeakPosGradWeek9) %in% c("days.payment.1",
                                                  "days.payment.2",
                                                  "days.payment.3",
                                                  "days.payment.4",
                                                  "days.payment.5",
                                                  "days.payment.6")  )  ]

if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13 <- PeakPosGradWeek13[, !(  names(PeakPosGradWeek13) %in% c("days.payment.1",
                                                    "days.payment.2",
                                                    "days.payment.3",
                                                    "days.payment.4",
                                                    "days.payment.5",
                                                    "days.payment.6")  )  ]

if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin <- PeakPosGradFin[, !(  names(PeakPosGradFin) %in% c("days.payment.1",
                                              "days.payment.2",
                                              "days.payment.3",
                                              "days.payment.4",
                                              "days.payment.5",
                                              "days.payment.6")  )  ]

## Tec ##

print("pay Engagementfor Tec")

if(nrow(BioTecWeek5)>1)
  BioTecWeek5$pay.engagement <- mapply(PayEngagement
                                           , BioTecWeek5$days.payment.1
                                           , BioTecWeek5$days.payment.2
                                           , BioTecWeek5$days.payment.3
                                           , BioTecWeek5$days.payment.4
                                           , BioTecWeek5$days.payment.5
                                           , BioTecWeek5$days.payment.6
                                           , 5)

if(nrow(BioTecWeek9)>1)
  BioTecWeek9$pay.engagement <- mapply(PayEngagement
                                           , BioTecWeek9$days.payment.1
                                           , BioTecWeek9$days.payment.2
                                           , BioTecWeek9$days.payment.3
                                           , BioTecWeek9$days.payment.4
                                           , BioTecWeek9$days.payment.5
                                           , BioTecWeek9$days.payment.6
                                           , 9)

if(nrow(BioTecWeek13)>1)
  BioTecWeek13$pay.engagement <- mapply(PayEngagement
                                            , BioTecWeek13$days.payment.1
                                            , BioTecWeek13$days.payment.2
                                            , BioTecWeek13$days.payment.3
                                            , BioTecWeek13$days.payment.4
                                            , BioTecWeek13$days.payment.5
                                            , BioTecWeek13$days.payment.6
                                            , 13)

if(nrow(BioTecFin)>1)
  BioTecFin$pay.engagement <- mapply(PayEngagement
                                         , BioTecFin$days.payment.1
                                         , BioTecFin$days.payment.2
                                         , BioTecFin$days.payment.3
                                         , BioTecFin$days.payment.4
                                         , BioTecFin$days.payment.5
                                         , BioTecFin$days.payment.6
                                         , 19)

if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5$pay.engagement <- mapply(PayEngagement
                                            , PeakTecWeek5$days.payment.1
                                            , PeakTecWeek5$days.payment.2
                                            , PeakTecWeek5$days.payment.3
                                            , PeakTecWeek5$days.payment.4
                                            , PeakTecWeek5$days.payment.5
                                            , PeakTecWeek5$days.payment.6
                                            , 5)

if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9$pay.engagement <- mapply(PayEngagement
                                            , PeakTecWeek9$days.payment.1
                                            , PeakTecWeek9$days.payment.2
                                            , PeakTecWeek9$days.payment.3
                                            , PeakTecWeek9$days.payment.4
                                            , PeakTecWeek9$days.payment.5
                                            , PeakTecWeek9$days.payment.6
                                            , 9)

if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13$pay.engagement <- mapply(PayEngagement
                                             , PeakTecWeek13$days.payment.1
                                             , PeakTecWeek13$days.payment.2
                                             , PeakTecWeek13$days.payment.3
                                             , PeakTecWeek13$days.payment.4
                                             , PeakTecWeek13$days.payment.5
                                             , PeakTecWeek13$days.payment.6
                                             , 13)

if(nrow(PeakTecFin)>1)
  PeakTecFin$pay.engagement <- mapply(PayEngagement
                                          , PeakTecFin$days.payment.1
                                          , PeakTecFin$days.payment.2
                                          , PeakTecFin$days.payment.3
                                          , PeakTecFin$days.payment.4
                                          , PeakTecFin$days.payment.5
                                          , PeakTecFin$days.payment.6
                                          , 19)



if(nrow(BioTecWeek5)>1)
  BioTecWeek5 <- BioTecWeek5[, !(  names(BioTecWeek5) %in% c("days.payment.1",
                                              "days.payment.2",
                                              "days.payment.3",
                                              "days.payment.4",
                                              "days.payment.5",
                                              "days.payment.6")  )  ]

if(nrow(BioTecWeek9)>1)
  BioTecWeek9 <- BioTecWeek9[, !(  names(BioTecWeek9) %in% c("days.payment.1",
                                              "days.payment.2",
                                              "days.payment.3",
                                              "days.payment.4",
                                              "days.payment.5",
                                              "days.payment.6")  )  ]

if(nrow(BioTecWeek13)>1)
  BioTecWeek13 <- BioTecWeek13[, !(  names(BioTecWeek13) %in% c("days.payment.1",
                                                "days.payment.2",
                                                "days.payment.3",
                                                "days.payment.4",
                                                "days.payment.5",
                                                "days.payment.6")  )  ]

if(nrow(BioTecFin)>1)
  BioTecFin <- BioTecFin[, !(  names(BioTecFin) %in% c("days.payment.1",
                                          "days.payment.2",
                                          "days.payment.3",
                                          "days.payment.4",
                                          "days.payment.5",
                                          "days.payment.6")  )  ]

if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5 <- PeakTecWeek5[, !(  names(PeakTecWeek5) %in% c("days.payment.1",
                                                "days.payment.2",
                                                "days.payment.3",
                                                "days.payment.4",
                                                "days.payment.5",
                                                "days.payment.6")  )  ]

if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9 <- PeakTecWeek9[, !(  names(PeakTecWeek9) %in% c("days.payment.1",
                                                "days.payment.2",
                                                "days.payment.3",
                                                "days.payment.4",
                                                "days.payment.5",
                                                "days.payment.6")  )  ]

if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13 <- PeakTecWeek13[, !(  names(PeakTecWeek13) %in% c("days.payment.1",
                                                  "days.payment.2",
                                                  "days.payment.3",
                                                  "days.payment.4",
                                                  "days.payment.5",
                                                  "days.payment.6")  )  ]

if(nrow(PeakTecFin)>1)
  PeakTecFin <- PeakTecFin[, !(  names(PeakTecFin) %in% c("days.payment.1",
                                            "days.payment.2",
                                            "days.payment.3",
                                            "days.payment.4",
                                            "days.payment.5",
                                            "days.payment.6")  )  ]

######## Payment amount
#
###########################

## Grad ##

print("payment balance for Grad")

if(nrow(BioGradWeek5)>1)
  BioGradWeek5$payment.balance <- mapply(PaymentBalance,
  BioGradWeek5$payment.amount.1,
  BioGradWeek5$payment.amount.2,
  BioGradWeek5$payment.amount.3,
  BioGradWeek5$payment.amount.4,
  BioGradWeek5$payment.amount.5,
  BioGradWeek5$payment.amount.6,
  BioGradWeek5$total.balance)

if(nrow(BioGradWeek9)>1)
  BioGradWeek9$payment.balance <- mapply(PaymentBalance,
  BioGradWeek9$payment.amount.1,
  BioGradWeek9$payment.amount.2,
  BioGradWeek9$payment.amount.3,
  BioGradWeek9$payment.amount.4,
  BioGradWeek9$payment.amount.5,
  BioGradWeek9$payment.amount.6,
  BioGradWeek9$total.balance)

if(nrow(BioGradWeek13)>1)
  BioGradWeek13$payment.balance <- mapply(PaymentBalance,
  BioGradWeek13$payment.amount.1,
  BioGradWeek13$payment.amount.2,
  BioGradWeek13$payment.amount.3,
  BioGradWeek13$payment.amount.4,
  BioGradWeek13$payment.amount.5,
  BioGradWeek13$payment.amount.6,
  BioGradWeek13$total.balance)

if(nrow(BioGradFin)>1)
  BioGradFin$payment.balance <- mapply(PaymentBalance,
  BioGradFin$payment.amount.1,
  BioGradFin$payment.amount.2,
  BioGradFin$payment.amount.3,
  BioGradFin$payment.amount.4,
  BioGradFin$payment.amount.5,
  BioGradFin$payment.amount.6,
  BioGradFin$total.balance)

if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5$payment.balance <- mapply(PaymentBalance,
  PeakGradWeek5$payment.amount.1,
  PeakGradWeek5$payment.amount.2,
  PeakGradWeek5$payment.amount.3,
  PeakGradWeek5$payment.amount.4,
  PeakGradWeek5$payment.amount.5,
  PeakGradWeek5$payment.amount.6,
  PeakGradWeek5$total.balance)

if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9$payment.balance <- mapply(PaymentBalance,
  PeakGradWeek9$payment.amount.1,
  PeakGradWeek9$payment.amount.2,
  PeakGradWeek9$payment.amount.3,
  PeakGradWeek9$payment.amount.4,
  PeakGradWeek9$payment.amount.5,
  PeakGradWeek9$payment.amount.6,
  PeakGradWeek9$total.balance)

if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13$payment.balance <- mapply(PaymentBalance,
  PeakGradWeek13$payment.amount.1,
  PeakGradWeek13$payment.amount.2,
  PeakGradWeek13$payment.amount.3,
  PeakGradWeek13$payment.amount.4,
  PeakGradWeek13$payment.amount.5,
  PeakGradWeek13$payment.amount.6,
  PeakGradWeek13$total.balance)

if(nrow(PeakGradFin)>1)
  PeakGradFin$payment.balance <- mapply(PaymentBalance,
  PeakGradFin$payment.amount.1,
  PeakGradFin$payment.amount.2,
  PeakGradFin$payment.amount.3,
  PeakGradFin$payment.amount.4,
  PeakGradFin$payment.amount.5,
  PeakGradFin$payment.amount.6,
  PeakGradFin$total.balance)

if(nrow(BioGradWeek5)>1)
  BioGradWeek5 <- BioGradWeek5[, !(  names(BioGradWeek5) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(BioGradWeek9)>1)
  BioGradWeek9 <- BioGradWeek9[, !(  names(BioGradWeek9) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(BioGradWeek13)>1)
  BioGradWeek13 <- BioGradWeek13[, !(  names(BioGradWeek13) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(BioGradFin)>1)
  BioGradFin <- BioGradFin[, !(  names(BioGradFin) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5 <- PeakGradWeek5[, !(  names(PeakGradWeek5) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9 <- PeakGradWeek9[, !(  names(PeakGradWeek9) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13 <- PeakGradWeek13[, !(  names(PeakGradWeek13) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(PeakGradFin)>1)
  PeakGradFin <- PeakGradFin[, !(  names(PeakGradFin) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

## PosGrad ##

print("payment balance for PosGrad")

if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5$payment.balance <- mapply(PaymentBalance,
  BioPosGradWeek5$payment.amount.1,
  BioPosGradWeek5$payment.amount.2,
  BioPosGradWeek5$payment.amount.3,
  BioPosGradWeek5$payment.amount.4,
  BioPosGradWeek5$payment.amount.5,
  BioPosGradWeek5$payment.amount.6,
  BioPosGradWeek5$total.balance)

if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9$payment.balance <- mapply(PaymentBalance,
  BioPosGradWeek9$payment.amount.1,
  BioPosGradWeek9$payment.amount.2,
  BioPosGradWeek9$payment.amount.3,
  BioPosGradWeek9$payment.amount.4,
  BioPosGradWeek9$payment.amount.5,
  BioPosGradWeek9$payment.amount.6,
  BioPosGradWeek9$total.balance)

if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13$payment.balance <- mapply(PaymentBalance,
  BioPosGradWeek13$payment.amount.1,
  BioPosGradWeek13$payment.amount.2,
  BioPosGradWeek13$payment.amount.3,
  BioPosGradWeek13$payment.amount.4,
  BioPosGradWeek13$payment.amount.5,
  BioPosGradWeek13$payment.amount.6,
  BioPosGradWeek13$total.balance)

if(nrow(BioPosGradFin)>1)
  BioPosGradFin$payment.balance <- mapply(PaymentBalance,
  BioPosGradFin$payment.amount.1,
  BioPosGradFin$payment.amount.2,
  BioPosGradFin$payment.amount.3,
  BioPosGradFin$payment.amount.4,
  BioPosGradFin$payment.amount.5,
  BioPosGradFin$payment.amount.6,
  BioPosGradFin$total.balance)

if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5$payment.balance <- mapply(PaymentBalance,
  PeakPosGradWeek5$payment.amount.1,
  PeakPosGradWeek5$payment.amount.2,
  PeakPosGradWeek5$payment.amount.3,
  PeakPosGradWeek5$payment.amount.4,
  PeakPosGradWeek5$payment.amount.5,
  PeakPosGradWeek5$payment.amount.6,
  PeakPosGradWeek5$total.balance)

if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9$payment.balance <- mapply(PaymentBalance,
  PeakPosGradWeek9$payment.amount.1,
  PeakPosGradWeek9$payment.amount.2,
  PeakPosGradWeek9$payment.amount.3,
  PeakPosGradWeek9$payment.amount.4,
  PeakPosGradWeek9$payment.amount.5,
  PeakPosGradWeek9$payment.amount.6,
  PeakPosGradWeek9$total.balance)

if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13$payment.balance <- mapply(PaymentBalance,
  PeakPosGradWeek13$payment.amount.1,
  PeakPosGradWeek13$payment.amount.2,
  PeakPosGradWeek13$payment.amount.3,
  PeakPosGradWeek13$payment.amount.4,
  PeakPosGradWeek13$payment.amount.5,
  PeakPosGradWeek13$payment.amount.6,
  PeakPosGradWeek13$total.balance)

if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin$payment.balance <- mapply(PaymentBalance,
  PeakPosGradFin$payment.amount.1,
  PeakPosGradFin$payment.amount.2,
  PeakPosGradFin$payment.amount.3,
  PeakPosGradFin$payment.amount.4,
  PeakPosGradFin$payment.amount.5,
  PeakPosGradFin$payment.amount.6,
  PeakPosGradFin$total.balance)

if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5 <- BioPosGradWeek5[, !(  names(BioPosGradWeek5) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9 <- BioPosGradWeek9[, !(  names(BioPosGradWeek9) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13 <- BioPosGradWeek13[, !(  names(BioPosGradWeek13) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(BioPosGradFin)>1)
  BioPosGradFin <- BioPosGradFin[, !(  names(BioPosGradFin) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5 <- PeakPosGradWeek5[, !(  names(PeakPosGradWeek5) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9 <- PeakPosGradWeek9[, !(  names(PeakPosGradWeek9) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13 <- PeakPosGradWeek13[, !(  names(PeakPosGradWeek13) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin <- PeakPosGradFin[, !(  names(PeakPosGradFin) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

## PosGrad ##

print("payment balance for Tec")

if(nrow(BioTecWeek5)>1)
  BioTecWeek5$payment.balance <- mapply(PaymentBalance,
  BioTecWeek5$payment.amount.1,
  BioTecWeek5$payment.amount.2,
  BioTecWeek5$payment.amount.3,
  BioTecWeek5$payment.amount.4,
  BioTecWeek5$payment.amount.5,
  BioTecWeek5$payment.amount.6,
  BioTecWeek5$total.balance)

if(nrow(BioTecWeek9)>1)
  BioTecWeek9$payment.balance <- mapply(PaymentBalance,
  BioTecWeek9$payment.amount.1,
  BioTecWeek9$payment.amount.2,
  BioTecWeek9$payment.amount.3,
  BioTecWeek9$payment.amount.4,
  BioTecWeek9$payment.amount.5,
  BioTecWeek9$payment.amount.6,
  BioTecWeek9$total.balance)

if(nrow(BioTecWeek13)>1)
  BioTecWeek13$payment.balance <- mapply(PaymentBalance,
  BioTecWeek13$payment.amount.1,
  BioTecWeek13$payment.amount.2,
  BioTecWeek13$payment.amount.3,
  BioTecWeek13$payment.amount.4,
  BioTecWeek13$payment.amount.5,
  BioTecWeek13$payment.amount.6,
  BioTecWeek13$total.balance)

if(nrow(BioTecFin)>1)
  BioTecFin$payment.balance <- mapply(PaymentBalance,
  BioTecFin$payment.amount.1,
  BioTecFin$payment.amount.2,
  BioTecFin$payment.amount.3,
  BioTecFin$payment.amount.4,
  BioTecFin$payment.amount.5,
  BioTecFin$payment.amount.6,
  BioTecFin$total.balance)

if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5$payment.balance <- mapply(PaymentBalance,
  PeakTecWeek5$payment.amount.1,
  PeakTecWeek5$payment.amount.2,
  PeakTecWeek5$payment.amount.3,
  PeakTecWeek5$payment.amount.4,
  PeakTecWeek5$payment.amount.5,
  PeakTecWeek5$payment.amount.6,
  PeakTecWeek5$total.balance)

if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9$payment.balance <- mapply(PaymentBalance,
  PeakTecWeek9$payment.amount.1,
  PeakTecWeek9$payment.amount.2,
  PeakTecWeek9$payment.amount.3,
  PeakTecWeek9$payment.amount.4,
  PeakTecWeek9$payment.amount.5,
  PeakTecWeek9$payment.amount.6,
  PeakTecWeek9$total.balance)

if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13$payment.balance <- mapply(PaymentBalance,
  PeakTecWeek13$payment.amount.1,
  PeakTecWeek13$payment.amount.2,
  PeakTecWeek13$payment.amount.3,
  PeakTecWeek13$payment.amount.4,
  PeakTecWeek13$payment.amount.5,
  PeakTecWeek13$payment.amount.6,
  PeakTecWeek13$total.balance)

if(nrow(PeakTecFin)>1)
  PeakTecFin$payment.balance <- mapply(PaymentBalance,
  PeakTecFin$payment.amount.1,
  PeakTecFin$payment.amount.2,
  PeakTecFin$payment.amount.3,
  PeakTecFin$payment.amount.4,
  PeakTecFin$payment.amount.5,
  PeakTecFin$payment.amount.6,
  PeakTecFin$total.balance)

if(nrow(BioTecWeek5)>1)
  BioTecWeek5 <- BioTecWeek5[, !(  names(BioTecWeek5) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(BioTecWeek9)>1)
  BioTecWeek9 <- BioTecWeek9[, !(  names(BioTecWeek9) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(BioTecWeek13)>1)
  BioTecWeek13 <- BioTecWeek13[, !(  names(BioTecWeek13) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(BioTecFin)>1)
  BioTecFin <- BioTecFin[, !(  names(BioTecFin) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5 <- PeakTecWeek5[, !(  names(PeakTecWeek5) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9 <- PeakTecWeek9[, !(  names(PeakTecWeek9) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13 <- PeakTecWeek13[, !(  names(PeakTecWeek13) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]

if(nrow(PeakTecFin)>1)
  PeakTecFin <- PeakTecFin[, !(  names(PeakTecFin) %in% c("payment.amount.1",
  "payment.amount.2",
  "payment.amount.3",
  "payment.amount.4",
  "payment.amount.5",
  "payment.amount.6",
  "total.balance"))]


#### Drop attrs unnecesary

if(nrow(PeakGradFin)>1)
  PeakGradFin <- PeakGradFin[,!(names(PeakGradFin)%in%c("x"))]
if(nrow(PeakGradFin)>1)
  PeakGradFin <- unique(PeakGradFin)
if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13 <- PeakGradWeek13[,!(names(PeakGradWeek13)%in%c("x"))]
if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13 <- unique(PeakGradWeek13)
if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9 <- PeakGradWeek9[,!(names(PeakGradWeek9)%in%c("x"))]
if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9 <- unique(PeakGradWeek9)
if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5 <- PeakGradWeek5[,!(names(PeakGradWeek5)%in%c("x"))]
if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5 <- unique(PeakGradWeek5)
if(nrow(PeakGrad)>1)
  PeakGrad <- PeakGrad[,!(names(PeakGrad)%in%c("x"))]
if(nrow(PeakGrad)>1)
  PeakGrad <- unique(PeakGrad)

if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin <- PeakPosGradFin[,!(names(PeakPosGradFin)%in%c("x"))]
if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin <- unique(PeakPosGradFin)
if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13 <- PeakPosGradWeek13[,!(names(PeakPosGradWeek13)%in%c("x"))]
if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13 <- unique(PeakPosGradWeek13)
if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9 <- PeakPosGradWeek9[,!(names(PeakPosGradWeek9)%in%c("x"))]
if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9 <- unique(PeakPosGradWeek9)
if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5 <- PeakPosGradWeek5[,!(names(PeakPosGradWeek5)%in%c("x"))]
if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5 <- unique(PeakPosGradWeek5)
if(nrow(PeakPosGrad)>1)
  PeakPosGrad <- PeakPosGrad[,!(names(PeakPosGrad)%in%c("x"))]
if(nrow(PeakPosGrad)>1)
  PeakPosGrad <- unique(PeakPosGrad)

if(nrow(PeakTecFin)>1)
  PeakTecFin <- PeakTecFin[,!(names(PeakTecFin)%in%c("x"))]
if(nrow(PeakTecFin)>1)
  PeakTecFin <- unique(PeakTecFin)
if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13 <- PeakTecWeek13[,!(names(PeakTecWeek13)%in%c("x"))]
if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13 <- unique(PeakTecWeek13)
if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9 <- PeakTecWeek9[,!(names(PeakTecWeek9)%in%c("x"))]
if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9 <- unique(PeakTecWeek9)
if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5 <- PeakTecWeek5[,!(names(PeakTecWeek5)%in%c("x"))]
if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5 <- unique(PeakTecWeek5)
if(nrow(PeakTec)>1)
  PeakTec <- PeakTec[,!(names(PeakTec)%in%c("x"))]
if(nrow(PeakTec)>1)
  PeakTec <- unique(PeakTec)

if(nrow(BioGradFin)>1)
  BioGradFin <- BioGradFin[,!(names(BioGradFin)%in%c("x"))]
if(nrow(BioGradFin)>1)
  BioGradFin <- unique(BioGradFin)
if(nrow(BioGradWeek13)>1)
  BioGradWeek13 <- BioGradWeek13[,!(names(BioGradWeek13)%in%c("x"))]
if(nrow(BioGradWeek13)>1)
  BioGradWeek13 <- unique(BioGradWeek13)
if(nrow(BioGradWeek9)>1)
  BioGradWeek9 <- BioGradWeek9[,!(names(BioGradWeek9)%in%c("x"))]
if(nrow(BioGradWeek9)>1)
  BioGradWeek9 <- unique(BioGradWeek9)
if(nrow(BioGradWeek5)>1)
  BioGradWeek5 <- BioGradWeek5[,!(names(BioGradWeek5)%in%c("x"))]
if(nrow(BioGradWeek5)>1)
  BioGradWeek5 <- unique(BioGradWeek5)
if(nrow(BioGrad)>1)
  BioGrad <- BioGrad[,!(names(BioGrad)%in%c("x"))]
if(nrow(BioGrad)>1)
  BioGrad <- unique(BioGrad)

if(nrow(BioPosGradFin)>1)
  BioPosGradFin <- BioPosGradFin[,!(names(BioPosGradFin)%in%c("x"))]
if(nrow(BioPosGradFin)>1)
  BioPosGradFin <- unique(BioPosGradFin)
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13 <- BioPosGradWeek13[,!(names(BioPosGradWeek13)%in%c("x"))]
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13 <- unique(BioPosGradWeek13)
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9 <- BioPosGradWeek9[,!(names(BioPosGradWeek9)%in%c("x"))]
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9 <- unique(BioPosGradWeek9)
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5 <- BioPosGradWeek5[,!(names(BioPosGradWeek5)%in%c("x"))]
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5 <- unique(BioPosGradWeek5)
if(nrow(BioPosGrad)>1)
  BioPosGrad <- BioPosGrad[,!(names(BioPosGrad)%in%c("x"))]
if(nrow(BioPosGrad)>1)
  BioPosGrad <- unique(BioPosGrad)

if(nrow(BioTecFin)>1)
  BioTecFin <- BioTecFin[,!(names(BioTecFin)%in%c("x"))]
if(nrow(BioTecFin)>1)
  BioTecFin <- unique(BioTecFin)
if(nrow(BioTecWeek13)>1)
  BioTecWeek13 <- BioTecWeek13[,!(names(BioTecWeek13)%in%c("x"))]
if(nrow(BioTecWeek13)>1)
  BioTecWeek13 <- unique(BioTecWeek13)
if(nrow(BioTecWeek9)>1)
  BioTecWeek9 <- BioTecWeek9[,!(names(BioTecWeek9)%in%c("x"))]
if(nrow(BioTecWeek9)>1)
  BioTecWeek9 <- unique(BioTecWeek9)
if(nrow(BioTecWeek5)>1)
  BioTecWeek5 <- BioTecWeek5[,!(names(BioTecWeek5)%in%c("x"))]
if(nrow(BioTecWeek5)>1)
  BioTecWeek5 <- unique(BioTecWeek5)
if(nrow(BioTec)>1)
  BioTec <- BioTec[,!(names(BioTec)%in%c("x"))]
if(nrow(BioTec)>1)
  BioTec <- unique(BioTec)

if(nrow(PeakGradFin)>1)
  rownames(PeakGradFin) <- paste0(as.character(PeakGradFin$student.id),".",as.character(PeakGradFin$academic.cycle))
if(nrow(PeakGradWeek13)>1)
  rownames(PeakGradWeek13) <- paste0(as.character(PeakGradWeek13$student.id),".",as.character(PeakGradWeek13$academic.cycle))
if(nrow(PeakGradWeek9)>1)
  rownames(PeakGradWeek9) <- paste0(as.character(PeakGradWeek9$student.id),".",as.character(PeakGradWeek9$academic.cycle))
if(nrow(PeakGradWeek5)>1)
  rownames(PeakGradWeek5) <- paste0(as.character(PeakGradWeek5$student.id),".",as.character(PeakGradWeek5$academic.cycle))
if(nrow(PeakGrad)>1)
  rownames(PeakGrad) <- paste0(as.character(PeakGrad$student.id),".",as.character(PeakGrad$academic.cycle))

if(nrow(PeakPosGradFin)>1)
  rownames(PeakPosGradFin) <- paste0(as.character(PeakPosGradFin$student.id),".",as.character(PeakPosGradFin$academic.cycle))
if(nrow(PeakPosGradWeek13)>1)
  rownames(PeakPosGradWeek13) <- paste0(as.character(PeakPosGradWeek13$student.id),".",as.character(PeakPosGradWeek13$academic.cycle))
if(nrow(PeakPosGradWeek9)>1)
  rownames(PeakPosGradWeek9) <- paste0(as.character(PeakPosGradWeek9$student.id),".",as.character(PeakPosGradWeek9$academic.cycle))
if(nrow(PeakPosGradWeek5)>1)
  rownames(PeakPosGradWeek5) <- paste0(as.character(PeakPosGradWeek5$student.id),".",as.character(PeakPosGradWeek5$academic.cycle))
if(nrow(PeakPosGrad)>1)
  rownames(PeakPosGrad) <- paste0(as.character(PeakPosGrad$student.id),".",as.character(PeakPosGrad$academic.cycle))

if(nrow(PeakTecFin)>1)
  rownames(PeakTecFin) <- paste0(as.character(PeakTecFin$student.id),".",as.character(PeakTecFin$academic.cycle))
if(nrow(PeakTecWeek13)>1)
  rownames(PeakTecWeek13) <- paste0(as.character(PeakTecWeek13$student.id),".",as.character(PeakTecWeek13$academic.cycle))
if(nrow(PeakTecWeek9)>1)
  rownames(PeakTecWeek9) <- paste0(as.character(PeakTecWeek9$student.id),".",as.character(PeakTecWeek9$academic.cycle))
if(nrow(PeakTecWeek5)>1)
  rownames(PeakTecWeek5) <- paste0(as.character(PeakTecWeek5$student.id),".",as.character(PeakTecWeek5$academic.cycle))
if(nrow(PeakTec)>1)
  rownames(PeakTec) <- paste0(as.character(PeakTec$student.id),".",as.character(PeakTec$academic.cycle))

if(nrow(BioGradFin)>1)
  rownames(BioGradFin) <- paste0(as.character(BioGradFin$student.id),".",as.character(BioGradFin$academic.cycle))
if(nrow(BioGradWeek13)>1)
  rownames(BioGradWeek13) <- paste0(as.character(BioGradWeek13$student.id),".",as.character(BioGradWeek13$academic.cycle))
if(nrow(BioGradWeek9)>1)
  rownames(BioGradWeek9) <- paste0(as.character(BioGradWeek9$student.id),".",as.character(BioGradWeek9$academic.cycle))
if(nrow(BioGradWeek5)>1)
  rownames(BioGradWeek5) <- paste0(as.character(BioGradWeek5$student.id),".",as.character(BioGradWeek5$academic.cycle))
if(nrow(BioGrad)>1)
  rownames(BioGrad) <- paste0(as.character(BioGrad$student.id),".",as.character(BioGrad$academic.cycle))

if(nrow(BioPosGradFin)>1)
  rownames(BioPosGradFin) <- paste0(as.character(BioPosGradFin$student.id),".",as.character(BioPosGradFin$academic.cycle))
if(nrow(BioPosGradWeek13)>1)
  rownames(BioPosGradWeek13) <- paste0(as.character(BioPosGradWeek13$student.id),".",as.character(BioPosGradWeek13$academic.cycle))
if(nrow(BioPosGradWeek9)>1)
  rownames(BioPosGradWeek9) <- paste0(as.character(BioPosGradWeek9$student.id),".",as.character(BioPosGradWeek9$academic.cycle))
if(nrow(BioPosGradWeek5)>1)
  rownames(BioPosGradWeek5) <- paste0(as.character(BioPosGradWeek5$student.id),".",as.character(BioPosGradWeek5$academic.cycle))
if(nrow(BioPosGrad)>1)
  rownames(BioPosGrad) <- paste0(as.character(BioPosGrad$student.id),".",as.character(BioPosGrad$academic.cycle))

if(nrow(BioTecFin)>1)
  rownames(BioTecFin) <- paste0(as.character(BioTecFin$student.id),".",as.character(BioTecFin$academic.cycle))
if(nrow(BioTecWeek13)>1)
  rownames(BioTecWeek13) <- paste0(as.character(BioTecWeek13$student.id),".",as.character(BioTecWeek13$academic.cycle))
if(nrow(BioTecWeek9)>1)
  rownames(BioTecWeek9) <- paste0(as.character(BioTecWeek9$student.id),".",as.character(BioTecWeek9$academic.cycle))
if(nrow(BioTecWeek5)>1)
  rownames(BioTecWeek5) <- paste0(as.character(BioTecWeek5$student.id),".",as.character(BioTecWeek5$academic.cycle))
if(nrow(BioTec)>1)
  rownames(BioTec) <- paste0(as.character(BioTec$student.id),".",as.character(BioTec$academic.cycle))

drop.to.novo <-
c("student.id",
"grado",
"semestre",
"semana",
"peak.date",
"zipcode",
"class.start.date",
"enrollment.date",
"withdrawal.type",
"campus",
"scholarship.flag",
"degree.level")

drop.to.veteran <-
c("student.id",
"grado",
"semestre",
"semana",
"peak.date",
"block",
"zipcode",
"enrollment.cohort",
"enrollment.year",
"class.start.date",
"enrollment.date",
"withdrawal.type",
"campus",
"scholarship.flag",
"degree.level")


print("Dropping from novo data sets")

if(nrow(BioGrad)>1)
  BioGrad <- BioGrad[, !(  names(BioGrad) %in% drop.to.novo )]
if(nrow(BioGradWeek5)>1)
  BioGradWeek5 <- BioGradWeek5[, !(  names(BioGradWeek5) %in% drop.to.novo )]
if(nrow(BioGradWeek9)>1)
  BioGradWeek9 <- BioGradWeek9[, !(  names(BioGradWeek9) %in% drop.to.novo )]
if(nrow(BioGradWeek13)>1)
  BioGradWeek13 <- BioGradWeek13[, !(  names(BioGradWeek13) %in% drop.to.novo )]
if(nrow(BioGradFin)>1)
  BioGradFin <- BioGradFin[, !(  names(BioGradFin) %in% drop.to.novo )]

if(nrow(BioPosGrad)>1)
  BioPosGrad <- BioPosGrad[, !(  names(BioPosGrad) %in% drop.to.novo )]
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5 <- BioPosGradWeek5[, !(  names(BioPosGradWeek5) %in% drop.to.novo )]
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9 <- BioPosGradWeek9[, !(  names(BioPosGradWeek9) %in% drop.to.novo )]
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13 <- BioPosGradWeek13[, !(  names(BioPosGradWeek13) %in% drop.to.novo )]
if(nrow(BioPosGradFin)>1)
  BioPosGradFin <- BioPosGradFin[, !(  names(BioPosGradFin) %in% drop.to.novo )]

if(nrow(BioTec)>1)
  BioTec <- BioTec[, !(  names(BioTec) %in% drop.to.novo )]
if(nrow(BioTecWeek5)>1)
  BioTecWeek5 <- BioTecWeek5[, !(  names(BioTecWeek5) %in% drop.to.novo )]
if(nrow(BioTecWeek9)>1)
  BioTecWeek9 <- BioTecWeek9[, !(  names(BioTecWeek9) %in% drop.to.novo )]
if(nrow(BioTecWeek13)>1)
  BioTecWeek13 <- BioTecWeek13[, !(  names(BioTecWeek13) %in% drop.to.novo )]
if(nrow(BioTecFin)>1)
  BioTecFin <- BioTecFin[, !(  names(BioTecFin) %in% drop.to.novo )]

print("Dropping from veteran data sets")

if(nrow(PeakGrad)>1)
  PeakGrad <- PeakGrad[, !(  names(PeakGrad) %in% drop.to.veteran )]
if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5 <- PeakGradWeek5[, !(  names(PeakGradWeek5) %in% drop.to.veteran )]
if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9 <- PeakGradWeek9[, !(  names(PeakGradWeek9) %in% drop.to.veteran )]
if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13 <- PeakGradWeek13[, !(  names(PeakGradWeek13) %in% drop.to.veteran )]
if(nrow(PeakGradFin)>1)
  PeakGradFin <- PeakGradFin[, !(  names(PeakGradFin) %in% drop.to.veteran )]

if(nrow(PeakPosGrad)>1)
  PeakPosGrad <- PeakPosGrad[, !(  names(PeakPosGrad) %in% drop.to.veteran )]
if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5 <- PeakPosGradWeek5[, !(  names(PeakPosGradWeek5) %in% drop.to.veteran )]
if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9 <- PeakPosGradWeek9[, !(  names(PeakPosGradWeek9) %in% drop.to.veteran )]
if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13 <- PeakPosGradWeek13[, !(  names(PeakPosGradWeek13) %in% drop.to.veteran )]
if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin <- PeakPosGradFin[, !(  names(PeakPosGradFin) %in% drop.to.veteran )]

if(nrow(PeakTec)>1)
  PeakTec <- PeakTec[, !(  names(PeakTec) %in% drop.to.veteran )]
if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5 <- PeakTecWeek5[, !(  names(PeakTecWeek5) %in% drop.to.veteran )]
if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9 <- PeakTecWeek9[, !(  names(PeakTecWeek9) %in% drop.to.veteran )]
if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13 <- PeakTecWeek13[, !(  names(PeakTecWeek13) %in% drop.to.veteran )]
if(nrow(PeakTecFin)>1)
  PeakTecFin <- PeakTecFin[, !(  names(PeakTecFin) %in% drop.to.veteran )]

### dropping grades from w5 and w9

if(nrow(BioGradWeek5)>1)
  BioGradWeek5 <- BioGradWeek5[, !( names(BioGradWeek5) %in% c("notas","frequencia") )]
if(nrow(BioGradWeek9)>1)
  BioGradWeek9 <- BioGradWeek9[, !( names(BioGradWeek9) %in% c("notas","frequencia") )]
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5 <- BioPosGradWeek5[, !( names(BioPosGradWeek5) %in% c("notas","frequencia") )]
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9 <- BioPosGradWeek9[, !( names(BioPosGradWeek9) %in% c("notas","frequencia") )]
if(nrow(BioTecWeek5)>1)
  BioTecWeek5 <- BioTecWeek5[, !( names(BioTecWeek5) %in% c("notas","frequencia") )]
if(nrow(BioTecWeek9)>1)
  BioTecWeek9 <- BioTecWeek9[, !( names(BioTecWeek9) %in% c("notas","frequencia") )]

if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5 <- PeakGradWeek5[, !( names(PeakGradWeek5) %in% c("notas","frequencia") )]
if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9 <- PeakGradWeek9[, !( names(PeakGradWeek9) %in% c("notas","frequencia") )]
if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5 <- PeakPosGradWeek5[, !( names(PeakPosGradWeek5) %in% c("notas","frequencia") )]
if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9 <- PeakPosGradWeek9[, !( names(PeakPosGradWeek9) %in% c("notas","frequencia") )]
if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5 <- PeakTecWeek5[, !( names(PeakTecWeek5) %in% c("notas","frequencia") )]
if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9 <- PeakTecWeek9[, !( names(PeakTecWeek9) %in% c("notas","frequencia") )]

### cast variables ###

#notas

if(nrow(BioGradWeek13)>1)
  BioGradWeek13$notas <- as.numeric(BioGradWeek13$notas)
if(nrow(BioGradFin)>1)
  BioGradFin$notas <- as.numeric(BioGradFin$notas)

if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13$notas <- as.numeric(BioPosGradWeek13$notas)
if(nrow(BioPosGradFin)>1)
  BioPosGradFin$notas <- as.numeric(BioPosGradFin$notas)

if(nrow(BioTecWeek13)>1)
  BioTecWeek13$notas <- as.numeric(BioTecWeek13$notas)
if(nrow(BioTecFin)>1)
  BioTecFin$notas <- as.numeric(BioTecFin$notas)

if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13$notas <- as.numeric(PeakGradWeek13$notas)
if(nrow(PeakGradFin)>1)
  PeakGradFin$notas <- as.numeric(PeakGradFin$notas)

if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13$notas <- as.numeric(PeakPosGradWeek13$notas)
if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin$notas <- as.numeric(PeakPosGradFin$notas)

if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13$notas <- as.numeric(PeakTecWeek13$notas)
if(nrow(PeakTecFin)>1)
  PeakTecFin$notas <- as.numeric(PeakTecFin$notas)

#city.aggregation
if(nrow(BioGrad)>1)
  BioGrad$city.aggregation <- as.factor(BioGrad$city.aggregation)
if(nrow(BioGradWeek5)>1)
  BioGradWeek5$city.aggregation <- as.factor(BioGradWeek5$city.aggregation)
if(nrow(BioGradWeek9)>1)
  BioGradWeek9$city.aggregation <- as.factor(BioGradWeek9$city.aggregation)
if(nrow(BioGradWeek13)>1)
  BioGradWeek13$city.aggregation <- as.factor(BioGradWeek13$city.aggregation)
if(nrow(BioGradFin)>1)
  BioGradFin$city.aggregation <- as.factor(BioGradFin$city.aggregation)

if(nrow(BioPosGrad)>1)
  BioPosGrad$city.aggregation <- as.factor(BioPosGrad$city.aggregation)
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5$city.aggregation <- as.factor(BioPosGradWeek5$city.aggregation)
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9$city.aggregation <- as.factor(BioPosGradWeek9$city.aggregation)
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13$city.aggregation <- as.factor(BioPosGradWeek13$city.aggregation)
if(nrow(BioPosGradFin)>1)
  BioPosGradFin$city.aggregation <- as.factor(BioPosGradFin$city.aggregation)

if(nrow(BioTec)>1)
  BioTec$city.aggregation <- as.factor(BioTec$city.aggregation)
if(nrow(BioTecWeek5)>1)
  BioTecWeek5$city.aggregation <- as.factor(BioTecWeek5$city.aggregation)
if(nrow(BioTecWeek9)>1)
  BioTecWeek9$city.aggregation <- as.factor(BioTecWeek9$city.aggregation)
if(nrow(BioTecWeek13)>1)
  BioTecWeek13$city.aggregation <- as.factor(BioTecWeek13$city.aggregation)
if(nrow(BioTecFin)>1)
  BioTecFin$city.aggregation <- as.factor(BioTecFin$city.aggregation)

#state.aggregation
if(nrow(BioGrad)>1)
  BioGrad$state.aggregation <- as.factor(BioGrad$state.aggregation)
if(nrow(BioGradWeek5)>1)
  BioGradWeek5$state.aggregation <- as.factor(BioGradWeek5$state.aggregation)
if(nrow(BioGradWeek9)>1)
  BioGradWeek9$state.aggregation <- as.factor(BioGradWeek9$state.aggregation)
if(nrow(BioGradWeek13)>1)
  BioGradWeek13$state.aggregation <- as.factor(BioGradWeek13$state.aggregation)
if(nrow(BioGradFin)>1)
  BioGradFin$state.aggregation <- as.factor(BioGradFin$state.aggregation)

if(nrow(BioPosGrad)>1)
  BioPosGrad$state.aggregation <- as.factor(BioPosGrad$state.aggregation)
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5$state.aggregation <- as.factor(BioPosGradWeek5$state.aggregation)
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9$state.aggregation <- as.factor(BioPosGradWeek9$state.aggregation)
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13$state.aggregation <- as.factor(BioPosGradWeek13$state.aggregation)
if(nrow(BioPosGradFin)>1)
  BioPosGradFin$state.aggregation <- as.factor(BioPosGradFin$state.aggregation)

if(nrow(BioTec)>1)
  BioTec$state.aggregation <- as.factor(BioTec$state.aggregation)
if(nrow(BioTecWeek5)>1)
  BioTecWeek5$state.aggregation <- as.factor(BioTecWeek5$state.aggregation)
if(nrow(BioTecWeek9)>1)
  BioTecWeek9$state.aggregation <- as.factor(BioTecWeek9$state.aggregation)
if(nrow(BioTecWeek13)>1)
  BioTecWeek13$state.aggregation <- as.factor(BioTecWeek13$state.aggregation)
if(nrow(BioTecFin)>1)
  BioTecFin$state.aggregation <- as.factor(BioTecFin$state.aggregation)

#high.school.agreggation
if(nrow(BioGrad)>1)
  BioGrad$high.school.agreggation <- as.factor(BioGrad$high.school.agreggation)
if(nrow(BioGradWeek5)>1)
  BioGradWeek5$high.school.agreggation <- as.factor(BioGradWeek5$high.school.agreggation)
if(nrow(BioGradWeek9)>1)
  BioGradWeek9$high.school.agreggation <- as.factor(BioGradWeek9$high.school.agreggation)
if(nrow(BioGradWeek13)>1)
  BioGradWeek13$high.school.agreggation <- as.factor(BioGradWeek13$high.school.agreggation)
if(nrow(BioGradFin)>1)
  BioGradFin$high.school.agreggation <- as.factor(BioGradFin$high.school.agreggation)

if(nrow(BioPosGrad)>1)
  BioPosGrad$high.school.agreggation <- as.factor(BioPosGrad$high.school.agreggation)
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5$high.school.agreggation <- as.factor(BioPosGradWeek5$high.school.agreggation)
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9$high.school.agreggation <- as.factor(BioPosGradWeek9$high.school.agreggation)
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13$high.school.agreggation <- as.factor(BioPosGradWeek13$high.school.agreggation)
if(nrow(BioPosGradFin)>1)
  BioPosGradFin$high.school.agreggation <- as.factor(BioPosGradFin$high.school.agreggation)

if(nrow(BioTec)>1)
  BioTec$high.school.agreggation <- as.factor(BioTec$high.school.agreggation)
if(nrow(BioTecWeek5)>1)
  BioTecWeek5$high.school.agreggation <- as.factor(BioTecWeek5$high.school.agreggation)
if(nrow(BioTecWeek9)>1)
  BioTecWeek9$high.school.agreggation <- as.factor(BioTecWeek9$high.school.agreggation)
if(nrow(BioTecWeek13)>1)
  BioTecWeek13$high.school.agreggation <- as.factor(BioTecWeek13$high.school.agreggation)
if(nrow(BioTecFin)>1)
  BioTecFin$high.school.agreggation <- as.factor(BioTecFin$high.school.agreggation)

#forma.ingresso.agreggation
if(nrow(BioGrad)>1)
  BioGrad$forma.ingresso.agreggation <- as.factor(BioGrad$forma.ingresso.agreggation)
if(nrow(BioGradWeek5)>1)
  BioGradWeek5$forma.ingresso.agreggation <- as.factor(BioGradWeek5$forma.ingresso.agreggation)
if(nrow(BioGradWeek9)>1)
  BioGradWeek9$forma.ingresso.agreggation <- as.factor(BioGradWeek9$forma.ingresso.agreggation)
if(nrow(BioGradWeek13)>1)
  BioGradWeek13$forma.ingresso.agreggation <- as.factor(BioGradWeek13$forma.ingresso.agreggation)
if(nrow(BioGradFin)>1)
  BioGradFin$forma.ingresso.agreggation <- as.factor(BioGradFin$forma.ingresso.agreggation)

if(nrow(BioTec)>1)
  BioTec$forma.ingresso.agreggation <- as.factor(BioTec$forma.ingresso.agreggation)
if(nrow(BioTecWeek5)>1)
  BioTecWeek5$forma.ingresso.agreggation <- as.factor(BioTecWeek5$forma.ingresso.agreggation)
if(nrow(BioTecWeek9)>1)
  BioTecWeek9$forma.ingresso.agreggation <- as.factor(BioTecWeek9$forma.ingresso.agreggation)
if(nrow(BioTecWeek13)>1)
  BioTecWeek13$forma.ingresso.agreggation <- as.factor(BioTecWeek13$forma.ingresso.agreggation)
if(nrow(BioTecFin)>1)
  BioTecFin$forma.ingresso.agreggation <- as.factor(BioTecFin$forma.ingresso.agreggation)

#faculty.agreggation
if(nrow(BioGrad)>1)
  BioGrad$faculty.agreggation <- as.factor(BioGrad$faculty.agreggation)
if(nrow(BioGradWeek5)>1)
  BioGradWeek5$faculty.agreggation <- as.factor(BioGradWeek5$faculty.agreggation)
if(nrow(BioGradWeek9)>1)
  BioGradWeek9$faculty.agreggation <- as.factor(BioGradWeek9$faculty.agreggation)
if(nrow(BioGradWeek13)>1)
  BioGradWeek13$faculty.agreggation <- as.factor(BioGradWeek13$faculty.agreggation)
if(nrow(BioGradFin)>1)
  BioGradFin$faculty.agreggation <- as.factor(BioGradFin$faculty.agreggation)

if(nrow(BioPosGrad)>1)
  BioPosGrad$faculty.agreggation <- as.factor(BioPosGrad$faculty.agreggation)
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5$faculty.agreggation <- as.factor(BioPosGradWeek5$faculty.agreggation)
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9$faculty.agreggation <- as.factor(BioPosGradWeek9$faculty.agreggation)
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13$faculty.agreggation <- as.factor(BioPosGradWeek13$faculty.agreggation)
if(nrow(BioPosGradFin)>1)
  BioPosGradFin$faculty.agreggation <- as.factor(BioPosGradFin$faculty.agreggation)

if(nrow(BioTec)>1)
  BioTec$faculty.agreggation <- as.factor(BioTec$faculty.agreggation)
if(nrow(BioTecWeek5)>1)
  BioTecWeek5$faculty.agreggation <- as.factor(BioTecWeek5$faculty.agreggation)
if(nrow(BioTecWeek9)>1)
  BioTecWeek9$faculty.agreggation <- as.factor(BioTecWeek9$faculty.agreggation)
if(nrow(BioTecWeek13)>1)
  BioTecWeek13$faculty.agreggation <- as.factor(BioTecWeek13$faculty.agreggation)
if(nrow(BioTecFin)>1)
  BioTecFin$faculty.agreggation <- as.factor(BioTecFin$faculty.agreggation)

if(nrow(PeakGrad)>1)
  PeakGrad$faculty.agreggation <- as.factor(PeakGrad$faculty.agreggation)
if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5$faculty.agreggation <- as.factor(PeakGradWeek5$faculty.agreggation)
if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9$faculty.agreggation <- as.factor(PeakGradWeek9$faculty.agreggation)
if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13$faculty.agreggation <- as.factor(PeakGradWeek13$faculty.agreggation)
if(nrow(PeakGradFin)>1)
  PeakGradFin$faculty.agreggation <- as.factor(PeakGradFin$faculty.agreggation)

if(nrow(PeakPosGrad)>1)
  PeakPosGrad$faculty.agreggation <- as.factor(PeakPosGrad$faculty.agreggation)
if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5$faculty.agreggation <- as.factor(PeakPosGradWeek5$faculty.agreggation)
if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9$faculty.agreggation <- as.factor(PeakPosGradWeek9$faculty.agreggation)
if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13$faculty.agreggation <- as.factor(PeakPosGradWeek13$faculty.agreggation)
if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin$faculty.agreggation <- as.factor(PeakPosGradFin$faculty.agreggation)

if(nrow(PeakTec)>1)
  PeakTec$faculty.agreggation <- as.factor(PeakTec$faculty.agreggation)
if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5$faculty.agreggation <- as.factor(PeakTecWeek5$faculty.agreggation)
if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9$faculty.agreggation <- as.factor(PeakTecWeek9$faculty.agreggation)
if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13$faculty.agreggation <- as.factor(PeakTecWeek13$faculty.agreggation)
if(nrow(PeakTecFin)>1)
  PeakTecFin$faculty.agreggation <- as.factor(PeakTecFin$faculty.agreggation)

#first.year.cummulative.gpa
if(nrow(PeakGrad)>1)
  PeakGrad$first.year.cummulative.gpa <- as.numeric(PeakGrad$first.year.cummulative.gpa)
if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5$first.year.cummulative.gpa <- as.numeric(PeakGradWeek5$first.year.cummulative.gpa)
if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9$first.year.cummulative.gpa <- as.numeric(PeakGradWeek9$first.year.cummulative.gpa)
if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13$first.year.cummulative.gpa <- as.numeric(PeakGradWeek13$first.year.cummulative.gpa)
if(nrow(PeakGradFin)>1)
  PeakGradFin$first.year.cummulative.gpa <- as.numeric(PeakGradFin$first.year.cummulative.gpa)

if(nrow(PeakPosGrad)>1)
  PeakPosGrad$first.year.cummulative.gpa <- as.numeric(PeakPosGrad$first.year.cummulative.gpa)
if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5$first.year.cummulative.gpa <- as.numeric(PeakPosGradWeek5$first.year.cummulative.gpa)
if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9$first.year.cummulative.gpa <- as.numeric(PeakPosGradWeek9$first.year.cummulative.gpa)
if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13$first.year.cummulative.gpa <- as.numeric(PeakPosGradWeek13$first.year.cummulative.gpa)
if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin$first.year.cummulative.gpa <- as.numeric(PeakPosGradFin$first.year.cummulative.gpa)

if(nrow(PeakTec)>1)
  PeakTec$first.year.cummulative.gpa <- as.numeric(PeakTec$first.year.cummulative.gpa)
if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5$first.year.cummulative.gpa <- as.numeric(PeakTecWeek5$first.year.cummulative.gpa)
if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9$first.year.cummulative.gpa <- as.numeric(PeakTecWeek9$first.year.cummulative.gpa)
if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13$first.year.cummulative.gpa <- as.numeric(PeakTecWeek13$first.year.cummulative.gpa)
if(nrow(PeakTecFin)>1)
  PeakTecFin$first.year.cummulative.gpa <- as.numeric(PeakTecFin$first.year.cummulative.gpa)

#cummulative.gpa.last
if(nrow(PeakGrad)>1)
  PeakGrad$cummulative.gpa.last <- as.numeric(PeakGrad$cummulative.gpa.last)
if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5$cummulative.gpa.last <- as.numeric(PeakGradWeek5$cummulative.gpa.last)
if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9$cummulative.gpa.last <- as.numeric(PeakGradWeek9$cummulative.gpa.last)
if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13$cummulative.gpa.last <- as.numeric(PeakGradWeek13$cummulative.gpa.last)
if(nrow(PeakGradFin)>1)
  PeakGradFin$cummulative.gpa.last <- as.numeric(PeakGradFin$cummulative.gpa.last)

if(nrow(PeakPosGrad)>1)
  PeakPosGrad$cummulative.gpa.last <- as.numeric(PeakPosGrad$cummulative.gpa.last)
if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5$cummulative.gpa.last <- as.numeric(PeakPosGradWeek5$cummulative.gpa.last)
if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9$cummulative.gpa.last <- as.numeric(PeakPosGradWeek9$cummulative.gpa.last)
if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13$cummulative.gpa.last <- as.numeric(PeakPosGradWeek13$cummulative.gpa.last)
if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin$cummulative.gpa.last <- as.numeric(PeakPosGradFin$cummulative.gpa.last)

if(nrow(PeakTec)>1)
  PeakTec$cummulative.gpa.last <- as.numeric(PeakTec$cummulative.gpa.last)
if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5$cummulative.gpa.last <- as.numeric(PeakTecWeek5$cummulative.gpa.last)
if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9$cummulative.gpa.last <- as.numeric(PeakTecWeek9$cummulative.gpa.last)
if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13$cummulative.gpa.last <- as.numeric(PeakTecWeek13$cummulative.gpa.last)
if(nrow(PeakTecFin)>1)
  PeakTecFin$cummulative.gpa.last <- as.numeric(PeakTecFin$cummulative.gpa.last)

#academic.period.gpa.last
if(nrow(PeakGrad)>1)
  PeakGrad$academic.period.gpa.last <- as.numeric(PeakGrad$academic.period.gpa.last)
if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5$academic.period.gpa.last <- as.numeric(PeakGradWeek5$academic.period.gpa.last)
if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9$academic.period.gpa.last <- as.numeric(PeakGradWeek9$academic.period.gpa.last)
if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13$academic.period.gpa.last <- as.numeric(PeakGradWeek13$academic.period.gpa.last)
if(nrow(PeakGradFin)>1)
  PeakGradFin$academic.period.gpa.last <- as.numeric(PeakGradFin$academic.period.gpa.last)

if(nrow(PeakPosGrad)>1)
  PeakPosGrad$academic.period.gpa.last <- as.numeric(PeakPosGrad$academic.period.gpa.last)
if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5$academic.period.gpa.last <- as.numeric(PeakPosGradWeek5$academic.period.gpa.last)
if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9$academic.period.gpa.last <- as.numeric(PeakPosGradWeek9$academic.period.gpa.last)
if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13$academic.period.gpa.last <- as.numeric(PeakPosGradWeek13$academic.period.gpa.last)
if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin$academic.period.gpa.last <- as.numeric(PeakPosGradFin$academic.period.gpa.last)

if(nrow(PeakTec)>1)
  PeakTec$academic.period.gpa.last <- as.numeric(PeakTec$academic.period.gpa.last)
if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5$academic.period.gpa.last <- as.numeric(PeakTecWeek5$academic.period.gpa.last)
if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9$academic.period.gpa.last <- as.numeric(PeakTecWeek9$academic.period.gpa.last)
if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13$academic.period.gpa.last <- as.numeric(PeakTecWeek13$academic.period.gpa.last)
if(nrow(PeakTecFin)>1)
  PeakTecFin$academic.period.gpa.last <- as.numeric(PeakTecFin$academic.period.gpa.last)

#enem.score
if(nrow(BioGrad)>1)
  BioGrad$enem.score <- as.numeric(BioGrad$enem.score)
if(nrow(BioGradWeek5)>1)
  BioGradWeek5$enem.score <- as.numeric(BioGradWeek5$enem.score)
if(nrow(BioGradWeek9)>1)
  BioGradWeek9$enem.score <- as.numeric(BioGradWeek9$enem.score)
if(nrow(BioGradWeek13)>1)
  BioGradWeek13$enem.score <- as.numeric(BioGradWeek13$enem.score)
if(nrow(BioGradFin)>1)
  BioGradFin$enem.score <- as.numeric(BioGradFin$enem.score)

if(nrow(BioPosGrad)>1)
  BioPosGrad$enem.score <- as.numeric(BioPosGrad$enem.score)
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5$enem.score <- as.numeric(BioPosGradWeek5$enem.score)
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9$enem.score <- as.numeric(BioPosGradWeek9$enem.score)
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13$enem.score <- as.numeric(BioPosGradWeek13$enem.score)
if(nrow(BioPosGradFin)>1)
  BioPosGradFin$enem.score <- as.numeric(BioPosGradFin$enem.score)

if(nrow(BioTec)>1)
  BioTec$enem.score <- as.numeric(BioTec$enem.score)
if(nrow(BioTecWeek5)>1)
  BioTecWeek5$enem.score <- as.numeric(BioTecWeek5$enem.score)
if(nrow(BioTecWeek9)>1)
  BioTecWeek9$enem.score <- as.numeric(BioTecWeek9$enem.score)
if(nrow(BioTecWeek13)>1)
  BioTecWeek13$enem.score <- as.numeric(BioTecWeek13$enem.score)
if(nrow(BioTecFin)>1)
  BioTecFin$enem.score <- as.numeric(BioTecFin$enem.score)

#vest.score
if(nrow(BioGrad)>1)
  BioGrad$vest.score <- as.numeric(BioGrad$vest.score)
if(nrow(BioGradWeek5)>1)
  BioGradWeek5$vest.score <- as.numeric(BioGradWeek5$vest.score)
if(nrow(BioGradWeek9)>1)
  BioGradWeek9$vest.score <- as.numeric(BioGradWeek9$vest.score)
if(nrow(BioGradWeek13)>1)
  BioGradWeek13$vest.score <- as.numeric(BioGradWeek13$vest.score)
if(nrow(BioGradFin)>1)
  BioGradFin$vest.score <- as.numeric(BioGradFin$vest.score)

if(nrow(BioPosGrad)>1)
  BioPosGrad$vest.score <- as.numeric(BioPosGrad$vest.score)
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5$vest.score <- as.numeric(BioPosGradWeek5$vest.score)
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9$vest.score <- as.numeric(BioPosGradWeek9$vest.score)
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13$vest.score <- as.numeric(BioPosGradWeek13$vest.score)
if(nrow(BioPosGradFin)>1)
  BioPosGradFin$vest.score <- as.numeric(BioPosGradFin$vest.score)

if(nrow(BioTec)>1)
  BioTec$vest.score <- as.numeric(BioTec$vest.score)
if(nrow(BioTecWeek5)>1)
  BioTecWeek5$vest.score <- as.numeric(BioTecWeek5$vest.score)
if(nrow(BioTecWeek9)>1)
  BioTecWeek9$vest.score <- as.numeric(BioTecWeek9$vest.score)
if(nrow(BioTecWeek13)>1)
  BioTecWeek13$vest.score <- as.numeric(BioTecWeek13$vest.score)
if(nrow(BioTecFin)>1)
  BioTecFin$vest.score <- as.numeric(BioTecFin$vest.score)




######## scholarship.type
#
###########################

print("changing na values to scholarship")

if(nrow(BioGrad)>1)
  BioGrad$scholarship.type <- mapply(ScholarshipType, BioGrad$scholarship.type)
if(nrow(BioGradWeek5)>1)
  BioGradWeek5$scholarship.type <- mapply(ScholarshipType, BioGradWeek5$scholarship.type)
if(nrow(BioGradWeek9)>1)
  BioGradWeek9$scholarship.type <- mapply(ScholarshipType, BioGradWeek9$scholarship.type)
if(nrow(BioGradWeek13)>1)
  BioGradWeek13$scholarship.type <- mapply(ScholarshipType, BioGradWeek13$scholarship.type)
if(nrow(BioGradFin)>1)
  BioGradFin$scholarship.type <- mapply(ScholarshipType, BioGradFin$scholarship.type)
if(nrow(PeakGrad)>1)
  PeakGrad$scholarship.type <- mapply(ScholarshipType, PeakGrad$scholarship.type)
if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5$scholarship.type <- mapply(ScholarshipType, PeakGradWeek5$scholarship.type)
if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9$scholarship.type <- mapply(ScholarshipType, PeakGradWeek9$scholarship.type)
if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13$scholarship.type <- mapply(ScholarshipType, PeakGradWeek13$scholarship.type)
if(nrow(PeakGradFin)>1)
  PeakGradFin$scholarship.type <- mapply(ScholarshipType, PeakGradFin$scholarship.type)

if(nrow(BioPosGrad)>1)
  BioPosGrad$scholarship.type <- mapply(ScholarshipType, BioPosGrad$scholarship.type)
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5$scholarship.type <- mapply(ScholarshipType, BioPosGradWeek5$scholarship.type)
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9$scholarship.type <- mapply(ScholarshipType, BioPosGradWeek9$scholarship.type)
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13$scholarship.type <- mapply(ScholarshipType, BioPosGradWeek13$scholarship.type)
if(nrow(BioPosGradFin)>1)
  BioPosGradFin$scholarship.type <- mapply(ScholarshipType, BioPosGradFin$scholarship.type)
if(nrow(PeakPosGrad)>1)
  PeakPosGrad$scholarship.type <- mapply(ScholarshipType, PeakPosGrad$scholarship.type)
if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5$scholarship.type <- mapply(ScholarshipType, PeakPosGradWeek5$scholarship.type)
if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9$scholarship.type <- mapply(ScholarshipType, PeakPosGradWeek9$scholarship.type)
if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13$scholarship.type <- mapply(ScholarshipType, PeakPosGradWeek13$scholarship.type)
if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin$scholarship.type <- mapply(ScholarshipType, PeakPosGradFin$scholarship.type)

if(nrow(BioTec)>1)
  BioTec$scholarship.type <- mapply(ScholarshipType, BioTec$scholarship.type)
if(nrow(BioTecWeek5)>1)
  BioTecWeek5$scholarship.type <- mapply(ScholarshipType, BioTecWeek5$scholarship.type)
if(nrow(BioTecWeek9)>1)
  BioTecWeek9$scholarship.type <- mapply(ScholarshipType, BioTecWeek9$scholarship.type)
if(nrow(BioTecWeek13)>1)
  BioTecWeek13$scholarship.type <- mapply(ScholarshipType, BioTecWeek13$scholarship.type)
if(nrow(BioTecFin)>1)
  BioTecFin$scholarship.type <- mapply(ScholarshipType, BioTecFin$scholarship.type)
if(nrow(PeakTec)>1)
  PeakTec$scholarship.type <- mapply(ScholarshipType, PeakTec$scholarship.type)
if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5$scholarship.type <- mapply(ScholarshipType, PeakTecWeek5$scholarship.type)
if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9$scholarship.type <- mapply(ScholarshipType, PeakTecWeek9$scholarship.type)
if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13$scholarship.type <- mapply(ScholarshipType, PeakTecWeek13$scholarship.type)
if(nrow(PeakTecFin)>1)
  PeakTecFin$scholarship.type <- mapply(ScholarshipType, PeakTecFin$scholarship.type)

######## Years to Enter (Novo)
#
###########################

print("calc happend years to enter")

if(nrow(BioGrad)>1)
  BioGrad$years.to.enter <- mapply(yearsToEnter,BioGrad$academic.cycle,BioGrad$high.school.graduation.year)
if(nrow(BioGradWeek5)>1)
  BioGradWeek5$years.to.enter <- mapply(yearsToEnter,BioGradWeek5$academic.cycle,BioGradWeek5$high.school.graduation.year)
if(nrow(BioGradWeek9)>1)
  BioGradWeek9$years.to.enter <- mapply(yearsToEnter,BioGradWeek9$academic.cycle,BioGradWeek9$high.school.graduation.year)
if(nrow(BioGradWeek13)>1)
  BioGradWeek13$years.to.enter <- mapply(yearsToEnter,BioGradWeek13$academic.cycle,BioGradWeek13$high.school.graduation.year)
if(nrow(BioGradFin)>1)
  BioGradFin$years.to.enter <- mapply(yearsToEnter,BioGradFin$academic.cycle,BioGradFin$high.school.graduation.year)
if(nrow(BioGrad)>1)
  BioGrad <- BioGrad[,!(names(BioGrad)%in%c("high.school.graduation.year"))]
if(nrow(BioGradWeek5)>1)
  BioGradWeek5 <- BioGradWeek5[,!(names(BioGradWeek5)%in%c("high.school.graduation.year"))]
if(nrow(BioGradWeek9)>1)
  BioGradWeek9 <- BioGradWeek9[,!(names(BioGradWeek9)%in%c("high.school.graduation.year"))]
if(nrow(BioGradWeek13)>1)
  BioGradWeek13 <- BioGradWeek13[,!(names(BioGradWeek13)%in%c("high.school.graduation.year"))]
if(nrow(BioGradFin)>1)
  BioGradFin <- BioGradFin[,!(names(BioGradFin)%in%c("high.school.graduation.year"))]

if(nrow(BioPosGrad)>1)
  BioPosGrad$years.to.enter <- mapply(yearsToEnter,BioPosGrad$academic.cycle,BioPosGrad$high.school.graduation.year)
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5$years.to.enter <- mapply(yearsToEnter,BioPosGradWeek5$academic.cycle,BioPosGradWeek5$high.school.graduation.year)
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9$years.to.enter <- mapply(yearsToEnter,BioPosGradWeek9$academic.cycle,BioPosGradWeek9$high.school.graduation.year)
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13$years.to.enter <- mapply(yearsToEnter,BioPosGradWeek13$academic.cycle,BioPosGradWeek13$high.school.graduation.year)
if(nrow(BioPosGradFin)>1)
  BioPosGradFin$years.to.enter <- mapply(yearsToEnter,BioPosGradFin$academic.cycle,BioPosGradFin$high.school.graduation.year)
if(nrow(BioPosGrad)>1)
  BioPosGrad <- BioPosGrad[,!(names(BioPosGrad)%in%c("high.school.graduation.year"))]
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5 <- BioPosGradWeek5[,!(names(BioPosGradWeek5)%in%c("high.school.graduation.year"))]
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9 <- BioPosGradWeek9[,!(names(BioPosGradWeek9)%in%c("high.school.graduation.year"))]
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13 <- BioPosGradWeek13[,!(names(BioPosGradWeek13)%in%c("high.school.graduation.year"))]
if(nrow(BioPosGradFin)>1)
  BioPosGradFin <- BioPosGradFin[,!(names(BioPosGradFin)%in%c("high.school.graduation.year"))]

if(nrow(BioTec)>1)
  BioTec$years.to.enter <- mapply(yearsToEnter,BioTec$academic.cycle,BioTec$high.school.graduation.year)
if(nrow(BioTecWeek5)>1)
  BioTecWeek5$years.to.enter <- mapply(yearsToEnter,BioTecWeek5$academic.cycle,BioTecWeek5$high.school.graduation.year)
if(nrow(BioTecWeek9)>1)
  BioTecWeek9$years.to.enter <- mapply(yearsToEnter,BioTecWeek9$academic.cycle,BioTecWeek9$high.school.graduation.year)
if(nrow(BioTecWeek13)>1)
  BioTecWeek13$years.to.enter <- mapply(yearsToEnter,BioTecWeek13$academic.cycle,BioTecWeek13$high.school.graduation.year)
if(nrow(BioTecFin)>1)
  BioTecFin$years.to.enter <- mapply(yearsToEnter,BioTecFin$academic.cycle,BioTecFin$high.school.graduation.year)
if(nrow(BioTec)>1)
  BioTec <- BioTec[,!(names(BioTec)%in%c("high.school.graduation.year"))]
if(nrow(BioTecWeek5)>1)
  BioTecWeek5 <- BioTecWeek5[,!(names(BioTecWeek5)%in%c("high.school.graduation.year"))]
if(nrow(BioTecWeek9)>1)
  BioTecWeek9 <- BioTecWeek9[,!(names(BioTecWeek9)%in%c("high.school.graduation.year"))]
if(nrow(BioTecWeek13)>1)
  BioTecWeek13 <- BioTecWeek13[,!(names(BioTecWeek13)%in%c("high.school.graduation.year"))]
if(nrow(BioTecFin)>1)
  BioTecFin <- BioTecFin[,!(names(BioTecFin)%in%c("high.school.graduation.year"))]

### Drop fields on week 13 and Fim

if(nrow(BioGradFin)>1)
  BioGradFin$academic.period.gpa <- as.numeric(BioGradFin$academic.period.gpa)
if(nrow(BioGradFin)>1)
  BioGradFin$failed.courses <- as.numeric(BioGradFin$failed.courses)
if(nrow(PeakGradFin)>1)
  PeakGradFin$academic.period.gpa <- as.numeric(PeakGradFin$academic.period.gpa)
if(nrow(PeakGradFin)>1)
  PeakGradFin$failed.courses <- as.numeric(PeakGradFin$failed.courses)

if(nrow(BioPosGradFin)>1)
  BioPosGradFin$academic.period.gpa <- as.numeric(BioPosGradFin$academic.period.gpa)
if(nrow(BioPosGradFin)>1)
  BioPosGradFin$failed.courses <- as.numeric(BioPosGradFin$failed.courses)
if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin$academic.period.gpa <- as.numeric(PeakPosGradFin$academic.period.gpa)
if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin$failed.courses <- as.numeric(PeakPosGradFin$failed.courses)

if(nrow(BioTecFin)>1)
  BioTecFin$academic.period.gpa <- as.numeric(BioTecFin$academic.period.gpa)
if(nrow(BioTecFin)>1)
  BioTecFin$failed.courses <- as.numeric(BioTecFin$failed.courses)
if(nrow(PeakTecFin)>1)
  PeakTecFin$academic.period.gpa <- as.numeric(PeakTecFin$academic.period.gpa)
if(nrow(PeakTecFin)>1)
  PeakTecFin$failed.courses <- as.numeric(PeakTecFin$failed.courses)

drop.to.fim <- c(
  "first.year.cummulative.gpa",
  "cummulative.gpa.last",
  "academic.period.gpa.last",
  "failed.courses.last",
  "cummulative.credits.earned.last",
  "cummulative.credits.attempted.last",
  "notas"
)

if(nrow(PeakGradFin)>1)
  PeakGradFin <- PeakGradFin[,!(names(PeakGradFin)%in%drop.to.fim)]
if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin <- PeakPosGradFin[,!(names(PeakPosGradFin)%in%drop.to.fim)]
if(nrow(PeakTecFin)>1)
  PeakTecFin <- PeakTecFin[,!(names(PeakTecFin)%in%drop.to.fim)]

drop.to.fim <- c(
  "notas"
)

if(nrow(BioGradFin)>1)
  BioGradFin <- BioGradFin[,!(names(BioGradFin)%in%drop.to.fim)]
if(nrow(BioPosGradFin)>1)
  BioPosGradFin <- BioPosGradFin[,!(names(BioPosGradFin)%in%drop.to.fim)]
if(nrow(BioTecFin)>1)
  BioTecFin <- BioTecFin[,!(names(BioTecFin)%in%drop.to.fim)]

###NANs

print("NaNs treatment")

if(nrow(BioGrad)>1)
  BioGrad[is.na(BioGrad)] <- 0
if(nrow(BioGradWeek5)>1)
  BioGradWeek5[is.na(BioGradWeek5)] <- 0
if(nrow(BioGradWeek9)>1)
  BioGradWeek9[is.na(BioGradWeek9)] <- 0
if(nrow(BioGradWeek13)>1)
  BioGradWeek13[is.na(BioGradWeek13)] <- 0
if(nrow(BioGradFin)>1)
  BioGradFin[is.na(BioGradFin)] <- 0
if(nrow(PeakGrad)>1)
  PeakGrad[is.na(PeakGrad)] <- 0
if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5[is.na(PeakGradWeek5)] <- 0
if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9[is.na(PeakGradWeek9)] <- 0
if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13[is.na(PeakGradWeek13)] <- 0
if(nrow(PeakGradFin)>1)
  PeakGradFin[is.na(PeakGradFin)] <- 0

if(nrow(BioPosGrad)>1)
  BioPosGrad[is.na(BioPosGrad)] <- 0
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5[is.na(BioPosGradWeek5)] <- 0
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9[is.na(BioPosGradWeek9)] <- 0
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13[is.na(BioPosGradWeek13)] <- 0
if(nrow(BioPosGradFin)>1)
  BioPosGradFin[is.na(BioPosGradFin)] <- 0
if(nrow(PeakPosGrad)>1)
  PeakPosGrad[is.na(PeakPosGrad)] <- 0
if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5[is.na(PeakPosGradWeek5)] <- 0
if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9[is.na(PeakPosGradWeek9)] <- 0
if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13[is.na(PeakPosGradWeek13)] <- 0
if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin[is.na(PeakPosGradFin)] <- 0

if(nrow(BioTec)>1)
  BioTec[is.na(BioTec)] <- 0
if(nrow(BioTecWeek5)>1)
  BioTecWeek5[is.na(BioTecWeek5)] <- 0
if(nrow(BioTecWeek9)>1)
  BioTecWeek9[is.na(BioTecWeek9)] <- 0
if(nrow(BioTecWeek13)>1)
  BioTecWeek13[is.na(BioTecWeek13)] <- 0
if(nrow(BioTecFin)>1)
  BioTecFin[is.na(BioTecFin)] <- 0
if(nrow(PeakTec)>1)
  PeakTec[is.na(PeakTec)] <- 0
if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5[is.na(PeakTecWeek5)] <- 0
if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9[is.na(PeakTecWeek9)] <- 0
if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13[is.na(PeakTecWeek13)] <- 0
if(nrow(PeakTecFin)>1)
  PeakTecFin[is.na(PeakTecFin)] <- 0

### prom GPA last and cummulative

#Grad
if(nrow(PeakGradWeek13)>1){
  PeakGradWeek13$gpa.cummulative.prom <- round( (PeakGradWeek13$cummulative.gpa.last+PeakGradWeek13$academic.period.gpa.last)/2 , 2)
  PeakGradWeek13$credits.cummulative.prom <- round( (PeakGradWeek13$cummulative.credits.earned.last+PeakGradWeek13$cummulative.credits.attempted.last)/2 ,2)
  PeakGradWeek13$cummulative.credits.aggregation <- round( PeakGradWeek13$credits.cummulative.prom/PeakGradWeek13$gpa.cummulative.prom ,2)
  drop.colums <- c("gpa.cummulative.prom",
                  "credits.cummulative.prom",
                  "cummulative.gpa.last",
                  "academic.period.gpa.last",
                  "cummulative.credits.earned.last",
                  "cummulative.credits.attempted.last")
  PeakGradWeek13 <- PeakGradWeek13[,!( colnames(PeakGradWeek13)%in%drop.colums )]
}

if(nrow(PeakGradWeek9)>1){
  PeakGradWeek9$gpa.cummulative.prom <- round( (PeakGradWeek9$cummulative.gpa.last+PeakGradWeek9$academic.period.gpa.last)/2 , 2)
  PeakGradWeek9$credits.cummulative.prom <- round( (PeakGradWeek9$cummulative.credits.earned.last+PeakGradWeek9$cummulative.credits.attempted.last)/2 ,2)
  PeakGradWeek9$cummulative.credits.aggregation <- round( PeakGradWeek9$credits.cummulative.prom/PeakGradWeek9$gpa.cummulative.prom ,2)
  drop.colums <- c("gpa.cummulative.prom",
                  "credits.cummulative.prom",
                  "cummulative.gpa.last",
                  "academic.period.gpa.last",
                  "cummulative.credits.earned.last",
                  "cummulative.credits.attempted.last")
  PeakGradWeek9 <- PeakGradWeek9[,!( colnames(PeakGradWeek9)%in%drop.colums )]
}

if(nrow(PeakGradWeek5)>1){
  PeakGradWeek5$gpa.cummulative.prom <- round( (PeakGradWeek5$cummulative.gpa.last+PeakGradWeek5$academic.period.gpa.last)/2 , 2)
  PeakGradWeek5$credits.cummulative.prom <- round( (PeakGradWeek5$cummulative.credits.earned.last+PeakGradWeek5$cummulative.credits.attempted.last)/2 ,2)
  PeakGradWeek5$cummulative.credits.aggregation <- round( PeakGradWeek5$credits.cummulative.prom/PeakGradWeek5$gpa.cummulative.prom ,2)
  drop.colums <- c("gpa.cummulative.prom",
                  "credits.cummulative.prom",
                  "cummulative.gpa.last",
                  "academic.period.gpa.last",
                  "cummulative.credits.earned.last",
                  "cummulative.credits.attempted.last")
  PeakGradWeek5 <- PeakGradWeek5[,!( colnames(PeakGradWeek5)%in%drop.colums )]
}

if(nrow(PeakGrad)>1){
  PeakGrad$gpa.cummulative.prom <- round( (PeakGrad$cummulative.gpa.last+PeakGrad$academic.period.gpa.last)/2 , 2)
  PeakGrad$credits.cummulative.prom <- round( (PeakGrad$cummulative.credits.earned.last+PeakGrad$cummulative.credits.attempted.last)/2 ,2)
  PeakGrad$cummulative.credits.aggregation <- round( PeakGrad$credits.cummulative.prom/PeakGrad$gpa.cummulative.prom ,2)
  drop.colums <- c("gpa.cummulative.prom",
                  "credits.cummulative.prom",
                  "cummulative.gpa.last",
                  "academic.period.gpa.last",
                  "cummulative.credits.earned.last",
                  "cummulative.credits.attempted.last")
  PeakGrad <- PeakGrad[,!( colnames(PeakGrad)%in%drop.colums )]
}


#PosGrad
if(nrow(PeakPosGradWeek13)>1){
  PeakPosGradWeek13$gpa.cummulative.prom <- round( (PeakPosGradWeek13$cummulative.gpa.last+PeakPosGradWeek13$academic.period.gpa.last)/2 , 2)
  PeakPosGradWeek13$credits.cummulative.prom <- round( (PeakPosGradWeek13$cummulative.credits.earned.last+PeakPosGradWeek13$cummulative.credits.attempted.last)/2 ,2)
  PeakPosGradWeek13$cummulative.credits.aggregation <- round( PeakPosGradWeek13$credits.cummulative.prom/PeakPosGradWeek13$gpa.cummulative.prom ,2)
  drop.colums <- c("gpa.cummulative.prom",
                  "credits.cummulative.prom",
                  "cummulative.gpa.last",
                  "academic.period.gpa.last",
                  "cummulative.credits.earned.last",
                  "cummulative.credits.attempted.last")
  PeakPosGradWeek13 <- PeakPosGradWeek13[,!( colnames(PeakPosGradWeek13)%in%drop.colums )]
}

if(nrow(PeakPosGradWeek9)>1){
  PeakPosGradWeek9$gpa.cummulative.prom <- round( (PeakPosGradWeek9$cummulative.gpa.last+PeakPosGradWeek9$academic.period.gpa.last)/2 , 2)
  PeakPosGradWeek9$credits.cummulative.prom <- round( (PeakPosGradWeek9$cummulative.credits.earned.last+PeakPosGradWeek9$cummulative.credits.attempted.last)/2 ,2)
  PeakPosGradWeek9$cummulative.credits.aggregation <- round( PeakPosGradWeek9$credits.cummulative.prom/PeakPosGradWeek9$gpa.cummulative.prom ,2)
  drop.colums <- c("gpa.cummulative.prom",
                  "credits.cummulative.prom",
                  "cummulative.gpa.last",
                  "academic.period.gpa.last",
                  "cummulative.credits.earned.last",
                  "cummulative.credits.attempted.last")
  PeakPosGradWeek9 <- PeakPosGradWeek9[,!( colnames(PeakPosGradWeek9)%in%drop.colums )]
}

if(nrow(PeakPosGradWeek5)>1){
  PeakPosGradWeek5$gpa.cummulative.prom <- round( (PeakPosGradWeek5$cummulative.gpa.last+PeakPosGradWeek5$academic.period.gpa.last)/2 , 2)
  PeakPosGradWeek5$credits.cummulative.prom <- round( (PeakPosGradWeek5$cummulative.credits.earned.last+PeakPosGradWeek5$cummulative.credits.attempted.last)/2 ,2)
  PeakPosGradWeek5$cummulative.credits.aggregation <- round( PeakPosGradWeek5$credits.cummulative.prom/PeakPosGradWeek5$gpa.cummulative.prom ,2)
  drop.colums <- c("gpa.cummulative.prom",
                  "credits.cummulative.prom",
                  "cummulative.gpa.last",
                  "academic.period.gpa.last",
                  "cummulative.credits.earned.last",
                  "cummulative.credits.attempted.last")
  PeakPosGradWeek5 <- PeakPosGradWeek5[,!( colnames(PeakPosGradWeek5)%in%drop.colums )]
}

if(nrow(PeakPosGrad)>1){
  PeakPosGrad$gpa.cummulative.prom <- round( (PeakPosGrad$cummulative.gpa.last+PeakPosGrad$academic.period.gpa.last)/2 , 2)
  PeakPosGrad$credits.cummulative.prom <- round( (PeakPosGrad$cummulative.credits.earned.last+PeakPosGrad$cummulative.credits.attempted.last)/2 ,2)
  PeakPosGrad$cummulative.credits.aggregation <- round( PeakPosGrad$credits.cummulative.prom/PeakPosGrad$gpa.cummulative.prom ,2)
  drop.colums <- c("gpa.cummulative.prom",
                  "credits.cummulative.prom",
                  "cummulative.gpa.last",
                  "academic.period.gpa.last",
                  "cummulative.credits.earned.last",
                  "cummulative.credits.attempted.last")
  PeakPosGrad <- PeakPosGrad[,!( colnames(PeakPosGrad)%in%drop.colums )]
}

#Tec
if(nrow(PeakTecWeek13)>1){
  PeakTecWeek13$gpa.cummulative.prom <- round( (PeakTecWeek13$cummulative.gpa.last+PeakTecWeek13$academic.period.gpa.last)/2 , 2)
  PeakTecWeek13$credits.cummulative.prom <- round( (PeakTecWeek13$cummulative.credits.earned.last+PeakTecWeek13$cummulative.credits.attempted.last)/2 ,2)
  PeakTecWeek13$cummulative.credits.aggregation <- round( PeakTecWeek13$credits.cummulative.prom/PeakTecWeek13$gpa.cummulative.prom ,2)
  drop.colums <- c("gpa.cummulative.prom",
                  "credits.cummulative.prom",
                  "cummulative.gpa.last",
                  "academic.period.gpa.last",
                  "cummulative.credits.earned.last",
                  "cummulative.credits.attempted.last")
  PeakTecWeek13 <- PeakTecWeek13[,!( colnames(PeakTecWeek13)%in%drop.colums )]
}

if(nrow(PeakTecWeek9)>1){
  PeakTecWeek9$gpa.cummulative.prom <- round( (PeakTecWeek9$cummulative.gpa.last+PeakTecWeek9$academic.period.gpa.last)/2 , 2)
  PeakTecWeek9$credits.cummulative.prom <- round( (PeakTecWeek9$cummulative.credits.earned.last+PeakTecWeek9$cummulative.credits.attempted.last)/2 ,2)
  PeakTecWeek9$cummulative.credits.aggregation <- round( PeakTecWeek9$credits.cummulative.prom/PeakTecWeek9$gpa.cummulative.prom ,2)
  drop.colums <- c("gpa.cummulative.prom",
                  "credits.cummulative.prom",
                  "cummulative.gpa.last",
                  "academic.period.gpa.last",
                  "cummulative.credits.earned.last",
                  "cummulative.credits.attempted.last")
  PeakTecWeek9 <- PeakTecWeek9[,!( colnames(PeakTecWeek9)%in%drop.colums )]
}

if(nrow(PeakTecWeek5)>1){
  PeakTecWeek5$gpa.cummulative.prom <- round( (PeakTecWeek5$cummulative.gpa.last+PeakTecWeek5$academic.period.gpa.last)/2 , 2)
  PeakTecWeek5$credits.cummulative.prom <- round( (PeakTecWeek5$cummulative.credits.earned.last+PeakTecWeek5$cummulative.credits.attempted.last)/2 ,2)
  PeakTecWeek5$cummulative.credits.aggregation <- round( PeakTecWeek5$credits.cummulative.prom/PeakTecWeek5$gpa.cummulative.prom ,2)
  drop.colums <- c("gpa.cummulative.prom",
                  "credits.cummulative.prom",
                  "cummulative.gpa.last",
                  "academic.period.gpa.last",
                  "cummulative.credits.earned.last",
                  "cummulative.credits.attempted.last")
  PeakTecWeek5 <- PeakTecWeek5[,!( colnames(PeakTecWeek5)%in%drop.colums )]
}

if(nrow(PeakTec)>1){
  PeakTec$gpa.cummulative.prom <- round( (PeakTec$cummulative.gpa.last+PeakTec$academic.period.gpa.last)/2 , 2)
  PeakTec$credits.cummulative.prom <- round( (PeakTec$cummulative.credits.earned.last+PeakTec$cummulative.credits.attempted.last)/2 ,2)
  PeakTec$cummulative.credits.aggregation <- round( PeakTec$credits.cummulative.prom/PeakTec$gpa.cummulative.prom ,2)
  drop.colums <- c("gpa.cummulative.prom",
                  "credits.cummulative.prom",
                  "cummulative.gpa.last",
                  "academic.period.gpa.last",
                  "cummulative.credits.earned.last",
                  "cummulative.credits.attempted.last")
  PeakTec <- PeakTec[,!( colnames(PeakTec)%in%drop.colums )]
}

###NANs

print("NaNs treatment")

if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13[PeakGradWeek13$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0
if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9[PeakGradWeek9$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0
if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5[PeakGradWeek5$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0
if(nrow(PeakGrad)>1)
  PeakGrad[PeakGrad$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0

if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13[PeakPosGradWeek13$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0
if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9[PeakPosGradWeek9$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0
if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5[PeakPosGradWeek5$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0
if(nrow(PeakPosGrad)>1)
  PeakPosGrad[PeakPosGrad$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0

if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13[PeakTecWeek13$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0
if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9[PeakTecWeek9$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0
if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5[PeakTecWeek5$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0
if(nrow(PeakTec)>1)
  PeakTec[PeakTec$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0

###NANs

print("NaNs treatment")

if(nrow(BioGrad)>1)
  BioGrad[is.na(BioGrad)] <- 0
if(nrow(BioGradWeek5)>1)
  BioGradWeek5[is.na(BioGradWeek5)] <- 0
if(nrow(BioGradWeek9)>1)
  BioGradWeek9[is.na(BioGradWeek9)] <- 0
if(nrow(BioGradWeek13)>1)
  BioGradWeek13[is.na(BioGradWeek13)] <- 0
if(nrow(BioGradFin)>1)
  BioGradFin[is.na(BioGradFin)] <- 0
if(nrow(PeakGrad)>1)
  PeakGrad[is.na(PeakGrad)] <- 0
if(nrow(PeakGradWeek5)>1)
  PeakGradWeek5[is.na(PeakGradWeek5)] <- 0
if(nrow(PeakGradWeek9)>1)
  PeakGradWeek9[is.na(PeakGradWeek9)] <- 0
if(nrow(PeakGradWeek13)>1)
  PeakGradWeek13[is.na(PeakGradWeek13)] <- 0
if(nrow(PeakGradFin)>1)
  PeakGradFin[is.na(PeakGradFin)] <- 0

if(nrow(BioPosGrad)>1)
  BioPosGrad[is.na(BioPosGrad)] <- 0
if(nrow(BioPosGradWeek5)>1)
  BioPosGradWeek5[is.na(BioPosGradWeek5)] <- 0
if(nrow(BioPosGradWeek9)>1)
  BioPosGradWeek9[is.na(BioPosGradWeek9)] <- 0
if(nrow(BioPosGradWeek13)>1)
  BioPosGradWeek13[is.na(BioPosGradWeek13)] <- 0
if(nrow(BioPosGradFin)>1)
  BioPosGradFin[is.na(BioPosGradFin)] <- 0
if(nrow(PeakPosGrad)>1)
  PeakPosGrad[is.na(PeakPosGrad)] <- 0
if(nrow(PeakPosGradWeek5)>1)
  PeakPosGradWeek5[is.na(PeakPosGradWeek5)] <- 0
if(nrow(PeakPosGradWeek9)>1)
  PeakPosGradWeek9[is.na(PeakPosGradWeek9)] <- 0
if(nrow(PeakPosGradWeek13)>1)
  PeakPosGradWeek13[is.na(PeakPosGradWeek13)] <- 0
if(nrow(PeakPosGradFin)>1)
  PeakPosGradFin[is.na(PeakPosGradFin)] <- 0

if(nrow(BioTec)>1)
  BioTec[is.na(BioTec)] <- 0
if(nrow(BioTecWeek5)>1)
  BioTecWeek5[is.na(BioTecWeek5)] <- 0
if(nrow(BioTecWeek9)>1)
  BioTecWeek9[is.na(BioTecWeek9)] <- 0
if(nrow(BioTecWeek13)>1)
  BioTecWeek13[is.na(BioTecWeek13)] <- 0
if(nrow(BioTecFin)>1)
  BioTecFin[is.na(BioTecFin)] <- 0
if(nrow(PeakTec)>1)
  PeakTec[is.na(PeakTec)] <- 0
if(nrow(PeakTecWeek5)>1)
  PeakTecWeek5[is.na(PeakTecWeek5)] <- 0
if(nrow(PeakTecWeek9)>1)
  PeakTecWeek9[is.na(PeakTecWeek9)] <- 0
if(nrow(PeakTecWeek13)>1)
  PeakTecWeek13[is.na(PeakTecWeek13)] <- 0
if(nrow(PeakTecFin)>1)
  PeakTecFin[is.na(PeakTecFin)] <- 0

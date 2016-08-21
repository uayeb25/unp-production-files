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

BioGrad$city.aggregation <- mapply(CityTransformation,BioGrad$city)
BioGrad <- BioGrad[,!( names(BioGrad) %in% c("city") )]

BioGradWeek5$city.aggregation <- mapply(CityTransformation,BioGradWeek5$city)
BioGradWeek5 <- BioGradWeek5[,!( names(BioGradWeek5) %in% c("city") )]

BioGradWeek9$city.aggregation <- mapply(CityTransformation,BioGradWeek9$city)
BioGradWeek9 <- BioGradWeek9[,!( names(BioGradWeek9) %in% c("city") )]

BioGradWeek13$city.aggregation <- mapply(CityTransformation,BioGradWeek13$city)
BioGradWeek13 <- BioGradWeek13[,!( names(BioGradWeek13) %in% c("city") )]

BioGradFin$city.aggregation <- mapply(CityTransformation,BioGradFin$city)
BioGradFin <- BioGradFin[,!( names(BioGradFin) %in% c("city") )]


print("Grad veteran students have the same city, we are going to drop this attr from the datasets. if you are sending real information please contact to uayeb.caballero@laureate.net to do aggreation in this")
PeakGrad <- PeakGrad[,!( names(PeakGrad) %in% c("city") )]
PeakGradWeek5 <- PeakGradWeek5[,!( names(PeakGradWeek5) %in% c("city") )]
PeakGradWeek9 <- PeakGradWeek9[,!( names(PeakGradWeek9) %in% c("city") )]
PeakGradWeek13 <- PeakGradWeek13[,!( names(PeakGradWeek13) %in% c("city") )]
PeakGradFin <- PeakGradFin[,!( names(PeakGradFin) %in% c("city") )]

### TEc ###

print("city aggregation to Tec novo students")

BioTec$city.aggregation <- mapply(CityTransformation,BioTec$city)
BioTec <- BioTec[,!( names(BioTec) %in% c("city") )]

BioTecWeek5$city.aggregation <- mapply(CityTransformation,BioTecWeek5$city)
BioTecWeek5 <- BioTecWeek5[,!( names(BioTecWeek5) %in% c("city") )]

BioTecWeek9$city.aggregation <- mapply(CityTransformation,BioTecWeek9$city)
BioTecWeek9 <- BioTecWeek9[,!( names(BioTecWeek9) %in% c("city") )]

BioTecWeek13$city.aggregation <- mapply(CityTransformation,BioTecWeek13$city)
BioTecWeek13 <- BioTecWeek13[,!( names(BioTecWeek13) %in% c("city") )]

BioTecFin$city.aggregation <- mapply(CityTransformation,BioTecFin$city)
BioTecFin <- BioTecFin[,!( names(BioTecFin) %in% c("city") )]

print("Tec veteran students have the same city, we are going to drop this attr from the datasets. if you are sending real information please contact to uayeb.caballero@laureate.net to do aggreation in this")
PeakTec <- PeakTec[,!( names(PeakTec) %in% c("city") )]
PeakTecWeek5 <- PeakTecWeek5[,!( names(PeakTecWeek5) %in% c("city") )]
PeakTecWeek9 <- PeakTecWeek9[,!( names(PeakTecWeek9) %in% c("city") )]
PeakTecWeek13 <- PeakTecWeek13[,!( names(PeakTecWeek13) %in% c("city") )]
PeakTecFin <- PeakTecFin[,!( names(PeakTecFin) %in% c("city") )]


### POSGRAD ###

print("city aggregation to PosGrad novo students")

BioPosGrad$city.aggregation <- mapply(CityTransformation,BioPosGrad$city)
BioPosGrad <- BioPosGrad[,!( names(BioPosGrad) %in% c("city") )]

BioPosGradWeek5$city.aggregation <- mapply(CityTransformation,BioPosGradWeek5$city)
BioPosGradWeek5 <- BioPosGradWeek5[,!( names(BioPosGradWeek5) %in% c("city") )]

BioPosGradWeek9$city.aggregation <- mapply(CityTransformation,BioPosGradWeek9$city)
BioPosGradWeek9 <- BioPosGradWeek9[,!( names(BioPosGradWeek9) %in% c("city") )]

BioPosGradWeek13$city.aggregation <- mapply(CityTransformation,BioPosGradWeek13$city)
BioPosGradWeek13 <- BioPosGradWeek13[,!( names(BioPosGradWeek13) %in% c("city") )]

BioPosGradFin$city.aggregation <- mapply(CityTransformation,BioPosGradFin$city)
BioPosGradFin <- BioPosGradFin[,!( names(BioPosGradFin) %in% c("city") )]


print("PosGrad veteran students have the same city, we are going to drop this attr from the datasets. if you are sending real information please contact to uayeb.caballero@laureate.net to do aggreation in this")
PeakPosGrad <- PeakPosGrad[,!( names(PeakPosGrad) %in% c("city") )]
PeakPosGradWeek5 <- PeakPosGradWeek5[,!( names(PeakPosGradWeek5) %in% c("city") )]
PeakPosGradWeek9 <- PeakPosGradWeek9[,!( names(PeakPosGradWeek9) %in% c("city") )]
PeakPosGradWeek13 <- PeakPosGradWeek13[,!( names(PeakPosGradWeek13) %in% c("city") )]
PeakPosGradFin <- PeakPosGradFin[,!( names(PeakPosGradFin) %in% c("city") )]


######## state transformations
#
###########################


### POSTGRAD ###

print("state aggregation to posgrad novo students")

BioPosGrad$state.aggregation <- mapply(StateTransformation,BioPosGrad$state)
BioPosGrad <- BioPosGrad[,!( names(BioPosGrad) %in% c("state") )]

BioPosGradWeek5$state.aggregation <- mapply(StateTransformation,BioPosGradWeek5$state)
BioPosGradWeek5 <- BioPosGradWeek5[,!( names(BioPosGradWeek5) %in% c("state") )]

BioPosGradWeek9$state.aggregation <- mapply(StateTransformation,BioPosGradWeek9$state)
BioPosGradWeek9 <- BioPosGradWeek9[,!( names(BioPosGradWeek9) %in% c("state") )]

BioPosGradWeek13$state.aggregation <- mapply(StateTransformation,BioPosGradWeek13$state)
BioPosGradWeek13 <- BioPosGradWeek13[,!( names(BioPosGradWeek13) %in% c("state") )]

BioPosGradFin$state.aggregation <- mapply(StateTransformation,BioPosGradFin$state)
BioPosGradFin <- BioPosGradFin[,!( names(BioPosGradFin) %in% c("state") )]


print("PosGrad veteran students have the same state, we are going to drop this attr from the datasets. if you are sending real information please contact to uayeb.caballero@laureate.net to do aggreation in this")
PeakPosGrad <- PeakPosGrad[,!( names(PeakPosGrad) %in% c("state") )]
PeakPosGradWeek5 <- PeakPosGradWeek5[,!( names(PeakPosGradWeek5) %in% c("state") )]
PeakPosGradWeek9 <- PeakPosGradWeek9[,!( names(PeakPosGradWeek9) %in% c("state") )]
PeakPosGradWeek13 <- PeakPosGradWeek13[,!( names(PeakPosGradWeek13) %in% c("state") )]
PeakPosGradFin <- PeakPosGradFin[,!( names(PeakPosGradFin) %in% c("state") )]

### GRAD ###

print("state aggregation to posgrad novo students")

BioGrad$state.aggregation <- mapply(StateTransformation,BioGrad$state)
BioGrad <- BioGrad[,!( names(BioGrad) %in% c("state") )]

BioGradWeek5$state.aggregation <- mapply(StateTransformation,BioGradWeek5$state)
BioGradWeek5 <- BioGradWeek5[,!( names(BioGradWeek5) %in% c("state") )]

BioGradWeek9$state.aggregation <- mapply(StateTransformation,BioGradWeek9$state)
BioGradWeek9 <- BioGradWeek9[,!( names(BioGradWeek9) %in% c("state") )]

BioGradWeek13$state.aggregation <- mapply(StateTransformation,BioGradWeek13$state)
BioGradWeek13 <- BioGradWeek13[,!( names(BioGradWeek13) %in% c("state") )]

BioGradFin$state.aggregation <- mapply(StateTransformation,BioGradFin$state)
BioGradFin <- BioGradFin[,!( names(BioGradFin) %in% c("state") )]

print("Grad veteran students have the same state, we are going to drop this attr from the datasets. if you are sending real information please contact to uayeb.caballero@laureate.net to do aggreation in this")
PeakGrad <- PeakGrad[,!( names(PeakGrad) %in% c("state") )]
PeakGradWeek5 <- PeakGradWeek5[,!( names(PeakGradWeek5) %in% c("state") )]
PeakGradWeek9 <- PeakGradWeek9[,!( names(PeakGradWeek9) %in% c("state") )]
PeakGradWeek13 <- PeakGradWeek13[,!( names(PeakGradWeek13) %in% c("state") )]
PeakGradFin <- PeakGradFin[,!( names(PeakGradFin) %in% c("state") )]

### TEC ###

print("state aggregation to tec novo students")

BioTec$state.aggregation <- mapply(StateTransformation,BioTec$state)
BioTec <- BioTec[,!( names(BioTec) %in% c("state") )]

BioTecWeek5$state.aggregation <- mapply(StateTransformation,BioTecWeek5$state)
BioTecWeek5 <- BioTecWeek5[,!( names(BioTecWeek5) %in% c("state") )]

BioTecWeek9$state.aggregation <- mapply(StateTransformation,BioTecWeek9$state)
BioTecWeek9 <- BioTecWeek9[,!( names(BioTecWeek9) %in% c("state") )]

BioTecWeek13$state.aggregation <- mapply(StateTransformation,BioTecWeek13$state)
BioTecWeek13 <- BioTecWeek13[,!( names(BioTecWeek13) %in% c("state") )]

BioTecFin$state.aggregation <- mapply(StateTransformation,BioTecFin$state)
BioTecFin <- BioTecFin[,!( names(BioTecFin) %in% c("state") )]

print("Tec veteran students have the same state, we are going to drop this attr from the datasets. if you are sending real information please contact to uayeb.caballero@laureate.net to do aggreation in this")
PeakTec <- PeakTec[,!( names(PeakTec) %in% c("state") )]
PeakTecWeek5 <- PeakTecWeek5[,!( names(PeakTecWeek5) %in% c("state") )]
PeakTecWeek9 <- PeakTecWeek9[,!( names(PeakTecWeek9) %in% c("state") )]
PeakTecWeek13 <- PeakTecWeek13[,!( names(PeakTecWeek13) %in% c("state") )]
PeakTecFin <- PeakTecFin[,!( names(PeakTecFin) %in% c("state") )]



######## high school transformations
#
###########################

print("high school aggregation to grad novo students")

BioGrad$high.school.agreggation <- mapply(HighSchoolTransformation,BioGrad$high.school)
BioGrad <- BioGrad[,!( names(BioGrad) %in% c("high.school") )]

BioGradWeek5$high.school.agreggation <- mapply(HighSchoolTransformation,BioGradWeek5$high.school)
BioGradWeek5 <- BioGradWeek5[,!( names(BioGradWeek5) %in% c("high.school") )]

BioGradWeek9$high.school.agreggation <- mapply(HighSchoolTransformation,BioGradWeek9$high.school)
BioGradWeek9 <- BioGradWeek9[,!( names(BioGradWeek9) %in% c("high.school") )]

BioGradWeek13$high.school.agreggation <- mapply(HighSchoolTransformation,BioGradWeek13$high.school)
BioGradWeek13 <- BioGradWeek13[,!( names(BioGradWeek13) %in% c("high.school") )]

BioGradFin$high.school.agreggation <- mapply(HighSchoolTransformation,BioGradFin$high.school)
BioGradFin <- BioGradFin[,!( names(BioGradFin) %in% c("high.school") )]

print("high school aggregation to Tec novo students")

BioTec$high.school.agreggation <- mapply(HighSchoolTransformation,BioTec$high.school)
BioTec <- BioTec[,!( names(BioTec) %in% c("high.school") )]

BioTecWeek5$high.school.agreggation <- mapply(HighSchoolTransformation,BioTecWeek5$high.school)
BioTecWeek5 <- BioTecWeek5[,!( names(BioTecWeek5) %in% c("high.school") )]

BioTecWeek9$high.school.agreggation <- mapply(HighSchoolTransformation,BioTecWeek9$high.school)
BioTecWeek9 <- BioTecWeek9[,!( names(BioTecWeek9) %in% c("high.school") )]

BioTecWeek13$high.school.agreggation <- mapply(HighSchoolTransformation,BioTecWeek13$high.school)
BioTecWeek13 <- BioTecWeek13[,!( names(BioTecWeek13) %in% c("high.school") )]

BioTecFin$high.school.agreggation <- mapply(HighSchoolTransformation,BioTecFin$high.school)
BioTecFin <- BioTecFin[,!( names(BioTecFin) %in% c("high.school") )]

print("high school aggregation to PosGrad novo students")

BioPosGrad$high.school.agreggation <- mapply(HighSchoolToPosGradTransformation,BioPosGrad$high.school)
BioPosGrad <- BioPosGrad[,!( names(BioPosGrad) %in% c("high.school") )]

BioPosGradWeek5$high.school.agreggation <- mapply(HighSchoolToPosGradTransformation,BioPosGradWeek5$high.school)
BioPosGradWeek5 <- BioPosGradWeek5[,!( names(BioPosGradWeek5) %in% c("high.school") )]

BioPosGradWeek9$high.school.agreggation <- mapply(HighSchoolToPosGradTransformation,BioPosGradWeek9$high.school)
BioPosGradWeek9 <- BioPosGradWeek9[,!( names(BioPosGradWeek9) %in% c("high.school") )]

BioPosGradWeek13$high.school.agreggation <- mapply(HighSchoolToPosGradTransformation,BioPosGradWeek13$high.school)
BioPosGradWeek13 <- BioPosGradWeek13[,!( names(BioPosGradWeek13) %in% c("high.school") )]

BioPosGradFin$high.school.agreggation <- mapply(HighSchoolToPosGradTransformation,BioPosGradFin$high.school)
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

BioGrad <- BioGrad[ !(BioGrad$forma.ingresso %in% drop.rows) ,]
BioGradWeek5 <- BioGradWeek5[ !(BioGradWeek5$forma.ingresso %in% drop.rows) ,]
BioGradWeek9 <- BioGradWeek9[ !(BioGradWeek9$forma.ingresso %in% drop.rows) ,]
BioGradWeek13 <- BioGradWeek13[ !(BioGradWeek13$forma.ingresso %in% drop.rows) ,]
BioGradFin <- BioGradFin[ !(BioGradFin$forma.ingresso %in% drop.rows) ,]

BioTec <- BioTec[ !(BioTec$forma.ingresso %in% drop.rows) ,]
BioTecWeek5 <- BioTecWeek5[ !(BioTecWeek5$forma.ingresso %in% drop.rows) ,]
BioTecWeek9 <- BioTecWeek9[ !(BioTecWeek9$forma.ingresso %in% drop.rows) ,]
BioTecWeek13 <- BioTecWeek13[ !(BioTecWeek13$forma.ingresso %in% drop.rows) ,]
BioTecFin <- BioTecFin[ !(BioTecFin$forma.ingresso %in% drop.rows) ,]

print("forma ingresso aggregation to grad novo students")

BioGrad$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioGrad$forma.ingresso)
BioGrad <- BioGrad[,!( names(BioGrad) %in% c("forma.ingresso") )]

BioGradWeek5$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioGradWeek5$forma.ingresso)
BioGradWeek5 <- BioGradWeek5[,!( names(BioGradWeek5) %in% c("forma.ingresso") )]

BioGradWeek9$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioGradWeek9$forma.ingresso)
BioGradWeek9 <- BioGradWeek9[,!( names(BioGradWeek9) %in% c("forma.ingresso") )]

BioGradWeek13$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioGradWeek13$forma.ingresso)
BioGradWeek13 <- BioGradWeek13[,!( names(BioGradWeek13) %in% c("forma.ingresso") )]

BioGradFin$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioGradFin$forma.ingresso)
BioGradFin <- BioGradFin[,!( names(BioGradFin) %in% c("forma.ingresso") )]

print("forma ingresso aggregation to Tec novo students")

BioTec$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioTec$forma.ingresso)
BioTec <- BioTec[,!( names(BioTec) %in% c("forma.ingresso") )]

BioTecWeek5$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioTecWeek5$forma.ingresso)
BioTecWeek5 <- BioTecWeek5[,!( names(BioTecWeek5) %in% c("forma.ingresso") )]

BioTecWeek9$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioTecWeek9$forma.ingresso)
BioTecWeek9 <- BioTecWeek9[,!( names(BioTecWeek9) %in% c("forma.ingresso") )]

BioTecWeek13$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioTecWeek13$forma.ingresso)
BioTecWeek13 <- BioTecWeek13[,!( names(BioTecWeek13) %in% c("forma.ingresso") )]

BioTecFin$forma.ingresso.agreggation <- mapply(FormaIngressoTransformation,BioTecFin$forma.ingresso)
BioTecFin <- BioTecFin[,!( names(BioTecFin) %in% c("forma.ingresso") )]

######## Forma Ingresso
#
###########################

print("drop 2015_1 to Grad")

BioGrad <- BioGrad[ !( BioGrad$academic.cycle %in% c("2015.1") ),]
BioGradWeek5 <- BioGradWeek5[ !( BioGradWeek5$academic.cycle %in% c("2015.1") ),]
BioGradWeek9 <- BioGradWeek9[ !( BioGradWeek9$academic.cycle %in% c("2015.1") ),]
BioGradWeek13 <- BioGradWeek13[ !(BioGradWeek13$academic.cycle %in% c("2015.1") ),]
BioGradFin <- BioGradFin[ !( BioGradFin$academic.cycle %in% c("2015.1") ),]

PeakGrad <- PeakGrad[ !( PeakGrad$academic.cycle %in% c("2015.1") ),]
PeakGradWeek5 <- PeakGradWeek5[ !( PeakGradWeek5$academic.cycle %in% c("2015.1") ),]
PeakGradWeek9 <- PeakGradWeek9[ !( PeakGradWeek9$academic.cycle %in% c("2015.1") ),]
PeakGradWeek13 <- PeakGradWeek13[ !( PeakGradWeek13$academic.cycle %in% c("2015.1") ),]
PeakGradFin <- PeakGradFin[ !( PeakGradFin$academic.cycle %in% c("2015.1") ),]

print("drop 2015_1 to Tec")

BioPosGrad <- BioPosGrad[ !( BioPosGrad$academic.cycle %in% c("2015.1") ),]
BioPosGradWeek5 <- BioPosGradWeek5[ !( BioPosGradWeek5$academic.cycle %in% c("2015.1") ),]
BioPosGradWeek9 <- BioPosGradWeek9[ !( BioPosGradWeek9$academic.cycle %in% c("2015.1") ),]
BioPosGradWeek13 <- BioPosGradWeek13[ !( BioPosGradWeek13$academic.cycle %in% c("2015.1") ),]
BioPosGradFin <- BioPosGradFin[ !( BioPosGradFin$academic.cycle %in% c("2015.1") ),]

PeakPosGrad <- PeakPosGrad[ !( PeakPosGrad$academic.cycle %in% c("2015.1") ),]
PeakPosGradWeek5 <- PeakPosGradWeek5[ !( PeakPosGradWeek5$academic.cycle %in% c("2015.1") ),]
PeakPosGradWeek9 <- PeakPosGradWeek9[ !( PeakPosGradWeek9$academic.cycle %in% c("2015.1") ),]
PeakPosGradWeek13 <- PeakPosGradWeek13[ !( PeakPosGradWeek13$academic.cycle %in% c("2015.1") ),]
PeakPosGradFin <- PeakPosGradFin[ !( PeakPosGradFin$academic.cycle %in% c("2015.1") ),]

print("drop 2015_1 to PosGrad")

BioTec <- BioTec[ !( BioTec$academic.cycle %in% c("2015.1") ),]
BioTecWeek5 <- BioTecWeek5[ !( BioTecWeek5$academic.cycle %in% c("2015.1") ),]
BioTecWeek9 <- BioTecWeek9[ !( BioTecWeek9$academic.cycle %in% c("2015.1") ),]
BioTecWeek13 <- BioTecWeek13[ !( BioTecWeek13$academic.cycle %in% c("2015.1") ),]
BioTecFin <- BioTecFin[ !( BioTecFin$academic.cycle %in% c("2015.1") ),]

PeakTec <- PeakTec[ !( PeakTec$academic.cycle %in% c("2015.1") ),]
PeakTecWeek5 <- PeakTecWeek5[ !( PeakTecWeek5$academic.cycle %in% c("2015.1") ),]
PeakTecWeek9 <- PeakTecWeek9[ !( PeakTecWeek9$academic.cycle %in% c("2015.1") ),]
PeakTecWeek13 <- PeakTecWeek13[ !( PeakTecWeek13$academic.cycle %in% c("2015.1") ),]
PeakTecFin <- PeakTecFin[ !( PeakTecFin$academic.cycle %in% c("2015.1") ),]

######## Program
#
###########################

print("Dropping program for grad nova students")

BioGrad <- BioGrad[, !( names(BioGrad) %in% c("program.of.study") )  ]
BioGradWeek5 <- BioGradWeek5[, !( names(BioGradWeek5) %in% c("program.of.study") )  ]
BioGradWeek9 <- BioGradWeek9[, !( names(BioGradWeek9) %in% c("program.of.study") )  ]
BioGradWeek13 <- BioGradWeek13[, !( names(BioGradWeek13) %in% c("program.of.study") )  ]
BioGradFin <- BioGradFin[, !( names(BioGradFin) %in% c("program.of.study") )  ]

print("Dropping program for grad veteran students")

PeakGrad <- PeakGrad[, !(names(PeakGrad) %in% c("program.of.study") ) ]
PeakGradWeek5 <- PeakGradWeek5[, !(names(PeakGradWeek5) %in% c("program.of.study") ) ]
PeakGradWeek9 <- PeakGradWeek9[, !(names(PeakGradWeek9) %in% c("program.of.study") ) ]
PeakGradWeek13 <- PeakGradWeek13[, !(names(PeakGradWeek13) %in% c("program.of.study") ) ]
PeakGradFin <- PeakGradFin[, !(names(PeakGradFin) %in% c("program.of.study") ) ]

print("Dropping program for Tec nova students")

BioTec <- BioTec[, !( names(BioTec) %in% c("program.of.study") )  ]
BioTecWeek5 <- BioTecWeek5[, !( names(BioTecWeek5) %in% c("program.of.study") )  ]
BioTecWeek9 <- BioTecWeek9[, !( names(BioTecWeek9) %in% c("program.of.study") )  ]
BioTecWeek13 <- BioTecWeek13[, !( names(BioTecWeek13) %in% c("program.of.study") )  ]
BioTecFin <- BioTecFin[, !( names(BioTecFin) %in% c("program.of.study") )  ]

print("Dropping program for Tec veteran students")

PeakTec <- PeakTec[, !(names(PeakTec) %in% c("program.of.study") ) ]
PeakTecWeek5 <- PeakTecWeek5[, !(names(PeakTecWeek5) %in% c("program.of.study") ) ]
PeakTecWeek9 <- PeakTecWeek9[, !(names(PeakTecWeek9) %in% c("program.of.study") ) ]
PeakTecWeek13 <- PeakTecWeek13[, !(names(PeakTecWeek13) %in% c("program.of.study") ) ]
PeakTecFin <- PeakTecFin[, !(names(PeakTecFin) %in% c("program.of.study") ) ]

print("Dropping program for PosGrad nova students")

BioPosGrad <- BioPosGrad[, !( names(BioPosGrad) %in% c("program.of.study") )  ]
BioPosGradWeek5 <- BioPosGradWeek5[, !( names(BioPosGradWeek5) %in% c("program.of.study") )  ]
BioPosGradWeek9 <- BioPosGradWeek9[, !( names(BioPosGradWeek9) %in% c("program.of.study") )  ]
BioPosGradWeek13 <- BioPosGradWeek13[, !( names(BioPosGradWeek13) %in% c("program.of.study") )  ]
BioPosGradFin <- BioPosGradFin[, !( names(BioPosGradFin) %in% c("program.of.study") )  ]

print("Dropping program for PosGrad veteran students")

PeakPosGrad <- PeakPosGrad[, !(names(PeakPosGrad) %in% c("program.of.study") ) ]
PeakPosGradWeek5 <- PeakPosGradWeek5[, !(names(PeakPosGradWeek5) %in% c("program.of.study") ) ]
PeakPosGradWeek9 <- PeakPosGradWeek9[, !(names(PeakPosGradWeek9) %in% c("program.of.study") ) ]
PeakPosGradWeek13 <- PeakPosGradWeek13[, !(names(PeakPosGradWeek13) %in% c("program.of.study") ) ]
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

BioGrad <- BioGrad[ !(trim(BioGrad$faculty)%in%drop.rows) ,]
BioGradWeek5 <- BioGradWeek5[ !(trim(BioGradWeek5$faculty)%in%drop.rows) ,]
BioGradWeek9 <- BioGradWeek9[ !(trim(BioGradWeek9$faculty)%in%drop.rows) ,]
BioGradWeek13 <- BioGradWeek13[ !(trim(BioGradWeek13$faculty)%in%drop.rows) ,]
BioGradFin <- BioGradFin[ !(trim(BioGradFin$faculty)%in%drop.rows) ,]

print("faculty aggregation to grad novo students")

BioGrad$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioGrad$faculty)
BioGrad <- BioGrad[,!( names(BioGrad) %in% c("faculty") )]

BioGradWeek5$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioGradWeek5$faculty)
BioGradWeek5 <- BioGradWeek5[,!( names(BioGradWeek5) %in% c("faculty") )]

BioGradWeek9$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioGradWeek9$faculty)
BioGradWeek9 <- BioGradWeek9[,!( names(BioGradWeek9) %in% c("faculty") )]

BioGradWeek13$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioGradWeek13$faculty)
BioGradWeek13 <- BioGradWeek13[,!( names(BioGradWeek13) %in% c("faculty") )]

BioGradFin$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioGradFin$faculty)
BioGradFin <- BioGradFin[,!( names(BioGradFin) %in% c("faculty") )]

print("Dropping some rows for Grad veteran students, dropping atypical values on faculty")

drop.rows <- c(
  "INSTITUCIONAL"
)

PeakGrad <- PeakGrad[ !(trim(PeakGrad$faculty)%in%drop.rows) ,]
PeakGradWeek5 <- PeakGradWeek5[ !(trim(PeakGradWeek5$faculty)%in%drop.rows) ,]
PeakGradWeek9 <- PeakGradWeek9[ !(trim(PeakGradWeek9$faculty)%in%drop.rows) ,]
PeakGradWeek13 <- PeakGradWeek13[ !(trim(PeakGradWeek13$faculty)%in%drop.rows) ,]
PeakGradFin <- PeakGradFin[ !(trim(PeakGradFin$faculty)%in%drop.rows) ,]

print("faculty aggregation to grad veteran students")

PeakGrad$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakGrad$faculty)
PeakGrad <- PeakGrad[,!( names(PeakGrad) %in% c("faculty") )]

PeakGradWeek5$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakGradWeek5$faculty)
PeakGradWeek5 <- PeakGradWeek5[,!( names(PeakGradWeek5) %in% c("faculty") )]

PeakGradWeek9$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakGradWeek9$faculty)
PeakGradWeek9 <- PeakGradWeek9[,!( names(PeakGradWeek9) %in% c("faculty") )]

PeakGradWeek13$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakGradWeek13$faculty)
PeakGradWeek13 <- PeakGradWeek13[,!( names(PeakGradWeek13) %in% c("faculty") )]

PeakGradFin$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakGradFin$faculty)
PeakGradFin <- PeakGradFin[,!( names(PeakGradFin) %in% c("faculty") )]

## Tec ##

print("faculty aggregation to tec novo students")

BioTec$faculty.agreggation <- mapply(FacultyTransformation_Tec,BioTec$faculty)
BioTec <- BioTec[,!( names(BioTec) %in% c("faculty") )]

BioTecWeek5$faculty.agreggation <- mapply(FacultyTransformation_Tec,BioTecWeek5$faculty)
BioTecWeek5 <- BioTecWeek5[,!( names(BioTecWeek5) %in% c("faculty") )]

BioTecWeek9$faculty.agreggation <- mapply(FacultyTransformation_Tec,BioTecWeek9$faculty)
BioTecWeek9 <- BioTecWeek9[,!( names(BioTecWeek9) %in% c("faculty") )]

BioTecWeek13$faculty.agreggation <- mapply(FacultyTransformation_Tec,BioTecWeek13$faculty)
BioTecWeek13 <- BioTecWeek13[,!( names(BioTecWeek13) %in% c("faculty") )]

BioTecFin$faculty.agreggation <- mapply(FacultyTransformation_Tec,BioTecFin$faculty)
BioTecFin <- BioTecFin[,!( names(BioTecFin) %in% c("faculty") )]

print("faculty aggregation to tec veteran students")

PeakTec$faculty.agreggation <- mapply(FacultyTransformation_Tec,PeakTec$faculty)
PeakTec <- PeakTec[,!( names(PeakTec) %in% c("faculty") )]

PeakTecWeek5$faculty.agreggation <- mapply(FacultyTransformation_Tec,PeakTecWeek5$faculty)
PeakTecWeek5 <- PeakTecWeek5[,!( names(PeakTecWeek5) %in% c("faculty") )]

PeakTecWeek9$faculty.agreggation <- mapply(FacultyTransformation_Tec,PeakTecWeek9$faculty)
PeakTecWeek9 <- PeakTecWeek9[,!( names(PeakTecWeek9) %in% c("faculty") )]

PeakTecWeek13$faculty.agreggation <- mapply(FacultyTransformation_Tec,PeakTecWeek13$faculty)
PeakTecWeek13 <- PeakTecWeek13[,!( names(PeakTecWeek13) %in% c("faculty") )]

PeakTecFin$faculty.agreggation <- mapply(FacultyTransformation_Tec,PeakTecFin$faculty)
PeakTecFin <- PeakTecFin[,!( names(PeakTecFin) %in% c("faculty") )]


## posgrad ##

print("Dropping some rows for PosGrad novo students, dropping atypical values on faculty")

drop.rows <- c(
  "ESCOLA TÉCNICA POTIGUAR",
  "INSTITUCIONAL",
  "ESCOLA DA SAÚDE - MOSSORÓ"
)

BioPosGrad <- BioPosGrad[ !(trim(BioPosGrad$faculty)%in%drop.rows) ,]
BioPosGradWeek5 <- BioPosGradWeek5[ !(trim(BioPosGradWeek5$faculty)%in%drop.rows) ,]
BioPosGradWeek9 <- BioPosGradWeek9[ !(trim(BioPosGradWeek9$faculty)%in%drop.rows) ,]
BioPosGradWeek13 <- BioPosGradWeek13[ !(trim(BioPosGradWeek13$faculty)%in%drop.rows) ,]
BioPosGradFin <- BioPosGradFin[ !(trim(BioPosGradFin$faculty)%in%drop.rows) ,]

print("faculty aggregation to posgrad novo students")

BioPosGrad$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioPosGrad$faculty)
BioPosGrad <- BioPosGrad[,!( names(BioPosGrad) %in% c("faculty") )]

BioPosGradWeek5$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioPosGradWeek5$faculty)
BioPosGradWeek5 <- BioPosGradWeek5[,!( names(BioPosGradWeek5) %in% c("faculty") )]

BioPosGradWeek9$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioPosGradWeek9$faculty)
BioPosGradWeek9 <- BioPosGradWeek9[,!( names(BioPosGradWeek9) %in% c("faculty") )]

BioPosGradWeek13$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioPosGradWeek13$faculty)
BioPosGradWeek13 <- BioPosGradWeek13[,!( names(BioPosGradWeek13) %in% c("faculty") )]

BioPosGradFin$faculty.agreggation <- mapply(FacultyTransformation_Grad,BioPosGradFin$faculty)
BioPosGradFin <- BioPosGradFin[,!( names(BioPosGradFin) %in% c("faculty") )]

print("faculty aggregation to posgrad veteran students")

PeakPosGrad$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakPosGrad$faculty)
PeakPosGrad <- PeakPosGrad[,!( names(PeakPosGrad) %in% c("faculty") )]

PeakPosGradWeek5$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakPosGradWeek5$faculty)
PeakPosGradWeek5 <- PeakPosGradWeek5[,!( names(PeakPosGradWeek5) %in% c("faculty") )]

PeakPosGradWeek9$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakPosGradWeek9$faculty)
PeakPosGradWeek9 <- PeakPosGradWeek9[,!( names(PeakPosGradWeek9) %in% c("faculty") )]

PeakPosGradWeek13$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakPosGradWeek13$faculty)
PeakPosGradWeek13 <- PeakPosGradWeek13[,!( names(PeakPosGradWeek13) %in% c("faculty") )]

PeakPosGradFin$faculty.agreggation <- mapply(FacultyTransformation_Grad,PeakPosGradFin$faculty)
PeakPosGradFin <- PeakPosGradFin[,!( names(PeakPosGradFin) %in% c("faculty") )]

######## Creditos
#
###########################

print("Calculing new credit variable for PosGrad")

BioPosGradWeek5$credits.aggregation <- mapply(CreditosTransformation,BioPosGradWeek5$total.creditos,BioPosGradWeek5$num.disciplinas)
BioPosGradWeek9$credits.aggregation <- mapply(CreditosTransformation,BioPosGradWeek9$total.creditos,BioPosGradWeek9$num.disciplinas)
BioPosGradWeek13$credits.aggregation <- mapply(CreditosTransformation,BioPosGradWeek13$total.creditos,BioPosGradWeek13$num.disciplinas)
BioPosGradFin$credits.aggregation <- mapply(CreditosTransformation,BioPosGradFin$total.creditos,BioPosGradFin$num.disciplinas)
BioPosGradWeek5 <- BioPosGradWeek5[,!( names(BioPosGradWeek5) %in% c("num.disciplinas","total.creditos") )]
BioPosGradWeek9 <- BioPosGradWeek9[,!( names(BioPosGradWeek9) %in% c("num.disciplinas","total.creditos") )]
BioPosGradWeek13 <- BioPosGradWeek13[,!( names(BioPosGradWeek13) %in% c("num.disciplinas","total.creditos") )]
BioPosGradFin <- BioPosGradFin[,!( names(BioPosGradFin) %in% c("num.disciplinas","total.creditos") )]

PeakPosGradWeek5$credits.aggregation <- mapply(CreditosTransformation,PeakPosGradWeek5$total.creditos.matriculado,PeakPosGradWeek5$num.disciplinas.matriculado)
PeakPosGradWeek9$credits.aggregation <- mapply(CreditosTransformation,PeakPosGradWeek9$total.creditos.matriculado,PeakPosGradWeek9$num.disciplinas.matriculado)
PeakPosGradWeek13$credits.aggregation <- mapply(CreditosTransformation,PeakPosGradWeek13$total.creditos.matriculado,PeakPosGradWeek13$num.disciplinas.matriculado)
PeakPosGradFin$credits.aggregation <- mapply(CreditosTransformation,PeakPosGradFin$total.creditos.matriculado,PeakPosGradFin$num.disciplinas.matriculado)
PeakPosGradWeek5 <- PeakPosGradWeek5[,!( names(PeakPosGradWeek5) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]
PeakPosGradWeek9 <- PeakPosGradWeek9[,!( names(PeakPosGradWeek9) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]
PeakPosGradWeek13 <- PeakPosGradWeek13[,!( names(PeakPosGradWeek13) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]
PeakPosGradFin <- PeakPosGradFin[,!( names(PeakPosGradFin) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]

print("Calculing new credit variable for Grad")

BioGradWeek5$credits.aggregation <- mapply(CreditosTransformation,BioGradWeek5$total.creditos,BioGradWeek5$num.disciplinas)
BioGradWeek9$credits.aggregation <- mapply(CreditosTransformation,BioGradWeek9$total.creditos,BioGradWeek9$num.disciplinas)
BioGradWeek13$credits.aggregation <- mapply(CreditosTransformation,BioGradWeek13$total.creditos,BioGradWeek13$num.disciplinas)
BioGradFin$credits.aggregation <- mapply(CreditosTransformation,BioGradFin$total.creditos,BioGradFin$num.disciplinas)
BioGradWeek5 <- BioGradWeek5[,!( names(BioGradWeek5) %in% c("num.disciplinas","total.creditos") )]
BioGradWeek9 <- BioGradWeek9[,!( names(BioGradWeek9) %in% c("num.disciplinas","total.creditos") )]
BioGradWeek13 <- BioGradWeek13[,!( names(BioGradWeek13) %in% c("num.disciplinas","total.creditos") )]
BioGradFin <- BioGradFin[,!( names(BioGradFin) %in% c("num.disciplinas","total.creditos") )]

PeakGradWeek5$credits.aggregation <- mapply(CreditosTransformation,PeakGradWeek5$total.creditos.matriculado,PeakGradWeek5$num.disciplinas.matriculado)
PeakGradWeek9$credits.aggregation <- mapply(CreditosTransformation,PeakGradWeek9$total.creditos.matriculado,PeakGradWeek9$num.disciplinas.matriculado)
PeakGradWeek13$credits.aggregation <- mapply(CreditosTransformation,PeakGradWeek13$total.creditos.matriculado,PeakGradWeek13$num.disciplinas.matriculado)
PeakGradFin$credits.aggregation <- mapply(CreditosTransformation,PeakGradFin$total.creditos.matriculado,PeakGradFin$num.disciplinas.matriculado)
PeakGradWeek5 <- PeakGradWeek5[,!( names(PeakGradWeek5) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]
PeakGradWeek9 <- PeakGradWeek9[,!( names(PeakGradWeek9) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]
PeakGradWeek13 <- PeakGradWeek13[,!( names(PeakGradWeek13) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]
PeakGradFin <- PeakGradFin[,!( names(PeakGradFin) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]

print("Calculing new credit variable for Tec")

BioTecWeek5$credits.aggregation <- mapply(CreditosTransformation,BioTecWeek5$total.creditos,BioTecWeek5$num.disciplinas)
BioTecWeek9$credits.aggregation <- mapply(CreditosTransformation,BioTecWeek9$total.creditos,BioTecWeek9$num.disciplinas)
BioTecWeek13$credits.aggregation <- mapply(CreditosTransformation,BioTecWeek13$total.creditos,BioTecWeek13$num.disciplinas)
BioTecFin$credits.aggregation <- mapply(CreditosTransformation,BioTecFin$total.creditos,BioTecFin$num.disciplinas)
BioTecWeek5 <- BioTecWeek5[,!( names(BioTecWeek5) %in% c("num.disciplinas","total.creditos") )]
BioTecWeek9 <- BioTecWeek9[,!( names(BioTecWeek9) %in% c("num.disciplinas","total.creditos") )]
BioTecWeek13 <- BioTecWeek13[,!( names(BioTecWeek13) %in% c("num.disciplinas","total.creditos") )]
BioTecFin <- BioTecFin[,!( names(BioTecFin) %in% c("num.disciplinas","total.creditos") )]

PeakTecWeek5$credits.aggregation <- mapply(CreditosTransformation,PeakTecWeek5$total.creditos.matriculado,PeakTecWeek5$num.disciplinas.matriculado)
PeakTecWeek9$credits.aggregation <- mapply(CreditosTransformation,PeakTecWeek9$total.creditos.matriculado,PeakTecWeek9$num.disciplinas.matriculado)
PeakTecWeek13$credits.aggregation <- mapply(CreditosTransformation,PeakTecWeek13$total.creditos.matriculado,PeakTecWeek13$num.disciplinas.matriculado)
PeakTecFin$credits.aggregation <- mapply(CreditosTransformation,PeakTecFin$total.creditos.matriculado,PeakTecFin$num.disciplinas.matriculado)
PeakTecWeek5 <- PeakTecWeek5[,!( names(PeakTecWeek5) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]
PeakTecWeek9 <- PeakTecWeek9[,!( names(PeakTecWeek9) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]
PeakTecWeek13 <- PeakTecWeek13[,!( names(PeakTecWeek13) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]
PeakTecFin <- PeakTecFin[,!( names(PeakTecFin) %in% c("num.disciplinas.matriculado","total.creditos.matriculado") )]


######## Day Payment
#
###########################


## Grad ##

print("pay Engagementfor grad")

BioGradWeek5$pay.engagement <- mapply(PayEngagement
                                      , BioGradWeek5$days.payment.1
                                      , BioGradWeek5$days.payment.2
                                      , BioGradWeek5$days.payment.3
                                      , BioGradWeek5$days.payment.4
                                      , BioGradWeek5$days.payment.5
                                      , BioGradWeek5$days.payment.6
                                      , 5)

BioGradWeek9$pay.engagement <- mapply(PayEngagement
                                      , BioGradWeek9$days.payment.1
                                      , BioGradWeek9$days.payment.2
                                      , BioGradWeek9$days.payment.3
                                      , BioGradWeek9$days.payment.4
                                      , BioGradWeek9$days.payment.5
                                      , BioGradWeek9$days.payment.6
                                      , 9)

BioGradWeek13$pay.engagement <- mapply(PayEngagement
                                      , BioGradWeek13$days.payment.1
                                      , BioGradWeek13$days.payment.2
                                      , BioGradWeek13$days.payment.3
                                      , BioGradWeek13$days.payment.4
                                      , BioGradWeek13$days.payment.5
                                      , BioGradWeek13$days.payment.6
                                      , 13)

BioGradFin$pay.engagement <- mapply(PayEngagement
                                       , BioGradFin$days.payment.1
                                       , BioGradFin$days.payment.2
                                       , BioGradFin$days.payment.3
                                       , BioGradFin$days.payment.4
                                       , BioGradFin$days.payment.5
                                       , BioGradFin$days.payment.6
                                       , 19)

PeakGradWeek5$pay.engagement <- mapply(PayEngagement
                                     , PeakGradWeek5$days.payment.1
                                     , PeakGradWeek5$days.payment.2
                                     , PeakGradWeek5$days.payment.3
                                     , PeakGradWeek5$days.payment.4
                                     , PeakGradWeek5$days.payment.5
                                     , PeakGradWeek5$days.payment.6
                                     , 5)

PeakGradWeek9$pay.engagement <- mapply(PayEngagement
                                     , PeakGradWeek9$days.payment.1
                                     , PeakGradWeek9$days.payment.2
                                     , PeakGradWeek9$days.payment.3
                                     , PeakGradWeek9$days.payment.4
                                     , PeakGradWeek9$days.payment.5
                                     , PeakGradWeek9$days.payment.6
                                     , 9)

PeakGradWeek13$pay.engagement <- mapply(PayEngagement
                                     , PeakGradWeek13$days.payment.1
                                     , PeakGradWeek13$days.payment.2
                                     , PeakGradWeek13$days.payment.3
                                     , PeakGradWeek13$days.payment.4
                                     , PeakGradWeek13$days.payment.5
                                     , PeakGradWeek13$days.payment.6
                                     , 13)

PeakGradFin$pay.engagement <- mapply(PayEngagement
                                      , PeakGradFin$days.payment.1
                                      , PeakGradFin$days.payment.2
                                      , PeakGradFin$days.payment.3
                                      , PeakGradFin$days.payment.4
                                      , PeakGradFin$days.payment.5
                                      , PeakGradFin$days.payment.6
                                      , 19)



BioGradWeek5 <- BioGradWeek5[, !(  names(BioGradWeek5) %in% c("days.payment.1",
                                              "days.payment.2",
                                              "days.payment.3",
                                              "days.payment.4",
                                              "days.payment.5",
                                              "days.payment.6")  )  ]

BioGradWeek9 <- BioGradWeek9[, !(  names(BioGradWeek9) %in% c("days.payment.1",
                                              "days.payment.2",
                                              "days.payment.3",
                                              "days.payment.4",
                                              "days.payment.5",
                                              "days.payment.6")  )  ]

BioGradWeek13 <- BioGradWeek13[, !(  names(BioGradWeek13) %in% c("days.payment.1",
                                              "days.payment.2",
                                              "days.payment.3",
                                              "days.payment.4",
                                              "days.payment.5",
                                              "days.payment.6")  )  ]

BioGradFin <- BioGradFin[, !(  names(BioGradFin) %in% c("days.payment.1",
                                              "days.payment.2",
                                              "days.payment.3",
                                              "days.payment.4",
                                              "days.payment.5",
                                              "days.payment.6")  )  ]

PeakGradWeek5 <- PeakGradWeek5[, !(  names(PeakGradWeek5) %in% c("days.payment.1",
                                              "days.payment.2",
                                              "days.payment.3",
                                              "days.payment.4",
                                              "days.payment.5",
                                              "days.payment.6")  )  ]

PeakGradWeek9 <- PeakGradWeek9[, !(  names(PeakGradWeek9) %in% c("days.payment.1",
                                              "days.payment.2",
                                              "days.payment.3",
                                              "days.payment.4",
                                              "days.payment.5",
                                              "days.payment.6")  )  ]

PeakGradWeek13 <- PeakGradWeek13[, !(  names(PeakGradWeek13) %in% c("days.payment.1",
                                              "days.payment.2",
                                              "days.payment.3",
                                              "days.payment.4",
                                              "days.payment.5",
                                              "days.payment.6")  )  ]

PeakGradFin <- PeakGradFin[, !(  names(PeakGradFin) %in% c("days.payment.1",
                                              "days.payment.2",
                                              "days.payment.3",
                                              "days.payment.4",
                                              "days.payment.5",
                                              "days.payment.6")  )  ]

## PosGrad ##

print("pay Engagementfor PosGrad")

BioPosGradWeek5$pay.engagement <- mapply(PayEngagement
                                      , BioPosGradWeek5$days.payment.1
                                      , BioPosGradWeek5$days.payment.2
                                      , BioPosGradWeek5$days.payment.3
                                      , BioPosGradWeek5$days.payment.4
                                      , BioPosGradWeek5$days.payment.5
                                      , BioPosGradWeek5$days.payment.6
                                      , 5)

BioPosGradWeek9$pay.engagement <- mapply(PayEngagement
                                      , BioPosGradWeek9$days.payment.1
                                      , BioPosGradWeek9$days.payment.2
                                      , BioPosGradWeek9$days.payment.3
                                      , BioPosGradWeek9$days.payment.4
                                      , BioPosGradWeek9$days.payment.5
                                      , BioPosGradWeek9$days.payment.6
                                      , 9)

BioPosGradWeek13$pay.engagement <- mapply(PayEngagement
                                       , BioPosGradWeek13$days.payment.1
                                       , BioPosGradWeek13$days.payment.2
                                       , BioPosGradWeek13$days.payment.3
                                       , BioPosGradWeek13$days.payment.4
                                       , BioPosGradWeek13$days.payment.5
                                       , BioPosGradWeek13$days.payment.6
                                       , 13)

BioPosGradFin$pay.engagement <- mapply(PayEngagement
                                    , BioPosGradFin$days.payment.1
                                    , BioPosGradFin$days.payment.2
                                    , BioPosGradFin$days.payment.3
                                    , BioPosGradFin$days.payment.4
                                    , BioPosGradFin$days.payment.5
                                    , BioPosGradFin$days.payment.6
                                    , 19)

PeakPosGradWeek5$pay.engagement <- mapply(PayEngagement
                                       , PeakPosGradWeek5$days.payment.1
                                       , PeakPosGradWeek5$days.payment.2
                                       , PeakPosGradWeek5$days.payment.3
                                       , PeakPosGradWeek5$days.payment.4
                                       , PeakPosGradWeek5$days.payment.5
                                       , PeakPosGradWeek5$days.payment.6
                                       , 5)

PeakPosGradWeek9$pay.engagement <- mapply(PayEngagement
                                       , PeakPosGradWeek9$days.payment.1
                                       , PeakPosGradWeek9$days.payment.2
                                       , PeakPosGradWeek9$days.payment.3
                                       , PeakPosGradWeek9$days.payment.4
                                       , PeakPosGradWeek9$days.payment.5
                                       , PeakPosGradWeek9$days.payment.6
                                       , 9)

PeakPosGradWeek13$pay.engagement <- mapply(PayEngagement
                                        , PeakPosGradWeek13$days.payment.1
                                        , PeakPosGradWeek13$days.payment.2
                                        , PeakPosGradWeek13$days.payment.3
                                        , PeakPosGradWeek13$days.payment.4
                                        , PeakPosGradWeek13$days.payment.5
                                        , PeakPosGradWeek13$days.payment.6
                                        , 13)

PeakPosGradFin$pay.engagement <- mapply(PayEngagement
                                     , PeakPosGradFin$days.payment.1
                                     , PeakPosGradFin$days.payment.2
                                     , PeakPosGradFin$days.payment.3
                                     , PeakPosGradFin$days.payment.4
                                     , PeakPosGradFin$days.payment.5
                                     , PeakPosGradFin$days.payment.6
                                     , 19)



BioPosGradWeek5 <- BioPosGradWeek5[, !(  names(BioPosGradWeek5) %in% c("days.payment.1",
                                              "days.payment.2",
                                              "days.payment.3",
                                              "days.payment.4",
                                              "days.payment.5",
                                              "days.payment.6")  )  ]

BioPosGradWeek9 <- BioPosGradWeek9[, !(  names(BioPosGradWeek9) %in% c("days.payment.1",
                                              "days.payment.2",
                                              "days.payment.3",
                                              "days.payment.4",
                                              "days.payment.5",
                                              "days.payment.6")  )  ]

BioPosGradWeek13 <- BioPosGradWeek13[, !(  names(BioPosGradWeek13) %in% c("days.payment.1",
                                                "days.payment.2",
                                                "days.payment.3",
                                                "days.payment.4",
                                                "days.payment.5",
                                                "days.payment.6")  )  ]

BioPosGradFin <- BioPosGradFin[, !(  names(BioPosGradFin) %in% c("days.payment.1",
                                          "days.payment.2",
                                          "days.payment.3",
                                          "days.payment.4",
                                          "days.payment.5",
                                          "days.payment.6")  )  ]

PeakPosGradWeek5 <- PeakPosGradWeek5[, !(  names(PeakPosGradWeek5) %in% c("days.payment.1",
                                                "days.payment.2",
                                                "days.payment.3",
                                                "days.payment.4",
                                                "days.payment.5",
                                                "days.payment.6")  )  ]

PeakPosGradWeek9 <- PeakPosGradWeek9[, !(  names(PeakPosGradWeek9) %in% c("days.payment.1",
                                                "days.payment.2",
                                                "days.payment.3",
                                                "days.payment.4",
                                                "days.payment.5",
                                                "days.payment.6")  )  ]

PeakPosGradWeek13 <- PeakPosGradWeek13[, !(  names(PeakPosGradWeek13) %in% c("days.payment.1",
                                                  "days.payment.2",
                                                  "days.payment.3",
                                                  "days.payment.4",
                                                  "days.payment.5",
                                                  "days.payment.6")  )  ]

PeakPosGradFin <- PeakPosGradFin[, !(  names(PeakPosGradFin) %in% c("days.payment.1",
                                            "days.payment.2",
                                            "days.payment.3",
                                            "days.payment.4",
                                            "days.payment.5",
                                            "days.payment.6")  )  ]

## Tec ##

print("pay Engagementfor Tec")

BioTecWeek5$pay.engagement <- mapply(PayEngagement
                                         , BioTecWeek5$days.payment.1
                                         , BioTecWeek5$days.payment.2
                                         , BioTecWeek5$days.payment.3
                                         , BioTecWeek5$days.payment.4
                                         , BioTecWeek5$days.payment.5
                                         , BioTecWeek5$days.payment.6
                                         , 5)

BioTecWeek9$pay.engagement <- mapply(PayEngagement
                                         , BioTecWeek9$days.payment.1
                                         , BioTecWeek9$days.payment.2
                                         , BioTecWeek9$days.payment.3
                                         , BioTecWeek9$days.payment.4
                                         , BioTecWeek9$days.payment.5
                                         , BioTecWeek9$days.payment.6
                                         , 9)

BioTecWeek13$pay.engagement <- mapply(PayEngagement
                                          , BioTecWeek13$days.payment.1
                                          , BioTecWeek13$days.payment.2
                                          , BioTecWeek13$days.payment.3
                                          , BioTecWeek13$days.payment.4
                                          , BioTecWeek13$days.payment.5
                                          , BioTecWeek13$days.payment.6
                                          , 13)

BioTecFin$pay.engagement <- mapply(PayEngagement
                                       , BioTecFin$days.payment.1
                                       , BioTecFin$days.payment.2
                                       , BioTecFin$days.payment.3
                                       , BioTecFin$days.payment.4
                                       , BioTecFin$days.payment.5
                                       , BioTecFin$days.payment.6
                                       , 19)

PeakTecWeek5$pay.engagement <- mapply(PayEngagement
                                          , PeakTecWeek5$days.payment.1
                                          , PeakTecWeek5$days.payment.2
                                          , PeakTecWeek5$days.payment.3
                                          , PeakTecWeek5$days.payment.4
                                          , PeakTecWeek5$days.payment.5
                                          , PeakTecWeek5$days.payment.6
                                          , 5)

PeakTecWeek9$pay.engagement <- mapply(PayEngagement
                                          , PeakTecWeek9$days.payment.1
                                          , PeakTecWeek9$days.payment.2
                                          , PeakTecWeek9$days.payment.3
                                          , PeakTecWeek9$days.payment.4
                                          , PeakTecWeek9$days.payment.5
                                          , PeakTecWeek9$days.payment.6
                                          , 9)

PeakTecWeek13$pay.engagement <- mapply(PayEngagement
                                           , PeakTecWeek13$days.payment.1
                                           , PeakTecWeek13$days.payment.2
                                           , PeakTecWeek13$days.payment.3
                                           , PeakTecWeek13$days.payment.4
                                           , PeakTecWeek13$days.payment.5
                                           , PeakTecWeek13$days.payment.6
                                           , 13)

PeakTecFin$pay.engagement <- mapply(PayEngagement
                                        , PeakTecFin$days.payment.1
                                        , PeakTecFin$days.payment.2
                                        , PeakTecFin$days.payment.3
                                        , PeakTecFin$days.payment.4
                                        , PeakTecFin$days.payment.5
                                        , PeakTecFin$days.payment.6
                                        , 19)



BioTecWeek5 <- BioTecWeek5[, !(  names(BioTecWeek5) %in% c("days.payment.1",
                                            "days.payment.2",
                                            "days.payment.3",
                                            "days.payment.4",
                                            "days.payment.5",
                                            "days.payment.6")  )  ]

BioTecWeek9 <- BioTecWeek9[, !(  names(BioTecWeek9) %in% c("days.payment.1",
                                            "days.payment.2",
                                            "days.payment.3",
                                            "days.payment.4",
                                            "days.payment.5",
                                            "days.payment.6")  )  ]

BioTecWeek13 <- BioTecWeek13[, !(  names(BioTecWeek13) %in% c("days.payment.1",
                                              "days.payment.2",
                                              "days.payment.3",
                                              "days.payment.4",
                                              "days.payment.5",
                                              "days.payment.6")  )  ]

BioTecFin <- BioTecFin[, !(  names(BioTecFin) %in% c("days.payment.1",
                                        "days.payment.2",
                                        "days.payment.3",
                                        "days.payment.4",
                                        "days.payment.5",
                                        "days.payment.6")  )  ]

PeakTecWeek5 <- PeakTecWeek5[, !(  names(PeakTecWeek5) %in% c("days.payment.1",
                                              "days.payment.2",
                                              "days.payment.3",
                                              "days.payment.4",
                                              "days.payment.5",
                                              "days.payment.6")  )  ]

PeakTecWeek9 <- PeakTecWeek9[, !(  names(PeakTecWeek9) %in% c("days.payment.1",
                                              "days.payment.2",
                                              "days.payment.3",
                                              "days.payment.4",
                                              "days.payment.5",
                                              "days.payment.6")  )  ]

PeakTecWeek13 <- PeakTecWeek13[, !(  names(PeakTecWeek13) %in% c("days.payment.1",
                                                "days.payment.2",
                                                "days.payment.3",
                                                "days.payment.4",
                                                "days.payment.5",
                                                "days.payment.6")  )  ]

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

BioGradWeek5$payment.balance <- mapply(PaymentBalance,
BioGradWeek5$payment.amount.1,
BioGradWeek5$payment.amount.2,
BioGradWeek5$payment.amount.3,
BioGradWeek5$payment.amount.4,
BioGradWeek5$payment.amount.5,
BioGradWeek5$payment.amount.6,
BioGradWeek5$total.balance)

BioGradWeek9$payment.balance <- mapply(PaymentBalance,
BioGradWeek9$payment.amount.1,
BioGradWeek9$payment.amount.2,
BioGradWeek9$payment.amount.3,
BioGradWeek9$payment.amount.4,
BioGradWeek9$payment.amount.5,
BioGradWeek9$payment.amount.6,
BioGradWeek9$total.balance)

BioGradWeek13$payment.balance <- mapply(PaymentBalance,
BioGradWeek13$payment.amount.1,
BioGradWeek13$payment.amount.2,
BioGradWeek13$payment.amount.3,
BioGradWeek13$payment.amount.4,
BioGradWeek13$payment.amount.5,
BioGradWeek13$payment.amount.6,
BioGradWeek13$total.balance)

BioGradFin$payment.balance <- mapply(PaymentBalance,
BioGradFin$payment.amount.1,
BioGradFin$payment.amount.2,
BioGradFin$payment.amount.3,
BioGradFin$payment.amount.4,
BioGradFin$payment.amount.5,
BioGradFin$payment.amount.6,
BioGradFin$total.balance)

PeakGradWeek5$payment.balance <- mapply(PaymentBalance,
PeakGradWeek5$payment.amount.1,
PeakGradWeek5$payment.amount.2,
PeakGradWeek5$payment.amount.3,
PeakGradWeek5$payment.amount.4,
PeakGradWeek5$payment.amount.5,
PeakGradWeek5$payment.amount.6,
PeakGradWeek5$total.balance)

PeakGradWeek9$payment.balance <- mapply(PaymentBalance,
PeakGradWeek9$payment.amount.1,
PeakGradWeek9$payment.amount.2,
PeakGradWeek9$payment.amount.3,
PeakGradWeek9$payment.amount.4,
PeakGradWeek9$payment.amount.5,
PeakGradWeek9$payment.amount.6,
PeakGradWeek9$total.balance)

PeakGradWeek13$payment.balance <- mapply(PaymentBalance,
PeakGradWeek13$payment.amount.1,
PeakGradWeek13$payment.amount.2,
PeakGradWeek13$payment.amount.3,
PeakGradWeek13$payment.amount.4,
PeakGradWeek13$payment.amount.5,
PeakGradWeek13$payment.amount.6,
PeakGradWeek13$total.balance)

PeakGradFin$payment.balance <- mapply(PaymentBalance,
PeakGradFin$payment.amount.1,
PeakGradFin$payment.amount.2,
PeakGradFin$payment.amount.3,
PeakGradFin$payment.amount.4,
PeakGradFin$payment.amount.5,
PeakGradFin$payment.amount.6,
PeakGradFin$total.balance)

BioGradWeek5 <- BioGradWeek5[, !(  names(BioGradWeek5) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

BioGradWeek9 <- BioGradWeek9[, !(  names(BioGradWeek9) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

BioGradWeek13 <- BioGradWeek13[, !(  names(BioGradWeek13) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

BioGradFin <- BioGradFin[, !(  names(BioGradFin) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

PeakGradWeek5 <- PeakGradWeek5[, !(  names(PeakGradWeek5) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

PeakGradWeek9 <- PeakGradWeek9[, !(  names(PeakGradWeek9) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

PeakGradWeek13 <- PeakGradWeek13[, !(  names(PeakGradWeek13) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

PeakGradFin <- PeakGradFin[, !(  names(PeakGradFin) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

## PosGrad ##

print("payment balance for PosGrad")

BioPosGradWeek5$payment.balance <- mapply(PaymentBalance,
BioPosGradWeek5$payment.amount.1,
BioPosGradWeek5$payment.amount.2,
BioPosGradWeek5$payment.amount.3,
BioPosGradWeek5$payment.amount.4,
BioPosGradWeek5$payment.amount.5,
BioPosGradWeek5$payment.amount.6,
BioPosGradWeek5$total.balance)

BioPosGradWeek9$payment.balance <- mapply(PaymentBalance,
BioPosGradWeek9$payment.amount.1,
BioPosGradWeek9$payment.amount.2,
BioPosGradWeek9$payment.amount.3,
BioPosGradWeek9$payment.amount.4,
BioPosGradWeek9$payment.amount.5,
BioPosGradWeek9$payment.amount.6,
BioPosGradWeek9$total.balance)

BioPosGradWeek13$payment.balance <- mapply(PaymentBalance,
BioPosGradWeek13$payment.amount.1,
BioPosGradWeek13$payment.amount.2,
BioPosGradWeek13$payment.amount.3,
BioPosGradWeek13$payment.amount.4,
BioPosGradWeek13$payment.amount.5,
BioPosGradWeek13$payment.amount.6,
BioPosGradWeek13$total.balance)

BioPosGradFin$payment.balance <- mapply(PaymentBalance,
BioPosGradFin$payment.amount.1,
BioPosGradFin$payment.amount.2,
BioPosGradFin$payment.amount.3,
BioPosGradFin$payment.amount.4,
BioPosGradFin$payment.amount.5,
BioPosGradFin$payment.amount.6,
BioPosGradFin$total.balance)

PeakPosGradWeek5$payment.balance <- mapply(PaymentBalance,
PeakPosGradWeek5$payment.amount.1,
PeakPosGradWeek5$payment.amount.2,
PeakPosGradWeek5$payment.amount.3,
PeakPosGradWeek5$payment.amount.4,
PeakPosGradWeek5$payment.amount.5,
PeakPosGradWeek5$payment.amount.6,
PeakPosGradWeek5$total.balance)

PeakPosGradWeek9$payment.balance <- mapply(PaymentBalance,
PeakPosGradWeek9$payment.amount.1,
PeakPosGradWeek9$payment.amount.2,
PeakPosGradWeek9$payment.amount.3,
PeakPosGradWeek9$payment.amount.4,
PeakPosGradWeek9$payment.amount.5,
PeakPosGradWeek9$payment.amount.6,
PeakPosGradWeek9$total.balance)

PeakPosGradWeek13$payment.balance <- mapply(PaymentBalance,
PeakPosGradWeek13$payment.amount.1,
PeakPosGradWeek13$payment.amount.2,
PeakPosGradWeek13$payment.amount.3,
PeakPosGradWeek13$payment.amount.4,
PeakPosGradWeek13$payment.amount.5,
PeakPosGradWeek13$payment.amount.6,
PeakPosGradWeek13$total.balance)

PeakPosGradFin$payment.balance <- mapply(PaymentBalance,
PeakPosGradFin$payment.amount.1,
PeakPosGradFin$payment.amount.2,
PeakPosGradFin$payment.amount.3,
PeakPosGradFin$payment.amount.4,
PeakPosGradFin$payment.amount.5,
PeakPosGradFin$payment.amount.6,
PeakPosGradFin$total.balance)

BioPosGradWeek5 <- BioPosGradWeek5[, !(  names(BioPosGradWeek5) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

BioPosGradWeek9 <- BioPosGradWeek9[, !(  names(BioPosGradWeek9) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

BioPosGradWeek13 <- BioPosGradWeek13[, !(  names(BioPosGradWeek13) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

BioPosGradFin <- BioPosGradFin[, !(  names(BioPosGradFin) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

PeakPosGradWeek5 <- PeakPosGradWeek5[, !(  names(PeakPosGradWeek5) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

PeakPosGradWeek9 <- PeakPosGradWeek9[, !(  names(PeakPosGradWeek9) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

PeakPosGradWeek13 <- PeakPosGradWeek13[, !(  names(PeakPosGradWeek13) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

PeakPosGradFin <- PeakPosGradFin[, !(  names(PeakPosGradFin) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

## PosGrad ##

print("payment balance for Tec")

BioTecWeek5$payment.balance <- mapply(PaymentBalance,
BioTecWeek5$payment.amount.1,
BioTecWeek5$payment.amount.2,
BioTecWeek5$payment.amount.3,
BioTecWeek5$payment.amount.4,
BioTecWeek5$payment.amount.5,
BioTecWeek5$payment.amount.6,
BioTecWeek5$total.balance)

BioTecWeek9$payment.balance <- mapply(PaymentBalance,
BioTecWeek9$payment.amount.1,
BioTecWeek9$payment.amount.2,
BioTecWeek9$payment.amount.3,
BioTecWeek9$payment.amount.4,
BioTecWeek9$payment.amount.5,
BioTecWeek9$payment.amount.6,
BioTecWeek9$total.balance)

BioTecWeek13$payment.balance <- mapply(PaymentBalance,
BioTecWeek13$payment.amount.1,
BioTecWeek13$payment.amount.2,
BioTecWeek13$payment.amount.3,
BioTecWeek13$payment.amount.4,
BioTecWeek13$payment.amount.5,
BioTecWeek13$payment.amount.6,
BioTecWeek13$total.balance)

BioTecFin$payment.balance <- mapply(PaymentBalance,
BioTecFin$payment.amount.1,
BioTecFin$payment.amount.2,
BioTecFin$payment.amount.3,
BioTecFin$payment.amount.4,
BioTecFin$payment.amount.5,
BioTecFin$payment.amount.6,
BioTecFin$total.balance)

PeakTecWeek5$payment.balance <- mapply(PaymentBalance,
PeakTecWeek5$payment.amount.1,
PeakTecWeek5$payment.amount.2,
PeakTecWeek5$payment.amount.3,
PeakTecWeek5$payment.amount.4,
PeakTecWeek5$payment.amount.5,
PeakTecWeek5$payment.amount.6,
PeakTecWeek5$total.balance)

PeakTecWeek9$payment.balance <- mapply(PaymentBalance,
PeakTecWeek9$payment.amount.1,
PeakTecWeek9$payment.amount.2,
PeakTecWeek9$payment.amount.3,
PeakTecWeek9$payment.amount.4,
PeakTecWeek9$payment.amount.5,
PeakTecWeek9$payment.amount.6,
PeakTecWeek9$total.balance)

PeakTecWeek13$payment.balance <- mapply(PaymentBalance,
PeakTecWeek13$payment.amount.1,
PeakTecWeek13$payment.amount.2,
PeakTecWeek13$payment.amount.3,
PeakTecWeek13$payment.amount.4,
PeakTecWeek13$payment.amount.5,
PeakTecWeek13$payment.amount.6,
PeakTecWeek13$total.balance)

PeakTecFin$payment.balance <- mapply(PaymentBalance,
PeakTecFin$payment.amount.1,
PeakTecFin$payment.amount.2,
PeakTecFin$payment.amount.3,
PeakTecFin$payment.amount.4,
PeakTecFin$payment.amount.5,
PeakTecFin$payment.amount.6,
PeakTecFin$total.balance)

BioTecWeek5 <- BioTecWeek5[, !(  names(BioTecWeek5) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

BioTecWeek9 <- BioTecWeek9[, !(  names(BioTecWeek9) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

BioTecWeek13 <- BioTecWeek13[, !(  names(BioTecWeek13) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

BioTecFin <- BioTecFin[, !(  names(BioTecFin) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

PeakTecWeek5 <- PeakTecWeek5[, !(  names(PeakTecWeek5) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

PeakTecWeek9 <- PeakTecWeek9[, !(  names(PeakTecWeek9) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

PeakTecWeek13 <- PeakTecWeek13[, !(  names(PeakTecWeek13) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]

PeakTecFin <- PeakTecFin[, !(  names(PeakTecFin) %in% c("payment.amount.1",
"payment.amount.2",
"payment.amount.3",
"payment.amount.4",
"payment.amount.5",
"payment.amount.6",
"total.balance"))]


#### Drop attrs unnecesary

PeakGradFin <- unique(PeakGradFin)
PeakGradWeek13 <- unique(PeakGradWeek13)
PeakGradWeek9 <- unique(PeakGradWeek9)
PeakGradWeek5 <- unique(PeakGradWeek5)
PeakGrad <- unique(PeakGrad)

PeakPosGradFin <- unique(PeakPosGradFin)
PeakPosGradWeek13 <- unique(PeakPosGradWeek13)
PeakPosGradWeek9 <- unique(PeakPosGradWeek9)
PeakPosGradWeek5 <- unique(PeakPosGradWeek5)
PeakPosGrad <- unique(PeakPosGrad)

PeakTecFin <- unique(PeakTecFin)
PeakTecWeek13 <- unique(PeakTecWeek13)
PeakTecWeek9 <- unique(PeakTecWeek9)
PeakTecWeek5 <- unique(PeakTecWeek5)
PeakTec <- unique(PeakTec)

BioGradFin <- unique(BioGradFin)
BioGradWeek13 <- unique(BioGradWeek13)
BioGradWeek9 <- unique(BioGradWeek9)
BioGradWeek5 <- unique(BioGradWeek5)
BioGrad <- unique(BioGrad)

BioPosGradFin <- unique(BioPosGradFin)
BioPosGradWeek13 <- unique(BioPosGradWeek13)
BioPosGradWeek9 <- unique(BioPosGradWeek9)
BioPosGradWeek5 <- unique(BioPosGradWeek5)
BioPosGrad <- unique(BioPosGrad)

BioTecFin <- unique(BioTecFin)
BioTecWeek13 <- unique(BioTecWeek13)
BioTecWeek9 <- unique(BioTecWeek9)
BioTecWeek5 <- unique(BioTecWeek5)
BioTec <- unique(BioTec)

rownames(PeakGradFin) <- paste0(as.character(PeakGradFin$student.id),".",as.character(PeakGradFin$academic.cycle))
rownames(PeakGradWeek13) <- paste0(as.character(PeakGradWeek13$student.id),".",as.character(PeakGradWeek13$academic.cycle))
rownames(PeakGradWeek9) <- paste0(as.character(PeakGradWeek9$student.id),".",as.character(PeakGradWeek9$academic.cycle))
rownames(PeakGradWeek5) <- paste0(as.character(PeakGradWeek5$student.id),".",as.character(PeakGradWeek5$academic.cycle))
rownames(PeakGrad) <- paste0(as.character(PeakGrad$student.id),".",as.character(PeakGrad$academic.cycle))

rownames(PeakPosGradFin) <- paste0(as.character(PeakPosGradFin$student.id),".",as.character(PeakPosGradFin$academic.cycle))
rownames(PeakPosGradWeek13) <- paste0(as.character(PeakPosGradWeek13$student.id),".",as.character(PeakPosGradWeek13$academic.cycle))
rownames(PeakPosGradWeek9) <- paste0(as.character(PeakPosGradWeek9$student.id),".",as.character(PeakPosGradWeek9$academic.cycle))
rownames(PeakPosGradWeek5) <- paste0(as.character(PeakPosGradWeek5$student.id),".",as.character(PeakPosGradWeek5$academic.cycle))
rownames(PeakPosGrad) <- paste0(as.character(PeakPosGrad$student.id),".",as.character(PeakPosGrad$academic.cycle))

rownames(PeakTecFin) <- paste0(as.character(PeakTecFin$student.id),".",as.character(PeakTecFin$academic.cycle))
rownames(PeakTecWeek13) <- paste0(as.character(PeakTecWeek13$student.id),".",as.character(PeakTecWeek13$academic.cycle))
rownames(PeakTecWeek9) <- paste0(as.character(PeakTecWeek9$student.id),".",as.character(PeakTecWeek9$academic.cycle))
rownames(PeakTecWeek5) <- paste0(as.character(PeakTecWeek5$student.id),".",as.character(PeakTecWeek5$academic.cycle))
rownames(PeakTec) <- paste0(as.character(PeakTec$student.id),".",as.character(PeakTec$academic.cycle))

rownames(BioGradFin) <- paste0(as.character(BioGradFin$student.id),".",as.character(BioGradFin$academic.cycle))
rownames(BioGradWeek13) <- paste0(as.character(BioGradWeek13$student.id),".",as.character(BioGradWeek13$academic.cycle))
rownames(BioGradWeek9) <- paste0(as.character(BioGradWeek9$student.id),".",as.character(BioGradWeek9$academic.cycle))
rownames(BioGradWeek5) <- paste0(as.character(BioGradWeek5$student.id),".",as.character(BioGradWeek5$academic.cycle))
rownames(BioGrad) <- paste0(as.character(BioGrad$student.id),".",as.character(BioGrad$academic.cycle))

rownames(BioPosGradFin) <- paste0(as.character(BioPosGradFin$student.id),".",as.character(BioPosGradFin$academic.cycle))
rownames(BioPosGradWeek13) <- paste0(as.character(BioPosGradWeek13$student.id),".",as.character(BioPosGradWeek13$academic.cycle))
rownames(BioPosGradWeek9) <- paste0(as.character(BioPosGradWeek9$student.id),".",as.character(BioPosGradWeek9$academic.cycle))
rownames(BioPosGradWeek5) <- paste0(as.character(BioPosGradWeek5$student.id),".",as.character(BioPosGradWeek5$academic.cycle))
rownames(BioPosGrad) <- paste0(as.character(BioPosGrad$student.id),".",as.character(BioPosGrad$academic.cycle))

rownames(BioTecFin) <- paste0(as.character(BioTecFin$student.id),".",as.character(BioTecFin$academic.cycle))
rownames(BioTecWeek13) <- paste0(as.character(BioTecWeek13$student.id),".",as.character(BioTecWeek13$academic.cycle))
rownames(BioTecWeek9) <- paste0(as.character(BioTecWeek9$student.id),".",as.character(BioTecWeek9$academic.cycle))
rownames(BioTecWeek5) <- paste0(as.character(BioTecWeek5$student.id),".",as.character(BioTecWeek5$academic.cycle))
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

BioGrad <- BioGrad[, !(  names(BioGrad) %in% drop.to.novo )]
BioGradWeek5 <- BioGradWeek5[, !(  names(BioGradWeek5) %in% drop.to.novo )]
BioGradWeek9 <- BioGradWeek9[, !(  names(BioGradWeek9) %in% drop.to.novo )]
BioGradWeek13 <- BioGradWeek13[, !(  names(BioGradWeek13) %in% drop.to.novo )]
BioGradFin <- BioGradFin[, !(  names(BioGradFin) %in% drop.to.novo )]

BioPosGrad <- BioPosGrad[, !(  names(BioPosGrad) %in% drop.to.novo )]
BioPosGradWeek5 <- BioPosGradWeek5[, !(  names(BioPosGradWeek5) %in% drop.to.novo )]
BioPosGradWeek9 <- BioPosGradWeek9[, !(  names(BioPosGradWeek9) %in% drop.to.novo )]
BioPosGradWeek13 <- BioPosGradWeek13[, !(  names(BioPosGradWeek13) %in% drop.to.novo )]
BioPosGradFin <- BioPosGradFin[, !(  names(BioPosGradFin) %in% drop.to.novo )]

BioTec <- BioTec[, !(  names(BioTec) %in% drop.to.novo )]
BioTecWeek5 <- BioTecWeek5[, !(  names(BioTecWeek5) %in% drop.to.novo )]
BioTecWeek9 <- BioTecWeek9[, !(  names(BioTecWeek9) %in% drop.to.novo )]
BioTecWeek13 <- BioTecWeek13[, !(  names(BioTecWeek13) %in% drop.to.novo )]
BioTecFin <- BioTecFin[, !(  names(BioTecFin) %in% drop.to.novo )]

print("Dropping from veteran data sets")

PeakGrad <- PeakGrad[, !(  names(PeakGrad) %in% drop.to.veteran )]
PeakGradWeek5 <- PeakGradWeek5[, !(  names(PeakGradWeek5) %in% drop.to.veteran )]
PeakGradWeek9 <- PeakGradWeek9[, !(  names(PeakGradWeek9) %in% drop.to.veteran )]
PeakGradWeek13 <- PeakGradWeek13[, !(  names(PeakGradWeek13) %in% drop.to.veteran )]
PeakGradFin <- PeakGradFin[, !(  names(PeakGradFin) %in% drop.to.veteran )]

PeakPosGrad <- PeakPosGrad[, !(  names(PeakPosGrad) %in% drop.to.veteran )]
PeakPosGradWeek5 <- PeakPosGradWeek5[, !(  names(PeakPosGradWeek5) %in% drop.to.veteran )]
PeakPosGradWeek9 <- PeakPosGradWeek9[, !(  names(PeakPosGradWeek9) %in% drop.to.veteran )]
PeakPosGradWeek13 <- PeakPosGradWeek13[, !(  names(PeakPosGradWeek13) %in% drop.to.veteran )]
PeakPosGradFin <- PeakPosGradFin[, !(  names(PeakPosGradFin) %in% drop.to.veteran )]

PeakTec <- PeakTec[, !(  names(PeakTec) %in% drop.to.veteran )]
PeakTecWeek5 <- PeakTecWeek5[, !(  names(PeakTecWeek5) %in% drop.to.veteran )]
PeakTecWeek9 <- PeakTecWeek9[, !(  names(PeakTecWeek9) %in% drop.to.veteran )]
PeakTecWeek13 <- PeakTecWeek13[, !(  names(PeakTecWeek13) %in% drop.to.veteran )]
PeakTecFin <- PeakTecFin[, !(  names(PeakTecFin) %in% drop.to.veteran )]

### dropping grades from w5 and w9

BioGradWeek5 <- BioGradWeek5[, !( names(BioGradWeek5) %in% c("notas","frequencia") )]
BioGradWeek9 <- BioGradWeek9[, !( names(BioGradWeek9) %in% c("notas","frequencia") )]
BioPosGradWeek5 <- BioPosGradWeek5[, !( names(BioPosGradWeek5) %in% c("notas","frequencia") )]
BioPosGradWeek9 <- BioPosGradWeek9[, !( names(BioPosGradWeek9) %in% c("notas","frequencia") )]
BioTecWeek5 <- BioTecWeek5[, !( names(BioTecWeek5) %in% c("notas","frequencia") )]
BioTecWeek9 <- BioTecWeek9[, !( names(BioTecWeek9) %in% c("notas","frequencia") )]

PeakGradWeek5 <- PeakGradWeek5[, !( names(PeakGradWeek5) %in% c("notas","frequencia") )]
PeakGradWeek9 <- PeakGradWeek9[, !( names(PeakGradWeek9) %in% c("notas","frequencia") )]
PeakPosGradWeek5 <- PeakPosGradWeek5[, !( names(PeakPosGradWeek5) %in% c("notas","frequencia") )]
PeakPosGradWeek9 <- PeakPosGradWeek9[, !( names(PeakPosGradWeek9) %in% c("notas","frequencia") )]
PeakTecWeek5 <- PeakTecWeek5[, !( names(PeakTecWeek5) %in% c("notas","frequencia") )]
PeakTecWeek9 <- PeakTecWeek9[, !( names(PeakTecWeek9) %in% c("notas","frequencia") )]

### cast variables ###

#notas

BioGradWeek13$notas <- as.numeric(BioGradWeek13$notas)
BioGradFin$notas <- as.numeric(BioGradFin$notas)

BioPosGradWeek13$notas <- as.numeric(BioPosGradWeek13$notas)
BioPosGradFin$notas <- as.numeric(BioPosGradFin$notas)

BioTecWeek13$notas <- as.numeric(BioTecWeek13$notas)
BioTecFin$notas <- as.numeric(BioTecFin$notas)

PeakGradWeek13$notas <- as.numeric(PeakGradWeek13$notas)
PeakGradFin$notas <- as.numeric(PeakGradFin$notas)

PeakPosGradWeek13$notas <- as.numeric(PeakPosGradWeek13$notas)
PeakPosGradFin$notas <- as.numeric(PeakPosGradFin$notas)

PeakTecWeek13$notas <- as.numeric(PeakTecWeek13$notas)
PeakTecFin$notas <- as.numeric(PeakTecFin$notas)

#city.aggregation
BioGrad$city.aggregation <- as.factor(BioGrad$city.aggregation)
BioGradWeek5$city.aggregation <- as.factor(BioGradWeek5$city.aggregation)
BioGradWeek9$city.aggregation <- as.factor(BioGradWeek9$city.aggregation)
BioGradWeek13$city.aggregation <- as.factor(BioGradWeek13$city.aggregation)
BioGradFin$city.aggregation <- as.factor(BioGradFin$city.aggregation)

BioPosGrad$city.aggregation <- as.factor(BioPosGrad$city.aggregation)
BioPosGradWeek5$city.aggregation <- as.factor(BioPosGradWeek5$city.aggregation)
BioPosGradWeek9$city.aggregation <- as.factor(BioPosGradWeek9$city.aggregation)
BioPosGradWeek13$city.aggregation <- as.factor(BioPosGradWeek13$city.aggregation)
BioPosGradFin$city.aggregation <- as.factor(BioPosGradFin$city.aggregation)

BioTec$city.aggregation <- as.factor(BioTec$city.aggregation)
BioTecWeek5$city.aggregation <- as.factor(BioTecWeek5$city.aggregation)
BioTecWeek9$city.aggregation <- as.factor(BioTecWeek9$city.aggregation)
BioTecWeek13$city.aggregation <- as.factor(BioTecWeek13$city.aggregation)
BioTecFin$city.aggregation <- as.factor(BioTecFin$city.aggregation)

#state.aggregation
BioGrad$state.aggregation <- as.factor(BioGrad$state.aggregation)
BioGradWeek5$state.aggregation <- as.factor(BioGradWeek5$state.aggregation)
BioGradWeek9$state.aggregation <- as.factor(BioGradWeek9$state.aggregation)
BioGradWeek13$state.aggregation <- as.factor(BioGradWeek13$state.aggregation)
BioGradFin$state.aggregation <- as.factor(BioGradFin$state.aggregation)

BioPosGrad$state.aggregation <- as.factor(BioPosGrad$state.aggregation)
BioPosGradWeek5$state.aggregation <- as.factor(BioPosGradWeek5$state.aggregation)
BioPosGradWeek9$state.aggregation <- as.factor(BioPosGradWeek9$state.aggregation)
BioPosGradWeek13$state.aggregation <- as.factor(BioPosGradWeek13$state.aggregation)
BioPosGradFin$state.aggregation <- as.factor(BioPosGradFin$state.aggregation)

BioTec$state.aggregation <- as.factor(BioTec$state.aggregation)
BioTecWeek5$state.aggregation <- as.factor(BioTecWeek5$state.aggregation)
BioTecWeek9$state.aggregation <- as.factor(BioTecWeek9$state.aggregation)
BioTecWeek13$state.aggregation <- as.factor(BioTecWeek13$state.aggregation)
BioTecFin$state.aggregation <- as.factor(BioTecFin$state.aggregation)

#high.school.agreggation
BioGrad$high.school.agreggation <- as.factor(BioGrad$high.school.agreggation)
BioGradWeek5$high.school.agreggation <- as.factor(BioGradWeek5$high.school.agreggation)
BioGradWeek9$high.school.agreggation <- as.factor(BioGradWeek9$high.school.agreggation)
BioGradWeek13$high.school.agreggation <- as.factor(BioGradWeek13$high.school.agreggation)
BioGradFin$high.school.agreggation <- as.factor(BioGradFin$high.school.agreggation)

BioPosGrad$high.school.agreggation <- as.factor(BioPosGrad$high.school.agreggation)
BioPosGradWeek5$high.school.agreggation <- as.factor(BioPosGradWeek5$high.school.agreggation)
BioPosGradWeek9$high.school.agreggation <- as.factor(BioPosGradWeek9$high.school.agreggation)
BioPosGradWeek13$high.school.agreggation <- as.factor(BioPosGradWeek13$high.school.agreggation)
BioPosGradFin$high.school.agreggation <- as.factor(BioPosGradFin$high.school.agreggation)

BioTec$high.school.agreggation <- as.factor(BioTec$high.school.agreggation)
BioTecWeek5$high.school.agreggation <- as.factor(BioTecWeek5$high.school.agreggation)
BioTecWeek9$high.school.agreggation <- as.factor(BioTecWeek9$high.school.agreggation)
BioTecWeek13$high.school.agreggation <- as.factor(BioTecWeek13$high.school.agreggation)
BioTecFin$high.school.agreggation <- as.factor(BioTecFin$high.school.agreggation)

#forma.ingresso.agreggation
BioGrad$forma.ingresso.agreggation <- as.factor(BioGrad$forma.ingresso.agreggation)
BioGradWeek5$forma.ingresso.agreggation <- as.factor(BioGradWeek5$forma.ingresso.agreggation)
BioGradWeek9$forma.ingresso.agreggation <- as.factor(BioGradWeek9$forma.ingresso.agreggation)
BioGradWeek13$forma.ingresso.agreggation <- as.factor(BioGradWeek13$forma.ingresso.agreggation)
BioGradFin$forma.ingresso.agreggation <- as.factor(BioGradFin$forma.ingresso.agreggation)

BioTec$forma.ingresso.agreggation <- as.factor(BioTec$forma.ingresso.agreggation)
BioTecWeek5$forma.ingresso.agreggation <- as.factor(BioTecWeek5$forma.ingresso.agreggation)
BioTecWeek9$forma.ingresso.agreggation <- as.factor(BioTecWeek9$forma.ingresso.agreggation)
BioTecWeek13$forma.ingresso.agreggation <- as.factor(BioTecWeek13$forma.ingresso.agreggation)
BioTecFin$forma.ingresso.agreggation <- as.factor(BioTecFin$forma.ingresso.agreggation)

#faculty.agreggation
BioGrad$faculty.agreggation <- as.factor(BioGrad$faculty.agreggation)
BioGradWeek5$faculty.agreggation <- as.factor(BioGradWeek5$faculty.agreggation)
BioGradWeek9$faculty.agreggation <- as.factor(BioGradWeek9$faculty.agreggation)
BioGradWeek13$faculty.agreggation <- as.factor(BioGradWeek13$faculty.agreggation)
BioGradFin$faculty.agreggation <- as.factor(BioGradFin$faculty.agreggation)

BioPosGrad$faculty.agreggation <- as.factor(BioPosGrad$faculty.agreggation)
BioPosGradWeek5$faculty.agreggation <- as.factor(BioPosGradWeek5$faculty.agreggation)
BioPosGradWeek9$faculty.agreggation <- as.factor(BioPosGradWeek9$faculty.agreggation)
BioPosGradWeek13$faculty.agreggation <- as.factor(BioPosGradWeek13$faculty.agreggation)
BioPosGradFin$faculty.agreggation <- as.factor(BioPosGradFin$faculty.agreggation)

BioTec$faculty.agreggation <- as.factor(BioTec$faculty.agreggation)
BioTecWeek5$faculty.agreggation <- as.factor(BioTecWeek5$faculty.agreggation)
BioTecWeek9$faculty.agreggation <- as.factor(BioTecWeek9$faculty.agreggation)
BioTecWeek13$faculty.agreggation <- as.factor(BioTecWeek13$faculty.agreggation)
BioTecFin$faculty.agreggation <- as.factor(BioTecFin$faculty.agreggation)

PeakGrad$faculty.agreggation <- as.factor(PeakGrad$faculty.agreggation)
PeakGradWeek5$faculty.agreggation <- as.factor(PeakGradWeek5$faculty.agreggation)
PeakGradWeek9$faculty.agreggation <- as.factor(PeakGradWeek9$faculty.agreggation)
PeakGradWeek13$faculty.agreggation <- as.factor(PeakGradWeek13$faculty.agreggation)
PeakGradFin$faculty.agreggation <- as.factor(PeakGradFin$faculty.agreggation)

PeakPosGrad$faculty.agreggation <- as.factor(PeakPosGrad$faculty.agreggation)
PeakPosGradWeek5$faculty.agreggation <- as.factor(PeakPosGradWeek5$faculty.agreggation)
PeakPosGradWeek9$faculty.agreggation <- as.factor(PeakPosGradWeek9$faculty.agreggation)
PeakPosGradWeek13$faculty.agreggation <- as.factor(PeakPosGradWeek13$faculty.agreggation)
PeakPosGradFin$faculty.agreggation <- as.factor(PeakPosGradFin$faculty.agreggation)

PeakTec$faculty.agreggation <- as.factor(PeakTec$faculty.agreggation)
PeakTecWeek5$faculty.agreggation <- as.factor(PeakTecWeek5$faculty.agreggation)
PeakTecWeek9$faculty.agreggation <- as.factor(PeakTecWeek9$faculty.agreggation)
PeakTecWeek13$faculty.agreggation <- as.factor(PeakTecWeek13$faculty.agreggation)
PeakTecFin$faculty.agreggation <- as.factor(PeakTecFin$faculty.agreggation)

#first.year.cummulative.gpa
PeakGrad$first.year.cummulative.gpa <- as.numeric(PeakGrad$first.year.cummulative.gpa)
PeakGradWeek5$first.year.cummulative.gpa <- as.numeric(PeakGradWeek5$first.year.cummulative.gpa)
PeakGradWeek9$first.year.cummulative.gpa <- as.numeric(PeakGradWeek9$first.year.cummulative.gpa)
PeakGradWeek13$first.year.cummulative.gpa <- as.numeric(PeakGradWeek13$first.year.cummulative.gpa)
PeakGradFin$first.year.cummulative.gpa <- as.numeric(PeakGradFin$first.year.cummulative.gpa)

PeakPosGrad$first.year.cummulative.gpa <- as.numeric(PeakPosGrad$first.year.cummulative.gpa)
PeakPosGradWeek5$first.year.cummulative.gpa <- as.numeric(PeakPosGradWeek5$first.year.cummulative.gpa)
PeakPosGradWeek9$first.year.cummulative.gpa <- as.numeric(PeakPosGradWeek9$first.year.cummulative.gpa)
PeakPosGradWeek13$first.year.cummulative.gpa <- as.numeric(PeakPosGradWeek13$first.year.cummulative.gpa)
PeakPosGradFin$first.year.cummulative.gpa <- as.numeric(PeakPosGradFin$first.year.cummulative.gpa)

PeakTec$first.year.cummulative.gpa <- as.numeric(PeakTec$first.year.cummulative.gpa)
PeakTecWeek5$first.year.cummulative.gpa <- as.numeric(PeakTecWeek5$first.year.cummulative.gpa)
PeakTecWeek9$first.year.cummulative.gpa <- as.numeric(PeakTecWeek9$first.year.cummulative.gpa)
PeakTecWeek13$first.year.cummulative.gpa <- as.numeric(PeakTecWeek13$first.year.cummulative.gpa)
PeakTecFin$first.year.cummulative.gpa <- as.numeric(PeakTecFin$first.year.cummulative.gpa)

#cummulative.gpa.last
PeakGrad$cummulative.gpa.last <- as.numeric(PeakGrad$cummulative.gpa.last)
PeakGradWeek5$cummulative.gpa.last <- as.numeric(PeakGradWeek5$cummulative.gpa.last)
PeakGradWeek9$cummulative.gpa.last <- as.numeric(PeakGradWeek9$cummulative.gpa.last)
PeakGradWeek13$cummulative.gpa.last <- as.numeric(PeakGradWeek13$cummulative.gpa.last)
PeakGradFin$cummulative.gpa.last <- as.numeric(PeakGradFin$cummulative.gpa.last)

PeakPosGrad$cummulative.gpa.last <- as.numeric(PeakPosGrad$cummulative.gpa.last)
PeakPosGradWeek5$cummulative.gpa.last <- as.numeric(PeakPosGradWeek5$cummulative.gpa.last)
PeakPosGradWeek9$cummulative.gpa.last <- as.numeric(PeakPosGradWeek9$cummulative.gpa.last)
PeakPosGradWeek13$cummulative.gpa.last <- as.numeric(PeakPosGradWeek13$cummulative.gpa.last)
PeakPosGradFin$cummulative.gpa.last <- as.numeric(PeakPosGradFin$cummulative.gpa.last)

PeakTec$cummulative.gpa.last <- as.numeric(PeakTec$cummulative.gpa.last)
PeakTecWeek5$cummulative.gpa.last <- as.numeric(PeakTecWeek5$cummulative.gpa.last)
PeakTecWeek9$cummulative.gpa.last <- as.numeric(PeakTecWeek9$cummulative.gpa.last)
PeakTecWeek13$cummulative.gpa.last <- as.numeric(PeakTecWeek13$cummulative.gpa.last)
PeakTecFin$cummulative.gpa.last <- as.numeric(PeakTecFin$cummulative.gpa.last)

#academic.period.gpa.last
PeakGrad$academic.period.gpa.last <- as.numeric(PeakGrad$academic.period.gpa.last)
PeakGradWeek5$academic.period.gpa.last <- as.numeric(PeakGradWeek5$academic.period.gpa.last)
PeakGradWeek9$academic.period.gpa.last <- as.numeric(PeakGradWeek9$academic.period.gpa.last)
PeakGradWeek13$academic.period.gpa.last <- as.numeric(PeakGradWeek13$academic.period.gpa.last)
PeakGradFin$academic.period.gpa.last <- as.numeric(PeakGradFin$academic.period.gpa.last)

PeakPosGrad$academic.period.gpa.last <- as.numeric(PeakPosGrad$academic.period.gpa.last)
PeakPosGradWeek5$academic.period.gpa.last <- as.numeric(PeakPosGradWeek5$academic.period.gpa.last)
PeakPosGradWeek9$academic.period.gpa.last <- as.numeric(PeakPosGradWeek9$academic.period.gpa.last)
PeakPosGradWeek13$academic.period.gpa.last <- as.numeric(PeakPosGradWeek13$academic.period.gpa.last)
PeakPosGradFin$academic.period.gpa.last <- as.numeric(PeakPosGradFin$academic.period.gpa.last)

PeakTec$academic.period.gpa.last <- as.numeric(PeakTec$academic.period.gpa.last)
PeakTecWeek5$academic.period.gpa.last <- as.numeric(PeakTecWeek5$academic.period.gpa.last)
PeakTecWeek9$academic.period.gpa.last <- as.numeric(PeakTecWeek9$academic.period.gpa.last)
PeakTecWeek13$academic.period.gpa.last <- as.numeric(PeakTecWeek13$academic.period.gpa.last)
PeakTecFin$academic.period.gpa.last <- as.numeric(PeakTecFin$academic.period.gpa.last)

#enem.score
BioGrad$enem.score <- as.numeric(BioGrad$enem.score)
BioGradWeek5$enem.score <- as.numeric(BioGradWeek5$enem.score)
BioGradWeek9$enem.score <- as.numeric(BioGradWeek9$enem.score)
BioGradWeek13$enem.score <- as.numeric(BioGradWeek13$enem.score)
BioGradFin$enem.score <- as.numeric(BioGradFin$enem.score)

BioPosGrad$enem.score <- as.numeric(BioPosGrad$enem.score)
BioPosGradWeek5$enem.score <- as.numeric(BioPosGradWeek5$enem.score)
BioPosGradWeek9$enem.score <- as.numeric(BioPosGradWeek9$enem.score)
BioPosGradWeek13$enem.score <- as.numeric(BioPosGradWeek13$enem.score)
BioPosGradFin$enem.score <- as.numeric(BioPosGradFin$enem.score)

BioTec$enem.score <- as.numeric(BioTec$enem.score)
BioTecWeek5$enem.score <- as.numeric(BioTecWeek5$enem.score)
BioTecWeek9$enem.score <- as.numeric(BioTecWeek9$enem.score)
BioTecWeek13$enem.score <- as.numeric(BioTecWeek13$enem.score)
BioTecFin$enem.score <- as.numeric(BioTecFin$enem.score)

#vest.score
BioGrad$vest.score <- as.numeric(BioGrad$vest.score)
BioGradWeek5$vest.score <- as.numeric(BioGradWeek5$vest.score)
BioGradWeek9$vest.score <- as.numeric(BioGradWeek9$vest.score)
BioGradWeek13$vest.score <- as.numeric(BioGradWeek13$vest.score)
BioGradFin$vest.score <- as.numeric(BioGradFin$vest.score)

BioPosGrad$vest.score <- as.numeric(BioPosGrad$vest.score)
BioPosGradWeek5$vest.score <- as.numeric(BioPosGradWeek5$vest.score)
BioPosGradWeek9$vest.score <- as.numeric(BioPosGradWeek9$vest.score)
BioPosGradWeek13$vest.score <- as.numeric(BioPosGradWeek13$vest.score)
BioPosGradFin$vest.score <- as.numeric(BioPosGradFin$vest.score)

BioTec$vest.score <- as.numeric(BioTec$vest.score)
BioTecWeek5$vest.score <- as.numeric(BioTecWeek5$vest.score)
BioTecWeek9$vest.score <- as.numeric(BioTecWeek9$vest.score)
BioTecWeek13$vest.score <- as.numeric(BioTecWeek13$vest.score)
BioTecFin$vest.score <- as.numeric(BioTecFin$vest.score)

#stop.out.flag.1
BioGrad$stop.out.flag.1 <- as.factor(BioGrad$stop.out.flag.1)
BioGradWeek5$stop.out.flag.1 <- as.factor(BioGradWeek5$stop.out.flag.1)
BioGradWeek9$stop.out.flag.1 <- as.factor(BioGradWeek9$stop.out.flag.1)
BioGradWeek13$stop.out.flag.1 <- as.factor(BioGradWeek13$stop.out.flag.1)
BioGradFin$stop.out.flag.1 <- as.factor(BioGradFin$stop.out.flag.1)

BioPosGrad$stop.out.flag.1 <- as.factor(BioPosGrad$stop.out.flag.1)
BioPosGradWeek5$stop.out.flag.1 <- as.factor(BioPosGradWeek5$stop.out.flag.1)
BioPosGradWeek9$stop.out.flag.1 <- as.factor(BioPosGradWeek9$stop.out.flag.1)
BioPosGradWeek13$stop.out.flag.1 <- as.factor(BioPosGradWeek13$stop.out.flag.1)
BioPosGradFin$stop.out.flag.1 <- as.factor(BioPosGradFin$stop.out.flag.1)

BioTec$stop.out.flag.1 <- as.factor(BioTec$stop.out.flag.1)
BioTecWeek5$stop.out.flag.1 <- as.factor(BioTecWeek5$stop.out.flag.1)
BioTecWeek9$stop.out.flag.1 <- as.factor(BioTecWeek9$stop.out.flag.1)
BioTecWeek13$stop.out.flag.1 <- as.factor(BioTecWeek13$stop.out.flag.1)
BioTecFin$stop.out.flag.1 <- as.factor(BioTecFin$stop.out.flag.1)

PeakGrad$stop.out.flag.1 <- as.factor(PeakGrad$stop.out.flag.1)
PeakGradWeek5$stop.out.flag.1 <- as.factor(PeakGradWeek5$stop.out.flag.1)
PeakGradWeek9$stop.out.flag.1 <- as.factor(PeakGradWeek9$stop.out.flag.1)
PeakGradWeek13$stop.out.flag.1 <- as.factor(PeakGradWeek13$stop.out.flag.1)
PeakGradFin$stop.out.flag.1 <- as.factor(PeakGradFin$stop.out.flag.1)

PeakPosGrad$stop.out.flag.1 <- as.factor(PeakPosGrad$stop.out.flag.1)
PeakPosGradWeek5$stop.out.flag.1 <- as.factor(PeakPosGradWeek5$stop.out.flag.1)
PeakPosGradWeek9$stop.out.flag.1 <- as.factor(PeakPosGradWeek9$stop.out.flag.1)
PeakPosGradWeek13$stop.out.flag.1 <- as.factor(PeakPosGradWeek13$stop.out.flag.1)
PeakPosGradFin$stop.out.flag.1 <- as.factor(PeakPosGradFin$stop.out.flag.1)

PeakTec$stop.out.flag.1 <- as.factor(PeakTec$stop.out.flag.1)
PeakTecWeek5$stop.out.flag.1 <- as.factor(PeakTecWeek5$stop.out.flag.1)
PeakTecWeek9$stop.out.flag.1 <- as.factor(PeakTecWeek9$stop.out.flag.1)
PeakTecWeek13$stop.out.flag.1 <- as.factor(PeakTecWeek13$stop.out.flag.1)
PeakTecFin$stop.out.flag.1 <- as.factor(PeakTecFin$stop.out.flag.1)


######## scholarship.type
#
###########################

print("changing na values to scholarship")

BioGrad$scholarship.type <- mapply(ScholarshipType, BioGrad$scholarship.type)
BioGradWeek5$scholarship.type <- mapply(ScholarshipType, BioGradWeek5$scholarship.type)
BioGradWeek9$scholarship.type <- mapply(ScholarshipType, BioGradWeek9$scholarship.type)
BioGradWeek13$scholarship.type <- mapply(ScholarshipType, BioGradWeek13$scholarship.type)
BioGradFin$scholarship.type <- mapply(ScholarshipType, BioGradFin$scholarship.type)
PeakGrad$scholarship.type <- mapply(ScholarshipType, PeakGrad$scholarship.type)
PeakGradWeek5$scholarship.type <- mapply(ScholarshipType, PeakGradWeek5$scholarship.type)
PeakGradWeek9$scholarship.type <- mapply(ScholarshipType, PeakGradWeek9$scholarship.type)
PeakGradWeek13$scholarship.type <- mapply(ScholarshipType, PeakGradWeek13$scholarship.type)
PeakGradFin$scholarship.type <- mapply(ScholarshipType, PeakGradFin$scholarship.type)

BioPosGrad$scholarship.type <- mapply(ScholarshipType, BioPosGrad$scholarship.type)
BioPosGradWeek5$scholarship.type <- mapply(ScholarshipType, BioPosGradWeek5$scholarship.type)
BioPosGradWeek9$scholarship.type <- mapply(ScholarshipType, BioPosGradWeek9$scholarship.type)
BioPosGradWeek13$scholarship.type <- mapply(ScholarshipType, BioPosGradWeek13$scholarship.type)
BioPosGradFin$scholarship.type <- mapply(ScholarshipType, BioPosGradFin$scholarship.type)
PeakPosGrad$scholarship.type <- mapply(ScholarshipType, PeakPosGrad$scholarship.type)
PeakPosGradWeek5$scholarship.type <- mapply(ScholarshipType, PeakPosGradWeek5$scholarship.type)
PeakPosGradWeek9$scholarship.type <- mapply(ScholarshipType, PeakPosGradWeek9$scholarship.type)
PeakPosGradWeek13$scholarship.type <- mapply(ScholarshipType, PeakPosGradWeek13$scholarship.type)
PeakPosGradFin$scholarship.type <- mapply(ScholarshipType, PeakPosGradFin$scholarship.type)

BioTec$scholarship.type <- mapply(ScholarshipType, BioTec$scholarship.type)
BioTecWeek5$scholarship.type <- mapply(ScholarshipType, BioTecWeek5$scholarship.type)
BioTecWeek9$scholarship.type <- mapply(ScholarshipType, BioTecWeek9$scholarship.type)
BioTecWeek13$scholarship.type <- mapply(ScholarshipType, BioTecWeek13$scholarship.type)
BioTecFin$scholarship.type <- mapply(ScholarshipType, BioTecFin$scholarship.type)
PeakTec$scholarship.type <- mapply(ScholarshipType, PeakTec$scholarship.type)
PeakTecWeek5$scholarship.type <- mapply(ScholarshipType, PeakTecWeek5$scholarship.type)
PeakTecWeek9$scholarship.type <- mapply(ScholarshipType, PeakTecWeek9$scholarship.type)
PeakTecWeek13$scholarship.type <- mapply(ScholarshipType, PeakTecWeek13$scholarship.type)
PeakTecFin$scholarship.type <- mapply(ScholarshipType, PeakTecFin$scholarship.type)

######## Years to Enter (Novo)
#
###########################

print("calc happend years to enter")

BioGrad$years.to.enter <- mapply(yearsToEnter,BioGrad$academic.cycle,BioGrad$high.school.graduation.year)
BioGradWeek5$years.to.enter <- mapply(yearsToEnter,BioGradWeek5$academic.cycle,BioGradWeek5$high.school.graduation.year)
BioGradWeek9$years.to.enter <- mapply(yearsToEnter,BioGradWeek9$academic.cycle,BioGradWeek9$high.school.graduation.year)
BioGradWeek13$years.to.enter <- mapply(yearsToEnter,BioGradWeek13$academic.cycle,BioGradWeek13$high.school.graduation.year)
BioGradFin$years.to.enter <- mapply(yearsToEnter,BioGradFin$academic.cycle,BioGradFin$high.school.graduation.year)
BioGrad <- BioGrad[,!(names(BioGrad)%in%c("high.school.graduation.year"))]
BioGradWeek5 <- BioGradWeek5[,!(names(BioGradWeek5)%in%c("high.school.graduation.year"))]
BioGradWeek9 <- BioGradWeek9[,!(names(BioGradWeek9)%in%c("high.school.graduation.year"))]
BioGradWeek13 <- BioGradWeek13[,!(names(BioGradWeek13)%in%c("high.school.graduation.year"))]
BioGradFin <- BioGradFin[,!(names(BioGradFin)%in%c("high.school.graduation.year"))]

BioPosGrad$years.to.enter <- mapply(yearsToEnter,BioPosGrad$academic.cycle,BioPosGrad$high.school.graduation.year)
BioPosGradWeek5$years.to.enter <- mapply(yearsToEnter,BioPosGradWeek5$academic.cycle,BioPosGradWeek5$high.school.graduation.year)
BioPosGradWeek9$years.to.enter <- mapply(yearsToEnter,BioPosGradWeek9$academic.cycle,BioPosGradWeek9$high.school.graduation.year)
BioPosGradWeek13$years.to.enter <- mapply(yearsToEnter,BioPosGradWeek13$academic.cycle,BioPosGradWeek13$high.school.graduation.year)
BioPosGradFin$years.to.enter <- mapply(yearsToEnter,BioPosGradFin$academic.cycle,BioPosGradFin$high.school.graduation.year)
BioPosGrad <- BioPosGrad[,!(names(BioPosGrad)%in%c("high.school.graduation.year"))]
BioPosGradWeek5 <- BioPosGradWeek5[,!(names(BioPosGradWeek5)%in%c("high.school.graduation.year"))]
BioPosGradWeek9 <- BioPosGradWeek9[,!(names(BioPosGradWeek9)%in%c("high.school.graduation.year"))]
BioPosGradWeek13 <- BioPosGradWeek13[,!(names(BioPosGradWeek13)%in%c("high.school.graduation.year"))]
BioPosGradFin <- BioPosGradFin[,!(names(BioPosGradFin)%in%c("high.school.graduation.year"))]

BioTec$years.to.enter <- mapply(yearsToEnter,BioTec$academic.cycle,BioTec$high.school.graduation.year)
BioTecWeek5$years.to.enter <- mapply(yearsToEnter,BioTecWeek5$academic.cycle,BioTecWeek5$high.school.graduation.year)
BioTecWeek9$years.to.enter <- mapply(yearsToEnter,BioTecWeek9$academic.cycle,BioTecWeek9$high.school.graduation.year)
BioTecWeek13$years.to.enter <- mapply(yearsToEnter,BioTecWeek13$academic.cycle,BioTecWeek13$high.school.graduation.year)
BioTecFin$years.to.enter <- mapply(yearsToEnter,BioTecFin$academic.cycle,BioTecFin$high.school.graduation.year)
BioTec <- BioTec[,!(names(BioTec)%in%c("high.school.graduation.year"))]
BioTecWeek5 <- BioTecWeek5[,!(names(BioTecWeek5)%in%c("high.school.graduation.year"))]
BioTecWeek9 <- BioTecWeek9[,!(names(BioTecWeek9)%in%c("high.school.graduation.year"))]
BioTecWeek13 <- BioTecWeek13[,!(names(BioTecWeek13)%in%c("high.school.graduation.year"))]
BioTecFin <- BioTecFin[,!(names(BioTecFin)%in%c("high.school.graduation.year"))]

### Drop fields on week 13 and Fim

BioGradFin$academic.period.gpa <- as.numeric(BioGradFin$academic.period.gpa)
BioGradFin$failed.courses <- as.numeric(BioGradFin$failed.courses)
PeakGradFin$academic.period.gpa <- as.numeric(PeakGradFin$academic.period.gpa)
PeakGradFin$failed.courses <- as.numeric(PeakGradFin$failed.courses)

BioPosGradFin$academic.period.gpa <- as.numeric(BioPosGradFin$academic.period.gpa)
BioPosGradFin$failed.courses <- as.numeric(BioPosGradFin$failed.courses)
PeakPosGradFin$academic.period.gpa <- as.numeric(PeakPosGradFin$academic.period.gpa)
PeakPosGradFin$failed.courses <- as.numeric(PeakPosGradFin$failed.courses)

BioTecFin$academic.period.gpa <- as.numeric(BioTecFin$academic.period.gpa)
BioTecFin$failed.courses <- as.numeric(BioTecFin$failed.courses)
PeakTecFin$academic.period.gpa <- as.numeric(PeakTecFin$academic.period.gpa)
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

PeakGradFin <- PeakGradFin[,!(names(PeakGradFin)%in%drop.to.fim)]
PeakPosGradFin <- PeakPosGradFin[,!(names(PeakPosGradFin)%in%drop.to.fim)]
PeakTecFin <- PeakTecFin[,!(names(PeakTecFin)%in%drop.to.fim)]

drop.to.fim <- c(
  "notas"
)

BioGradFin <- BioGradFin[,!(names(BioGradFin)%in%drop.to.fim)]
BioPosGradFin <- BioPosGradFin[,!(names(BioPosGradFin)%in%drop.to.fim)]
BioTecFin <- BioTecFin[,!(names(BioTecFin)%in%drop.to.fim)]

###NANs

print("NaNs treatment")

BioGrad[is.na(BioGrad)] <- 0
BioGradWeek5[is.na(BioGradWeek5)] <- 0
BioGradWeek9[is.na(BioGradWeek9)] <- 0
BioGradWeek13[is.na(BioGradWeek13)] <- 0
BioGradFin[is.na(BioGradFin)] <- 0
PeakGrad[is.na(PeakGrad)] <- 0
PeakGradWeek5[is.na(PeakGradWeek5)] <- 0
PeakGradWeek9[is.na(PeakGradWeek9)] <- 0
PeakGradWeek13[is.na(PeakGradWeek13)] <- 0
PeakGradFin[is.na(PeakGradFin)] <- 0

BioPosGrad[is.na(BioPosGrad)] <- 0
BioPosGradWeek5[is.na(BioPosGradWeek5)] <- 0
BioPosGradWeek9[is.na(BioPosGradWeek9)] <- 0
BioPosGradWeek13[is.na(BioPosGradWeek13)] <- 0
BioPosGradFin[is.na(BioPosGradFin)] <- 0
PeakPosGrad[is.na(PeakPosGrad)] <- 0
PeakPosGradWeek5[is.na(PeakPosGradWeek5)] <- 0
PeakPosGradWeek9[is.na(PeakPosGradWeek9)] <- 0
PeakPosGradWeek13[is.na(PeakPosGradWeek13)] <- 0
PeakPosGradFin[is.na(PeakPosGradFin)] <- 0

BioTec[is.na(BioTec)] <- 0
BioTecWeek5[is.na(BioTecWeek5)] <- 0
BioTecWeek9[is.na(BioTecWeek9)] <- 0
BioTecWeek13[is.na(BioTecWeek13)] <- 0
BioTecFin[is.na(BioTecFin)] <- 0
PeakTec[is.na(PeakTec)] <- 0
PeakTecWeek5[is.na(PeakTecWeek5)] <- 0
PeakTecWeek9[is.na(PeakTecWeek9)] <- 0
PeakTecWeek13[is.na(PeakTecWeek13)] <- 0
PeakTecFin[is.na(PeakTecFin)] <- 0

### prom GPA last and cummulative

#Grad
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


#PosGrad
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

#Tec
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

###NANs

print("NaNs treatment")

PeakGradWeek13[PeakGradWeek13$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0
PeakGradWeek9[PeakGradWeek9$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0
PeakGradWeek5[PeakGradWeek5$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0
PeakGrad[PeakGrad$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0

PeakPosGradWeek13[PeakPosGradWeek13$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0
PeakPosGradWeek9[PeakPosGradWeek9$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0
PeakPosGradWeek5[PeakPosGradWeek5$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0
PeakPosGrad[PeakPosGrad$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0

PeakTecWeek13[PeakTecWeek13$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0
PeakTecWeek9[PeakTecWeek9$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0
PeakTecWeek5[PeakTecWeek5$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0
PeakTec[PeakTec$cummulative.credits.aggregation == "Inf","cummulative.credits.aggregation"] <- 0

###NANs

print("NaNs treatment")

BioGrad[is.na(BioGrad)] <- 0
BioGradWeek5[is.na(BioGradWeek5)] <- 0
BioGradWeek9[is.na(BioGradWeek9)] <- 0
BioGradWeek13[is.na(BioGradWeek13)] <- 0
BioGradFin[is.na(BioGradFin)] <- 0
PeakGrad[is.na(PeakGrad)] <- 0
PeakGradWeek5[is.na(PeakGradWeek5)] <- 0
PeakGradWeek9[is.na(PeakGradWeek9)] <- 0
PeakGradWeek13[is.na(PeakGradWeek13)] <- 0
PeakGradFin[is.na(PeakGradFin)] <- 0

BioPosGrad[is.na(BioPosGrad)] <- 0
BioPosGradWeek5[is.na(BioPosGradWeek5)] <- 0
BioPosGradWeek9[is.na(BioPosGradWeek9)] <- 0
BioPosGradWeek13[is.na(BioPosGradWeek13)] <- 0
BioPosGradFin[is.na(BioPosGradFin)] <- 0
PeakPosGrad[is.na(PeakPosGrad)] <- 0
PeakPosGradWeek5[is.na(PeakPosGradWeek5)] <- 0
PeakPosGradWeek9[is.na(PeakPosGradWeek9)] <- 0
PeakPosGradWeek13[is.na(PeakPosGradWeek13)] <- 0
PeakPosGradFin[is.na(PeakPosGradFin)] <- 0

BioTec[is.na(BioTec)] <- 0
BioTecWeek5[is.na(BioTecWeek5)] <- 0
BioTecWeek9[is.na(BioTecWeek9)] <- 0
BioTecWeek13[is.na(BioTecWeek13)] <- 0
BioTecFin[is.na(BioTecFin)] <- 0
PeakTec[is.na(PeakTec)] <- 0
PeakTecWeek5[is.na(PeakTecWeek5)] <- 0
PeakTecWeek9[is.na(PeakTecWeek9)] <- 0
PeakTecWeek13[is.na(PeakTecWeek13)] <- 0
PeakTecFin[is.na(PeakTecFin)] <- 0

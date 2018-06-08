rm("all.data.set")

##### Functions #######

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
  ifelse(startsWith(toupper(trim(city)),"MOSS"),"MOSSORO",
      ifelse(startsWith(toupper(trim(city)),"NAT"),"NATAL","OUTRAS"))}

StateTransformation <- function(state){
  group_1 <- c("RIO GRANDE DO NORTE","RN")
  if( trim(state) %in% group_1  ){"RIO GRANDE DO NORTE"}else{"OUTROS"}
    }

HighSchoolTransformation <- function(highschool){
  highschool <- toupper(iconv(as.character(highschool), to = "ASCII//TRANSLIT"))
  ifelse(startsWith(trim(highschool),"EE") | startsWith(trim(highschool),"E E") | startsWith(trim(highschool),"ESCOLA ESTADUAL"),"ESCOLA ESTADUAL",
    ifelse(startsWith(trim(highschool),"SEM DEFIN") | startsWith(trim(highschool),"UNIVERSIDADE"),"SEM DEFINICAO",
      ifelse(startsWith(trim(highschool),"COLEGIO"),"COLEGIO",
        ifelse(startsWith(trim(highschool),"INSTITUTO"),"INSTITUTO",
         "OUTRAS"))))}

HighSchoolToPosGradTransformation <- function(highschool){
ifelse(startsWith(toupper(trim(highschool)),"UNIVERSIDADE POTIGUAR"),"LIU OUTCOMES","OTHER OUTCOMES")}

FormaIngressoTransformation <- function(formaingresso){
  formaingresso <- toupper(iconv(as.character(unlist(formaingresso)), to = "ASCII//TRANSLIT"))
  ifelse(startsWith(toupper(trim(formaingresso)),"ENEM"),"ENEM",
         ifelse(startsWith(toupper(trim(formaingresso)),"VESTI"),"VESTIBULAR","OUTRAS"))}

SchoolTransformation <- function(school){
  school <- toupper(iconv(as.character(gsub("ÇA ","ÇAO",school)), to = "ASCII//TRANSLIT"))
  ifelse(
    regexpr(pattern = '-',school) < 0,
    trim(as.character(school)),
    trim(substr(school,1,regexpr(pattern = '-',school)-1)))
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
  if(week >= 5){
    dp3 <- if (is.na(dp3)) (week - 4) * days.on.week * -1 else dp3
    if( dp3 < 0 ){
      result <- dp3 / days.on.week / weeks.on.month
      penalty3 <- ceiling(result * -1) * penalty.p3
    }  
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
  p1 <- as.numeric(p1)
  p2 <- as.numeric(p2)
  p3 <- as.numeric(p3)
  p4 <- as.numeric(p4)
  p5 <- as.numeric(p5)
  p6 <- as.numeric(p6)
  totalbalance <- as.numeric(totalbalance)

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

fast.validation <- function(x){
  if(is.na(x))
    x <- 0

  if(x==0)
    "Zero"
  else
    "No Zero"
}

#### Dropping zeros #####

print("Removing missing values for total.creditos...")

for(d in ls()[startsWith(ls(),"df.")]){
  assign(d,get(d)[complete.cases(get(d)[names(get(d)) == "total.creditos"]),])}

#### city and State transformations ####

print("Starting city and state aggregation...")
remove(list=ls(pattern = "tmp"))

for(d in ls()[startsWith(ls(),"df.")]){
  if(nrow(get(d))>0){
  df.tmp <- get(d)
  df.tmp$city.aggregation <- as.data.frame(mapply(CityTransformation,df.tmp$city))
  colnames(df.tmp$city.aggregation) <- "city.aggregation"
  assign(d,df.tmp)}
}

for(d in ls()[startsWith(ls(),"df.")]){
  if(nrow(get(d))>0){
  df.tmp <- get(d)
  df.tmp$state.aggregation <- as.data.frame(mapply(StateTransformation,df.tmp$state))
  assign(d,df.tmp)}
}

#### High school transformations ####

print("Starting feeder school aggregation...")
remove(list=ls(pattern = "tmp"))

for (d in ls(pattern = "df.Bio")){
  if(nrow(get(d))>0){
  df.tmp <- get(d)
  df.tmp$high.school.aggregation <- as.data.frame(mapply(HighSchoolTransformation,df.tmp$high.school))
  assign(d,df.tmp)
}}

print("Starting postgrad outcomes aggregation...")
remove(list=ls(pattern = "tmp"))

for(d in ls(pattern = "Bio.Pos")){
  if(nrow(get(d))>0){
  df.tmp <- get(d)
  df.tmp$high.school.aggregation <- as.data.frame(mapply(HighSchoolToPosGradTransformation,df.tmp$high.school))
  assign(d,df.tmp)
  }
}

#### Forma Ingresso transformations ####

drop.rows <- c("EXTENSAO","INTERCAMBIO/LAUREATE","ANALISE DE CURRICULO","VESTIBULAR SIA",
  "PROCESSO SELETIVO","VESTIBULAR TIA","CONCESSAO DE VAGA")

for (d in ls(pattern = "df.Bio")){
  
  df.tmp <- get(d)
  df.tmp$forma.ingresso.aggregation <- as.data.frame(mapply(FormaIngressoTransformation,df.tmp$forma.ingresso))
  assign(d,df.tmp[!df.tmp$forma.ingresso %in% drop.rows,])

}

### Type of Scholarship transformation ###

print("changing na values to scholarship")
remove(list=ls(pattern = "tmp"))

for (d in ls(pattern = "df.")){
  if(nrow(get(d))>0){
  df.tmp <- get(d)
  df.tmp$scholarship.type <- as.data.frame(mapply(ScholarshipType, df.tmp$scholarship.type))
  assign(d,df.tmp)
}}


### Faculty ###
print("Standardizing school information...")
remove(list=ls(pattern = "tmp"))

for(d in ls(pattern = "df.")){
  if(nrow(get(d))>0){
  df.tmp <- get(d)
  df.tmp$faculty <- as.data.frame(mapply(SchoolTransformation,df.tmp$faculty))
  assign(d,df.tmp)
  }}


### Years to enter ###

print("calc happend years to enter")
remove(list=ls(pattern = "tmp"))

for (d in ls(pattern = "df.Bio")){
  df.tmp <- get(d)
  df.tmp$years.to.enter <- as.data.frame(mapply(yearsToEnter,
                                                df.tmp$academic.cycle,
                                                df.tmp$high.school.graduation.year))
  assign(d,df.tmp)
}


### Creditos ###

# tmp1 <- c()
# for (d in ls(pattern = "df.")){
#   tmp0 <- cbind(names(get(d)),d)
#   tmp1 <- rbind(tmp0,tmp1)
# 
# }
#   tmp1[startsWith(tmp1[,1],"num.disciplinas"),]


print("Calculating new credit variable...")

for(d in ls(pattern = "df.")){
  if(nrow(get(d))>0){
  tmp <- as.data.frame(mapply(CreditosTransformation,
                              as.numeric(unlist(get(d)[startsWith(names(get(d)),"total.creditos")])),
                              as.numeric(unlist(get(d)[startsWith(names(get(d)),"num.disciplinas")]))))
  colnames(tmp) <- c("credits.aggregation")
  assign(d,cbind(get(d),tmp))}}


### Others NAs  ###

remove(list=ls(pattern = "tmp"))

for (d in ls(pattern = "df.Bio.Gra")){
  if(nrow(get(d))>0){
  df.tmp <- get(d)
  df.tmp[df.tmp$other.financial.aid.flag == "00/01/1900",c("other.financial.aid.flag")] <- as.factor("0")
  assign(d,df.tmp)
}}

### Day Payment ###

print("Calculating new payment engagement...")

for (f in c("df.Bio","df.Peak")){
  for (d in degrees){
    for (w in weeks){
      as.numeric(get(paste0(f,".",d,".S",w))$days.payment.1)
      as.numeric(get(paste0(f,".",d,".S",w))$days.payment.2)
      as.numeric(get(paste0(f,".",d,".S",w))$days.payment.3)
      as.numeric(get(paste0(f,".",d,".S",w))$days.payment.4)
      as.numeric(get(paste0(f,".",d,".S",w))$days.payment.5)
      as.numeric(get(paste0(f,".",d,".S",w))$days.payment.6)
      as.numeric(get(paste0(f,".",d,".S",w))$payment.amount.1)
      as.numeric(get(paste0(f,".",d,".S",w))$payment.amount.2)
      as.numeric(get(paste0(f,".",d,".S",w))$payment.amount.3)
      as.numeric(get(paste0(f,".",d,".S",w))$payment.amount.4)
      as.numeric(get(paste0(f,".",d,".S",w))$payment.amount.5)
      as.numeric(get(paste0(f,".",d,".S",w))$payment.amount.6)
      as.numeric(get(paste0(f,".",d,".S",w))$total.balance)
    }}}


for (f in c("df.Bio","df.Peak")){
  for (d in degrees){
    for (w in weeks){
      if(nrow(get(paste0(f,".",d,".S",w)))>0){
        tmp <- as.data.frame(mapply(PayEngagement
                                      ,as.numeric(get(paste0(f,".",d,".S",w))$days.payment.1)
                                      ,as.numeric(get(paste0(f,".",d,".S",w))$days.payment.2)
                                      ,as.numeric(get(paste0(f,".",d,".S",w))$days.payment.3)
                                      ,as.numeric(get(paste0(f,".",d,".S",w))$days.payment.4)
                                      ,as.numeric(get(paste0(f,".",d,".S",w))$days.payment.5)
                                      ,as.numeric(get(paste0(f,".",d,".S",w))$days.payment.6)
                                      ,1))
        colnames(tmp) <- c("pay.engagement")
        assign(paste0(f,".",d,".S",w),cbind(get(paste0(f,".",d,".S",w)),tmp))
      }
    }
  }
}


### Payment amount ###

print("Calculating payment balance...")

for (f in c("df.Bio","df.Peak")){
  for (d in degrees){
    for (w in weeks){
      if(nrow(get(paste0(f,".",d,".S",w)))>0){
        tmp <- as.data.frame(mapply(PaymentBalance
                                    ,as.numeric(get(paste0(f,".",d,".S",w))$payment.amount.1)
                                    ,as.numeric(get(paste0(f,".",d,".S",w))$payment.amount.2)
                                    ,as.numeric(get(paste0(f,".",d,".S",w))$payment.amount.3)
                                    ,as.numeric(get(paste0(f,".",d,".S",w))$payment.amount.4)
                                    ,as.numeric(get(paste0(f,".",d,".S",w))$payment.amount.5)
                                    ,as.numeric(get(paste0(f,".",d,".S",w))$payment.amount.6)
                                    ,as.numeric(get(paste0(f,".",d,".S",w))$total.balance)))
        colnames(tmp) <- c("payment.balance")
        assign(paste0(f,".",d,".S",w),cbind(get(paste0(f,".",d,".S",w)),tmp))
      }
    }
  }
}



### Duplicated cases ###

print("Remove duplicated cases...")

remove(list=ls(pattern = "tmp"))

for(d in ls(pattern = "df.")){
  if(nrow(get(d))>0){  
  df.tmp <- get(d)
  df.tmp.id <- as.data.frame(table(paste0(df.tmp$student.id,".",df.tmp$academic.cycle)))
  duplicated.id <- as.character(df.tmp.id[df.tmp.id$Freq > 1,1])
  row.names(df.tmp) <- c(paste0(df.tmp$student.id,".",df.tmp$academic.cycle))
  df.tmp <- df.tmp[!row.names(df.tmp) %in% duplicated.id,]
  }
}

remove(list=ls(pattern = "tmp"))

##########################################
### Criacao do dicionario de variaveis ###
##########################################

cols <- c()
for (n in ls(pattern = "df.")){cols <- c(cols,n)}
dic <- as.data.frame(matrix(c(""), nrow = 0, ncol = 2))

tmp1 <- c()
for(d in ls(pattern = "df.")){
  tmp0 <- as.data.frame(names(get(d)))
  tmp1 <- rbind(tmp1,tmp0)
  
}

dic <- rbind(cbind(as.data.frame(unique(tmp1[,1])),as.data.frame("")))
colnames(dic) <- c("vars", "type")

for(d in ls(pattern = "df.")){
  tmp2 <- as.data.frame(table(names(get(d))))
  colnames(tmp2) <- c("vars", d)
  
  dic <- merge(dic, tmp2, by = "vars", all.x = TRUE)
  
}

for(d in ls(pattern = "df.")){dic[d] <- ifelse(is.na(dic[d]),0,1)}


write.csv2(dic,"dictionary.csv",row.names = F)

dic$type <- ifelse(dic$vars %in% 
                     c(
                       "days.payment.1"
                       ,"days.payment.2"
                       ,"days.payment.3"
                       ,"days.payment.4"
                       ,"days.payment.5"
                       ,"days.payment.6"
                       ,"payment.amount.1"
                       ,"payment.amount.2"
                       ,"payment.amount.3"
                       ,"payment.amount.4"
                       ,"payment.amount.5"
                       ,"payment.amount.6"
                       ,"payment.balance"
                       ,"num.academic.cycles"
                       ,"num.academic.cycles.continuing"
                       ,"num.disciplinas.matriculado"
                       ,"cummulative.credits.attempted.last"
                       ,"cummulative.credits.earned.last"
                       ,"cummulative.gpa.last"
                       ,"first.year.cummulative.gpa"
                       ,"academic.period.gpa.last"
                       ,"age"
                       ,"enrollment.days"
                       ,"enrollment.year"
                       ,"failed.courses.last"
                       ,"frequencia"
                       ,"notas"
                       ,"pay.engagement"
                       ,"scholarship.months"
                       ,"total.balance"
                       ,"credits.aggregation"
                       ,"failed.courses"
                       ,"final.mean"
                       ,"mean.sucesso.final"
                       ,"mean.sucesso.local"
                       ,"num.disciplinas"
                       ,"sum.bin.recup"
                       ,"total.creditos"
                       ,"total.disciplinas"
                       ,"total.creditos.matriculado"
                       ,"academic.period.gpa"
                       ,"years.to.enter"
                       ,"enem.score"
                       ,"high.school.graduation.year"
                       ,"vest.score"
                       ), "numeric","factor")

### Mudar o tipo da variavel ###

print("Changing the type of the variables...")
remove(list=ls(pattern = "tmp"))

for(d in ls(pattern = "df.")){
  
  df.tmp <- get(d)

  for (i in dic[dic[names(dic) == d]== 1,"vars"]){
    
    ifelse(dic[dic$vars == i,2] == "numeric", 
           df.tmp[i] <- as.data.frame(as.numeric(unlist(df.tmp[,names(df.tmp) == i]))),
           df.tmp[i] <- as.data.frame(as.factor(unlist(df.tmp[,names(df.tmp) == i]))))
    
    ifelse(is.na(df.tmp[i]),0,df.tmp[i])
    
  }
  
  assign(paste0(d),df.tmp)
  
}

remove(list=ls(pattern = "tmp"))

### Mean GPA last and cummulative

print("Calculating mean GPA last and cummulative...")

for (d in ls(pattern = "df.Peak")){
  
  df.tmp <- get(d)
  df.tmp$gpa.cummulative.avg <- round((df.tmp$cummulative.gpa.last + df.tmp$academic.period.gpa.last)/2,2)
  df.tmp$cred.cummulative.avg <- round((df.tmp$cummulative.credits.earned.last +df.tmp$cummulative.credits.attempted.last)/2,2)
  df.tmp$cummulative.credits.aggregation <- round(df.tmp$cred.cummulative.avg/df.tmp$gpa.cummulative.avg,2)
  ifelse(df.tmp$cummulative.credits.aggregation == "Inf",
         df.tmp$cummulative.credits.aggregation <- 0,
         df.tmp$cummulative.credits.aggregation)
  
  assign(d,df.tmp)
  
}


###########################################################
### Limpeza de variaveis desnecessarias pelo dicionario ###
###########################################################

print("Removing unnecessary variables...")

remove(list=ls(pattern = "tmp"))

for (d in ls(pattern = "df")){
  if (nrow(get(d)) > 0){
    df.tmp <- get(d)
    rownames(df.tmp) <- paste0(df.tmp$student.id,".",df.tmp$academic.cycle)
    assign(d,df.tmp)
  }
}


remove(list=ls(pattern = "tmp"))

drop.to.bio <- c("student.id","degree","semestre","semana","peak.date","zipcode","class.start.date",
    "enrollment.date","withdrawal.type","campus","scholarship.flag","degree.level")

drop.to.peak <- c("student.id","degree","semestre","semana","peak.date","block","zipcode","enrollment.cohort",
    "enrollment.year","class.start.date","enrollment.date","withdrawal.type","campus","scholarship.flag",
"degree.level","gpa.cummulative.prom","credits.cummulative.prom","cummulative.gpa.last","academic.period.gpa.last",
"cummulative.credits.earned.last","cummulative.credits.attempted.last")

drop.to.fim <- c("first.year.cummulative.gpa","cummulative.gpa.last","academic.period.gpa.last","failed.courses.last",
  "cummulative.credits.earned.last","cummulative.credits.attempted.last","notas")

drop.to.all <- c("city","state","high.school","forma.ingresso","program.of.study")

dic[dic$vars %in% drop.to.fim,endsWith(names(dic),"Fin")] <- 0
dic[dic$vars %in% drop.to.all, startsWith(names(dic),"df.")] <- 0
dic[dic$vars %in% drop.to.bio,startsWith(names(dic),"df.Bio")] <- 0
dic[dic$vars %in% drop.to.peak,startsWith(names(dic),"df.Peak")] <- 0
dic[dic$vars %in% c("notas","frequencia"),endsWith(names(dic),c("S5","S9"))] <- 0
dic[startsWith(as.character(dic$vars),"days.payment"), startsWith(names(dic),"df.")] <- 0
dic[startsWith(as.character(dic$vars),"payment.amount"), startsWith(names(dic),"df.")] <- 0


print("Dropping unused columns")

for (d in ls(pattern = "df.")){
  assign(d,
         get(d)[names(get(d)) %in% dic[dic[names(dic) == d]==1,1]])}


remove(bin.recup)
remove(sucesso.final)
remove(sucesso.local)


print("Removing NAs in all variables...")

for (d in ls(pattern = "df.")){
  if(nrow(get(d))>0){
    for (n in names(get(d))){
      df.tmp <- get(d)
      df.tmp[is.na(df.tmp[n]),n] <- 0
      assign(d,df.tmp)
    }
  }
}


#####################################
#### Rename Dataframes to models ####
#####################################


grads <- c("Gra","Pos","Tec")
kinds <- c("Bio","Peak")
models <- c("Ini","S5","S9","S13","Fin")

for (a in ls(pattern = "df")){
  for (k in kinds){
    for(g in grads){
      for (m in models){
        assign(paste0(k,gsub("Pos","PosGrad",gsub("Gra","Grad",g)),gsub("Ini","",gsub("S","Week",m)))
                      ,get(paste0("df.",k,".",g,".",m)))
      }}}}
          
rm(list=ls(pattern = "df."))
        

########################################
### Remove new levels of information ###
########################################

# models.list <- c(ls(pattern = "Bio"),ls(pattern = "Peak"))
# 
# for (m in models.list){
#   
#   if(table(get(m)["marital.status"])["OUTRO"] > 0)
#   {
#     df.tmp <- get(m)
#     assign(m,df.tmp[df.tmp["marital.status"]!="OUTRO"])}
#   
# }

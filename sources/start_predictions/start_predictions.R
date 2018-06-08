getOriginDataSet <- function(dataset){
  my.dataset <- get(dataset)
  my.dataset}


getOriginalStudentID <- function(string){
  string <- trim(string)
  substring(string,1,nchar(string)-7)
}

general.risk <- function(probability){
  risk <- ""
  if( probability <= 0.25 ){
    risk <- "GREEN"
  }else if(probability > 0.25 & probability <= 0.5){
    risk <- "YELLOW"
  }else if(probability > 0.5 & probability <= 0.75){
    risk <- "ORANGE"
  }else{
    risk <- "RED"
  }
  risk
}

AddDeciles <- function(data.set){
  
  data.set$risk_decile <-  -1
  
  data.set <- data.set[order(-data.set$probs),]
  nrow(data.set)/10
  
  size <- round(nrow(data.set)/10)
  
  for (decile in 1:10) {
    if(decile != 10){
      this.rows <- row.names(head(data.set[data.set$risk_decile == -1,],size))
      data.set[rownames(data.set)%in%this.rows,"risk_decile"] <- decile
      nrow(data.set)
      table(data.set$risk_decile)
    }else{
      data.set[data.set$risk_decile == -1,"risk_decile"] <- decile
    }
    
  }
  
  data.set
}


dic <- read.csv2("dictionary.csv",header = T, sep = ";")

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


models.list <- c(ls(pattern = "Bio"),ls(pattern = "Peak"))
colnames(dic) <- c("vars","type",models.list)

for(d in models.list){
  
  df.tmp <- as.data.frame(get(d))
  
  for (i in dic[dic[names(dic) == d]== 1,"vars"]){
    
    ifelse(dic[dic$vars == i,2] == "numeric", 
           df.tmp[i] <- as.data.frame(as.numeric(unlist(df.tmp[,names(df.tmp) == i]))),
           df.tmp[i] <- as.data.frame(as.factor(unlist(df.tmp[,names(df.tmp) == i]))))
    
    ifelse(is.na(df.tmp[i]),0,df.tmp[i])
    
  }
  
  assign(d,df.tmp)
  
}


#############################

my.env <- environment()


grads <- c("Grad","PosGrad","Tec")
kinds <- c("Bio","Peak")


for (grad in grads) {
  for (kind in kinds) {
  
    
    
    ###### check which model should to use ####
    #                                         #
    ###########################################
    
    
    
    ##### Load Data ####
    #                  #
    ####################
    
    ##### Choose model #####
    string <- paste0(kind,grad)
    
    try.with <- c("Week5","Week9","Week13","Fin")
    model <- ""
    for (i.want.try.with in try.with) {
      
      try.now <- as.data.frame(get(paste0(string,i.want.try.with)))#,envir = my.env))
      if(nrow(try.now)>1)
        model <- i.want.try.with
    }
    
    string <- paste0(string,model)
    
    #######################
    print(paste0("predicting for ",string))
    
    
    model_text <- paste0("model_",string)
    if(need_model_update(model_text)){
      download_unp_model(model_text,"models",".RData")
      update_model(model_text,"0")
      send.slack.notification.model.downloaded(model_text)
    }
    
    load(paste0("models_gbm/",model_text,".RData"))
    
    test <- get(string)#, envir = my.env)
    
    #### cast variables ####
    
    no.count <- c()
    for(m.var in gbmFit$coefnames){
      if( nrow(as.data.frame(table(grepl(m.var, gbmFit$coefnames[!(gbmFit$coefnames%in%m.var)] )))) > 1 & !(m.var == "age")   )
        no.count <- c(no.count,m.var)
    }
    
    
#     int.vars <- names(test)[names(test)%in%gbmFit$coefnames]
#     for (int.var in int.vars[!(int.vars%in%no.count)] ) {
#       test[,int.var] <- as.numeric(test[,int.var])
#     }
#     
#     factor.variable <- c("other.financial.aid.flag","worker.flag","fies.flag","scholarship.months")
#     for (coln in colnames(test)) {
#       for (fv in factor.variable) {
#         if(coln == fv & !(string %in% c("PeakGrad","PeakGradWeek5","PeakGradWeek9","PeakGradWeek13","PeakGradFin",
#                                      "PeakPosGrad","PeakPosGradWeek5","PeakPosGradWeek9","PeakPosGradWeek13","PeakPosGradFin",
#                                      "PeakTec","PeakTecWeek5","PeakTecWeek9","PeakTecWeek13","PeakTecFin")) ){
#           test[,coln] <- as.factor(test[,coln])
#         }
#       }
#     }
#     
#     factor.variable <- c("other.financial.aid.flag","worker.flag","fies.flag","prouni.flag","scholarship.months")
#     for (coln in colnames(test)) {
#       for (fv in factor.variable) {
#         if(coln == fv & !(string %in% c("PeakGrad","PeakGradWeek5","PeakGradWeek9","PeakGradWeek13","PeakGradFin",
#                                      "PeakPosGrad","PeakPosGradWeek5","PeakPosGradWeek9","PeakPosGradWeek13","PeakPosGradFin",
#                                      "PeakTec","PeakTecWeek5","PeakTecWeek9","PeakTecWeek13","PeakTecFin")) ){
#           test[,coln] <- as.factor(test[,coln])
#         }
#       }
#     }
# 	
# 	factor.variable <- c("frequencia","other.financial.aid.flag")
#     for (coln in colnames(test)) {
#       for (fv in factor.variable) {
#         if(coln == fv){
#           test[,coln] <- as.numeric(test[,coln])
#         }
#       }
#     }
#     
#     
#     
    #### Predictions        ####
    #                          #
    ############################
    
    continue <- TRUE 
    if( sum(test$block %in% c("")) > 0 ){
      test <- test[!(test$block %in% c("")),]
      test$block <- as.character(test$block)
      test$block <- as.factor(test$block)
    }
	
	#In this case, I'm removing the person who have "Outro" as it's marital status, because it wasn't predicted by the model.	
	
	  if( sum(test$marital.status %in% c("OUTRO") ) > 0 ){
	    
	    test <- test[!(test$marital.status == "OUTRO"),]
	    test$marital.status <- as.character(test$marital.status)
	    test$marital.status <- as.factor(test$marital.status)
	    
	  }
	
	  
    
    predictions.prob <- predict(gbmFit,test,type="response")
    
    
    if(continue){
      
      #general risk
      test$probs <- predictions.prob
      test$general_risk <- mapply(general.risk, test$probs)
      
      #######################
      
      
      test <- AddDeciles(test)  
      
      
      ############# Features ##############
      
      model <- gbmFit
      test2 <- test
      test2 <- test2[, !( names(test2) %in% c("1","2","risk_decile","stop.out.flag.1") ) ]
      
      
      
      BaseCase <-  subset(test2, probs == min(test2$probs))
      BaseCase <- BaseCase[, !( names(BaseCase) %in% c("probs") ) ]
      
      x <- varImp(model)
      x$feature <- rownames(x)
      rownames(x) <- 1:nrow(x)
      x <- x[order(-x$Overall),]
      importances <- c()
      for( xx in x$feature ){
        for(i in names(test2)){
          if(grepl(paste0("xx",i),paste0("xx",xx))){
            importances <- c(importances, i)
          }
        }
      }
      importances <- unique(importances)
      
      for(use in importances){
        test2[,paste0(use,"_base")] <- BaseCase[,use]
        if(!is.na(BaseCase[,use])){test2[ test2[,use] != BaseCase[,use], use ] <- BaseCase[,use]}
        
        new.prob <- predict(model,test2,type = "response")
        test2$new.prob <- new.prob
        
        test2[, paste0(use,"_input")] <- round( test2$probs - test2$new.prob ,4)
        test2$probs <- test2$new.prob
        
      }
      
      
      
      for(use in names(test2)){
        if(!grepl("_input",use)){
          test2 <- test2[, !(names(test2) %in% c(use)) ]
        }
      }
      
      test2$probability <- test$probs
      test2$student <- rownames(test2)
      rownames(test2) <- 1:nrow(test2)
      
      importances <- c()
      for(f in names(test2)[ !(names(test2) %in% c("student","probability") )  ] ){
        tmp <- test2[,names(test2) %in% c("student",f) ]
        tmp$feature <- f
        names(tmp) <- c("input","student","feature")
        importances <- rbind(importances,tmp)
      }
      
      
      final_importances <- 
        ddply(.data = importances, 
              .variables = ~student, 
              .fun = function(x){
                
                x <- x[order(-x$input),]
                
                data.frame(student = as.character(unique(x$student)), 
                           fact1 = as.character(x$feature[1]),
                           fact2 = as.character(x$feature[2]),
                           fact3 = as.character(x$feature[3]),
                           fact4 = as.character(x$feature[4]))
                
              })

      test$student <- rownames(test)
      rownames(test) <- 1:nrow(test)
      
      export <- merge(test,final_importances, by=c("student"))
      head(export)
      
      #write.csv(export, file=paste0(string,"export.csv"))
      write.csv(test2, file=paste0("outputs/",string,"ImpFeatures.csv"))
      
      ####### Send Notification ##########
      #                                  #
      ####################################
      
      test <- export
      test$student.id <- mapply(getOriginalStudentID, test$student)
      row.names(test) <- 1:nrow(test)
      test <- test[,!(names(test)%in%c("1","2","student"))]
      
      complete.data.set <- getOriginDataSet(string)
      complete.data.set$student.id <- getOriginalStudentID(rownames(complete.data.set))
      add.fields <- names(test)[!(names(test)%in%names(complete.data.set))]
      add.fields <- c("student.id",add.fields)
      test.sample <- test[,add.fields]
      test.all.fields <- merge(complete.data.set,test.sample,by = c("student.id"))
	  
      test.all.fields$semana <- current.week[current.week$degree == grad,"max.semana"]
      test$semana <- current.week[current.week$degree == grad,"max.semana"]
	  
		  model.processed <- paste0("Week-",
		            current.week[current.week$degree == grad,"max.semana"],
								"-",
								if(kind=="Bio") "Novo" else "Veteran",
								"-For-",
								grad)
	  
		  semesters <- semesters[!is.na(semesters)]
		  write.csv2(test, file = paste0("outputs/",model.processed ,semesters,".csv") )
      write.csv2(test.all.fields, file = paste0("outputs/",model.processed ,"_extended_version_",semesters,".csv") )
      
      summary_ <- as.data.frame(round(prop.table(table(test$general_risk))*100,2))
      
      red 	 <- ifelse(length(subset(summary_,Var1=="RED")$Freq) == 0, 0,  subset(summary_,Var1=="RED")$Freq)
	    orange <- ifelse(length(subset(summary_,Var1=="ORANGE")$Freq) == 0, 0,  subset(summary_,Var1=="ORANGE")$Freq)
	    yellow <- ifelse(length(subset(summary_,Var1=="YELLOW")$Freq) == 0, 0,  subset(summary_,Var1=="YELLOW")$Freq)
	    green	 <- ifelse(length(subset(summary_,Var1=="GREEN")$Freq) == 0, 0,  subset(summary_,Var1=="GREEN")$Freq)
	  

      if(!dev){


        send.email.model.completed(model.processed,
                                    nrow(test),
                                    red,
								                    orange,
								                    yellow,
								                    green)
      }




      send.slack.model.completed(model.processed,
                                  nrow(test),
                                  red,
								                  orange,
								                  yellow,
								                  green)
      
    }
    
    
  }
}

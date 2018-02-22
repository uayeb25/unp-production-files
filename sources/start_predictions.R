library(caret)
library(plyr)


getOriginDataSet <- function(dataset.name){
  
  if(dataset.name == "BioGrad")
    my.dataset <- all.data.set@BioGrad
  if(dataset.name == "BioGradWeek5")
    my.dataset <- all.data.set@BioGradWeek5
  if(dataset.name == "BioGradWeek9")
    my.dataset <- all.data.set@BioGradWeek9
  if(dataset.name == "BioGradWeek13")
    my.dataset <- all.data.set@BioGradWeek13
  if(dataset.name == "BioGradFin")
    my.dataset <- all.data.set@BioGradFin
  if(dataset.name == "PeakGrad")
    my.dataset <- all.data.set@PeakGrad
  if(dataset.name == "PeakGradWeek5")
    my.dataset <- all.data.set@PeakGradWeek5
  if(dataset.name == "PeakGradWeek9")
    my.dataset <- all.data.set@PeakGradWeek9
  if(dataset.name == "PeakGradWeek13")
    my.dataset <- all.data.set@PeakGradWeek13
  if(dataset.name == "PeakGradFin")
    my.dataset <- all.data.set@PeakGradFin
  if(dataset.name == "BioPosGrad")
    my.dataset <- all.data.set@BioPosGrad
  if(dataset.name == "BioPosGradWeek5")
    my.dataset <- all.data.set@BioPosGradWeek5
  if(dataset.name == "BioPosGradWeek9")
    my.dataset <- all.data.set@BioPosGradWeek9
  if(dataset.name == "BioPosGradWeek13")
    my.dataset <- all.data.set@BioPosGradWeek13
  if(dataset.name == "BioPosGradFin")
    my.dataset <- all.data.set@BioPosGradFin
  if(dataset.name == "PeakPosGrad")
    my.dataset <- all.data.set@PeakPosGrad
  if(dataset.name == "PeakPosGradWeek5")
    my.dataset <- all.data.set@PeakPosGradWeek5
  if(dataset.name == "PeakPosGradWeek9")
    my.dataset <- all.data.set@PeakPosGradWeek9
  if(dataset.name == "PeakPosGradWeek13")
    my.dataset <- all.data.set@PeakPosGradWeek13
  if(dataset.name == "PeakPosGradFin")
    my.dataset <- all.data.set@PeakPosGradFin
  if(dataset.name == "BioTec")
    my.dataset <- all.data.set@BioTec
  if(dataset.name == "BioTecWeek5")
    my.dataset <- all.data.set@BioTecWeek5
  if(dataset.name == "BioTecWeek9")
    my.dataset <- all.data.set@BioTecWeek9
  if(dataset.name == "BioTecWeek13")
    my.dataset <- all.data.set@BioTecWeek13
  if(dataset.name == "BioTecFin")
    my.dataset <- all.data.set@BioTecFin
  if(dataset.name == "PeakTec")
    my.dataset <- all.data.set@PeakTec
  if(dataset.name == "PeakTecWeek5")
    my.dataset <- all.data.set@PeakTecWeek5
  if(dataset.name == "PeakTecWeek9")
    my.dataset <- all.data.set@PeakTecWeek9
  if(dataset.name == "PeakTecWeek13")
    my.dataset <- all.data.set@PeakTecWeek13
  if(dataset.name == "PeakTecFin")
    my.dataset <- all.data.set@PeakTecFin
  
  my.dataset
  
}

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
    str <- paste0(kind,grad)
    
    try.with <- c("Week5","Week9","Week13","Fin")
    model <- ""
    for (i.want.try.with in try.with) {
      
      try.now <- get(paste0(str,i.want.try.with),envir = my.env)
      if(nrow(try.now)>1)
        model <- i.want.try.with
    }
    
    str <- paste0(str,model)
    
    #######################
    print(paste0("predicting for ",str))
    
    
    model_text <- paste0("model_",str)
    if( need_model_update(model_text)){
      download_unp_model(model_text,"models",".RData")
      update_model(model_text,"0")
      send.slack.notification.model.downloaded(model_text)
    }
    
    load(paste0("models_gbm/",model_text,".RData"))
    test <- get(str, envir = my.env)
    
    
    #### cast variables ####
    
    no.count <- c()
    for(m.var in gbmFit$coefnames){
      if( nrow(as.data.frame(table(grepl(m.var, gbmFit$coefnames[!(gbmFit$coefnames%in%m.var)] )))) > 1 & !(m.var == "age")   )
        no.count <- c(no.count,m.var)
    }
    
    
    int.vars <- names(test)[names(test)%in%gbmFit$coefnames]
    for (int.var in int.vars[!(int.vars%in%no.count)] ) {
      test[,int.var] <- as.numeric(test[,int.var])
    }
    
    factor.variable <- c("other.financial.aid.flag","worker.flag","fies.flag","scholarship.months")
    for (coln in colnames(test)) {
      for (fv in factor.variable) {
        if(coln == fv & !(str %in% c("PeakGrad","PeakGradWeek5","PeakGradWeek9","PeakGradWeek13","PeakGradFin",
                                     "PeakPosGrad","PeakPosGradWeek5","PeakPosGradWeek9","PeakPosGradWeek13","PeakPosGradFin",
                                     "PeakTec","PeakTecWeek5","PeakTecWeek9","PeakTecWeek13","PeakTecFin")) ){
          test[,coln] <- as.factor(test[,coln])
        }
      }
    }
    
    factor.variable <- c("other.financial.aid.flag","worker.flag","fies.flag","prouni.flag","scholarship.months")
    for (coln in colnames(test)) {
      for (fv in factor.variable) {
        if(coln == fv & !(str %in% c("PeakGrad","PeakGradWeek5","PeakGradWeek9","PeakGradWeek13","PeakGradFin",
                                     "PeakPosGrad","PeakPosGradWeek5","PeakPosGradWeek9","PeakPosGradWeek13","PeakPosGradFin",
                                     "PeakTec","PeakTecWeek5","PeakTecWeek9","PeakTecWeek13","PeakTecFin")) ){
          test[,coln] <- as.factor(test[,coln])
        }
      }
    }
	
	factor.variable <- c("frequencia","other.financial.aid.flag")
    for (coln in colnames(test)) {
      for (fv in factor.variable) {
        if(coln == fv){
          test[,coln] <- as.numeric(test[,coln])
        }
      }
    }
    
    
    
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
	
	
	if( sum(test$schedule %in% c("DIURNO") ) > 0 && grad == "Tec"  ){
	  
	  test <- test[!(test$schedule == "DIURNO"),]
	  test$schedule <- as.character(test$schedule)
	  test$schedule <- as.factor(test$schedule)
	  
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
        test2[ test2[,use] != BaseCase[,use], use ] <- BaseCase[,use]
        
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
      
      
      #write.csv(export, file=paste0(str,"export.csv"))
      write.csv(test2, file=paste0("outputs/",str,"ImpFeatures.csv"))
      
      ####### Send Notification ##########
      #                                  #
      ####################################
      
      test <- export
      test$student.id <- mapply(getOriginalStudentID, test$student)
      row.names(test) <- 1:nrow(test)
      test <- test[,!(names(test)%in%c("1","2","student"))]
      
      complete.data.set <- getOriginDataSet(str)
      add.fields <- names(test)[!(names(test)%in%names(complete.data.set))]
      add.fields <- c("student.id",add.fields)
      test.sample <- test[,add.fields]
      test.all.fields <- merge(complete.data.set,test.sample,by = c("student.id"))
	  
	 
      
      test.all.fields$semana <- as.character(unique(subset(main.data.frames@AllWeek, grado == grad)$semana))
      test$semana <- as.character(unique(subset(main.data.frames@AllWeek, grado == grad)$semana))
	  
		model.processed <- paste0("Week-",
								as.character(unique(subset(main.data.frames@AllWeek, grado == grad)$semana)),
								"-",
								if(kind=="Bio") "Novo" else "Veteran",
								"-For-",
								grad)
	  
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


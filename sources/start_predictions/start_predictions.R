library(caret)


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

general.risk <- function(probability,cutoff1,cutoff2){
  risk <- ""
  if( probability <= cutoff1 ){
    risk <- "LOW"
  }else if(probability > cutoff1 & probability <= cutoff2){
    risk <- "MEDIUM"
  }else{
    risk <- "HIGH"
  }
  risk
}


validate.risk <- function(val,kind){

  my.env <- environment()

  if(kind == "academic")
    rules <- get("academic.rules" ,envir =  my.env)
  else
    rules <- get("finance.rules", envir = my.env)
  
  val <- as.numeric(val)
  rules$inf.limit <- as.numeric(rules$inf.limit)
  rules$max.limit <- as.numeric(rules$max.limit)

  risk <- "LOW"
  for (i in 1:nrow(rules)) {
    rule <- rules[i,]
    if( val >= rule$inf.limit & val <= rule$max.limit ){
      if(i==1)
        risk <- "HIGH"
      else
        risk <- "MEDIUM"
    }
  }
  risk
}

AddDeciles <- function(data.set,prediccion){
  
  prediccion <- data.frame(prediccion)
  names(prediccion) <- c("1","2")
  data.set <- cbind(data.set,prediccion)
  
  data.set$risk_decile <-  -1
  
  data.set <- data.set[order(-data.set$'2'),]
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


### Load rules and cutoff ###


if(need_model_update("metrics") ){
  download_unp_model("metrics","models",".RData")
  update_model("metrics","0")
  send.slack.notification.model.downloaded("metrics")
}
load("models_gbm/metrics.RData")

if(need_model_update("clusterRules")){
  download_unp_model("clusterRules","models",".RData")
  update_model("clusterRules","0")
  send.slack.notification.model.downloaded("clusterRules")
}
load("models_gbm/clusterRules.RData")

#############################

my.env <- environment()


grads <- c("Grad","Tec")
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

    

    #### Predictions        ####
    #                          #
    ############################
    
    continue <- TRUE
    
    test$other.financial.aid.flag <- as.numeric(test$other.financial.aid.flag)
    
    tryToPredict <- tryCatch({
      predictions.prob <- predict(gbmFit,test,type="prob")
    }, warning = function(war){
      if(!dev)
        send.email.message("We have detect an error, a support admin will contact with you!","UNP")
      continue <- FALSE
    } ,error = function(err){
      if(!dev)
        send.email.message("We have detect an error, a support admin will contact with you!","UNP")
      continue <- FALSE
    })
    
    
    
    if(continue){
      
      base <- max(predictions.prob[,2]) + 0.01
      new_base <- .95
      predictions.prob[,2] <- (predictions.prob[,2]*new_base)/base
      
      
      cut.off <- models.metrics[ models.metrics$model == str ,]$cut.off[1]
      
      ##ordenamos nuestra liesta
      to.order <- predictions.prob[,2]
      ordered <- to.order[order(-to.order)]
      
      ##calculamos porcentaje historico de high
      percent_drop_out <- models.metrics[ models.metrics$model == str ,]$percent_drop_out[1]
      first.limit <- round(length(ordered)*percent_drop_out)
      cutoff2 <- tail(head(ordered,first.limit),1)
      
      ##calculamos porcentaje historico de medium
      medium <- models.metrics[ models.metrics$model == str ,]$medium[1]
      new.med <- medium + percent_drop_out
      second.limit <- round(length(ordered)*new.med)
      cutoff1 <- tail(head(ordered,second.limit),1)
      
      #prediction class
      new.predict.class <-  rep(0, length(predictions.prob[,2]))
      new.predict.class[predictions.prob[,2] >= cutoff2] <- 1
      new.predict.class <- as.factor(new.predict.class)
      test$prediction <- new.predict.class
      
      #general risk
      test$probs <- predictions.prob[,2]
      test$general_risk <- mapply(general.risk, test$probs, cutoff1, cutoff2)
      
      #print(table(test$prediction))
      
      
      ### Sub Set ###
      #             #
      ###############
      
      students.drop.out <- subset(test, prediction == 1)
      
      ### Cluster treatment      ###
      #                            #
      ##############################
      
      my.rules <- subset(clusterRules, model == str)
      
      #Academic
      academic.rules <- subset(my.rules, category == "academic")
      academic.rules <- academic.rules[order(-academic.rules$prob),]
      academic.variable <- as.character(unique(academic.rules$variable))
      test$academic.risk <- mapply(validate.risk, test[,academic.variable],"academic")
      test$academic.variable <- academic.variable
      
      #finance
      finance.rules <- subset(my.rules, category == "finance")
      finance.rules <- finance.rules[order(-finance.rules$prob),]
      finance.variable <- as.character(unique(finance.rules$variable))
      test$finance.risk <- mapply(validate.risk, test[,finance.variable],"finance")
      test$finance.variable <- finance.variable
      
      #######################
      
      test[test$finance.risk == "HIGH" & test$general_risk != "HIGH","finance.risk"] <-  "MEDIUM"
      test[test$academic.risk == "HIGH" & test$general_risk != "HIGH","academic.risk"] <-  "MEDIUM"
      
      test[test$prediction == 0,"finance.risk"] <- "LOW"
      test[test$prediction == 0,"finance.variable"] <- ""
      
      test[test$prediction == 0,"academic.risk"] <- "LOW"
      test[test$prediction == 0,"academic.variable"] <- ""
      
      #######################
      
      tryAddDecil <- tryCatch({
        test <- AddDeciles(test,predictions.prob)  
      }, warning = function(war){
        if(!dev)
          send.email.message(paste0("Error adding decil risk on ",str),"UNP")
        
        test$risk_decile <- 1
      }, error = function(err){
        if(!dev)
          send.email.message(paste0("Error adding decil risk on ",str),"UNP")
        
        test$risk_decile <- 1
      })
      
      ####### Send Notification ##########
      #                                  #
      ####################################
      
      University <- "UnP"
      model.processed <- paste0("Week-",
                                as.character(unique(subset(main.data.frames@AllWeek, grado == grad)$semana)),
                                "-",
                                if(kind=="Bio") "Novo" else "Veteran",
                                "-For-",
                                grad)
      high.finance <- as.character(round(subset(as.data.frame(table(test$finance.risk)), Var1 == "HIGH")$Freq/nrow(test),2)*100)
      medium.finance <- as.character(round(subset(as.data.frame(table(test$finance.risk)), Var1 == "MEDIUM")$Freq/nrow(test),2)*100)
      high.academic <- as.character(round(subset(as.data.frame(table(test$academic.risk)), Var1 == "HIGH")$Freq/nrow(test),2)*100)
      medium.academic <- as.character(round(subset(as.data.frame(table(test$academic.risk)), Var1 == "MEDIUM")$Freq/nrow(test),2)*100)
      risk <- as.character(round(subset(as.data.frame(table(test$general_risk)),Var1 == "HIGH")$Freq/nrow(test),2)*100)
      safe <- as.character(round(subset(as.data.frame(table(test$general_risk)),Var1 == "MEDIUM")$Freq/nrow(test),2)*100)
      
      high.finance <- if(length(high.finance)==0) "0" else high.finance
      medium.finance <- if(length(medium.finance)==0) "0" else medium.finance
      high.academic <- if(length(high.academic)==0) "0" else high.academic
      medium.academic <- if(length(medium.academic)==0) "0" else medium.academic
      risk <- if(length(risk)==0) "0" else risk
      safe <- if(length(safe)==0) "0" else safe
      
      print("sending notification")
      if(!dev)
        send.email.model.completed(University,model.processed,high.finance,medium.finance,high.academic,medium.academic,risk,safe)
      
      send.slack.model.completed(University,model.processed,high.finance,medium.finance,high.academic,medium.academic,risk,safe)  
      
      #### Printing output
      print("making output")
      
      
      test$student.id <- mapply(getOriginalStudentID, row.names(test))
      row.names(test) <- 1:nrow(test)
      test <- test[,!(names(test)%in%c("1","2"))]
      
      complete.data.set <- getOriginDataSet(str)
      add.fields <- names(test)[!(names(test)%in%names(complete.data.set))]
      add.fields <- c("student.id",add.fields)
      test.sample <- test[,add.fields]
      test.all.fields <- merge(complete.data.set,test.sample,by = c("student.id"))
      
      

      making.output <- tryCatch({
        test.all.fields$semana <- as.character(unique(subset(main.data.frames@AllWeek, grado == grad)$semana))
        test$semana <- as.character(unique(subset(main.data.frames@AllWeek, grado == grad)$semana))
        write.csv2(test, file = paste0("outputs/",model.processed,semesters,".csv") )
        write.csv2(test.all.fields, file = paste0("outputs/",model.processed,"_extended_version_",semesters,".csv") )  
      },warning = function(war){
        if(!dev)
          send.email.message("PLEASE CLOSE ANY CONNECTION WITH THE OUTPUTS RIGH NOW WE ARE TRYING TO GENERATE","UNP")
      }, error = function(err){
        if(!dev)
          send.email.message("PLEASE CLOSE ANY CONNECTION WITH THE OUTPUTS RIGH NOW WE ARE TRYING TO GENERATE","UNP")
      })
      
    }


  }
}



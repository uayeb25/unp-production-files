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

general.risk <- function(probability,cutoff){
  risk <- ""
  if( probability < cutoff ){
    risk <- "LOW"
  }else if(probability >= cutoff & probability < .9){
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

  q10 <- quantile(data.set[,"2"]
                  ,probs = seq(0,1,by = 0.1)
  )

  if(length(unique(q10)) < 11){
    u <- length(unique(q10))
    for(i in 2:(u-1)){
      if(as.logical(q10[i] == q10[i-1])){
        q10[i] <- (q10[i-1]+q10[i+1])/2
      }
    }
  }

  data.set$risk_decile <- cut(data.set[,"2"]
                              ,breaks = q10
                              ,include.lowest = T
                              ,labels = 10:1)

  data.set$risk_decile <- factor(as.character(data.set$risk_decile)
                                 ,levels = c('1','2','3','4','5','6',
                                             '7','8','9','10'))
  data.set
}


### Load rules and cutoff ###


if(need_model_update("metrics")){
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
    if( need_model_update(model_text) ){
      download_unp_model(model_text,"models",".RData")
      update_model(model_text,"0")
      send.slack.notification.model.downloaded(model_text)
    }

    load(paste0("models_gbm/",model_text,".RData"))
    test <- get(str, envir = my.env)

    factor.variable <- c("other.financial.aid.flag")
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

    predictions.prob <- predict(gbmFit,test,type="prob")
    test <- AddDeciles(test,predictions.prob)

    cut.off <- models.metrics[ models.metrics$model == str ,]$cut.off[1]

    new.predict.class <-  rep(0, length(predictions.prob[,2]))
    new.predict.class[predictions.prob[,2] >= cut.off] <- 1
    new.predict.class <- as.factor(new.predict.class)

    test$prediction <- new.predict.class
    test$probs <- predictions.prob[,2]
    test$general_risk <- mapply(general.risk, test$probs, cut.off)

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

    test[test$prediction == 0,"academic.risk"] <- "LOW"
    test[test$prediction == 0,"academic.variable"] <- ""

    #finance
    finance.rules <- subset(my.rules, category == "finance")
    finance.rules <- finance.rules[order(-finance.rules$prob),]
    finance.variable <- as.character(unique(finance.rules$variable))
    test$finance.risk <- mapply(validate.risk, test[,finance.variable],"finance")
    test$finance.variable <- finance.variable

    test[test$prediction == 0,"finance.risk"] <- "LOW"
    test[test$prediction == 0,"finance.variable"] <- ""

    ####### Send Notification ##########
    #                                  #
    ####################################

    University <- "UnP"
    model.processed <- paste0("Week-",
                              as.character(unique(main.data.frames@AllWeek$semana)),
                              "-",
                              if(kind=="Bio") "Novo" else "Veteran",
                              "-For-",
                              grad)
    high.finance <- as.character(subset(as.data.frame(table(test$finance.risk)), Var1 == "HIGH")$Freq)
    medium.finance <- as.character(subset(as.data.frame(table(test$finance.risk)), Var1 == "MEDIUM")$Freq)
    high.academic <- as.character(subset(as.data.frame(table(test$academic.risk)), Var1 == "HIGH")$Freq)
    medium.academic <- as.character(subset(as.data.frame(table(test$academic.risk)), Var1 == "MEDIUM")$Freq)
    risk <- as.character(subset(as.data.frame(table(test$prediction)),Var1 == 1)$Freq)
    safe <- as.character(subset(as.data.frame(table(test$prediction)),Var1 == 0)$Freq)

    high.finance <- if(length(high.finance)==0) "0" else high.finance
    medium.finance <- if(length(medium.finance)==0) "0" else medium.finance
    high.academic <- if(length(high.academic)==0) "0" else high.academic
    medium.academic <- if(length(medium.academic)==0) "0" else medium.academic
    risk <- if(length(risk)==0) "0" else risk
    safe <- if(length(safe)==0) "0" else safe

    print("sending notification")
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
    
    
    
    write.csv2(test, file = paste0("outputs/",model.processed,semesters,".csv") )
    write.csv2(test.all.fields, file = paste0("outputs/",model.processed,"_extended_version_",semesters,".csv") )



  }
}

library(caret)



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
models <- get("Week", envir = my.env)
kinds <- c("Bio","Peak")



for (grad in grads) {
  for (kind in kinds) {

      
     
    ###### check which model should to use ####
    #                                         #
    ###########################################
    
     
      
    ##### Load Data ####
    #                  #
    ####################
    
    
    str <- paste0(kind,grad,model)
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
    model <- str
    high.finance <- as.character(subset(as.data.frame(table(test$finance.risk)), Var1 == "HIGH")$Freq)
    medium.finance <- as.character(subset(as.data.frame(table(test$finance.risk)), Var1 == "MEDIUM")$Freq)
    high.academic <- as.character(subset(as.data.frame(table(test$academic.risk)), Var1 == "HIGH")$Freq)
    medium.academic <- as.character(subset(as.data.frame(table(test$academic.risk)), Var1 == "MEDIUM")$Freq)
    risk <- as.character(subset(as.data.frame(table(test$prediction)),Var1 == 1)$Freq)
    safe <- as.character(subset(as.data.frame(table(test$prediction)),Var1 == 0)$Freq)
    
    print("sending notification")
    send.email.model.completed(University,model,high.finance,medium.finance,high.academic,medium.academic,risk,safe)
    
    #### Printing output 
    print("making output")
    write.csv2(test, file = paste0("outputs/",str,".csv") )
      
      
    
  }
}
library(dplyr)
library(plyr)



###  GRAD DATASETS ####

SplitDataSet <- function(semesters,main.data.frames){


  AllFim <- main.data.frames@AllFim
  AllWeek <- main.data.frames@AllWeek
  AllBio <- main.data.frames@AllBio
  AllPeak <- main.data.frames@AllPeak

  semesters <-  unique(AllFim$academic.cycle)

  all.data.set <- new("AllDataSet",
                      BioGrad = as.data.frame(c()),
                      BioGradWeek5 = as.data.frame(c()),
                      BioGradWeek9 = as.data.frame(c()),
                      BioGradWeek13 = as.data.frame(c()),
                      BioGradFin = as.data.frame(c()),
                      PeakGrad = as.data.frame(c()),
                      PeakGradWeek5 = as.data.frame(c()),
                      PeakGradWeek9 = as.data.frame(c()),
                      PeakGradWeek13 = as.data.frame(c()),
                      PeakGradFin = as.data.frame(c()),

                      BioPosGrad = as.data.frame(c()),
                      BioPosGradWeek5 = as.data.frame(c()),
                      BioPosGradWeek9 = as.data.frame(c()),
                      BioPosGradWeek13 = as.data.frame(c()),
                      BioPosGradFin = as.data.frame(c()),
                      PeakPosGrad = as.data.frame(c()),
                      PeakPosGradWeek5 = as.data.frame(c()),
                      PeakPosGradWeek9 = as.data.frame(c()),
                      PeakPosGradWeek13 = as.data.frame(c()),
                      PeakPosGradFin = as.data.frame(c()),

                      BioTec = as.data.frame(c()),
                      BioTecWeek5 = as.data.frame(c()),
                      BioTecWeek9 = as.data.frame(c()),
                      BioTecWeek13 = as.data.frame(c()),
                      BioTecFin = as.data.frame(c()),
                      PeakTec = as.data.frame(c()),
                      PeakTecWeek5 = as.data.frame(c()),
                      PeakTecWeek9 = as.data.frame(c()),
                      PeakTecWeek13 = as.data.frame(c()),
                      PeakTecFin = as.data.frame(c())
  )


  my.grado <- "Grad"
  Fim <- subset(AllFim, grado == my.grado)
  Fim <- Fim[,c("student.id","academic.cycle","stop.out.flag.1","grado","academic.period.gpa","failed.courses")]

  BioWeeks <- c()
  for(semester in semesters){
    BioGrad <- subset(AllBio, grado == my.grado & academic.cycle == semester)
    BioWeek <- subset(AllWeek, student.id %in% BioGrad$student.id & grado == my.grado & academic.cycle == semester )
    BioWeeks <- rbind(BioWeeks,BioWeek)
  }

  nFin <- nrow(Fim)
  BioWeeks$semana <- mapply(setWeek,nFin,BioWeeks$semana)

  BioGrad <- subset(AllBio, grado == my.grado)
  #BioGradWeek1 <- subset(BioWeeks, semana == 0)
  BioGradWeek5 <- subset(BioWeeks, semana == 5)
  BioGradWeek9 <- subset(BioWeeks, semana == 9)
  BioGradWeek13 <- subset(BioWeeks, semana == 13)
  BioGradFin <- subset(BioWeeks, semana == 19)
  
  #BioGradWeek1 <- merge(BioGradWeek1,BioGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado","block"))
  BioGradWeek5 <- merge(BioGradWeek5,BioGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado","block"))
  BioGradWeek9 <- merge(BioGradWeek9,BioGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado","block"))
  BioGradWeek13 <- merge(BioGradWeek13,BioGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado","block"))
  BioGradFin <- merge(BioGradFin,BioGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado","block"))
  BioGradFin <- merge(BioGradFin,Fim, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))


  PeakWeeks <- c()
  for(semester in semesters){
    PeakGrad <- subset(AllPeak, grado == my.grado & academic.cycle == semester)
    PeakWeek <- subset(AllWeek, student.id %in% PeakGrad$student.id & grado == my.grado & academic.cycle == semester )
    PeakWeeks <- rbind(PeakWeeks,PeakWeek)
  }

  nFin <- nrow(Fim)
  PeakWeeks$semana <- mapply(setWeek,nFin,PeakWeeks$semana)

  PeakGrad <- subset(AllPeak, grado == my.grado)
  #PeakGradWeek1 <- subset(PeakWeeks, semana == 0)
  PeakGradWeek5 <- subset(PeakWeeks, semana == 5)
  PeakGradWeek9 <- subset(PeakWeeks, semana == 9)
  PeakGradWeek13 <- subset(PeakWeeks, semana == 13)
  PeakGradFin <- subset(PeakWeeks, semana == 19)

  #PeakGradWeek1 <- merge(PeakGradWeek1,PeakGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))
  PeakGradWeek5 <- merge(PeakGradWeek5,PeakGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))
  PeakGradWeek9 <- merge(PeakGradWeek9,PeakGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))
  PeakGradWeek13 <- merge(PeakGradWeek13,PeakGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))
  PeakGradFin <- merge(PeakGradFin,PeakGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))
  PeakGradFin <- merge(PeakGradFin,Fim, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))

  ###  Tec DATASETS ####

  my.grado <- "Tec"

  Fim <- subset(AllFim, grado == my.grado)
  Fim <- Fim[,c("student.id","academic.cycle","stop.out.flag.1","grado","academic.period.gpa","failed.courses")]

  BioWeeks <- c()
  for(semester in semesters){
    BioTec <- subset(AllBio, grado == my.grado & academic.cycle == semester)
    BioWeek <- subset(AllWeek, student.id %in% BioTec$student.id & grado == my.grado & academic.cycle == semester )
    BioWeeks <- rbind(BioWeeks,BioWeek)
  }
  nFin <- nrow(Fim)
  BioWeeks$semana <- mapply(setWeek,nFin,BioWeeks$semana)

  BioTec <- subset(AllBio, grado == my.grado)
  #BioTecWeek1 <- subset(BioWeeks, semana == 0)
  BioTecWeek5 <- subset(BioWeeks, semana == 5)
  BioTecWeek9 <- subset(BioWeeks, semana == 9)
  BioTecWeek13 <- subset(BioWeeks, semana == 13)
  BioTecFin <- subset(BioWeeks, semana == 19)

  #BioTecWeek1 <- merge(BioTecWeek1,BioTec, by=c("student.id","academic.cycle","stop.out.flag.1","grado","block"))
  BioTecWeek5 <- merge(BioTecWeek5,BioTec, by=c("student.id","academic.cycle","stop.out.flag.1","grado","block"))
  BioTecWeek9 <- merge(BioTecWeek9,BioTec, by=c("student.id","academic.cycle","stop.out.flag.1","grado","block"))
  BioTecWeek13 <- merge(BioTecWeek13,BioTec, by=c("student.id","academic.cycle","stop.out.flag.1","grado","block"))
  BioTecFin <- merge(BioTecFin,BioTec, by=c("student.id","academic.cycle","stop.out.flag.1","grado","block"))
  BioTecFin <- merge(BioTecFin,Fim, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))


  PeakWeeks <- c()
  for(semester in semesters){
    PeakTec <- subset(AllPeak, grado == my.grado & academic.cycle == semester)
    PeakWeek <- subset(AllWeek, student.id %in% PeakTec$student.id & grado == my.grado & academic.cycle == semester )
    PeakWeeks <- rbind(PeakWeeks,PeakWeek)
  }
  nFin <- nrow(Fim)
  PeakWeeks$semana <- mapply(setWeek,nFin,PeakWeeks$semana)

  PeakTec <- subset(AllPeak, grado == my.grado)
  #PeakTecWeek1 <- subset(PeakWeeks, semana == 0)
  PeakTecWeek5 <- subset(PeakWeeks, semana == 5)
  PeakTecWeek9 <- subset(PeakWeeks, semana == 9)
  PeakTecWeek13 <- subset(PeakWeeks, semana == 13)
  PeakTecFin <- subset(PeakWeeks, semana == 19)

  #PeakTecWeek1 <- merge(PeakTecWeek1,PeakTec, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))
  PeakTecWeek5 <- merge(PeakTecWeek5,PeakTec, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))
  PeakTecWeek9 <- merge(PeakTecWeek9,PeakTec, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))
  PeakTecWeek13 <- merge(PeakTecWeek13,PeakTec, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))
  PeakTecFin <- merge(PeakTecFin,PeakTec, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))
  PeakTecFin <- merge(PeakTecFin,Fim, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))


  ###  PosGrad DATASETS ####

  my.grado <- "PosGrad"

  Fim <- subset(AllFim, grado == my.grado)
  Fim <- Fim[,c("student.id","academic.cycle","stop.out.flag.1","grado","academic.period.gpa","failed.courses")]

  BioWeeks <- c()
  for(semester in semesters){
    BioPosGrad <- subset(AllBio, grado == my.grado & academic.cycle == semester)
    BioWeek <- subset(AllWeek, student.id %in% BioPosGrad$student.id & grado == my.grado & academic.cycle == semester )
    BioWeeks <- rbind(BioWeeks,BioWeek)
  }
  nFin <- nrow(Fim)
  BioWeeks$semana <- mapply(setWeek,nFin,BioWeeks$semana)

  BioPosGrad <- subset(AllBio, grado == my.grado)
  #BioPosGradWeek1 <- subset(BioWeeks, semana == 0)
  BioPosGradWeek5 <- subset(BioWeeks, semana == 5)
  BioPosGradWeek9 <- subset(BioWeeks, semana == 9)
  BioPosGradWeek13 <- subset(BioWeeks, semana == 13)
  BioPosGradFin <- subset(BioWeeks, semana == 19)

  #BioPosGradWeek1 <- merge(BioPosGradWeek1,BioPosGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado","block"))
  BioPosGradWeek5 <- merge(BioPosGradWeek5,BioPosGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado","block"))
  BioPosGradWeek9 <- merge(BioPosGradWeek9,BioPosGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado","block"))
  BioPosGradWeek13 <- merge(BioPosGradWeek13,BioPosGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado","block"))
  BioPosGradFin <- merge(BioPosGradFin,BioPosGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado","block"))
  BioPosGradFin <- merge(BioPosGradFin,Fim, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))


  PeakWeeks <- c()
  for(semester in semesters){
    PeakPosGrad <- subset(AllPeak, grado == my.grado & academic.cycle == semester)
    PeakWeek <- subset(AllWeek, student.id %in% PeakPosGrad$student.id & grado == my.grado & academic.cycle == semester )
    PeakWeeks <- rbind(PeakWeeks,PeakWeek)
  }
  nFin <- nrow(Fim)
  PeakWeeks$semana <- mapply(setWeek,nFin,PeakWeeks$semana)

  PeakPosGrad <- subset(AllPeak, grado == my.grado)
  #PeakPosGradWeek1 <- subset(PeakWeeks, semana == 0)
  PeakPosGradWeek5 <- subset(PeakWeeks, semana == 5)
  PeakPosGradWeek9 <- subset(PeakWeeks, semana == 9)
  PeakPosGradWeek13 <- subset(PeakWeeks, semana == 13)
  PeakPosGradFin <- subset(PeakWeeks, semana == 19)

  #PeakPosGradWeek1 <- merge(PeakPosGradWeek1,PeakPosGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))
  PeakPosGradWeek5 <- merge(PeakPosGradWeek5,PeakPosGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))
  PeakPosGradWeek9 <- merge(PeakPosGradWeek9,PeakPosGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))
  PeakPosGradWeek13 <- merge(PeakPosGradWeek13,PeakPosGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))
  PeakPosGradFin <- merge(PeakPosGradFin,PeakPosGrad, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))
  PeakPosGradFin <- merge(PeakPosGradFin,Fim, by=c("student.id","academic.cycle","stop.out.flag.1","grado"))

  #BioPosGrad <- BioPosGradWeek1[ ,!(names(BioPosGradWeek1) %in% c("peak.date","notas","frecuencia")) ]
  #PeakPosGrad <- PeakPosGradWeek1[ ,!(names(PeakPosGradWeek1) %in% c("peak.date","notas","frecuencia")) ]
  #BioGrad <- BioGradWeek1[ ,!(names(BioGradWeek1) %in% c("peak.date","notas","frecuencia")) ]
  #PeakGrad <- PeakGradWeek1[ ,!(names(PeakGradWeek1) %in% c("peak.date","notas","frecuencia")) ]
  #BioTec <- BioTecWeek1[ ,!(names(BioTecWeek1) %in% c("peak.date","notas","frecuencia")) ]
  #PeakTec <- PeakTecWeek1[ ,!(names(PeakTecWeek1) %in% c("peak.date","notas","frecuencia")) ]
  
  
  all.data.set@BioGrad <- BioGrad
  all.data.set@BioGradWeek5 <- BioGradWeek5
  all.data.set@BioGradWeek9 <- BioGradWeek9
  all.data.set@BioGradWeek13 <- BioGradWeek13
  all.data.set@BioGradFin <- BioGradFin
  all.data.set@PeakGrad <- PeakGrad
  all.data.set@PeakGradWeek5 <- PeakGradWeek5
  all.data.set@PeakGradWeek9 <- PeakGradWeek9
  all.data.set@PeakGradWeek13 <- PeakGradWeek13
  all.data.set@PeakGradFin <- PeakGradFin

  all.data.set@BioPosGrad <- BioPosGrad
  all.data.set@BioPosGradWeek5 <- BioPosGradWeek5
  all.data.set@BioPosGradWeek9 <- BioPosGradWeek9
  all.data.set@BioPosGradWeek13 <- BioPosGradWeek13
  all.data.set@BioPosGradFin <- BioPosGradFin
  all.data.set@PeakPosGrad <- PeakPosGrad
  all.data.set@PeakPosGradWeek5 <- PeakPosGradWeek5
  all.data.set@PeakPosGradWeek9 <- PeakPosGradWeek9
  all.data.set@PeakPosGradWeek13 <- PeakPosGradWeek13
  all.data.set@PeakPosGradFin <- PeakPosGradFin

  all.data.set@BioTec <- BioTec
  all.data.set@BioTecWeek5 <- BioTecWeek5
  all.data.set@BioTecWeek9 <- BioTecWeek9
  all.data.set@BioTecWeek13 <- BioTecWeek13
  all.data.set@BioTecFin <- BioTecFin
  all.data.set@PeakTec <- PeakTec
  all.data.set@PeakTecWeek5 <- PeakTecWeek5
  all.data.set@PeakTecWeek9 <- PeakTecWeek9
  all.data.set@PeakTecWeek13 <- PeakTecWeek13
  all.data.set@PeakTecFin <- PeakTecFin

  all.data.set

}

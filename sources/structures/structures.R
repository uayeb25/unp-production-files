setClass("MainClasses",representation(
  AllPeak = "data.frame",
  AllFim = "data.frame",
  AllBio = "data.frame",
  AllWeek = "data.frame"
))

setClass("AllDataSet",representation(
  BioGrad = "data.frame",
  BioGradWeek5 = "data.frame",
  BioGradWeek9 = "data.frame",
  BioGradWeek13 = "data.frame",
  BioGradFin = "data.frame",
  PeakGrad = "data.frame",
  PeakGradWeek5 = "data.frame",
  PeakGradWeek9 = "data.frame",
  PeakGradWeek13 = "data.frame",
  PeakGradFin = "data.frame",
  
  BioPosGrad = "data.frame",
  BioPosGradWeek5 = "data.frame",
  BioPosGradWeek9 = "data.frame",
  BioPosGradWeek13 = "data.frame",
  BioPosGradFin = "data.frame",
  PeakPosGrad = "data.frame",
  PeakPosGradWeek5 = "data.frame",
  PeakPosGradWeek9 = "data.frame",
  PeakPosGradWeek13 = "data.frame",
  PeakPosGradFin = "data.frame",
  
  BioTec = "data.frame",
  BioTecWeek5 = "data.frame",
  BioTecWeek9 = "data.frame",
  BioTecWeek13 = "data.frame",
  BioTecFin = "data.frame",
  PeakTec = "data.frame",
  PeakTecWeek5 = "data.frame",
  PeakTecWeek9 = "data.frame",
  PeakTecWeek13 = "data.frame",
  PeakTecFin = "data.frame"
  
  
))

main.data.frames <- new("MainClasses",
                        AllPeak = as.data.frame(c()),
                        AllFim = as.data.frame(c()),
                        AllBio = as.data.frame(c()),
                        AllWeek = as.data.frame(c()))

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


setWeek <- function(nFim,week){
  
  
  if(week >= 1 & week <= 4){
    my.week <- 0
  }else if(week >= 5 & week <= 8){
    my.week <- 5
  }else if(week >= 9 & week <= 12){
    my.week <- 9
  }else if(nFim > 1 & week >= 19){
    my.week <- 19
  }else{
    my.week <- 13
  }
  
  my.week
  
}
load.data <-  function(semesters){
  
  
  grados <- c("Grad","Tec","PosGrad")
  
  
  setBase("files")
  AllBio <- c()
  kind <- "BioIngressantes"
  for( grado in grados ){
    for( semester in semesters ){
      name <- paste0(grado,kind,"_",semester,".csv")
      tmp <- read.csv2( name,
                        sep = ";",
                        fileEncoding = "ISO-8859-2",
                        na.strings = c("NA","N/A","NULL")  )
      tmp$grado <- grado
      names(tmp) <- tolower(names(tmp))
      if(grado == "PosGrad"){
        names(tmp) <- names(AllBio)
      }
      print(name)
      AllBio <- rbind(AllBio,tmp)
      print("Completed!")
    }
  }
  
  
  
  setBase("files")
  AllFim <- c()
  
  kind <- "FimSemestre"
  for( grado in grados ){
    for( semester in semesters ){
      name <- paste0(grado,kind,"_",semester,".csv")
      tmp <- read.csv2( name,
                        sep = ";",
                        fileEncoding = "ISO-8859-2",
                        na.strings = c("NA","N/A","NULL")  )
      tmp$grado <- grado
      names(tmp) <- tolower(names(tmp))
      print(name)
      AllFim <- rbind(AllFim,tmp)
      print("Completed!")
    }
  }
  
  setBase("files")
  AllPeak <- c()
  kind <- "PeakSemestre"
  for( grado in grados ){
    for( semester in semesters ){
      name <- paste0(grado,kind,"_",semester,".csv")
      tmp <- read.csv2( name,
                        sep = ";",
                        fileEncoding = "ISO-8859-2",
                        na.strings = c("NA","N/A","NULL")  )
      tmp$grado <- grado
      names(tmp) <- tolower(names(tmp))
      print(name)
      AllPeak <- rbind(AllPeak,tmp)
      print("Completed!")
    }
  }
  
  
  setBase("files")
  AllWeek <- c()
  kind <- "Semanal"
  for( grado in grados ){
    for( semester in semesters ){
      name <- paste0(grado,kind,"_",semester,".csv")
      tmp <- read.csv2( name,
                        sep = ";",
                        fileEncoding = "ISO-8859-2",
                        na.strings = c("NA","N/A","NULL")  )
      tmp$grado <- grado
      names(tmp) <- tolower(names(tmp))
      print(name)
      AllWeek <- rbind(AllWeek,tmp)
      print("Completed!")
    }
  }
  
  #Drop stop.out.flag 
  main.data.frames@AllPeak <- main.data.frames@AllPeak[,!(names(main.data.frames@AllPeak)%in%c("stop.out.flag.1"))]
  main.data.frames@AllFim <- main.data.frames@AllFim[,!(names(main.data.frames@AllFim)%in%c("stop.out.flag.1"))]
  main.data.frames@AllBio <- main.data.frames@AllBio[,!(names(main.data.frames@AllBio)%in%c("stop.out.flag.1"))]
  main.data.frames@AllWeek <- main.data.frames@AllWeek[,!(names(main.data.frames@AllWeek)%in%c("stop.out.flag.1"))]

  main.data.frames <- new("MainClasses",
                          AllPeak = AllPeak,
                          AllFim = AllFim,
                          AllBio = AllBio,
                          AllWeek = AllWeek)
  
  main.data.frames
}



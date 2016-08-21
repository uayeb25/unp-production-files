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
  

  main.data.frames <- new("MainClasses",
                          AllPeak = AllPeak,
                          AllFim = AllFim,
                          AllBio = AllBio,
                          AllWeek = AllWeek)
  
  main.data.frames
}



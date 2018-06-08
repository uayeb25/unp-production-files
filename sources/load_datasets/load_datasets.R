setBase("files")

degrees <- c("Grad","Tec","PosGrad")
folders <- c("Bio","Fim","Peak","Semanal_","Notas")

for (f in folders){
  
  assign(paste0("df.",f), c(""))
  
  for (a in Sys.glob(paste0("*.csv"))){
    tmp <- read.csv2(a, header = T, dec = ".", sep = ";",
                   fileEncoding = "ISO-8859-2",
                   na.strings = c("NA","N/A","NULL"))
    
    names(tmp) <- tolower(names(tmp))
    tmp$degree <- substr(a,1,3)
    
    if (regexpr(f,a) > 0) 
    {assign(paste0("df.",f),rbind(get(paste0("df.",f)),tmp))}
  }

}

df.Semanal <- df.Semanal_
df.Fin <- df.Fim
df.Fim <- NULL
remove(df.Semanal_)
remove(tmp)

nFim <- ifelse(is.null(nrow(df.Fin)),0,nrow(df.Fin))

### Function to update the week in the right model ###

setWeek <- function(nFim,week){
  
  my.week <- 
    ifelse(week >= 1 & week <= 4,0,
           ifelse(week >= 5 & week <= 8,5,
                  ifelse(week >= 9 & week <= 12,9,
                         ifelse(nFim > 1 & week >= 19,19,13))))
  
  my.week
  
}

current.week <- ddply(df.Semanal,"degree", summarise,max.semana  = max(semana))
current.week <- current.week[current.week$degree != "",]
current.week$degree <- gsub("Pos","PosGrad",gsub("Gra","Grad",current.week$degree))

df.Semanal$semana <- setWeek(nFim,df.Semanal$semana)




for (d in ls(pattern = "df")){
  
  assign(d,get(d)[get(d)["student.id"] != "",])

}

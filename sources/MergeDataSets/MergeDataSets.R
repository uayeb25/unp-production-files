rm("main.data.frames")

#Vector of weeks that will be created
weeks <- c(5,9,13) 

#Vector of degrees
degrees <- unique(df.Bio$degree) 

#Vector of semesters
semesters <- unique(df.Bio$academic.cycle) 

#Filter df only with the important weeks
df.Semanal <- df.Semanal[df.Semanal$semana %in% c(0,5,9,13,19),] 


#Filter df only with the important columns
df.Fin <- df.Fin[,c("student.id","academic.cycle","stop.out.flag.1","degree"
                        ,"academic.period.gpa","failed.courses")] 


# #Check if the file "Fin" is not empty...
if (sum(as.numeric(df.Fin$failed.courses)) == 0 &
    sum(as.numeric(df.Fin$stop.out.flag.1)) == 0 &
    sum(as.numeric(df.Fin$academic.period.gpa)) == 0){df.Fin <- df.Fin[0,]}

####################################
## Dataframe - begin of semester ###
####################################

for (i in c("df.Bio","df.Peak")){
  for (d in degrees){
    assign(paste0(i,".",d,".Ini"),
           subset(get(i), degree == d))
  }
}

#########################################################
## Dataframe - middle of semester (weeks 5, 9 and 13) ###
#########################################################

for (i in c("df.Bio","df.Peak")){
  for (d in degrees){
    for (w in weeks){
      assign(paste0(i,".",d,".S",w),
        merge(get(i),
        subset(df.Semanal, semana == w & degree == d),
        by = c("student.id","academic.cycle","stop.out.flag.1","degree")))
    }
  }
}

#########################
## Dataframe - grades ###
#########################

df.Notas <- df.Notas[!df.Notas$formulamedia == "",]

bin.recup <- ifelse(df.Notas$unidadeavaliativa == "RECUPERACAO" |
                      df.Notas$unidadeavaliativa ==  "SUBS" |
                      df.Notas$unidadeavaliativa == "PROVAFINAL",1,0)

sucesso.local <- ifelse(as.numeric(df.Notas$notas)/as.numeric(df.Notas$mediaminima) >= 1,1,0)
sucesso.final <- ifelse(as.numeric(df.Notas$mediafinal)/as.numeric(df.Notas$mediaminima) >= 1,1,0)

df.Notas$sucesso.local <- sucesso.local
df.Notas$sucesso.final <- sucesso.final
df.Notas$bin.recup <- bin.recup

grades.summary <- ddply(df.Notas,c("student.id", "academic.cycle","degree"), summarise,
                            total.disciplinas  = length(idmatriculadisciplina)
                          , mean.sucesso.local = mean(sucesso.local, na.rm = T)
                          , mean.sucesso.final = mean(sucesso.final, na.rm = T)
                          , sum.bin.recup = sum(bin.recup)
                          , final.mean = mean(as.numeric(mediafinal), na.rm = T))

MergeStopOut <- 
  unique(rbind(
    cbind(df.Bio$student.id, df.Bio$academic.cycle, df.Bio$degree, df.Bio$stop.out.flag.1),
    cbind(df.Peak$student.id, df.Peak$academic.cycle, df.Peak$degree, df.Peak$stop.out.flag.1)
      ))

colnames(MergeStopOut) <- c("student.id","academic.cycle","degree","stop.out.flag.1")
grades.summary <- merge(grades.summary,MergeStopOut,by = c("student.id","academic.cycle","degree"))

##################################
## Dataframe - end of semester ###
##################################

for (i in c("df.Bio","df.Peak")){
  for (d in degrees){
    assign(paste0(i,".",d,".Fin"),
           merge(
             get(i),subset(df.Fin, degree == d),
                 by = c("student.id","academic.cycle","stop.out.flag.1","degree")))
  }
}

for (i in c("df.Bio","df.Peak")){
  for (d in degrees){
    assign(paste0(i,".",d,".Fin"),
           merge(
             get(paste0(i,".",d,".Fin")),grades.summary,
             by = c("student.id","academic.cycle","stop.out.flag.1","degree")))
  }
}


for (f in as.factor(folders)){remove(list = ls()[endsWith(ls(),f)])}
remove(MergeStopOut,grades.summary)
remove(df.Fin, df.Semanal)



#*********
#concat output files
#********

options(scipen=999)
directory <-"/vcu_gpfs2/home/thomasns/ColliderBiasPhRM/"
OutputNames<-NULL
scriptNames<-NULL
script<-NULL

for ( i in 1:50){
  OutputNames<-rbind(OutputNames, paste0(directory, 'ALLoutB',"_", i, '.csv'))
  scriptNames<-rbind(scriptNames,paste0(directory, 'ALLoutB',"_", i, '.R'))
  script <- rbind(script,paste0(directory, 'ALLoutB',"_", i, '.sh'))
}

#*********
#concat all
#*********
system(paste0('cat ',paste0(OutputNames, collapse=" "),' > ',paste0('CombinedOutputALLoutB','.csv')))

#*********
#*********
#Identify and restart failed runs, if concat fails
#*********
#*********
FailedRuns<-OutputNames[gsub(directory, "", OutputNames) %in% list.files(path=directory)==FALSE]
FailedRuns<-gsub(directory,"",FailedRuns)
FailedRuns<-gsub(".csv","",FailedRuns)
FailedRuns<-as.numeric(gsub(paste0('ALLoutB', "_"),"",FailedRuns))
runJob <- paste0('qsub ', script[FailedRuns])
runJob#print failed jobs
#run failed jobs
for ( i in 1:length(runJob)){
  system(runJob[i])
}


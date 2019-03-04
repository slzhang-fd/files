date()
jobid<-Sys.getenv("PODNAME");
a=1
write.csv(a, file=paste0("/tmp/persistent/",jobid,".rda"))

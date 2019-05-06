date()
jobid<-Sys.getenv("PODNAME");
a=rnorm(1)
save(a, file=paste0("/tmp/persistent/",jobid,".rda"))

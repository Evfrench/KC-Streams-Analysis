


lab_chlorophyll_correction<-function(Value,Parameter='Chlorophyll a',Date){
  
  ifelse(Parameter=='Chlorophyll a'&Date<as.Date("1996-07-01"),1.14*Value,Value)
  
}
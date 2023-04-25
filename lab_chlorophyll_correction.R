# Clark's lab data method changes adjustment function [https://green2.kingcounty.gov/ScienceLibrary/Document.aspx?ArticleID=324]

lab_chlorophyll_correction <- function(Value,Parameter,Date){
  if(Parameter=='Chlorophyll a' & Date<as.Date("1996-07-01")){
    1.14*Value
  } else {
    Value
  }  
}
# Clark's lab data method changes adjustment function [https://green2.kingcounty.gov/ScienceLibrary/Document.aspx?ArticleID=324]
#
# Checks for the 'Chlorophyll a' parameter
#
lab_chlorophyll_correction <- function(Value,Parameter='None',Date=as.Date("2999-07-01")){
  if(Parameter=='Chlorophyll a' & Date<as.Date("1996-07-01")){
    1.14*Value
  } else {
    Value
  }
}

# Clark's lab data method changes adjustment function [https://green2.kingcounty.gov/ScienceLibrary/Document.aspx?ArticleID=324]

lab_change_correction <- function(Value, Parameter, Date) {
  if (!require(lubridate)) install.packages('lubridate')
  Date <- as.Date(Date)
  Year <- lubridate::year(Date)

  # 
  if(Parameter == 'Total Phosphorus' & Year <= 2006) {
    #
    # Total Phosphorus
    if(Date < as.Date('1998-07-01')) {
      if(Value < 0.024) {
        1.224 * (1.776 * Value ^ 1.203) ^ 1.031)
      } else {
        1.224 * (0.9347 * Value ^ 1.056) ^ 1.031)
      }
    } else {
        1.224 * Value ^ 1.031
    }
  } else if (Parameter == 'Total Nitrogen' & Year <= 2006) {
    1.005 * Value ^ 0.9921
  } else if (Parameter == 'Orthophosphate Phosphorus' & Year <= 2006) {
      if (Value < 0.0087){
        2.109 * Value ^ 1.090
      } else if(Value < 0.0424){
        0.6358 * Value ^ 0.8621
      } else {
        0.9366 * Value ^ 0.9823
      }
  } else if (Parameter == 'Nitrite + Nitrate Nitrogen' & Year <= 2006) {
    if(Value < 0.678){
      0.002 + 0.9747 * Value
    } else {
      0.024 + 0.9381 * Value
    },
  } else {
    Value
  }
}

#example
# Date <- c('2006-12-15', '2006-12-15', '2006-12-15')
# Value <- c(0.012, 0.1, 0.0005)
# Parameter <- 'Total Phosphorus'

# lab_change_correction(Value = Value, Date = Date, Parameter = Parameter)

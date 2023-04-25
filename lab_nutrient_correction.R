# Clark's lab data method changes adjustment function [https://green2.kingcounty.gov/ScienceLibrary/Document.aspx?ArticleID=324]
#
# Checks the following parameters:
# Total Phosphorus, Total Nitrogen, Orthophosphate Phosphorus, Nitrite + Nitrate Nitrogen
# Function is pass by reference (edits variables in place)
#
lab_change_correction <- function(Value, Parameter = 'None', Date) {
  if (!require(lubridate)) install.packages('lubridate')
  Date <- as.Date(Date)
  Year <- lubridate::year(Date)
  #
  ifelse(Parameter == 'Total Phosphorus' & Year <= 2006,
    ifelse(Date < as.Date('1998-07-01'),
      ifelse(Value < 0.024, 
        1.224 * (1.776 * Value ^ 1.203) ^ 1.031,
        1.224 * (0.9347 * Value ^ 1.056) ^ 1.031
      ),
      1.224 * Value ^ 1.031
    ),
  #
  ifelse(Parameter == 'Total Nitrogen' & Year <= 2006, 
    1.005 * Value ^ 0.9921,
  #
  ifelse(Parameter == 'Orthophosphate Phosphorus' & Year <= 2006,
    ifelse(Value < 0.0087, 
      2.109 * Value ^ 1.090,
      ifelse(Value < 0.0424, 
        0.6358 * Value ^ 0.8621,
        0.9366 * Value ^ 0.9823)
    ),
  #
  ifelse(Parameter == 'Nitrite + Nitrate Nitrogen' & Year <= 2006,
    ifelse(Value < 0.678, 
      0.002 + 0.9747 * Value,
      0.024 + 0.9381 * Value),

# If nothing matches, just leave data alone 
    Value
  )
  )))
}

#example
# Date <- c('2006-12-15', '2006-12-15', '2006-12-15')
# Value <- c(0.012, 0.1, 0.0005)
# Parameter <- 'Total Phosphorus'

# lab_nutrient_correction(Value = Value, Date = Date, Parameter = Parameter)

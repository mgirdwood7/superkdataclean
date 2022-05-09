# functions

# Function to coalesce multiple long form rows together into one (
# E.g. where all different baseline 'redcap arms' do not overlap, to be joined together as one timepoint

coalesce_by_column <- function(df) {
  return(dplyr::coalesce(!!! as.list(df)))
}
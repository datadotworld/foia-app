listAgencies <- function(){
  
  agencies <- read.csv("data/foia_agency_names.csv", sep="\t")
  
  vect <- as.vector(agencies$name)
  vect <- sort(vect)
  return (vect)
}
listAgencies <- function(){
  
  agencies <- read.csv("https://query.data.world/s/d2tekflwrada5cmi74xhpnwof",header=T, sep="\t", quote = "")
  
  vect <- as.vector(agencies$name)
  # add blank to beginning of vector
  vect2 <- as.vector("")
  vect3 <- as.vector(rbind(vect2,vect)) 
  # vect <- sort(vect)
  return (vect3)
}
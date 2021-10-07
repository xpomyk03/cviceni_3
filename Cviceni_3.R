#cviceni 3

# mince

VratitMince <- function(vratna_castka){
  vratit <- c()
  mince <- c(50, 20, 10, 5, 2, 1)
  
  while (vratna_castka > 0){
    for (i in 1:length(mince)){
      x <- floor(vratna_castka/mince[i])
      if (x > 0){
        vratit <- c(vratit, mince[i])
        vratna_castka = vratna_castka - mince[i]
        break
      }
    }
  }
  return(vratit)
}

#cviceni 3

# mince
#  mince <- c(50, 20, 10, 5, 2, 1)

VratitMince <- function(vratna_castka, mince, pocet_minci){
  vratit <- c()

  
  while (vratna_castka > 0){
    for (i in 1:length(mince)){
      x <- floor(vratna_castka/mince[i])
      if (x > 0 & pocet_minci[i] > 0){
        vratit <- c(vratit, mince[i])
        pocet_minci[i] = pocet_minci[i]-1
        vratna_castka = vratna_castka - mince[i]
        break
      }
    }
  }
  return(vratit)
}


# nejcokoladovejsi cesta
M = matrix(data = c(1,2,3,4,5,6,7,8,9), nrow = 3, ncol = 3);

Jadro <- function(M, r, s){
  C <- M[r,s]
  Cdolu <- M[r+1,s]
  Csikmo <- M[r+1,s+1]
  return(max(Cdolu, Csikmo))
}


Cokolada <- function(M, r, s){
  if(r == dim(M)[1]){
    return(M[r,s])
  }else{
    C <- M[r,s]
    Cdolu <- Cokolada(M, r+1,s)
    Csikmo <- Cokolada(M, r+1, s+1)
    return(max(Cdolu, Csikmo) + C)
  }
}


CokoIter <- function(M){
  s <- dim(M)
  k1 <- seq(from=s[1]-1, to=1, by=-1)
  for (r in k1){
    k2 <- seq(from=r,to=1,by=-1)
    for (s in k2){
      Cdolu <- M[r+1,s]+M[r,s]
      Csikmo <- M[r+1,s+1]+M[r,s]
      M[r,s] <- max(c(Cdolu, Csikmo))
    }
  }
  return(M[1,1])
}



#Ukol 3 - Hanojska vez

Hanojska <- function(n, zKolik, naKolik){
  if (n==1){
    print(paste('Presun disk z koliku',zKolik,'na kolik', naKolik))
  }else{
    volny <- 6 - zKolik - naKolik
    Hanojska(n-1,zKolik, volny)
    print(paste('Presun disk z koliku',zKolik,'na kolik', naKolik))
    Hanojska(n-1,volny, naKolik)
  }
}

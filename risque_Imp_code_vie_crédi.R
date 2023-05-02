library(readxl)
dataF<-read_excel("TGF05-TGH05.xls", sheet =1)
dataH<-read_excel("TGF05-TGH05.xls", sheet =2)


matF <- as.matrix(dataF[2:123 , 2:107])
matH <- as.matrix(dataH[2:123 , 2:107])



#prime pour les femmes 
rente  <- 2000
v <- 1/(1+0.02)

age <- seq(60,100,5)

annee_naissance <- 2022- age 
cln<- 2022 - age -1899 #donne le numéro de la colonne, pour une personne qui a 80 ans en 2022 on veut trouver l'année de sa naissance 

l <- length(age)
p <- 0.2

T <- 4



#pour un age X donné, on écrit une fonction qui calcule le numérateur de la prime

numerateur_femme <- c()
denominateur_femme <- c()
sum_up<-matrix(nrow = T, ncol = length(age) )

for (x in 1:length(age)){ # on fait une boucle for sur les individus, par tranche d'âge 
  
  actualisation_femme <- cumprod(c(1, rep(v, T-1)))
  
  p_credi_femme <- matF[(age[x]:age[x]+T) , cln[x] ]  / matF[age[x], cln[x]] 
  
  #on fixe une colonne, celle de l'année de naissance du crédirentier, et on avance sur les lignes pour k variant de 0 à T-1
  
  
  mat_sum_up <- matrix(0,nrow = T, ncol = T)
  mat_sum_up[1,] <- rep(1, ncol(mat_sum_up) ) #on affecte des 1 à la première ligne
  
  for (i in 2 : nrow(mat_sum_up)) {
    for(j in 0:ncol(mat_sum_up)-1){
      mat_sum_up[i,j] <- matF[ age[x] +i-1 +j , cln[x] ]  / matF[age[x]+ j, cln[x] ] 
      
    }
    
    
  }
  
  
  
 
  sum_up[ , x] <- t(actualisation_femme %*% mat_sum_up)
  
  
  numerateur_femme[x] <- rente * sum( actualisation_femme * p_credi_femme * p * sum_up[x] )
  
  denominateur_femme[x] <- sum(p_credi_femme * (1-p) * actualisation_femme)
  
  print(sum_up)
}




prime_annuelle_femme <- as.numeric(numerateur_femme)/ as.numeric(denominateur_femme) 




#prime pour les hommes 


numerateur_homme<- c()
denominateur_homme <- c()
sum_upH<-matrix(nrow = T, ncol = length(age) )

for (x in 1:length(age)){ # on fait une boucle for sur les individus, par tranche d'âge 
  
  actualisation_homme<- cumprod(c(1, rep(v, T-1)))
  
  p_credi_homme <- matH[(age[x]:age[x]+T) , cln[x] ]  / matH[age[x], cln[x]] 
  
  #on fixe une colonne, celle de l'année de naissance du crédirentier, et on avance sur les lignes pour k variant de 0 à T-1
  
  
  mat_sum_upH <- matrix(0,nrow = T, ncol = T)
  mat_sum_upH[1,] <- rep(1, ncol(mat_sum_upH) ) #on affecte des 1 à la première ligne
  
  for (i in 2 : nrow(mat_sum_upH)) {
    for(j in 0:ncol(mat_sum_upH)-1){
      mat_sum_upH[i,j] <- matH[ age[x] +i-1 +j , cln[x] ]  / matH[age[x]+ j, cln[x] ] 
      
    }
   
    
  }
  
 
  sum_upH[ , x] <- t(actualisation_homme %*% mat_sum_upH)
  
  
  numerateur_homme[x] <- rente * sum( actualisation_homme * p_credi_homme * p * sum_upH[x] )
  
  denominateur_homme[x] <- sum(p_credi_homme * (1-p) * actualisation_homme)
  
  
}




prime_annuelle_homme <- as.numeric(numerateur_homme)/ as.numeric(denominateur_homme) 


#on peut désormais implémenter les graphiques

plot(age, prime_annuelle_homme, type = "l", lwd = "3", col = "tomato3", main = "Prime annuelle risque rente impayées - tables TGH05/TGF05", xlab="âge du crédirentier", ylab='Prime annuelle', ylim = c(1915, 1940))
lines(age, prime_annuelle_femme, type = "l", lwd = "3", col = "blue") 
legend('bottomleft', 
       c('crédirentier homme', 'crédirentier femme'),
       col = c('tomato3','blue'),
       cex=0.9,
       lwd = 3,
       box.lty = 0,
       bg = 'gray95',
       inset = .05
)


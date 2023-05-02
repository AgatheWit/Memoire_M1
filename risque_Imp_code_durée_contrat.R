library(readxl)
dataF<-read_excel("TGF05-TGH05.xls", sheet =1)
dataH<-read_excel("TGF05-TGH05.xls", sheet =2)


matF <- as.matrix(dataF[2:123 , 2:107])
matH <- as.matrix(dataH[2:123 , 2:107])



#prime pour les femmes 
rente  <- 2000
v <- 1/(1+0.02)

age <- 90 # fixer un age 

annee_naissance <- 2022- age 
cln<- 2022 - age -1899 #donne le numéro de la colonne, pour une personne qui a 80 ans en 2022 on veut trouver l'année de sa naissance 

p <- 0.4

T <- seq(0, 122 - age , length.out = 8)  

#sélectionne la colonne

# linspace pour faire varier le p + telecharger une librairie 





#pour un age X donné, on écrit une fonction qui calcule le numérateur de la prime

numerateur_femme <- c()
denominateur_femme <- c()
sum_up<-c()


for (k in 1:length(T)){ # on fait une boucle for sur les individus, par tranche d'âge 
  
  if( k ==0 | k==1){
    numerateur_femme[k] <- 0
    denominateur_femme[k] <-2
    
  }
  
  else {
  actualisation_femme <- cumprod(c(1, rep(v, k-1)))
  
  p_credi_femme <- matF[(age:age+k) , cln ]  / matF[age, cln] 
  
  #on fixe une colonne, celle de l'année de naissance du crédirentier, et on avance sur les lignes pour k variant de 0 à T-1
  
  
  mat_sum_up <- matrix(0,nrow = k, ncol = k)
  mat_sum_up[1,] <- rep(1, ncol(mat_sum_up) ) #on affecte des 1 à la première ligne
  
  for (i in 2 : nrow(mat_sum_up)) {
    for(j in 0:ncol(mat_sum_up)-1){
      mat_sum_up[i,j] <- matF[ age +i-1 +j , cln ]  / matF[age+ j, cln ] 
      
    }
    #print(mat_sum_up)
    
  }
  
  # print(actualisation_femme)
  
  #print(actualisation_femme %*% mat_sum_up)
  sum_up <- t(actualisation_femme %*% mat_sum_up)
  
  
  numerateur_femme[k] <- rente * p *sum( actualisation_femme * p_credi_femme * sum_up )
  
  denominateur_femme[k] <- sum(p_credi_femme * (1-p) * actualisation_femme)
  
  }
  #print(numerateur_femme)
  #print(denominateur_femme)
}




prime_annuelle_femme <- as.numeric(numerateur_femme)/ as.numeric(denominateur_femme) 




#prime pour les hommes 


numerateur_homme<- c()
denominateur_homme <- c()
sum_upH<- c()
  #matrix(nrow = length(T), ncol = length(age) )

for (k in 1:length(T)){ # on fait une boucle for sur les individus, par tranche d'âge 
  
  if(k==0 | k ==1){
    numerateur_homme[k] <- 0
    
    denominateur_homme[k] <- 2
    
  }
  else {
  actualisation_homme <- cumprod(c(1, rep(v, k-1)))
  
  p_credi_homme <- matH[(age:age+k) , cln ]  / matH[age, cln] 
  
  #on fixe une colonne, celle de l'année de naissance du crédirentier, et on avance sur les lignes pour k variant de 0 à T-1
  
  
  mat_sum_upH<- matrix(0,nrow = k, ncol = k)
  mat_sum_upH[1,] <- rep(1, ncol(mat_sum_upH) ) #on affecte des 1 à la première ligne
  
  for (i in 2 : nrow(mat_sum_upH)) {
    for(j in 0:ncol(mat_sum_upH)-1){
      mat_sum_upH[i,j] <- matH[ age +i-1 +j , cln ]  / matH[age+ j, cln ] 
      
    }
    #print(mat_sum_up)
    
  }
  
  # print(actualisation_femme)
  
  #print(actualisation_femme %*% mat_sum_up)
  sum_upH <- t(actualisation_homme %*% mat_sum_upH)
  
  
  numerateur_homme[k] <- rente * p *sum( actualisation_homme * p_credi_homme * sum_upH )
  
  denominateur_homme[k] <- sum(p_credi_homme * (1-p) * actualisation_homme)
  
  }
  #print(numerateur_femme)
  #print(denominateur_femme)
}




prime_annuelle_homme <- as.numeric(numerateur_homme)/ as.numeric(denominateur_homme) 

print(prime_annuelle_homme)



#on peut désormais implémenter les graphiques

plot(T, prime_annuelle_homme, type = "l", lwd = "3", col = "tomato3", main = "Prime annuelle risque rente impayées - tables TGH05/TGF05", xlab="durée du contrat", ylab='Prime annuelle', ylim= c(0, 7000))
lines(T, prime_annuelle_femme, type = "l", lwd = "3", col = "blue") 
legend('topleft', 
       c('crédirentier homme', 'crédirentier femme'),
       col = c('tomato3','blue'),
       cex=0.9,
       lwd = 3,
       box.lty = 0,
       bg = 'gray95',
       inset = .05
)

print(length(T))

print(length(prime_annuelle_homme))
print(prime_annuelle_homme)

library(readr)
car_dat <- read_csv("C:/Users/Juli/Desktop/Progetto/data/CarPrice_Assignment.csv")

test_string<-toString(car_dat$CarName[1])
strsplit(test_string,"-")
test_string2<-toString(car_dat$CarName[4])
test_vec<-c(strsplit(test_string2," "))
test_car_comp<-c(rep(NA,nrow(car_dat)))
test_car_list<-vector(mode = "list", length = nrow(car_dat))

for(i in 1:nrow(car_dat)){
  test_string<-toString(car_dat$CarName[i])
  test_car_list[[i]]<-c(strsplit(test_string," "))
  test_car_pre<-unlist(test_car_list[[i]][1])
  test_car_comp[i]<-test_car_pre[1]
}

# Crea un nuovo vettore per i nomi delle compagnie corrette.

car_comp_final<-c(rep(NA,nrow(car_dat)))

# Ciclo for per correggere i nomi delle compagnie con errori di battitura.

for(i in 1:nrow(car_dat)){
  if(test_car_comp[i]=="maxda"){
    car_comp_final[i]<-"mazda"
  }
  else if(test_car_comp[i]=="Nissan"){
    car_comp_final[i]<-"nissan"
  }
  else if(test_car_comp[i]=="porcshce"){
    car_comp_final[i]<-"toyota"
  }
  else if(test_car_comp[i]=="vokswagen"){
    car_comp_final[i]<-"volkswagen"
  }
  else if(test_car_comp[i]=="vw"){
    car_comp_final[i]<-"volkswagen"
  }
  else if(test_car_comp[i]=="toyouta"){
    car_comp_final[i]<-"toyota"
  }
  else{
    car_comp_final[i]<-test_car_comp[i]
  }
}

# Funzione per mettere la prima lettera in maiuscolo.

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# Applica la funzione firstup a car_comp_final per mettere in maiuscolo la prima lettera di ogni nome di compagnia.

car_comp_final<-firstup(car_comp_final)

# Aggiunge la colonna car_company al dataset car_dat con i nomi delle compagnie corretti.

car_dat$car_company<-car_comp_final

# Crea un istogramma del prezzo delle macchine.

hist(car_dat$price)

# possiamo usare la regressione esponenziale o il modello log-lineare

# rimuovi car id e car name poiché non sono utili nell'analisi
# car name si sovrappone alle variabili car company
car_dat2<-car_dat[,-c(1,3)]

#estrai variabili di tipo carattere
character_var<-car_dat2[, sapply(car_dat2, class) == 'character']
character_var[sapply(character_var, is.character)] <- lapply(character_var[sapply(character_var, is.character)],
                                                             as.factor)

#estrai variabili non di tipo carattere
no_chara<-car_dat2[, sapply(car_dat2, class) != 'character']
#summary(no_chara)
car_dat3<-cbind(character_var,no_chara)

# modello gamma con tutte le variabili esplicative
gamma_model_price1<-glm(price~., data= car_dat3[, -c(7,9)],family= Gamma(link = "log"))
sum_mod_gamma<-summary(gamma_model_price1)

# utilizzo di stepAIC per effettuare la selezione stepwise
library(MASS)
gamma_step_model <- stepAIC(gamma_model_price1, direction = "both", 
                      trace = TRUE)
gamma_step_sum<-summary(gamma_step_model)

# gamma_squareb<-glm(formula = price ~ aspiration + carbody + enginelocation + 
# cylindernumber + car_company + wheelbase + carlength + carwidth + 
# carheight + curbweight + enginesize + boreratio + peakrpm + 
# citympg + highwaympg, family = Gamma(link = "log"), data = car_dat3[,-c(7, 9)])

gamma_square<-glm(formula = price ~ fueltype + aspiration + doornumber + carbody + drivewheel + enginelocation + 
                    cylindernumber + car_company + symboling + wheelbase + carlength + carwidth + 
                    carheight + curbweight + enginesize + boreratio + stroke + compressionratio + horsepower + peakrpm + 
                    citympg + highwaympg+ I(wheelbase^2)+I(carlength^2)+
                    I(carwidth^2)+I(carheight^2)+I(curbweight^2)+I(enginesize^2)+
                    I(boreratio^2)+I(peakrpm^2)+I(citympg^2)+I(highwaympg^2)
                  , family = Gamma(link = "log"), data = car_dat3[,-c(7, 9)])

# Selezione stepwise per il modello quadratico.

gamma_square_step<-stepAIC(gamma_square,direction = "both",trace=FALSE)

# lrttest per modello completo vs modello stepwise (effetto principale)

loglik_full<-logLik(gamma_model_price1)
loglik_step<-logLik(gamma_step_model)
test_stat1<-loglik_full-loglik_step
p_val1<-1-pchisq(test_stat1,8)
# Il nostro modello è uguale al modello completo (effetto principale)

# lrttest per modello quadratico completo vs modello stepwise quadratico
loglik_sq_full<-logLik(gamma_square)
loglik_sq_step<-logLik(gamma_square_step)
test_stat2<-loglik_sq_full-loglik_sq_step
pval2<-1-pchisq(test_stat2,22)

# Il nostro modello è uguale al modello completo (modello quadratico)

# Confronto tra modello ad effetto principale e modello quadratico
AIC(gamma_step_model)
AIC(gamma_square_step)

# Il modello quadratico ha l'AIC inferiore rispetto al modello ad effetto principale. Pertanto, il modello quadratico è il migliore.

# adeguatezza del modello
deviance(gamma_step_model)
deviance(gamma_square_step)
pchisq(deviance(gamma_square_step),df.residual(gamma_square_step),lower.tail = FALSE)
# il modello è adeguato

sum_square_step<-summary(gamma_square_step)

# Intervallo di confidenza al 95% per gli stime
coefficient_dat<-sum_square_step$coefficients[,c(1,2)]
LL<-coefficient_dat[,1]-qt(0.975,df.residual(gamma_square_step))*coefficient_dat[,2]
UL<-coefficient_dat[,1]+qt(0.975,df.residual(gamma_square_step))*coefficient_dat[,2]
conf_int_dat<-data.frame(coefficient_dat,LL,UL)
names(conf_int_dat)[3]<-"Lower Limit"
names(conf_int_dat)[4]<-"Upper Limit"
(conf_int_dat)

# Pseudo R2 per il modello stepwise quadratico.

pseudo_R2_square_step<-1-(sum_square_step$deviance/sum_square_step$null.deviance)
#0.9661938

# test con residui standardizzati

predict_car<-fitted(gamma_square_step)
price_actual<-car_dat3$price
std_res_car<-(price_actual-predict_car)/predict_car

# grafico dei residui standardizzati
pdf("C:/Users/Juli/Desktop/Progetto/Figures/std_residui_pattern.pdf", width = 8, height = 5)
plot(predict_car, std_res_car, xlab = 'valori previsti', ylab = 'residui standardizzati')
abline(h = 0, col = "blue")
dev.off()

# analisi: nessun pattern evidente nei residui standardizzati indicando varianza costante

# grafico di normalità dei residui standardizzati

pdf("C:/Users/Juli/Desktop/Progetto/Figures/qqplot_std_res_auto.pdf", width = 8, height = 5)
qqnorm(std_res_car,ylab="quantili campione",xlab="Quantili teorici") 
qqline(std_res_car)
dev.off()

# analisi: tutti i residui standardizzati si trovano sulla linea qq, indicando che i residui seguono una distribuzione normale approssimativamente
#follow approximately normal distribution

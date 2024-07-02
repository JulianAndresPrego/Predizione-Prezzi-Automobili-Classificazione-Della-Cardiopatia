
library(readr)
library(ggplot2)
library(dplyr)

##### Prezzo dell'auto ###### 
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
# correggi i typo
car_comp_final<-c(rep(NA,nrow(car_dat)))
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
  else if(test_car_comp[i]=="vokswEtàn"){
    car_comp_final[i]<-"volkswEtàn"
  }
  else if(test_car_comp[i]=="vw"){
    car_comp_final[i]<-"volkswEtàn"
  }
  else if(test_car_comp[i]=="toyouta"){
    car_comp_final[i]<-"toyota"
  }
  else{
    car_comp_final[i]<-test_car_comp[i]
  }
}
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
car_comp_final<-firstup(car_comp_final)
car_dat$car_company<-car_comp_final
hist(car_dat$price)
# possiamo utilizzare la regressione esponenziale o il modello log-lineare
# rimuovi l'ID dell'auto e il nome dell'auto poiché non sono utili nell'analisi
# il nome dell'auto si sovrappone alle variabili della casa automobilistica
car_dat2<-car_dat[,-c(1,3)]
# estrai le variabili di tipo carattere
character_var<-car_dat2[, sapply(car_dat2, class) == 'character']
character_var[sapply(character_var, is.character)] <- lapply(character_var[sapply(character_var, is.character)],
                                                             as.factor)
# estrai le variabili non di tipo carattere
no_chara<-car_dat2[, sapply(car_dat2, class) != 'character']
#summary(no_chara)
car_dat3<-cbind(character_var,no_chara)
# modello esponenziale
expo_model_price<-glm(price~., data= car_dat3[, -c(6,7,9)],family= Gamma(link = "log"))
(sum_mod_expo<-summary(expo_model_price,dispersion=1))



## grafici di distribuzione
p1 <- ggplot(car_dat3) + aes(x =price) + 
  geom_histogram(aes(y=..density..), size=1,color="blue", fill="white", binwidth = 2000)+ 
  geom_density(alpha=.05, fill="red", size = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(line = element_line(size = 0.5)) +
    xlab('Prezzo') + ylab('Densità')

p2 <- ggplot(car_dat3) + aes(x = log(price)) + 
  geom_histogram(aes(y=..density..), size=1,color="blue", fill="white")+ 
  geom_density(alpha=.05, fill="red", size = 1) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(line = element_line(size = 0.5))+
    xlab('Log(Prezzo)') + ylab('Densità') 

p <- 
  ggpubr::ggarrange(p1, p2, 
                  labels = c("A", "B"),
                  ncol = 1, nrow = 2)
p
ggsave('C:/Users/Juli/Desktop/Progetto/Figures/distribuzione_prezzo_auto.pdf', p, width = 7, height = 8)

## Box plots
p <- 
  ggplot(car_dat3, aes(x=car_company, y=price, fill=fueltype)) +
  geom_boxplot(lwd=0.25) + 
  xlab('Casa automobilistica') + ylab('Prezzo')+
  scale_fill_manual(values=c("#cc0099", "#3399ff"), 
                    name = "Tipo di carburante", 
                    labels = c("Diesel", "Gas"))+
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5))

ggsave('C:/Users/Juli/Desktop/Progetto/Figures/distribuzione_casa_automobilistica.pdf', p, width = 8, height = 5)


car_dat3$cylindernumber <- factor(car_dat3$cylindernumber, levels=c('two', 'three', 'four', 'five',  'six', 'eight', 'twelve'),
                                  labels=c('Due', 'Tre', 'Quattro', 'Cinque', 'Sei', 'Otto', 'Dodici'))

p <- 
    ggplot(car_dat3) +
    aes(x = enginesize, y = price, color = cylindernumber) +
    geom_point(color = "black") +
    geom_smooth(method = "lm", alpha = 0.2) + 
    xlab('Dimensione del motore') + ylab('Prezzo')+
    scale_colour_brewer(palette = "Set1", name = "Numero di cilindri")+
    theme_classic()

ggsave('C:/Users/Juli/Desktop/Progetto/Figures/dimensione_motore.pdf', p, width = 8, height = 5)


res <- car_dat3 %>% mutate(category=cut(wheelbase, breaks=4, labels=c("1","2","3", "4")))

p <- 
  ggplot(res) +
  aes(x = category, y = price) +
  geom_boxplot(color = "#3399ff") + stat_summary(fun=median, geom="line", aes(group=1), color = "#cc0099")  + 
  stat_summary(fun=median, geom="point", color = "#cc0099")+ #ggtitle('Leffetto quadratico dell'interasse sul prezzo')+
  xlab('Gruppi di interasse') + ylab('Prezzo')+
  theme_classic()
p
ggsave('C:/Users/Juli/Desktop/Progetto/Figures/gruppi_interasse.pdf', p, width = 4, height = 5)


##### Malattie cardiache ######

#Carga dei dati
malattia_cardiaca <- read_csv("C:/Users/Juli/Desktop/Progetto/data/heart.csv")
colnames(malattia_cardiaca) <- c("Età", "Sesso", "TipoDoloreToracico", "PressioneSanguignaRiposo",
                                 "Colesterolo", "GlicemiaBasale", "ECGRiposo",
                                 "FrequenzaCardiacaMassima", "AnginaEsercizio", "DepressioneST",
                                 "PendenzaST", "MalattiaCardiaca")
#Re-codificare i fattori
malattia_cardiaca$MalattiaCardiaca <- factor(malattia_cardiaca$MalattiaCardiaca, levels = c(0, 1), labels = c('No', 'Si'))
malattia_cardiaca$Sesso <- factor(malattia_cardiaca$Sesso, levels = c('F', 'M'), labels = c('Femmina', 'Maschio'))

# Grafico del tipo di dolore toracico per sesso

df <- malattia_cardiaca %>% group_by(MalattiaCardiaca, TipoDoloreToracico, Sesso) %>% count()
p_tipo_dolore <- 
  ggplot(df, aes(y = n, x=TipoDoloreToracico, fill=MalattiaCardiaca)) + facet_grid(.~Sesso)+
  geom_bar(stat = 'identity') +
  xlab('Tipo di Dolore al Petto') + ylab('Conteggio')+
  scale_fill_manual(values=c("#3399ff", "#cc0099"), 
                    name = "Malattia cardiaca")+
  theme_classic() + 
  theme(strip.background = element_blank(), strip.text = element_text(size = 12))


p<- 
  ggplot(malattia_cardiaca, aes(x=Età, fill = MalattiaCardiaca))+ facet_grid(.~Sesso)+
  geom_bar() +
  xlab('Età') + ylab('Conteggio')+
  scale_fill_manual(values=c("#3399ff", "#cc0099"), 
                    name = "Malattia cardiaca")+
  theme_classic() + 
  theme(line = element_line(size = 0.5), strip.background = element_blank(), strip.text = element_text(size = 12), 
        legend.title = element_text(size=8), legend.text = element_text(size = 6)) + 
  guides(color = guide_legend(override.aes = list(size = 0.2)))

p
ggsave('C:/Users/Juli/Desktop/Progetto/Figures/Età_heart_Sesso.pdf', p, width = 8, height = 4)

##Box plot Colesterolo

p <- ggplot(malattia_cardiaca, aes(y = Colesterolo, x=MalattiaCardiaca))+
  geom_boxplot(colour = "#cc0099") +
  xlab('Malattia Cardiaca') + ylab('Colesterolo')+
  scale_fill_manual(values=c("#3399ff", "#cc0099"), 
                    name = "Malattia Cardiaca")+
  theme_classic() + 
  theme(strip.background = element_blank(), strip.text = element_text(size = 12))

p
ggsave('C:/Users/Juli/Desktop/Progetto/Figures/colesterolo.jpeg', p, width = 4, height = 5)

### exercise

df <- malattia_cardiaca %>% group_by(MalattiaCardiaca, AnginaEsercizio, Sesso) %>% count()
ex_ang_plot <- 
  ggplot(df, aes(y = n, x=AnginaEsercizio, fill=MalattiaCardiaca)) + facet_grid(.~Sesso)+
  geom_bar(stat = 'identity') +
  xlab('Esercizio Angina') + ylab('Conteggio')+
  scale_fill_manual(values=c("#3399ff", "#cc0099"), 
                    name = "Malattia Cardiaca")+
  theme_classic() + 
  theme(strip.background = element_blank(), strip.text = element_text(size = 12))


p <-
  ggpubr::ggarrange(p_tipo_dolore, ex_ang_plot, 
                  labels = c("A", "B"),
                  ncol = 1, nrow = 2)



p
ggsave('C:/Users/Juli/Desktop/Progetto/Figures/tipo_dolore_exercise_angina.pdf', p, width = 8, height = 8)
## 
p <- 
  ggplot(malattia_cardiaca, aes(y = DepressioneST, x=MalattiaCardiaca))+
  geom_boxplot(colour = "#cc0099") +
  xlab('Malattia Cardiaca') + ylab('DepressioneST')+
  theme_classic() + 
  theme(strip.background = element_blank(), strip.text = element_text(size = 12))

p
ggsave('C:/Users/Juli/Desktop/Progetto/Figures/Depressione.pdf', p, width = 5, height = 5)

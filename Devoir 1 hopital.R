##ouverture du fichier et definition du dataframe
setwd("~/Documents/R/Datas")
hop<-read.csv2("satisfaction_hopital.csv")
##Verification du contenu du Dataframe
str(hop)

##-----------------------------------------------------------------------------------------------------
## QUESTION 1 
## Pour les trois variables catégorielles du fichier présentez les pourcentages de sujets relevant de chacune des modalités

##Les trois variables catégorielles sont "service", "sexe", "profession"
##Definition des variables par commodité
sexe<-hop$sexe
prof<-hop$profession
service<-hop$service

##Verification si elles contiennent de valeurs NA
table(sexe,useNA="always")
table(prof,useNA="always")
table(service,useNA="always")

##Definition de la variable THF qui renvoi les effectifs d'hommes et de femmes
THF<-table(factor(sexe,levels=c(0,1),labels=c("Hommes","Femmes")))

##Renvoi un graphique en baton rendant compte des effectifs d'hommes et de femmes en %
barplot(prop.table(THF)*100,ylim=c(0,60), main="effectifs homme/femme")

##Definition de la variable prof qui inclut les valeurs NA dans les resultats
Tprof<-table(factor(prof,levels=c(1,2,3,4,5,6,7,8,NA),labels=c("1","2","3","4","5","6","7","8","NA"),exclude=NULL),useNA="always")

##Renvoi un graphique en baton rendant compte des effectifs pour chaque profession en %
barplot(prop.table(Tprof)*100,ylim=c(0,25), main="effectifs par profession")

##Definition de la variable Tservice qui renvoi les proportions pour chaques reponses en incluant les valeurs NA dans les resultats
Tservice<-table(factor(service))

##Renvoi un graphique camembert qui presente les differentes proportions pour chaques réponses
pie(prop.table(Tservice)*100)

##-----------------------------------------------------------------------------------------------------
## QUESTION 2
## Pour les autres variables, donnez de façon synthétique : moyenne, médiane, écart-type, minimum, maximum, nombre de données disponibles (non manquantes)

## On supprime les variables catégorielles
hop2 <- subset(hop, select = -c(service, sexe, profession))

##Renvoi sous forme de tableau plusieurs données statistiques pour chaques variables
library(prettyR)
describe(hop2, num.desc=c("mean","median","sd","min","max","valid.n"))

##-----------------------------------------------------------------------------------------------------
## QUESTION 3
## Faites un histogramme du score de relation (score.relation)

## Renvoi un histogramme pour le score de relation
SCR<-hop$score.relation
hist(SCR, main="Score de Relation",xlab="Score de relation",ylab="Reponses")

##-----------------------------------------------------------------------------------------------------
## QUESTION 4
## À l’aide de deux « boxplots », représentez côte à côte la distribution du score de relation chez les hommes et les femmes.

##Renvoi deux boites a moustaches qui compare les reponses Homme et Femme a la question "score de relation"
Sexe<-factor(sexe,levels=c(0,1),labels=c("Hommes","Femmes"))
boxplot(hop$score.relation~Sexe, main="Score de relation Homme et Femme")

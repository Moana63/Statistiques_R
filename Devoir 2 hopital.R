##On définit le répertoire de travail et on charge le document

setwd("~/Documents/R/Datas")
d<-read.csv2("satisfaction_hopital.csv")

##Verification de la liste des variables du dataframe et des valeurs uniques de la variable "recommander"

names(d)
unique(d$recommander)

##-----------------------------------------------------------------------------
##Question 1
##Recoder la variable recommander en variable binaire. Toutes valeurs superieures a 1 seront recodées en 1 et celles inferieures ou egales a 1 seront recodées en 0:

recommander.b<-ifelse(d$recommander>1,1,0)

##Verification que le recodage a bien fonctionné:

table(recommander.b,d$recommander,deparse.level=2,useNA="always")

##-----------------------------------------------------------------------------
## Question 2
##On utilise la fonction twoby2 qui permet de tester la force de la relation entre deux variables binaires. Elle permet de visualiser l'OR et son intervalle de confiance a 95%:

sexe<-d$sexe
library(Epi)
twoby2(sexe,recommander.b)

##l'OR est proche de 1 et le p est plus grand que 0.05, les deux variables sont statistiquement indépendantes.

##-----------------------------------------------------------------------------
##Question 3
##La corrélation de Pearson necesite que l'on vérifit si une des deux variables score.relation ou age suit une loi normale:

hist(d$score.relation)
hist(d$age)
qqnorm(d$age);qqline(d$age)

##On considere que l'age suit une loi normale (histogramme en forme de "cloche" et les données suivent a peu pres la ligne QQ norme, sauf aux extrémitées), on peut donc utiliser la corrélation de Pearson

cor.test(d$age,d$score.relation)

##p est superieur a 0.05, et cor est faible, on considere donc que les variables sont indépendantes.

##-----------------------------------------------------------------------------
##Question 4
##Pour comparer deux moyennes du score de relation dans les sous groupes Homme et Femme on utilisera le test t de student. Pour ce test les deux variances (ou ecart type) du groupe doivent etre égales.
##Pour le vérifier on peut utiliser la fonction tapply:

with(d,tapply(score.relation,sexe,sd,na.rm=TRUE))

## On considère que les écart type sont proches et que l'echantillon est assez grand (534). On peut donc utiliser le test de student:

t.test(d$score.relation~sexe,var.equal = TRUE)

##p est beaucoup plus grand que 0.05, la differences du score de relation dans le groupe des femmes et celui des hommes n'est pas significative.

install.packages("tidyverse")
library(tidyverse)
install.packages("ggridges")
library(ggridges)
library(tidyr)
library(dplyr)
library(ggplot2)
install.packages("GGally")
library(GGally)
install.packages("corrplot")
library(corrplot)
install.packages("devtools")
library(devtools)
install.packages("gridExtra")
library("gridExtra")
install.packages("dotwhisker")
library(dotwhisker)
install.packages("broom")
library(broom)
install.packages("tidyr")
library(tidyr)

dataset <- read.csv(data/"dataset.csv") #import conjunto de dados "dataset"

#Parte I
########################### 1.Análise Descritiva Dataser ###########################

length(dataset) #número de atributos/colunas
head(dataset) #primeiras linhas do dataset
tail(dataset) #últimas linhas do dataset
ncol(dataset) #número de colunas
nrow(dataset) #número de registos/linhas
str(dataset) #estrutura dos objetos
dim(dataset) #nº de registos e nº de atributos do datataset
summary(dataset) #resumo de toda a informação do dataset

sum(is.na(dataset$Survived)) #conta os missing values da variável Survived
sum(is.na(dataset$Pclass)) #conta os missing values da variável Pclass
sum(is.na(dataset$Name)) #conta os missing values da variável Name
sum(is.na(dataset$Sex)) #conta os missing values da variável Sex
sum(is.na(dataset$Age)) #conta os missing values da variável Age
sum(is.na(dataset$SibSp)) #conta os missing values da variável SibSp
sum(is.na(dataset$Parch)) #conta os missing values da variável Parch
sum(is.na(dataset$Ticket)) #conta os missing values da variável Ticket
sum(is.na(dataset$Fare)) #conta os missing values da variável Fare
sum(is.na(dataset$Cabin)) #conta os missing values da variável Cabin
sum(is.na(dataset$Embarked)) #conta os missing values da variável Embarked

sd(dataset$PassengerId) #calcula o desvio padrão da variável PassengerId
sd(dataset$Survived) #calcula o desvio padrão da variável Survived
sd(dataset$Pclass) #calcula o desvio padrão da variável Pclass
sd(dataset$Age) #calcula o desvio padrão da variável Age
sd(dataset$SibSp) #calcula o desvio padrão da variável SibSp
sd(dataset$Parch) #calcula o desvio padrão da variável Parch
sd(dataset$Fare) #calcula o desvio padrão da variável Fare


########################### Análise Exploratória ###########################

#Mostra o frequência das idades dos passageiros a bordo
hist(dataset$Age,
     col = "#FFCCCC",
     xlab = "Idade",
     ylab = "Nº de passageiros",
     ylim = c(0,250),
     main = "Frequência das idades dos passageiros a bordo")

#Mostra o número de irmãos e esposas a bordo"
hist(dataset$SibSp,
     col = "#CC6633",
     xlab = "Nº de irmãos/esposas a bordo",
     ylab = "Nº de passageiros",
     ylim = c(0,800),
     main = "Nº de passageiros que tinham irmãos/esposas a bordo")


#Mostra a taxa de sobreviventes e não sobreviventes segundo a classe em que viajavam
counts = table(dataset$Survived, dataset$Pclass)
barplot(counts, 
        main = "Taxa de sobrevivência por classe de passageiros",
        ylim = c(0,600),
        names.arg = c("1st", "2nd", "3rd"),
        xlab = "Classe", 
        col = c("#CC99CC", "#660066"))
legend("topleft", 
       inset = .02,
       legend = c("Not survived","Survived"), 
       fill = c("#CC99CC", "#660066"), 
       horiz = TRUE)


#Mostra a taxa de sobreviventes e não sobreviventes por sexo
counts = table(dataset$Survived, dataset$Sex)
barplot(counts, 
        main = "Taxa de sobrevivência por sexo",
        ylim = c(0,600),
        col = c("#33FFCC", "#0099CC"))
legend("topleft", 
       inset = .01,
       legend = c("Not survived","Survived"), 
       fill =  c("#33FFCC", "#0099CC"))


#Mostra a taxa de sobreviventes e não sobreviventes por porta de embarque
counts = table(dataset$Embarked, dataset$Survived)
barplot(counts,
        col = c("#00CCCC","#009999","#006666"),
        ylab = "Nº de passageiros",
        names.arg = c("Not survived","Survived"),
        ylim = c(0,600),
        main = " Taxa de sobrevivência por porta de embarque")
legend("topleft", 
       inset = .25,
       legend = c("Cherbourg", "Queenstown", "Southampton"),
       fill = c("#00CCCC","#009999","#006666"))


########################### 2.Pré-Processamento de Dados ###########################

#a)Substitui os valores em falta do atributo "Age" pela média das idades.
dataset$Age[is.na(dataset$Age)] <- mean(dataset$Age,na.rm = TRUE)
sum(is.na(dataset$Age)) #verificar se ainda existe algum valor em falta


#b)Criar um novo atributo “Sex_Num”, de tipo numérico, baseado no atributo já existente “Sex".
# “male” -> 0, “female” -> 1
dataset$Sex_Num <- ifelse(dataset$Sex == "male", 0 ,1)
summary(dataset) #verificar a crição do atributo "Sex_Num"


#c) Criar dois novos atributos: “Survived_Fac”, de tipo factor com 2 níveis (yes e no).
dataset$Survived_Fac <- factor(dataset$Survived, levels = 0:1, labels=c("No","Yes"))


#c) cont. “Survived_Num”, de tipo numérico, baseados no atributo já existente “Survived”.
dataset$Survived_Num <- as.numeric(dataset$Survived)
is.numeric(dataset$Survived_Num) #Verificar se o atributo #“Survived_Num” é numérico


#d) Atributo “Embarked”:- substituir os valores “C” pelo valor “Cherbourg”,“Q” pelo valor “Queensland” e “S” pelo valor “Southampton".
dataset$Embarked[dataset$Embarked=="C"] <- "Cherbourg"
dataset$Embarked[dataset$Embarked=="Q"] <- "Queensland"
dataset$Embarked[dataset$Embarked=="S"] <- "Southampton"


#d) cont. Substituir os valores em falta pelo valor “Southampton".
dataset$Embarked %>% replace_na('Southampton')
dataset$Embarked[dataset$Embarked==""] <- "Southampton"
sum(is.na(dataset$Embarked)) #conta os missing values da variável Embarked


#d) cont. Converter o atributo em questão em factor com 3 níveis (ordem à escolha).
dataset$Embarked <- as.factor(dataset$Embarked)
summary(dataset$Embarked)#Verificar se o atributo "Embarked" esta dividido em facto ccom 3 níveis


#e)Criar um novo atributo “Pclass_Fac” de tipo factor com 3 níveis (ordem à escolha) baseado no atributo já existente “Pclass”.
dataset$Pclass_Fac <- as.factor(ifelse(dataset$Pclass == 1, 'First Class',
                                       ifelse(dataset$Pclass == 2 , 'Second Class', 'Third Class')))
summary(dataset$Pclass_Fac) #Verificar se o atributo "Pclass_Fac" foi criado


#f) Alterar o nome dos atributos “Pclass_Fac” para “Class_Fac”; 
ncol(dataset) #Verificar nº de colunas
names(dataset)[16] <- "Class_Fac"


#f) cont. e “Pclass” para “Class”
names(dataset)[3] <- "Class"
summary(dataset) #Verificar a alteração dos nomes dos atributos


#g) Criar um novo atributo: “Age_Fac” de tipo factor com 5 níveis, baseado no atributo já existente “Age”.
dataset$Age_Fac <- ifelse(dataset$Age <13, 'Child',
                          ifelse(dataset$Age <20, 'Teen',
                                 ifelse(dataset$Age <36, 'YoungAdult',
                                        ifelse(dataset$Age <56, 'Adult', 'Elderly'))))

dataset$Age_Fac <- factor(dataset$Age_Fac, levels=c("Child", "Teen", "YoungAdult", "Adult", "Elderly"))
summary(dataset)

names(dataset)


########################### 3. Visualizações ###########################

#Distribuição etária por classe de bilhete
dataset %>%
  ggplot(mapping =  aes(x = Class_Fac, y = Age)) +
  geom_point(colour = "#1380A1", size = 1) +
  geom_jitter(aes(colour = Survived_Fac))+ 
  geom_boxplot(alpha = 0.7, outlier.colour = NA)+
  labs(title = "Distribuição etária por Classe de bilhete",
       x = "Ticket Class",
       y = "Age")

#Densidade de sobreviventes por idade, class, porta de  embarque e tarifa de bilhete
dataset %>%
  ggplot(mapping = aes(Age, Fare, color = Survived_Fac, shape = Sex)) +
  geom_point() +
  scale_y_log10() +
  facet_grid(Class_Fac ~ Embarked) +
  labs(title = "Densidade de sobrevivência por sexo, classe, idade, tarifa de bilhete 
e porta de embarque")


#Prioridade e padrão de salvamento
dataset%>%
  filter(Fare<=300)%>%
  ggplot(aes(x= Age, y= Fare)) +
  geom_point(aes( colour= Survived_Fac, size= Fare)) +
  facet_grid(Sex~Class_Fac) +
  labs(title = "Prioridade e padrão de salvamento",
       x = "Age",
       y = "Fare",
       subtitle = "As crianças e mulheres por ordem de classe de bilhete foram as primeiras a ser consideradas 
no plano de salvamento com prioridade para as mulheres, crianças e adultos >= 60 anos")


#Relações familiares entre Parche SibSp
dataset %>%
  ggplot(aes(Parch, SibSp, color = Survived_Fac)) +
  geom_count() +
  labs(title = "Relações familiares entre Parch e SibSp",
       x = "Parch (nº de pais/crianças a bordo)",
       y = "SibSp (nº de irmãos/esposas a bordo)",
       subtitle = "Verifica quantos casos existiam para cada combinação entre o Parch e o SibSp. O tamanho dos 
círculos é proporcional ao número de casos. As cores mostram se o passageiro sobreviveu ou não")


#Densidade de sobrevivência por sexo
ggplot(dataset, aes(x=Age)) +
  geom_density(aes(fill = Survived_Fac), alpha = 0.5) +
  facet_wrap(~Sex) +
  labs(title = "Densidade de sobrevivência por sexo")


#Relação entre Parch e Sex
dataset %>%
  ggplot() +
  geom_bar(aes(Parch, fill = Sex), position = "dodge") +
  scale_y_log10() +
  labs(title = "Relação entre Parch e Sex")




df = read.csv("breast_cancer.csv")

########################################### Análise Descritiva Breast_Cancer#################################################

#-------------------------------------------------------------------------------------------------------#

length(breast_cancer)                             #N?mero de atributos/colunas
head(breast_cancer)                               #Primeiras linhas do dataset
tail(breast_cancer)                               #?ltimas linhas do dataset
ncol(breast_cancer)                               #N?mero de colunas
nrow(breast_cancer)                               #N?mero de registos/linhas
str(breast_cancer)                                #Estrutura dos objetos
dim(breast_cancer)                                #N?mero de registos e n?mero de atributos do datataset
summary(breast_cancer)                            #resumo de toda a informa??o do dataset
names(breast_cancer)

#-------------------------------------------------------------------------------------------------------#

sum(is.na(breast_cancer$diagnosis))               #Conta os missing values da vari?vel diagnosis

sum(is.na(breast_cancer$radius_mean))             #Conta os missing values da vari?vel radius_mean
sum(is.na(breast_cancer$texture_mean))            #Conta os missing values da vari?vel texture_mean
sum(is.na(breast_cancer$perimeter_mean))          #Conta os missing values da vari?vel perimeter_mean
sum(is.na(breast_cancer$area_mean))               #Conta os missing values da vari?vel area_mean
sum(is.na(breast_cancer$smoothness_mean))         #Conta os missing values da vari?vel smoothness_mean
sum(is.na(breast_cancer$compactness_mean))        #Conta os missing values da vari?vel compactness_mean
sum(is.na(breast_cancer$concavity_mean))          #Conta os missing values da vari?vel concavity_mean
sum(is.na(breast_cancer$concave_points_mean))     #Conta os missing values da vari?vel concave_points_mean
sum(is.na(breast_cancer$symmetry_mean))           #Conta os missing values da vari?vel symmetry_mean
sum(is.na(breast_cancer$fractal_dimension_mean))  #Conta os missing values da vari?vel fractal_dimension_mean

sum(is.na(breast_cancer$radius_se))               #Conta os missing values da vari?vel radius_se
sum(is.na(breast_cancer$texture_se))              #Conta os missing values da vari?vel texture_se
sum(is.na(breast_cancer$perimeter_se))            #Conta os missing values da vari?vel perimeter_se
sum(is.na(breast_cancer$area_se))                 #Conta os missing values da vari?vel area_se
sum(is.na(breast_cancer$smoothness_se))           #Conta os missing values da vari?vel smoothness_se
sum(is.na(breast_cancer$compactness_se))          #Conta os missing values da vari?vel compactness_se
sum(is.na(breast_cancer$concavity_se))            #Conta os missing values da vari?vel concavity_se
sum(is.na(breast_cancer$concave_points_se))       #Conta os missing values da vari?vel concave_points_se
sum(is.na(breast_cancer$symmetry_se))             #Conta os missing values da vari?vel symmetry_se
sum(is.na(breast_cancer$fractal_dimension_se))    #Conta os missing values da vari?vel fractal_dimension_se

sum(is.na(breast_cancer$radius_worst))            #Conta os missing values da vari?vel radius_worst
sum(is.na(breast_cancer$texture_worst))           #Conta os missing values da vari?vel texture_worst
sum(is.na(breast_cancer$perimeter_worst))         #Conta os missing values da vari?vel perimeter_worst
sum(is.na(breast_cancer$area_worst))              #Conta os missing values da vari?vel area_worst
sum(is.na(breast_cancer$smoothness_worst))        #Conta os missing values da vari?vel smoothness_worst
sum(is.na(breast_cancer$compactness_worst))       #Conta os missing values da vari?vel compactness_worst
sum(is.na(breast_cancer$concavity_worst))         #Conta os missing values da vari?vel concavity_worst
sum(is.na(breast_cancer$concave_points_worst))    #Conta os missing values da vari?vel concave_points_worst
sum(is.na(breast_cancer$symmetry_worst))          #Conta os missing values da vari?vel symmetry_worst
sum(is.na(breast_cancer$fractal_dimension_worst)) #Conta os missing values da vari?vel fractal_dimension_worst

#-----------------------------------------------------------------------------------------------------#
sd(breast_cancer$id)                              #Calcula o desvio padr?o da vari?vel id
sd(breast_cancer$X1)                              #Calcula o desvio padr?o da vari?vel X1
sd(breast_cancer$radius_mean)                     #Calcula o desvio padr?o da vari?vel radius_mean
sd(breast_cancer$texture_mean)                    #Calcula o desvio padr?o da vari?vel texture_mean
sd(breast_cancer$perimeter_mean)                  #Calcula o desvio padr?o da vari?vel perimeter_mean
sd(breast_cancer$area_mean)                       #Calcula o desvio padr?o da vari?vel area_mean
sd(breast_cancer$smoothness_mean)                 #Calcula o desvio padr?o da vari?vel smoothness_mean
sd(breast_cancer$compactness_mean)                #Calcula o desvio padr?o da vari?vel compactness_mean
sd(breast_cancer$concavity_mean)                  #Calcula o desvio padr?o da vari?vel concavity_mean
sd(breast_cancer$concave_points_mean)             #Calcula o desvio padr?o da vari?vel concave_points_mean
sd(breast_cancer$symmetry_mean)                   #Calcula o desvio padr?o da vari?vel symmetry_mean
sd(breast_cancer$fractal_dimension_mean)          #Calcula o desvio padr?o da vari?vel fractal_dimension_mean

sd(breast_cancer$radius_se)                       #Calcula o desvio padr?o da vari?vel radius_se
sd(breast_cancer$texture_se)                      #Calcula o desvio padr?o da vari?vel texture_se
sd(breast_cancer$perimeter_se)                    #Calcula o desvio padr?o da vari?vel perimeter_se
sd(breast_cancer$area_se)                         #Calcula o desvio padr?o da vari?vel area_se
sd(breast_cancer$smoothness_se)                   #Calcula o desvio padr?o da vari?vel smoothness_se
sd(breast_cancer$compactness_se)                  #Calcula o desvio padr?o da vari?vel compactness_se
sd(breast_cancer$concavity_se)                    #Calcula o desvio padr?o da vari?vel concavity_se
sd(breast_cancer$concave_points_se)               #Calcula o desvio padr?o da vari?vel concave_points_se
sd(breast_cancer$symmetry_se)                     #Calcula o desvio padr?o da vari?vel symmetry_se
sd(breast_cancer$fractal_dimension_se)            #Calcula o desvio padr?o da vari?vel fractal_dimension_se

sd(breast_cancer$radius_worst)                    #Calcula o desvio padr?o da vari?vel radius_worst
sd(breast_cancer$texture_worst)                   #Calcula o desvio padr?o da vari?vel texture_worst
sd(breast_cancer$perimeter_worst)                 #Calcula o desvio padr?o da vari?vel perimeter_worst
sd(breast_cancer$area_worst)                      #Calcula o desvio padr?o da vari?vel area_worst
sd(breast_cancer$smoothness_worst)                #Calcula o desvio padr?o da vari?vel smoothness_worst
sd(breast_cancer$compactness_worst)               #Calcula o desvio padr?o da vari?vel compactness_worst
sd(breast_cancer$concavity_worst)                 #Calcula o desvio padr?o da vari?vel concavity_worst
sd(breast_cancer$concave_points_worst)            #Calcula o desvio padr?o da vari?vel concave_points_worst
sd(breast_cancer$symmetry_worst)                  #Calcula o desvio padr?o da vari?vel symmetry_worst
sd(breast_cancer$fractal_dimension_worst)         #Calcula o desvio padr?o da vari?vel fractal_dimension_worst


#Parte II

############################################ 2. Análise PCA ###################################################

#Step 1 - Instalar e carregar os packages que iremos utilizar nesta An?lise

install.packages("FactoMineR")
install.packages("corrplot")
install.packages("factoextra")


library(FactoMineR)
library(corrplot)
library(factoextra) 
library(ggplot2)

#Step 2 - Retirar Atributos que n?o s?o relevantes para a An?lise

breast_cancer$X1 <- NULL
breast_cancer$id <- NULL

summary(breast_cancer)
names(breast_cancer)

#Step 3 - Executar a fun??o

breast.pca <- PCA(breast_cancer,
                  scale.unit=TRUE,
                  quali.sup=1,
                  graph=TRUE)

#Step 4 - Manter 72,7% da vari?ncia do conjunto de dados original

summary(breast.pca)

#Step 5 - Mostrar a correla??o entre as vari?veis

co <- cor(breast_cancer[,2:31])
corrplot(co, method = "circle")

#Alternativas de View
#corrplot(co, method = "number")
#corrplot(co, method="square")


#Step 6 - An?lise de PCA 


#An?lise de PCA dos Registo
breast.pca$ind # resultados dos valores dos indiv?duos (registos)
plot(breast.pca, choix="ind", axes=1:2)


# An?lise de PCA das Vari?veis
#gr?fico alternativo
fviz_pca_var(breast.pca,
             col.var =
               "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)

# An?lise de PCA dos Registos (tendo em conta o atributo 1 - diagnosis)
plot(breast.pca, choix="ind", habillage=1, cex=0.7)



#Step 7 - An?lise dos eigenvalues
breast.pca$eig

# gr?fico: eigenvalues associado a cada uma das componentes
barplot(breast.pca$eig[,1],main="Eigenvalues",
        names.arg = paste("dim" ,1:nrow(breast.pca$eig)))


#Gr?fico alternativo
fviz_pca_ind(breast.pca,
             col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


# analisar/visualizar os eigenvalues
fviz_eig(breast.pca)

# Eigenvalues
# extrair os eigenvalues/vari?ncia dos
# components principais
eig.val <- get_eigenvalue(breast.pca)
eig.val

#An?lise de resultados dos registos:
round(scale(breast_cancer[,2:12]),1)





adult <- read.csv("C:/Users/Newton/Desktop/DA/R/Data-Science & Machine-Learning/Training Exercises/Machine Learning Projects/CSV files for ML Projects/adult_sal.csv")
head(adult)

library(dplyr)
adult <- select(adult,-X)

head(adult)
str(adult)
summary(adult)



#Employeer Type
table(adult$type_employer)

unemployed <- function(job){
  job <-as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return ('Unemployed')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('Self-emp')
  }else if (job=='State-gov' | job =='Federal-gov' | job=='Local-gov'){
    return('Government')
  }else{
    return(job)
  }
}

adult$type_employer <-sapply(adult$type_employer,unemployed)
table(adult$type_employer)



#
table(adult$marital)

group_marital <- function(mar){
  mar <- as.character(mar)
  
  # Not-Married
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
    
    # Never-Married   
  }else if(mar=='Never-married'){
    return(mar)
    
    #Married
  }else{
    return('Married')
  }
}


adult$marital <-sapply(adult$marital,group_marital)

table(adult$marital)

#
levels(adult$country)
table(adult$marital)

table(adult$country)
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')


contry_grp <- function(cont){
  if (cont %in% Asia){
    return('Asia')
  }else if (cont %in% North.America){
    return('North.America')
  }else if (cont %in% Europe){
    return('Europe')
  }else if (cont %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')
  }
}


adult$country <- sapply(adult$country,contry_grp)
table(adult$country)

str(adult)

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)


adult[adult == '?'] <- NA
table(adult$type_employer)


adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$occupation <- sapply(adult$occupation,factor)
adult$income <- sapply(adult$income,factor)


library(Amelia)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))


# May take awhile
adult <- na.omit(adult)


missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

#EDA
library(ggplot2)
ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()

ggplot(adult,aes(hr_per_week)) + geom_histogram() + theme_bw()



names(adult)[names(adult)=="country"] <- "region"

ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black')+theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Import Library
library(caTools)

# Set a random see so your "random" results are the same as this notebook
set.seed(101) 

# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(adult$income, SplitRatio = 0.70) # SplitRatio = percent of sample==TRUE

# Training Data
train = subset(adult, sample == TRUE)

# Testing Data
test = subset(adult, sample == FALSE)

model = glm(income ~ ., family = binomial(logit), data = train)

summary(model)

new.step.model <- step(model)

summary(new.step.model)

test$predicted.income = predict(model, newdata=test, type="response")
table(test$income, test$predicted.income > 0.5)
#Accuracy
(6372+1423)/(6372+1423+548+872)

#recall
6732/(6372+548)

#precision
6732/(6372+872)

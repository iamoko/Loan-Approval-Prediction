library(ggplot2)
library(dplyr)
library(rpart)
library(plyr)
library(car)

train<-read.csv(file="Datasets/train.csv",na.strings = c(""," ",NA))
test<-read.csv(file="Datasets/test.csv",na.strings = c(""," ",NA))
#library(mlr)
#summarizeColumns(train)
str(train)
#Applicanttincome and coapplicantincome are numeric variables
boxplot(train$ApplicantIncome,train$CoapplicantIncome,names=c("App Income","Coapp Income"),main="Income")

#They have many outliers thus The distributions are right-asymetric
boxplot(train$LoanAmount,main="Loan AMount")

hist(train$Loan_Amount_Term,breaks=500,main="Loan Amount Term")

#shows missing values
sapply(train, function(x) sum(is.na(x)))

#credit history should be a factor variable because it has few different values
train$Credit_History <-as.factor(train$Credit_History)
test$Credit_History <- as.factor(test$Credit_History)

barplot(table(train$Loan_Status), main = "Loan Status")




#############################  LOAN STATUS BY OTHER VARIABLES ##############################################
#print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Gender)+ggtitle("Loan Status by Gender of Applicant"))

#print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Married)+ggtitle("Loan Status by Marital Status of Applicant"))
#a larger proportion of not married applicants are refused than maRried ones
#print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Dependents)+ggtitle("Loan Status by number of Dependents of Applicant"))
#a smaller proportion of applicants with 2 dependents is refused than other numbers
#print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Education)+ggtitle("Loan Status by Education of Applicant"))
#a larger proportion on non graduates are refused than graduates
#print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Self_Employed)+ggtitle("Loan Status by Employment status of Applicant"))
#not self employed seemS to be slightly preferred
#print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Loan_Amount_Term)+ggtitle("Loan Status by terms  of loan"))
#most of the loans are for 360 months thus difficult to see
#print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Credit_History)+ggtitle("Loan Status by credit history of Applicant"))
#Almost all applicants with history=0 are refused
#print(ggplot(train, aes(x=Loan_Status))+geom_bar()+facet_grid(.~Property_Area)+ggtitle("Loan Status by property area"))
#it's easiest to get a loan if the property is semi urban and hardest if it is rural
#print(ggplot(train, aes(x=Loan_Status,y=ApplicantIncome))+geom_boxplot()+ggtitle("Loan Status by Applicant income"))

#print(ggplot(train, aes(x=Loan_Status,y=CoapplicantIncome))+geom_boxplot()+ggtitle("Loan Status by coapplicant income"))

#print(ggplot(train, aes(x=Loan_Status,y=LoanAmount))+geom_boxplot()+ggtitle("Loan Status by Loan Amount"))
########################################################################################################################



########################################### TIDYING THE DATA   ##################################################################

alldata<-rbind(train[,2:12],test[,2:12])

############################################     MARRIED MISSING VALUES  ####################
print(ggplot(data=alldata[alldata$ApplicantIncome<20000,],aes(CoapplicantIncome,fill=Married))+geom_bar(position="dodge")+facet_grid(Gender~.))
#Adding another column to dataframe for totalincome
library(plyr)
alldata2<-mutate(alldata,TotalIncome=ApplicantIncome+CoapplicantIncome)
print(ggplot(data=alldata2,aes(TotalIncome,fill=Married))+geom_bar(position="dodge")+facet_grid(Gender~.))
alldata2<-mutate(alldata,TotalIncome=ApplicantIncome+CoapplicantIncome)

#Assuming that the institution coapplicants are most possible the wife or husband for marriage
alldata2$Married[is.na(alldata2$Married) & alldata2$CoapplicantIncome==0]<-"No"
alldata2$Married[is.na(alldata2$Married)]<- "Yes"



############################################     GENDER AND DEPENDENTS   #####################
alldata2[is.na(alldata2$Gender) & is.na(alldata2$Dependents),]  #checking for missing both gender && dependents

#solution for missing both by choice
alldata2$Gender[is.na(alldata2$Gender) & is.na(alldata2$Dependents)] <- "Male"
#print(ggplot(alldata2,aes(x=Dependents, fill=Gender)) + geom_bar() + facet_grid(.~Married))  



###     FROM THE PLOT     ###
alldata2$Dependents[is.na(alldata2$Dependents) & alldata2$Married=="No"]<- "0"
mm <- alldata2[(alldata2$Gender=="Male" & alldata2$Married=="Yes"),c(3,6:9,11)]
mmtrain<-mm[!is.na(mm$Dependents),]
mmtest<- mm[is.na(mm$Dependents),]

depFit <- rpart(data=mmtrain,Dependents~.,xval=3)
#accuracy
p<-predict(depFit,mmtrain,type="class")
acc=sum(p==mmtrain[,1])/length(p)
acc
###########      MISSING DEPENDANTS VALUES FINALLY ################
alldata2$Dependents[is.na(alldata2$Dependents) & alldata2$Gender=="Male" & alldata2$Married == "Yes"]<- predict(depFit,newdata=mmtest,type="class")
#View(alldata2)


###########      MISSING GENDER    ###############################
#Gender with no missing values
gtrain<-alldata2[!is.na(alldata2$Gender),1:7]
gtest<-alldata2[is.na(alldata2$Gender),1:7]
genFit<-rpart(data=gtrain,Gender~.,xval=3)
#Accuracy for Gender Prediction
p<-predict(genFit,gtrain,type="class")
acc<-sum(p==gtrain[,1])/length(p)
acc
########### Filling in the missing gender values ################################
alldata2$Gender[is.na(alldata2$Gender)]<-predict(genFit,gtest,type="class")



############################################    MISSING SELF EMPLOYED      ###################
#since earlier, most are not self employed. These missing values will be imputed using the mode ="No"
alldata2$Self_Employed[is.na(alldata$Self_Employed)] <- "No"



############################################    MISSING CREDIT HISTORY      #########################
#nas in credithistory treated specially

alldata2$Credit_History<-car::recode(alldata2$Credit_History,"NA=2")



############################################    MISSING LOAN AMOUNT         ####################
ltrain<-alldata2[!is.na(alldata2$LoanAmount) & alldata2$LoanAmount<500,c(1:8,10)]
ltest <- alldata2[is.na(alldata2$LoanAmount),c(1:8,10)]
loanFit <- glm(data=ltrain,LoanAmount~.,na.action=na.exclude)
#impute
summary(loanFit)
alldata2$LoanAmount[is.na(alldata2$LoanAmount)] <- predict(loanFit,newdata=ltest)


############################################    MISSING LOAN AMOUNT TERM      ##################
alldata2$Loan_Amount_Term <- as.factor(alldata2$Loan_Amount_Term)
#print(ggplot(data=alldata2,aes(x=Loan_Amount_Term))+geom_bar())
alldata2$Loan_Amount_Term[is.na(alldata2$Loan_Amount_Term)]<-"360"  #bcoz its the mode
alldata2$Loan_Amount_Term[alldata2$Loan_Amount_Term=='350']<-'360'
alldata2$Loan_Amount_Term[alldata2$Loan_Amount_Term=='6']<-'60'
#print(ggplot(data=alldata2,aes(x=Loan_Amount_Term))+geom_bar())


##################################################################################################################################################################

newtrain <- cbind(Loan_Status=train$Loan_Status,alldata2[1:614,])
str(newtrain)

#Dummy Loan status for test set
Loan_Status <- as.factor(sample(c("N","Y"),replace=TRUE,size=dim(test)[1]))
newtest <- cbind(alldata2[615:981,], Loan_Status)



model <- glm(Loan_Status~Married+Credit_History +Property_Area, data=newtrain,family='binomial')
summary(model)


p<-predict(model,newtest,type="response")
Loan_Statuslog <- ifelse(p > 0.5, 1, 0)
Loan_Status <- factor(Loan_Statuslog, levels=c(0, 1))

acc=sum(Loan_Statuslog)/length(p)
print(paste("Accuracy ", acc*100), quote=F)



gettwe <- cbind(test,Loan_Status)
#write.csv(gettwe[,-c(2:12)], file = "Datasets/Final.csv", row.names = FALSE)

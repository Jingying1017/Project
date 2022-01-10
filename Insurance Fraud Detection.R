rm(list = ls())
Project=read.csv(file.choose(),header=T, stringsAsFactors=TRUE)
attach(Project)
library(tree)

#EDA
glm.fit1=glm(fraud_reported~.,family="binomial",data=Project)
summary(glm.fit1)
glm.fit2=glm(fraud_reported~policy_state+umbrella_limit+insured_hobbies+insured_relationship+capital_loss+collision_type+incident_severity+witnesses+auto_make,family="binomial",data=Project) 
summary(glm.fit2) 
glm.fit3=glm(fraud_reported~umbrella_limit+insured_hobbies+insured_relationship+collision_type+incident_severity,family="binomial",data=Project) 
summary(glm.fit3)

set.seed(1)
plot(Project$insured_relationship, Project$fraud_reported,xlab="Relationships", ylab ="Reported", main="Result vs. Relationships")  
plot(Project$incident_severity, Project$fraud_reported, xlab="Incident Severity", ylab ="Reported", main="Result vs. Incident Severity")  
plot(Project$collision_type, Project$fraud_reported, xlab="Collision Type", ylab ="Reported", main="Result vs. Collision Type")  
plot(Project$insured_hobbies, Project$fraud_reported, xlab="Hobbies", ylab ="Reported", main="Result vs. Insured Hobbies")  

#Logistic regression 
set.seed(1) 
train=sample(nrow(Project),nrow(Project)*0.8) 
Project.test=Project[-train, ] 
test.truevalue=fraud_reported[-train] 

glm.fit2=glm(fraud_reported~umbrella_limit+insured_hobbies+insured_relationship+collision_type+incident_severity,data=Project,subset=train,family="binomial") 
glm.probs2=predict(glm.fit2,Project.test, type="response") 
glm.pred2=rep("N",200) 
glm.pred2[glm.probs2>.5]="Y" 
table(glm.pred2,test.truevalue) 
mean(glm.pred2==test.truevalue)

#Classification Tree
tree.model=tree(fraud_reported~umbrella_limit+insured_hobbies+insured_relationship+collision_type+incident_severity,Project,subset =train)
fraud_reported.test=fraud_reported[-train]

cv.model=cv.tree(tree.model,K=10,FUN=prune.misclass) 
cv.model

prune.model=prune.tree(tree.model,best=5)
plot(prune.model)
text(prune.model,pretty=0)

prunetree.pred=predict(prune.model,Project.test,type="class")
table(prunetree.pred,fraud_reported.test)


#RandomForest and Bagging
library(randomForest)
set.seed(1) 
bag.Project=randomForest(fraud_reported~umbrella_limit+insured_hobbies+insured_relationship+collision_type+incident_severity,data=Project,subset=train,mtry=5,importance=TRUE) 
bag.Project

rf.Project=randomForest(fraud_reported~umbrella_limit+insured_hobbies+insured_relationship+collision_type+incident_severity,data=Project,subset=train,mtry=2,importance=TRUE) 
rf.Project






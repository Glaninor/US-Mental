dpq = DPQ_I
dpq <- centralImputation(dpq)
judge = vector(mode = "numeric",length = nrow(dpq))
dpq = cbind(dpq,judge)
dpqsum = as.data.frame(matrix(ncol=2,nrow(dpq)))
for (i in 1:nrow(dpq))
{
dpqsum[i,2] <- sum(dpq[i,2:11])
}

for (i in 1:nrow(dpq))
{
  if (dpqsum[i,2]>1)
    dpq[i,12]<-1
  else dpq[i,12]<-0
}

dpq=dpq[,c(1,12)]

all <- centralImputation(all)
all.judge = left_join(dpq,all,by="seqn")
all.judge <- all.judge[,-which(names(all.judge)=="dpq010"):-which(names(all.judge)=="dpq100")]
acq = ACQ_I[,c(1:4)]
acq <- centralImputation(acq)
language = vector(mode="numeric",length=nrow(acq))
acq=cbind(acq,language)
for (i in 1:nrow(acq))
{
  if(acq[i,3]==8)
  {  acq[i,5]<-2}
  if(acq[i,4]==9)
  {  acq[i,5]<-0}
  
}
acq[which(is.na(acq$acd011a)),5]=1
acq=acq[,-2:-4]
acq$seqn <- as.numeric(acq$seqn)
all.judge = left_join(all.judge,acq,by="seqn")

cov.mental = cov(all.judge,method = "spearman")
cor.mental = cor(all.judge,method = "spearman")
cor.mental <- centralImputation(cor.mental)
pcor.mental = cor2pcor(cor.mental)
colnames(pcor.mental) = colnames(cor.mental)
rownames(pcor.mental) = rownames(cor.mental)
pcor.judge = pcor.mental[,2]
pcor.judge = pcor.judge[c(-1,-2)]
summary(pcor.judge)
judge.model <- pcor.judge[which(abs(pcor.judge)>0.05)]
View(judge.model)
Xname <- names(judge.model)
X = cbind(all.judge$seqn,all.judge$judge)
colnames(X) = c("seqn","judge")
X <- as.data.frame(X)
for (i in 1:18)
{
  X = cbind(X,all.judge[,which(names(all.judge)==Xname[i])])
}
colnames(X) <- c("seqn","judge","cdq001","hsd010","diq275","dbq229","dlq140","kiq480","mcq170k","mcq240l","sld012","slq050","slq120","smq895","dr1tnumf","dr1tm221","drd350cq","dr2.320z","dr1dbih.y","dr1ip182" )

logit.model = as.data.frame(X)
predict.data = with(logit.model,data.frame(logit.model))
ntrain = sample(nrow(predict.data),floor(0.7*nrow(predict.data)),replace=FALSE)
train = predict.data[ntrain,]
test = predict.data[-ntrain,]
train[which(is.na(train[1,]))]

judge.glm = glm(judge~.,family=binomial(link="logit"),data=train)
summary(judge.glm)
judge.glm.step = step(judge.glm)
summary(judge.glm.step)
exp(coef(judge.glm.step))

judgePredict <-predict(judge.glm,newdata=test,type = "response")
predict <- prediction(judgePredict,test$judge)
predict.auc <- performance(predict,'auc')
predict.auc@y.values


pre.period <- c(1,floor(0.7*nrow(predict.data)))
post.period <- c((floor(0.7*nrow(predict.data))+1),nrow(predict.data))
data <- predict.data[,-1]
impact <- CausalImpact(data, pre.period, post.period)
mental.causal <- plot(impact)

data.step <- cbind(predict.data$judge,predict.data$hsd010,predict.data$dlq140,predict.data$kiq480,predict.data$sld012,predict.data$slq050,predict.data$slq120,predict.data$dr1tnumf,predict.data$dr1ip182)
impact.step <- CausalImpact(data.step, pre.period, post.period)
mental.plot2 <- plot(impact.step)


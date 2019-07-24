dpq = na.omit(DPQ_I)
judge = vector(mode = "numeric",length = nrow(dpq))
dpq = cbind(dpq,judge)
for (i in 1:nrow(dpq))
{
  if ((dpq[i,2]>=2)|(dpq[i,3]>=2)|(dpq[i,4]>=2)|(dpq[i,5]>=2)|(dpq[i,6]>=2)|(dpq[i,7]>=2)|(dpq[i,8]>=2)|(dpq[i,9]>=2)|(dpq[i,10]>=2))
    dpq[i,12]<-1
  else dpq[i,12]<-0
}
dpq=dpq[,c(1,12)]

all <- centralImputation(all)
all.judge = left_join(dpq,all,by="seqn")
cov.mental = cov(all.judge,method = "spearman")
cor.mental = cor(all.judge,method = "spearman")
pcor.mental = cor2pcor(cov.mental)
colnames(pcor.mental) = colnames(cor.mental)
rownames(pcor.mental) = rownames(cor.mental)
pcor.judge = pcor.mental[,2]
pcor.judge = pcor.judge[c(-1,-2)]
pcor.judge.ad = pcor.judge[-which(abs(pcor.judge)>1)]
summary(pcor.judge.ad)
judge.model <- pcor.judge.ad[which(abs(pcor.judge.ad)>0.053)]

acq = ACQ_I[,c(1:4)]
language = vector(mode="numeric",length=nrow(acq))
acq=cbind(acq,language)
for (i in 1:nrow(acq))
{
  if(acq[i,2]!=1)
    acq[i,5]<-0
}
acq[which(is.na(acq$acd011a)),5]=1
acq=acq[,-2:-4]

logit.model = as.data.frame(cbind(all.judge$seqn,all.judge$judge,all.judge$alq120u,all.judge$alq151,all.judge$dr2tmois))
colnames(logit.model)=c("seqn","judge","alq120u","alq151","dr2tmois")
logit.model = left_join(logit.model,acq,by="seqn")
predict.data = with(logit.model,data.frame(language,alq120u,alq151,dr2tmois,judge))
ntrain = sample(nrow(predict.data),floor(0.75*nrow(predict.data)),replace=FALSE)
train = predict.data[ntrain,]
test = predict.data[-ntrain,]

judge.glm = glm(judge~language+alq120u+alq151+dr2tmois,family="binomial",data=train)
summary(judge.glm)
exp(coef(judge.glm))

judgePredict <-predict(judge.glm,newdata=test,type = "response")
Predict.data = cbind(test,judgePredict)

# SEGMENTATION AND CLASSIFICATION OF STOCKS IN BOMBAY STOCKS EXCHANGE (BSE) BASED ON FUNDAMENTAL RATIO
# PG - FINAL YEAR PROJECT

#============== importing data ===============#

data			<-		read.csv(file.choose(),header=T,sep=",")
head(data)
tail(data)
data1			<- 		data[,-1]
row.names(data1)  <- 		NULL
head(data1)
#==============================================#


#============== REQUIRED PACKAGE'S ==========#
install.packages("ggplot2")
install.packages("factoextra")
install.packages("tidyverse")
install.packages("dendextend")
install.packages("cluster")

library(ggplot2)
library(factoextra)
library(tidyverse)
library(dendextend)
library(cluster)
#==============================================#


#============== PRINCIPLE COMPONENT ANALYSIS ===========#
?princomp
pca_1			<-		princomp(data[,2:12],cor=TRUE)
out <- loadings(pca_1)
print(out, digits = 3, cutoff = 0, sort = FALSE)

out
pca_summary		<-		summary(pca_1)
vari			<-		c(2.0092172303,1.696272329,1.3008406159,1.0955633683,1.043314567,0.9979902108,0.8868101904,
					0.7941072588,0.5166551970,0.4371324662,0.1620964789)
pca_1$loadings(cutoff=0)
pca_score		<-		predict(pca_1,data)
head(pca_score)
data_A		<-		data.frame(pca_score)
head(data_A)
head(data_A$Comp.1)

input 		<- 		read.csv(file.choose(),header=T,sep=",")
head(input)

data_A$pc_scr		<-	(var[1]*data_A$Comp.1)+(var[2]*data_A$Comp.2)+(var[3]*data_A$Comp.3)+(var[4]*data_A$Comp.4)+
					(var[5]*data_A$Comp.5)+
					(var[6]*data_A$Comp.6)+(var[7]*data_A$Comp.7)+(var[8]*data_A$Comp.8)+
					(var[9]*data_A$Comp.9)+(var[10]*data_A$Comp.10)+(var[11]*data_A$Comp.11)

data$company 	<- 		input$STOCKS_NAME

data_A$pc_scr_s 	<- 		(data_A$pc_scr - mean(data_A$pc_scr)) / sd(data_A$pc_scr)
fix(data)
head(data_A)

data$pc_scr_s 	<- 		(data$pc_scr - mean(data$pc_scr)) / sd(data$pc_scr)
fix(data)
head(data_A)

outliers 		<- 		data[which(data$pc_scr_s > 2 | data$pc_scr_s < -2),]
fix(outliers)
head(outliers) 

#=======================================================#


#====================  CLUSTER ANALYSIS  ================#
#================= CALCULATING LINKAGE METHOD ===========#
#================ REMOVING OUTLIERS DATA (481)==========#

modified			<-		read.csv(file.choose(),header=T,sep=",")
head(modified)
tail(modified)

modified_1 			<- 		modified[,-1]
row.names(modified_1) 	<- 		NULL
head(modified_1)

wilks_lambda		<-		numeric(29)
for (i in 2:30)
{
cluster 			<- 		dist(modified[,2:12],method="euclidean")
clust				<-		hclust(cluster,method="ward.D")
sub_grp 			<- 		cutree(clust, k = i)
grp_data			<-		modified %>% mutate(cluster = sub_grp)
fit 				<- 		manova(as.matrix(modified_1) ~ grp_data$cluster)
out				<- 		summary(fit, test = "Wilks")
wilks_lambda[i]		<- 		out$stats[5]
}

#ploting DENDOGRAM
plot(wilks_lambda, type="o",main="single")

#PLOTING DENDOGRAM (dendextend)

cluster_plot 		<- 	dist(modified[,2:12],method="euclidean")
fit				<-	hclust(cluster,method="ward.D")
hcd_ward 			<-	as.dendrogram(fit)
dend 				<- 	hcd_ward
dend 				<- 	color_branches(dend, k =7, groupLabels = TRUE)
plot(dend, leaflab = "none",main="euclidean&centroid")

#===============================================================#

cluster 			<- 		dist(modified[,2:12],method="euclidean")
clust				<-		hclust(cluster,method="ward.D")
sub_grp 			<- 		cutree(clust, k = 7)
grp_data			<-		modified %>% mutate(cluster = sub_grp)
grp_data$unit 		<- 		rep(1,nrow(grp_data))
# fix(grp_data)
table(sub_grp)

#ORGINAL DATA MEAN VECTOR
segment <- grp_data$cluster
org_data			<-cbind(modified_A,segment)
org_data$unit <- rep(1,nrow(org_data))
head(org_data)
mean_vector 		<- 		aggregate(org_data[,2:12],by=list(org_data$segment),FUN=mean)
fix(org_data)
no_of_companies   	<- 		aggregate(org_data$unit,by=list(org_data$segment),FUN=sum)
out2 				<- 		cbind(mean_vector,no_of_companies)
fix(out2)
selected_1			<- 		org_data[which(org_data$segment==1),]
fix(selected_1)


#STD DATA MEAN VECTOR
mean_vector 		<- 		aggregate(grp_data[,2:12],by=list(grp_data$cluster),FUN=mean)
no_of_companies   	<- 		aggregate(grp_data$unit,by=list(org_data$unit),FUN=sum)
out2 				<- 		cbind(mean_vector,no_of_companies)
out2
# fix(out2)
write.csv(cluster_data1,"F:/PROJECT/FILES/clustering_data1.csv")

fix(selected_1)
cluster_data1		<-		selected_2
selected_1			<- 		org_data[which(org_data$segment==1),]
selected_2			<- 		grp_data[which(grp_data$cluster==2),]
selected_3			<- 		grp_data[which(grp_data$cluster==3),]
selected_4			<- 		grp_data[which(grp_data$cluster==4),]
selected_5			<- 		grp_data[which(grp_data$cluster==5),]
selected_6			<- 		grp_data[which(grp_data$cluster==6),]
selected_7			<- 		grp_data[which(grp_data$cluster==7),]

model				<-		data.frame(rbind(selected_1,selected_2,
						selected_3,selected_4,selected_5,
						selected_6,selected_7))

str(model)
model$cluster		<-		as.factor(model$cluster)
model$STOCKS_NAME		<-		as.numeric(model$STOCKS_NAME)
model$EPS			<-		as.numeric(model$EPS)
model$OPM			<-		as.numeric(model$OPM)
model$PAT			<-		as.numeric(model$PAT)
model$ROA			<-		as.numeric(model$ROA)
model$CURRENT_RATIO	<-		as.numeric(model$CURRENT_RATIO)
model$DIV_YIELD		<-		as.numeric(model$DIV_YIELD)
model$DEPT_TO_EQTY	<-		as.numeric(model$DEPT_TO_EQTY)
model$ROE			<-		as.numeric(model$ROE)
model$ROCE			<-		as.numeric(model$ROCE)
model$PLEDGED		<-		as.numeric(model$PLEDGED)
model$MARKET_CAPITAL	<-		as.numeric(model$MARKET_CAPITAL)
model$unit			<-		as.numeric(model$unit)
#===============================================================#


#===============================================================#
#======================== KNN METHOD ===========================#
install.packages("class")					#req packages
library(class)							#library


#======================== KNN METHOD ===========================#

ind_vars 			<- 		model_1[,1:11]
target 			<- 		model_1[,12]
head(ind_vars)

#LOOCV using (KNN)

pred 				<- 		numeric(481)
for(i in 1:nrow(ind_vars))
{
pred[i] 			<- 		knn(ind_vars[-i,],ind_vars[i,],cl = target[-i], k=1)
}

knn1_tab			<-		table(pred,target)

pre	<-	4/148
pre1	<-	4/191
table(pre,pre1)

knn.cv(ind_vars, target, k = 10, l = 0, prob = FALSE, use.all = TRUE)

new_companies_classified <- 		knn(ind_vars,original_score_data_2,cl = target, k=1)
table(new_companies_classified)
dim(ind_vars)
dim(original_score_data_1)

#==================================================#


#=============BALANCE_COMPANIES_SCORE================#

original_A			<-		read.csv(file.choose(),header=T,sep=",")
head(original_A)
original_A_11		<- 		original_A[,-1]
original_A_11_s		<- 		scale(original_A_11)
head(original_A_11_s)

pred_new_companies	<- 		knn(ind_vars,original_A_11_s,  model_1[,12], k = 1)
pred_new_companies2 	<- 		cbind(original_A,pred_new_companies)
segment1 			<- 		pred_new_companies2[which(pred_new_companies2$pred_new_companies==1),]
fix(segment1)
table(pred_new_companies)
length(target)
head(ind_vars)


new_comp_std		<-	data.frame(cbind(original_score_data$Name,original_score_data_strd))
head(new_comp_std)

write.csv(new_comp_std,"F:/PROJECT/FILES/origina_std.csv")


pred_new <- cbind(original_score_data_A[,-1],new_companies_classified)
segment1 <- pred_new[which(pred_new$new_companies_classified==1),]
fix(segment1)
pred_score_original	<-	predict(original_score_data_1,knn1_tab)

write.csv(original_score_data_2,"F:/PROJECT/FILES/origina_std.csv")

#=======================================================#





#==================PSEDO-F-STATISTICS===================#

library(NbClust)
opt_clust = NbClust(modified[,2:12],distance="euclidean", min.nc = 2,
        max.nc = 20, method = "ward.D", index = "ch", alphaBeale = 0.1)
opt_clust

plot(opt_clust$All.index, type="o")


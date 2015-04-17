#need to handle missing data better
	#Get a sum of the property score with missing values and the highest possible score (given that certain variables are excluded)
	#make the score a ratio of actual to highest possible
	#make threshhold be at a given percent of total score rather than the integer it was previously
#need to make it more dynamic (is there a better way than just updating the correlations?)
#Need to have an automated way to update the data so it works with this code
#The single most heavily weighted thing is the question about character, which is more subjective than other things and Pura's judgment may be very different than the reviewers on that one
#everything is linear, a downside of not using regression analysis.

abatement.data = read.csv(file="data.csv", na.strings = "N/A")
structures = abatement.data[which(abatement.data$Vacant.Lot.Code==0|is.na(abatement.data$Vacant.Lot.Code)),]
attach(structures)
str.mat = as.matrix(cbind(MVA.Code,Market.Code,Review,Character.Code,Roof.Code,Exterior.Code,Foundation.Code,Overall.Condition.Code,Recommendation.Code))
str.mat.complete = str.mat[complete.cases(str.mat),]
recommendation = str.mat.complete[,ncol(str.mat.complete)]
recommendation = replace(recommendation,recommendation==1,"Demolish")
recommendation = replace(recommendation,recommendation==0,"Sell")
ind.vars = str.mat.complete[,1:(ncol(str.mat.complete)-1)]

#scaling data so everything is weighted the same
var.max = apply(ind.vars,2,function(x)max(abs(x)))
var.weights = max(abs(var.max))/var.max
ind.vars.weighted = matrix(0,nrow = nrow(ind.vars),ncol = length(var.weights))
for(i in 1:ncol(ind.vars)){
	ind.vars.weighted[,i] = ind.vars[,i]*var.weights[i]}
colnames(ind.vars.weighted) = colnames(ind.vars)
scores = rowSums(ind.vars.weighted)
threshhold = quantile(scores,.65)#should play around with adjusting threshhold, using .65 because that's about how many go to sale
projection = c()
for(i in 1:length(scores)){
	if(scores[i]>threshhold){projection[i] = "Demolish"}
	else{projection[i] = "Sell"}
}
validity = sum(projection==recommendation)/length(projection==recommendation)

#make weights based on correlations
cor.vars = cor(str.mat.complete)
recommend.cor = cor.vars[nrow(cor.vars),1:(ncol(cor.vars)-1)]
max.weight = max(abs(recommend.cor))
rel.cors = abs(recommend.cor/max.weight)
cor.weights = var.weights*rel.cors

ind.vars.weighted.cor = matrix(0,nrow = nrow(ind.vars),ncol = length(cor.weights))
for(i in 1:ncol(ind.vars)){
	ind.vars.weighted.cor[,i] = ind.vars[,i]*cor.weights[i]}
colnames(ind.vars.weighted.cor) = colnames(ind.vars)

cor.scores = rowSums(ind.vars.weighted.cor)
threshhold.cor = quantile(cor.scores,.65)
projection.cor = c()
for(i in 1:length(cor.scores)){
	if(cor.scores[i]>threshhold.cor){projection.cor[i] = "Demolish"}
	else{projection.cor[i] = "Sell"}
}
cor.validity = sum(projection.cor==recommendation)/length(projection.cor==recommendation)

wrong = which(projection.cor!=recommendation)
projection.cor[wrong]
str.mat.complete[wrong,]


#correlation weights with missing values
#Reading in the data
structures = abatement.data[which(abatement.data$Vacant.Lot.Code==0|is.na(abatement.data$Vacant.Lot.Code)),]
attach(structures)
str.df = data.frame(cbind(MVA.Code,Market.Code,Review,Character.Code,Roof.Code,Exterior.Code,Foundation.Code,Overall.Condition.Code,Recommendation.Code))
rownames(str.df) = Case.Number
recommendation = str.df[,ncol(str.df)]
recommendation = replace(recommendation,recommendation == 1,"Demolish")
recommendation = replace(recommendation,recommendation == 0,"Sell")
ind.vars = str.df[,1:(ncol(str.df)-1)]

#scaling everything and making correlation weights
var.max = apply(ind.vars,2,function(x)max(abs(x),na.rm = TRUE))
var.weights = max(abs(var.max))/var.max
cor.vars = cor(str.df,use="pairwise.complete.obs") #this uses all cases between the variables...may be better to only use the fully complete cases so everything gets the same number of inputs
recommend.cor = cor.vars[nrow(cor.vars),1:(ncol(cor.vars)-1)]
max.weight = max(abs(recommend.cor))
rel.cors = abs(recommend.cor/max.weight)
cor.weights = var.weights*rel.cors

#Applying weights to variables and getting scores and highest possible scores
ind.vars.weighted = data.frame(mapply(`*`,ind.vars,cor.weights))
rownames(ind.vars.weighted) = rownames(str.df)
weighted.scores = rowSums(ind.vars.weighted,na.rm=TRUE)
max.by.var = apply(ind.vars.weighted, MARGIN = 2, max, na.rm = TRUE)
na.index = apply(ind.vars.weighted,MARGIN = c(1,2),function(x) if(is.na(x)){x = 0} else{x = 1})
case.max = rowSums(data.frame(sweep(na.index,MARGIN = 2,max.by.var,`*`)))
raw.scores = rowSums(ind.vars.weighted,na.rm = TRUE)
final.scores = raw.scores/case.max

#Making determination based on final.scores
projection = c()
threshhold = .65
for(i in 1:length(final.scores)){
	if(final.scores[i]>threshhold){projection[i] = "Demolish"}
	else{projection[i] = "Sell"}
}
validity.rating = sum(projection==recommendation)/length(projection==recommendation)
wrong = which(projection!=recommendation)
projection[wrong]

setwd("/Users/deepthought42/Dropbox/Studium/CACI/CACI_projects/final/new")

library(ggplot2)
library(reshape2)
library(stringr)
library(rpart)
library(rpart.plot)
library(randomForest)

mxl_betai = read.csv("ind_pref_price_neg.csv")


ind_pref = data.frame(mxl_betai)
cols = names(ind_pref)


add_ommited_pw = function(ind_pref, att_name, cols){
    #adds part worths(pw) for the ommited attribute levels to the preference dataframe
  attribute_cols = cols[grep(att_name, cols)]
  new_col_name =  paste0(att_name, as.character(max(grep("[0-9]", attribute_cols)) + 1))
  ind_pref[, new_col_name] =  0 - rowSums(ind_pref[,attribute_cols])
  return(ind_pref)
}  

for(att in c("battery", "weight", "sound")){
  ind_pref = add_ommited_pw(ind_pref, att, cols = cols)
}



#add importance values

#scale price coef to represent utilitz change per 10 USD price change
price.imp = 80/10 * abs(ind_pref$price)
importance = data.frame(price.imp)

cols = names(ind_pref)
for(att in c("battery", "weight", "sound")){
  attribute_cols = cols[grep(att, cols)]
  range_name = paste0(att, '.imp')
  importance[,range_name] = apply(ind_pref[,attribute_cols],1,function(x) abs(max(x) - min(x)))
}

rs = rowSums(importance)
rel.imp = apply(importance,2, function(x) x/rs)
rel.imp = data.frame(rel.imp)

ind_pref = cbind(ind_pref, rel.imp)

#Calculate Willingness to pay 

for(att in c("battery", "weight", "sound")){
  attribute_cols = cols[grep(att, cols)]
  
  for(col in attribute_cols){
    #get WTP per column by deviding through ommited column and multiplying with price coef
    col_WTP = (ind_pref[,col] - ind_pref[,attribute_cols[length(attribute_cols)]]) * abs(ind_pref$price) * 10
    
    wtp_name = paste0(col, ".WTP")
    ind_pref[,wtp_name] = col_WTP
  }
  
  
  range_name = paste0(att, '.imp')
  importance[,range_name] = apply(ind_pref[,attribute_cols],1,function(x) abs(max(x) - min(x)))
}

WTP = ind_pref[,names(ind_pref)[grep("WTP", names(ind_pref))]]
#WTP = cbind(clust = ind_pref$clust, WTP)

PW = ind_pref[,3:15]
PW = PW[, order(names(PW))]
PW = cbind(ind_pref[,1:2], PW)


##################### Clustering
clust_df = scale(cbind(PW, rel.imp))
d= dist(clust_df)

hc = hclust(d, method = "ward.D")
plot(hc)

plot(hc$height[288:280]~seq(1:9), main = "Elbow plot of squared cluster distances ", ylab = "Squared distance", xlab = "cluster #")
lines(hc$height[288:280]~seq(1:9))

clust = cutree(hc, 4)
table(clust)

####### plot clusters
####### plot Importances
names(rel.imp) = c("Price","Battery","Weight", "Sound")

ri = split(rel.imp, clust)
cl.imps = sapply(ri, colMeans)
imp.pl = melt(cl.imps)
names(imp.pl) = c("Attribute", "Segment", "Relative Importance")
library(plyr)
imp.pl$Segment = mapvalues(imp.pl$Segment,c(1,2,3,4), c("Prem Sound",
                                         "Basic Sound",
                                         "Prem All-Round",
                                         "Basic Flexible"))


ggplot(imp.pl, aes(x = Segment, y = `Relative Importance`, fill = Attribute))+
  geom_bar(stat="sum") + scale_fill_grey() +scale_size(guide=FALSE)


###### MDS IMP
mds = cmdscale(dist(rel.imp))
profit = lm(cbind(Price,Battery,Weight,Sound) ~  mds[,1] + mds[,2] -1, data = rel.imp)

a = coef(profit)[1, ]
b = coef(profit)[2, ]

plot(mds, col = clust)

abline(h = 0)
abline(v = 0)

arrows(x0 = c(0, 0, 0),
       y0 = c(0, 0, 0),
       x1 = 0.3*a,
       y1 = 0.3 *b)

text(0.31 * a, 0.31 * b, names(rel.imp))

mds_df = data.frame(mds, Segment = clust)

mds_df$Segment = mapvalues(mds_df$Segment,c(1,2,3,4), c("Premium Sound",
                                                        "Basic Sound",
                                                        "Premium All-Round",
                                                        "Basic Flexible"))
arrow_df = data.frame(xstart = rep(0,4), ystart = rep(0,4), xend = 0.3*a, yend = 0.3 *b)

ggplot(mds_df, aes(X1, X2)) + geom_point(aes(shape=Segment)) +
  geom_segment(data = arrow_df, 
               aes(x = xstart, xend = xend, y = ystart,yend = yend),
                      arrow=arrow(length=unit(2, "mm")), color='darkgrey') +
  geom_text(data=arrow_df, aes(label=names(rel.imp),
                                      x=xend ,
                                      y= yend +c(-0.02,0.02,0.02,-0.02), size = 4))+scale_size(guide=FALSE)

ggplot(mds_df, aes(X1, X2)) + geom_point(aes(col=Segment)) +
    geom_segment(data = arrow_df, 
                 aes(x = xstart, xend = xend, y = ystart,yend = yend),
                 arrow=arrow(length=unit(2, "mm")), color='darkgrey') +
    geom_text(data=arrow_df, aes(label=names(rel.imp),
                                 x=xend ,
                                 y= yend +c(-0.02,0.02,0.02,-0.02), size = 4))+scale_size(guide=FALSE)


###### plot WTP
WTP = WTP + 130

lWTP = split(WTP, clust)
cl.wtp = sapply(lWTP, colMeans)
wtp.pl = melt(cl.wtp)


X <-regmatches(wtp.pl$Var1, gregexpr("[[:digit:]]+", wtp.pl$Var1))
X = as.numeric(unlist(X))
X


wtp.pl = cbind(X, wtp.pl)
names(wtp.pl)[2:3] = c("level", "cluster")
wtp.pl$level = as.character(wtp.pl$level)
wtp.pl$level =  str_replace_all(wtp.pl$level, "[[:digit:]].*", "")
wtp.pl$level = paste0(toupper(substr(wtp.pl$level, 1,1)), substr(wtp.pl$level, 2,nchar(wtp.pl$level)))
wtp.pl$cluster = mapvalues(wtp.pl$cluster,c(1,2,3,4), c("Premium Sound",
                                                        "Basic Sound",
                                                        "Premium All-Round",
                                                        "Basic Flexible"))
names(wtp.pl) = c("Attribute level", "level", "Segment", "Willingness to Pay")
ggplot(wtp.pl, aes(x = `Attribute level`, y = `Willingness to Pay`)) +
  geom_line(aes(linetype = Segment)) +
  facet_grid(.~level)




############################################

list.files()
choice = read.csv("effect_ml_pri_neg.csv")
lef = choice[,1:4]
choice = choice[,5:16]
choice[choice == -1] = 0 
dummy1 = cbind(lef,choice)

dummy = mlogit.data(dummy1, choice = "choice", shape = "long",
            id.var = "id", alt.var = "alt")

mxl_bluetooth <- gmnl(choice ~ 0 + none+price+battery1+battery2+battery3+battery4+
                        weight1+weight2+weight3+sound1+sound2+sound3, 
                      data = dummy, model = "mixl", correlation = FALSE, 
                      haltons = NULL, R = 2000, panel = TRUE, tol = 1e-12, print.level = 1,
                      ranp = c(none = "n", price = "n", battery1 = "n", battery2 = "n",battery3 = "n",battery4 = "n", 
                               weight1 = "n", weight2 = "n",weight3 = "n", sound1 = "n", sound2 = "n", sound3 = "n"))

dum.coe = mxl_bluetooth$coefficients[1:12]
############################################ market share

params.dum = read.csv("dummy_parameter.csv", row.names = 1)

#clust 1
prodA = c(0,15, #highest price
          0,0,1,0, #medium battery
          0,0,0, #heaviest
          0,0,0) #best sound

#clust 2
prodB = c(0,10, #low price
          1,0,0,0, #shortest battery
          0,0,0, #heaviest          
          0,0,1) #2 best sound

#clust 3
prodC = c(0,13, #medium high price
          0,1,0,0, # 2nd shortest battery
          1,0,0, #lightest
          0,1,0) #3nd best sound

#clust 4
prodD = c(0,8, #low price
          0,0,1,0, # 2nd best battery
          0,0,0, #heaviest          
          1,0,0) #worst sound

none = c(1,rep(0,11))

products = as.matrix(rbind(prodA, prodB, prodC,prodD,none))
colnames(products) = names(params.dum)
products

util = products %*% t(params.dum)
util.exp = exp(util)
share = matrix(data = NA, nrow = nrow(products), ncol = 5)

for(cl in 1:ncol(share)){
  for(p in 1:nrow(share)){
    util.p = util.exp[p,cl]
    util.tot = sum(util.exp[,cl])
    #util.alt = sum(util.exp[1:nrow(share)!= p,cl])
    share[p,cl] = util.p/util.tot
  }
}
round(share,4)

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

share.displ = matrix(percent(share), ncol = ncol(share), nrow = nrow(share))
prod2 = data.frame(Price = c(150,100,130,80,0),
                   Battery = c( "12 h","8 h","10 h","12 h ", "-"),
                   Sound = c(5,4.5,4,3.5,0),
                   Weight = c("700 g", "700 g", "400 g", "700 g","-"))



library(xtable)

xtable(cbind(prod2, share.displ))

########### individual data analysis
indiv = read.csv("indivData_sub_price_neg.csv")

#replace Residence that occures less than 5 times with "other" category
table(indiv$Residence)[order(table(indiv$Residence))]<5
rep.idx = names(table(indiv$Residence))[table(indiv$Residence)<5]
indiv$Residence = as.character(indiv$Residence)
indiv$Residence[indiv$Residence %in% rep.idx] = "other"
indiv$Residence = as.factor(indiv$Residence)

#names(clust) = indiv$id

#indiv$clust = clust

indiv$Income = ifelse(indiv$Income == 8,-1,
                  ifelse(indiv$Income==1, 500,
                  ifelse(indiv$Income==2,1000,
                  ifelse(indiv$Income==3,1500,
                  ifelse(indiv$Income==4,2000,
                  ifelse(indiv$Income==5,2500,
                  ifelse(indiv$Income==6,3000,3500)))))))

indiv$BrandAwareness_None = as.factor(indiv$BrandAwareness_None) 
indiv$avgBrandAvereness = rowSums(indiv[, 4:11])/8
indiv$avgSubjKnow = rowMeans(indiv[,13:17])
indiv$avgPII = rowMeans(indiv[,18:22])
#imp.diff = indiv[,23:26]/100 - rel.imp

indiv$CorImp_battery =  cor(indiv$RelImp_battery, rel.imp$Battery)
indiv$CorImp_weight = cor(indiv$RelImp_weight, rel.imp$Weight)
indiv$CorImp_sound = cor(indiv$RelImp_sound, rel.imp$Sound)
indiv$CorImp_price = cor(indiv$RelImp_price, rel.imp$Price)

indiv$clust = factor(clust, labels = c("Premium Sound", "Basic Sound", "Premium Alround", "Basic Flexible"))


names(indiv)
#relevant columns in indiv dataframe
relv.col = c(2:26,28,30,31,32,33,35,36,38:45)
dtest = indiv[,relv.col]


#Train model plot trees
DT = rpart(clust ~ . , data = dtest)
rpart.plot(DT)
DT$cptable
trimDT = prune(DT, 0.021) #Allow for 4 splits
rpart.plot(trimDT)

trimDT = prune(DT, 0.019)
rpart.plot(trimDT)

trimDT = prune(DT, 0.011)
rpart.plot(trimDT)


#remove importance measurements
dtest2 = cbind(dtest[,1:21],dtest[,26:40])
DT = rpart(clust ~ . , data = dtest2)
rpart.plot(DT)
prp(DT)


DT$cptable
trimDT = prune(DT, 0.020) #Allow for 12 splits
rpart.plot(trimDT)


plot(trimDT, uniform=TRUE,margin=0.2)
text(trimDT, use.n=TRUE, all=TRUE, cex=.8)


trimDT = prune(DT, 0.023) #Allow for 7 splits
rpart.plot(trimDT, box.palette = "Grays")

#Summary stats 
indiv2 = read.csv("indivData_sub_price_neg.csv")
names(indiv2)
summary(indiv2)[,c(28,30,31,33,35,37)]
xt = xtable(summary(indiv2)[,c(28,30,31,33,35,37)])
xt

#Biases Agelabel 30, Residence31, OccupationLabel33, EducationLabel35
names(indiv)

Own = tapply(indiv$Own, indiv$clust, mean)
PlanToBuy = tapply(indiv$IntentToBuy, indiv$clust, mean)
BA = indiv[,4:11]

ModBrand = sapply(split(indiv[,4:11], indiv$clust), function(x) names(x)[which.max(colSums(x))])
AvgBrandAware = tapply(indiv$avgBrandAvereness, indiv$clust, mean)
AvgSubjKnow = tapply(indiv$avgSubjKnow, indiv$clust, mean)
AvgPII = tapply(indiv$avgPII, indiv$clust, mean)
Isfemale = tapply(indiv$GenderLabel, indiv$clust, function(x) mean(as.character(x)=='female' ))
AgeGr = tapply(indiv$AgeLabel, indiv$clust, function(x) names(which.max(table(x))))
Education = tapply(indiv$EducationLabel, indiv$clust, function(x) names(which.max(table(x))))
tapply(indiv$OccupationLabel, indiv$clust, function(x) names(which.max(table(x))))

Income2 = ifelse(indiv$Income == -1,-1,
                      ifelse(indiv$Income== 500,250,
                             ifelse(indiv$Income==1000,750,
                                    ifelse(indiv$Income==1500,1250,
                                           ifelse(indiv$Income==2000,1750,
                                                  ifelse(indiv$Income==2500,2250,
                                                         ifelse(indiv$Income==3000,2750, 4000)))))))
AvgIncome = tapply(Income2, indiv$clust, function(x) mean(x[x>0]))

indOV = data.frame(Own, PlanToBuy,ModBrand, AvgBrandAware,AvgSubjKnow, AvgPII, Isfemale,AgeGr, Education, AvgIncome)
indOV[, sapply(indOV, is.numeric)] = round(indOV[, sapply(indOV, is.numeric)],2)
indOV
xtable(indOV)


################ market share old
params = as.data.frame(sapply(split(PW, clust), colMeans))
params = cbind(full = colMeans(PW), params)

ommit.idx = c(7,11,15)
params_eff = params[-ommit.idx,]

row.names(params_eff)
prodA = c(0,1.3, #highest price
          0,0,1,0, #medium battery
          -1,-1,-1, #best sound
          0,0,1) #3nd lightest

prodB = c(0,0.8, #low price
          1,0,0,0, #shortest battery
          -1,-1,-1, #2 best sound
          -1,-1,-1) #heaviest

prodC = c(0,1.2, #2nd highest price
          0,1,0,0, # 2nd shortest battery
          0,1,0, #3nd best sound
          1,0,0) #lightest

prodD = c(0,0.9, #2nd highest price
          0,0,0,1, # 2nd best battery
          0,0,1, #2nd best sound
          -1,-1,-1) #heaviest
none = c(1,rep(0,11))
  
products = as.matrix(rbind(prodA, prodB, prodC,prodD,none))
products

para.ma = as.matrix(params_eff)
para.ma

util = products %*% para.ma
util.exp = exp(util)

share = matrix(data = NA, nrow = nrow(products), ncol = ncol(para.ma))


####
raw = read.csv("effect_ml_pri_neg.csv")
raw.c = raw[raw$choice==T,]
mean(raw.c$none)


##########

cl = 1
p = 1
for(cl in 1:ncol(share)){


  for(p in 1:nrow(share)){
    util.p = util.exp[p,cl]
    util.tot = sum(util.exp[,cl])
    #util.alt = sum(util.exp[1:nrow(share)!= p,cl])
    share[p,cl] = util.p/util.tot
  }
}

row.names(share) = row.names(util)
colnames(share) = colnames(para.ma)
round(share,4)

i = 1
for(i in 1:ncol(para.ma)){
  utilit
}

utility = as.matrix(params_eff) %*% products

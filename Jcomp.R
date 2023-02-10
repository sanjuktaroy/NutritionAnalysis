library(car)
calories<-read.csv("/Users/jaisurajbantupalli/Downloads/Calories.csv")

str(calories)

names(calories)


names(calories)=c(
  "Fast Food Restaurant",
  "Type",
  "ServingSize",
  "Calories",
  "TotalFat",
  "SaturatedFat",
  "TransFat",
  "Sodium",
  "Carbs",
  "Sugars",
  "Protein"
)

str(calories)

calories1<-calories[,3:11]

str(calories1)

any(is.na(calories1))==TRUE

which(is.na(calories1))
is.na(calories1)

calories1$TransFat[which(is.na(calories1$TransFat))]<-mean(calories1$TransFat,na.rm = TRUE)

any(is.na(calories1$TransFat))==TRUE

any(is.na(calories1))==TRUE

nrow(calories1)

head(calories1, 5)

summary(calories1)

calories1$Sodiumg=calories1$Sodium/1000

head(calories1)


pairs(calories1)


model1<-lm(Calories~ServingSize+TotalFat+SaturatedFat+TransFat+Sodiumg+Carbs+Sugars+Protein, data=calories1)


summary(model1)

vif(model1) ##Variance inflation Factor to check colinearity 


model2<-lm(Calories~TotalFat+SaturatedFat+TransFat+Sodiumg+Carbs+Sugars+Protein, data=calories1)

summary(model2)

vif(model2)


model3<-lm(Calories~TotalFat+TransFat+Carbs+Protein, data=calories1)

summary(model3)

vif(model3)



## Grouping by restuarant and type ####
library(dplyr)
df2<-calories%>%group_by(Type,`Fast Food Restaurant`)%>%summarise(Servingavg=mean(ServingSize),Caloriesavg=mean(Calories),TotalFatavg=mean(TotalFat),SaturatedFatavg=mean(SaturatedFat),TransFatavg=mean(TransFat),Sodiumavg=mean(Sodium),Carbsavg=mean(Carbs),Sugarsavg=mean(Sugars),Proteinavg=mean(Protein))

any(is.na(df2))==TRUE

which(is.na(df2))
is.na(df2)

df2$TransFatavg[which(is.na(df2$TransFatavg))]<-mean(df2$TransFatavg,na.rm = TRUE)

any(is.na(df2$TransFatavg))==TRUE

any(is.na(df2))==TRUE


##Plotting the box plots###
boxplot(calories$ServingSize,calories$Calories,calories$TotalFat,calories$SaturatedFat,calories$TransFat,calories$Sodium,calories$Carbs,calories$Sugars,calories$Protein,main = "Box plots")
boxplot(calories$ServingSize,main="Serving Size")
boxplot(calories$Calories,main = "Calories")
boxplot(calories$TotalFat,main = "Total Fat")
boxplot(calories$SaturatedFat,main = "Saturated Fat")
boxplot(calories$TransFat,main = "Trans Fat")
boxplot(calories$Sodium,main = "Sodium")
boxplot(calories$Carbs,main = "Carbs")
boxplot(calories$Sugars,main = "Sugar")
boxplot(calories$Protein,main = "Protein")



#### Ratio of Fat to Serving Size ratio per fast food restaurent and food item
df3<-df2
df3$Ratio_TotalFat_ServingSize=df3$TotalFatavg/df3$Servingavg

FatsperServing <- subset(df3, select = c(`Fast Food Restaurant`,Type ,TotalFatavg,Servingavg,Ratio_TotalFat_ServingSize ))


#####Sugar Outliers Removal#####
quartiles <- quantile(calories$Sugars, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(calories$Sugars)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(calories, calories$Sugars > Lower & calories$Sugars < Upper)

dim(data_no_outlier)

boxplot(data_no_outlier$Sugars,main = "Sugar")

calories<-data_no_outlier


#####Carbs Outliers Removal#####
quartiles <- quantile(calories$Carbs, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(calories$Carbs)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(calories, calories$Carbs > Lower & calories$Carbs < Upper)

dim(data_no_outlier)

boxplot(data_no_outlier$Carbs,main = "Carbs")

calories<-data_no_outlier



#####ServingSize Outliers Removal#####
quartiles <- quantile(calories$ServingSize, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(calories$ServingSize)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(calories, calories$ServingSize > Lower & calories$ServingSize < Upper)

dim(data_no_outlier)

boxplot(data_no_outlier$ServingSize,main = "Serving Size")

calories<-data_no_outlier


#####Calories Outliers Removal#####
quartiles <- quantile(calories$Calories, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(calories$Calories)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(calories, calories$Calories > Lower & calories$Calories < Upper)

dim(data_no_outlier)

boxplot(data_no_outlier$Calories,main = "Calories")

calories<-data_no_outlier



#####TotalFat Outliers Removal#####
quartiles <- quantile(calories$TotalFat, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(calories$TotalFat)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(calories, calories$TotalFat > Lower & calories$TotalFat < Upper)

dim(data_no_outlier)

boxplot(data_no_outlier$TotalFat,main = "TotalFat")

calories<-data_no_outlier



#####SaturatedFat Outliers Removal#####
quartiles <- quantile(calories$SaturatedFat, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(calories$SaturatedFat)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(calories, calories$SaturatedFat > Lower & calories$SaturatedFat < Upper)

dim(data_no_outlier)

boxplot(data_no_outlier$SaturatedFat,main = "SaturatedFat")

calories<-data_no_outlier


# Syntax
calories=na.omit(calories)

#####Sodium Outliers Removal#####
quartiles <- quantile(calories$Sodium, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(calories$Sodium)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(calories, calories$Sodium > Lower & calories$Sodium < Upper)

dim(data_no_outlier)

boxplot(data_no_outlier$Sodium,main = "Sodium")

calories<-data_no_outlier



#####TransFat Outliers Removal#####
quartiles <- quantile(calories$TransFat, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(calories$TransFat)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(calories, calories$TransFat > Lower & calories$TransFat < Upper)

dim(data_no_outlier)

boxplot(data_no_outlier$TransFat,main = "TransFat")


calories<-data_no_outlier


#####Protein Outliers Removal#####
quartiles <- quantile(calories$Protein, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(calories1$Protein)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(calories, calories$Protein > Lower & calories$Protein < Upper)

dim(data_no_outlier)

boxplot(data_no_outlier$Protein,main = "Protein")

calories<-data_no_outlier


##To show Box plots for Protein of each food item

boxplot(calories$Protein ~ calories$`Fast Food Restaurant`)

##To show Box plots for TransFat of each food item
boxplot(calories$TransFat ~ calories$`Fast Food Restaurant`)

colors = c("green","orange","yellow","red","black","brown","pink","blue","violet","cyan","azure")
barplot(FatsperServing$Ratio_TotalFat_ServingSize,main = "Fat to Serving Size Ratio", names.arg = FatsperServing$Type, xlab = "Ratio", ylab = "Restuarent", col = colors)

legend("topright", 
       legend = c("Burger King", "Carl's Jr.", "Chick-fil-A", "Dairy Queen", "Hardee's","Jack in the Box","McDonald's","Sonic","Wendy's","Whataburger","White Castle"), 
       fill = c("green","orange","yellow","red","black","brown","pink","blue","violet","cyan"))



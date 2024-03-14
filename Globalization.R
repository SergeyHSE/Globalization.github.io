library(foreign)
library(car)
library(gplots)
library(apsrtable)
library(plm)
library(tseries)
library(lmtest)
library(readxl)
library(texreg)
library(dplyr)
library(caret)
library(tidyverse)

#######################################
#              !!!AFRICA!!!           #
#######################################

DataGlobal <- read_excel("Panel_Africa.xlsx")

column_names <- names(DataGlobal)
print(column_names) 

# Remove column with missed variables
df <- subset(DataGlobal, select = - GlobalPeaceIndex)

# Replace spaces with underscores
df <- df %>%
  rename_all(~gsub(" ", "_", .))

#preprocess_options <- preProcess(df, method = c("center", "scale"))
#df <- predict(preprocess_options, df)

#y's dynamics by year across countries on different grafs
coplot(Hungry ~ Year|Country_Name, type="l", data=df)
coplot(Hungry ~ Year|Country_Name, type="b", data=df)

#y's dynamics by year on alone graf
scatterplot(Hungry ~ Year|Country_Name, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=df)

#heterogeneity across countries ("plotmeans" draw a 95% confidence interval around the means)
plotmeans(Hungry ~ Country_Name, main="Heterogeineity across branches", data=df)

#heterogeneity across years
plotmeans(Hungry~Year, main="Heterogeineity across months", data=df)

#FE
fixed <- plm(Hungry ~ Export+Gini_coef+Import+TertierySchool+School+SecondarySchool, data=df, index=c("Country_Name", "Year"), model="within")
summary(fixed)
write.csv(tidy(fixed) , "fixed.csv" )
# Display the fixed effects (constants for each country)
fixef(fixed)

#RE ("swar")
random <- plm(Hungry ~ Export+Gini_coef+Import+TertierySchool+School+SecondarySchool,
              data=df, index=c("Country_Name", "Year"), model = "random", random.method = "walhus")
summary(random)
write.csv(tidy(random), "random.csv")


#Choice between RE and FE (Hausman test)
phtest(fixed, random)


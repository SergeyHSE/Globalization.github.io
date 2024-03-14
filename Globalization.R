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

#Time-FE
fixed.time <- plm(Hungry ~ Export+Gini_coef+Import+TertierySchool+School+SecondarySchool + factor(Year), data=df, index=c("Country_Name", "Year"), model="within")
summary(fixed.time)
write.csv(tidy(fixed.time), "fixed_time.csv")

#Choice time-FE and FE
pFtest(fixed.time, fixed)
plmtest(fixed, c("time"), type=("bp"))

#Pool
pool <- plm(Hungry ~ Export+Gini_coef+Import+TertierySchool+School+SecondarySchool,
            data=df, index=c("Country_Name", "Year"), model="pooling")
summary(pool)
pFtest(fixed, pool)
# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better)
plmtest(pool, type=c("bp"))


#Testing for cross-sectional dependence/contemporaneous correlation: using Breusch-Pagan LM test of independence and Pasaran CD test
pcdtest(random, test=c("lm"))
pcdtest(random, test=c("cd"))

pcdtest(fixed, test=c("lm"))
pcdtest(fixed, test=c("cd"))


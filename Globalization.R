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

#Testing for serial correlation
pbgtest(fixed)
pbgtest(fixed.time)
pbgtest(random)

#Heteroskedastisity
bptest(Hungry ~ Export+Gini_coef+Import+TertierySchool+School+SecondarySchool+factor(Country_Name), data=df, studentize=F)

# Define the model formula
model_formula <- Hungry ~ Export + Gini_coef + Import + 
  TertierySchool + School + SecondarySchool + lag(Hungry, 1) + lag(Export, 1) + lag(Import, 1) + lag(Gini_coef, 1) + lag(School, 1) + lag(SecondarySchool, 1) + lag(TertierySchool, 1)

# Specify the panel data model with Newey-West standard errors
ab.model <- plm(
  formula = model_formula,
  data = df,
  model = "random",            
  random.method = "walhus",
  effect = "twoways",       
  vcov = "kernel",             # Newey-West standard errors
  kernel = "bartlett",         # Bartlett kernel for Newey-West
  index = c("Country_Name", "Year"),  # Panel index
)

summary(ab.model)
pbgtest(ab.model)

result_summary <- summary(ab.model)
coefficients_table <- as.data.frame(coef(result_summary))
r_squared <- result_summary$rsquared
coefficients_table

###################################################################
#                      ASIA                                       #
###################################################################

DataGlobal <- read_excel("Panel_Asia.xlsx")

# Remove column with missed variables
df <- subset(DataGlobal, select = - GlobalPeaceIndex)

# Replace spaces with underscores

df <- df %>%
  rename_all(~gsub(" ", "_", .))

preprocess_options <- preProcess(df, method = c("center", "scale"))
df <- predict(preprocess_options, df)

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
pcdtest(fixed, test=c("lm"))
pcdtest(fixed, test=c("cd"))

#Testing for serial correlation
pbgtest(fixed)

#Heteroskedastisity
bptest(Hungry ~ Export+Gini_coef+Import+TertierySchool+School+SecondarySchool+factor(Country_Name), data=df, studentize=F)

# Define the model formula
model_formula <- Hungry ~ Export + Gini_coef + Import + 
  TertierySchool + School + SecondarySchool + lag(Hungry, 1) + lag(Export, 1) + lag(Import, 1) + lag(Gini_coef, 1) + lag(School, 1) + lag(SecondarySchool, 1) + lag(TertierySchool, 1)

# Specify the panel data model with Newey-West standard errors
ab.model <- plm(
  formula = model_formula,
  data = df,
  model = "within",   
  effect = "time",       
  vcov = "kernel",             # Newey-West standard errors
  kernel = "bartlett",         # Bartlett kernel for Newey-West
  index = c("Country_Name", "Year"),  # Panel index
)

summary(ab.model)
pbgtest(ab.model)

result_summary <- summary(ab.model)
coefficients_table <- as.data.frame(coef(result_summary))
r_squared <- result_summary$rsquared
coefficients_table

###############################################################################
#                     !!!!!AMERICA!!!!!!!!!                                   #
###############################################################################


DataGlobal <- read_excel("Panel_America.xlsx")

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

#Testing for serial correlation
pbgtest(random)

#Heteroskedastisity
bptest(Hungry ~ Export+Gini_coef+Import+TertierySchool+School+SecondarySchool+factor(Country_Name), data=df, studentize=F)

# Define the model formula
model_formula <- Hungry ~ Export + Gini_coef + Import + 
  TertierySchool + School + SecondarySchool + lag(Hungry, 1) + lag(Export, 1) + lag(Import, 1) + lag(Gini_coef, 1) + lag(School, 1) + lag(SecondarySchool, 1) + lag(TertierySchool, 1)

# Specify the panel data model with Newey-West standard errors
ab.model <- plm(
  formula = model_formula,
  data = df,
  model = "random",            
  random.method = "walhus",
  effect = "twoways",       
  vcov = "kernel",             # Newey-West standard errors
  kernel = "bartlett",         # Bartlett kernel for Newey-West
  index = c("Country_Name", "Year"),  # Panel index
)

summary(ab.model)
pbgtest(ab.model)

result_summary <- summary(ab.model)
coefficients_table <- as.data.frame(coef(result_summary))
r_squared <- result_summary$rsquared
coefficients_table

######################################
#           pdynmc
######################################

library(pdynmc)

# Define your model (replace Year with your time variable name if different)
m1 <- pdynmc(
  dat = df,
  varname.i = "Country_Name",
  varname.t = "Year",
  use.mc.diff = TRUE,
  use.mc.lev = FALSE,
  use.mc.nonlin = FALSE,
  include.y = TRUE,
  varname.y = "Hungry",
  lagTerms.y = 1,
  fur.con = TRUE,
  fur.con.diff = TRUE,
  fur.con.lev = FALSE,
  varname.reg.fur = c(
    "Export",
    "Gini_coef",
    "Import",
    "TertierySchool",
    "School",
    "SecondarySchool"
  ),
  lagTerms.reg.fur = c(0, 0, 0, 0, 0, 1),
  include.dum = TRUE,
  dum.diff = TRUE,
  dum.lev = FALSE,
  varname.dum = "Year",
  w.mat = "iid.err",
  std.err = "dbl.corrected",
  estimation = "twostep",
  opt.meth
  = "none"
)

summary(m1)

##########################################################################
#                      !!!!!!WITH PEACE INDEX!!!!!!!!!!                 ##
##########################################################################

# AFRICA


DataGlobal <- read_excel("Panel_Africa.xlsx")

df <- DataGlobal

# Replace spaces with underscores
df <- df %>%
  rename_all(~gsub(" ", "_", .))

df <- na.omit(df)

df$GlobalPeaceIndex <- (df$GlobalPeaceIndex / 5) * 100

#FE
fixed <- plm(Hungry ~ Export+Gini_coef+GlobalPeaceIndex+Import+TertierySchool+School+SecondarySchool, data=df, index=c("Country_Name", "Year"), model="within")
summary(fixed)

#RE ("swar")
random <- plm(Hungry ~ Export+Gini_coef+GlobalPeaceIndex+Import+TertierySchool+School+SecondarySchool,
              data=df, index=c("Country_Name", "Year"), model = "random", random.method = "walhus")
summary(random)

#Choice between RE and FE (Hausman test)
phtest(fixed, random)

#Time-FE
fixed.time <- plm(Hungry ~ Export+Gini_coef+GlobalPeaceIndex+Import+TertierySchool+School+SecondarySchool + factor(Year), data=df, index=c("Country_Name", "Year"), model="within")
summary(fixed.time)

#Choice time-FE and FE
pFtest(fixed.time, fixed)
plmtest(fixed, c("time"), type=("bp"))

#Pool
pool <- plm(Hungry ~ Export+Gini_coef+GlobalPeaceIndex+Import+TertierySchool+School+SecondarySchool,
            data=df, index=c("Country_Name", "Year"), model="pooling")
summary(pool)
pFtest(fixed, pool)
# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better)
plmtest(pool, type=c("bp"))

#Testing for cross-sectional dependence/contemporaneous correlation: using Breusch-Pagan LM test of independence and Pasaran CD test
pcdtest(fixed, test=c("lm"))
pcdtest(fixed, test=c("cd"))

#Testing for serial correlation
pbgtest(fixed)

#Heteroskedastisity
bptest(Hungry ~ Export+Gini_coef+Import+GlobalPeaceIndex+TertierySchool+School+SecondarySchool+factor(Country_Name), data=df, studentize=F)

# Define the model formula
model_formula <- Hungry ~ Export + Gini_coef + GlobalPeaceIndex + Import + 
  TertierySchool + School + SecondarySchool + lag(Hungry, 1) + lag(Export, 1) +
  lag(Import, 1) + lag(Gini_coef, 1) + lag(School, 1) + lag(SecondarySchool, 1) +
  lag(TertierySchool, 1) + lag(GlobalPeaceIndex, 1)

# Specify the panel data model with Newey-West standard errors
ab.model <- plm(
  formula = model_formula,
  data = df,
  model = "within",            
  #random.method = "walhus",
  effect = "time",       
  vcov = "kernel",             # Newey-West standard errors
  kernel = "bartlett",         # Bartlett kernel for Newey-West
  index = c("Country_Name", "Year"),  # Panel index
)

summary(ab.model)
pbgtest(ab.model)

result_summary <- summary(ab.model)
coefficients_table <- as.data.frame(coef(result_summary))
r_squared <- result_summary$rsquared
coefficients_table

# ASIA

DataGlobal <- read_excel("Panel_Asia.xlsx")

df <- DataGlobal

# Replace spaces with underscores
df <- df %>%
  rename_all(~gsub(" ", "_", .))

df <- na.omit(df)

df$GlobalPeaceIndex <- (df$GlobalPeaceIndex / 5) * 100

#FE
fixed <- plm(Hungry ~ Export+Gini_coef+GlobalPeaceIndex+Import+TertierySchool+School+SecondarySchool, data=df, index=c("Country_Name", "Year"), model="within")
summary(fixed)

#RE ("swar")
random <- plm(Hungry ~ Export+Gini_coef+GlobalPeaceIndex+Import+TertierySchool+School+SecondarySchool,
              data=df, index=c("Country_Name", "Year"), model = "random", random.method = "walhus")
summary(random)

#Choice between RE and FE (Hausman test)
phtest(fixed, random)

#Time-FE
fixed.time <- plm(Hungry ~ Export+Gini_coef+GlobalPeaceIndex+Import+TertierySchool+School+SecondarySchool + factor(Year), data=df, index=c("Country_Name", "Year"), model="within")
summary(fixed.time)

#Choice time-FE and FE
pFtest(fixed.time, fixed)
plmtest(fixed, c("time"), type=("bp"))

#Pool
pool <- plm(Hungry ~ Export+Gini_coef+GlobalPeaceIndex+Import+TertierySchool+School+SecondarySchool,
            data=df, index=c("Country_Name", "Year"), model="pooling")

summary(pool)
pFtest(fixed, pool)
# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better)
plmtest(pool, type=c("bp"))


#Testing for cross-sectional dependence/contemporaneous correlation: using Breusch-Pagan LM test of independence and Pasaran CD test
pcdtest(fixed, test=c("lm"))
pcdtest(fixed, test=c("cd"))

#Testing for serial correlation
pbgtest(fixed)

#Heteroskedastisity
bptest(Hungry ~ Export+Gini_coef+Import+GlobalPeaceIndex+TertierySchool+School+SecondarySchool+factor(Country_Name), data=df, studentize=F)


#Analysis baseline data
library(lme4)
library(lmerTest)
library(arm)
library(foreign)
library(ggplot2)
library(Hmisc)
library(dplyr)
library(tidyr)
library(forcats)
library(mice)
library(VIM)


#Describing data
describe(coronadataBS)
coronadataBS <- as_tibble(coronadataBS)

countrytableN <- coronadataBS %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(N=n())

#Select countries with more than 500 participants
coronadataBS500 <- countrytableN %>%
  dplyr::filter(N>500)

coronadataBS500 <- coronadataBS500 %>%
  select(-Citizen)

describe(coronadataBS500)

coronadataBS500 <- as.data.frame(coronadataBS500)
save(coronadataBS500, file = "coronadataBS500.Rdata")

coronadataBSfill <- as_tibble(coronadataBSfill)


# Impute Stringency Index, last observation carried forward, maximum of seven 
coronadataBS500<-coronadataBS500[with(coronadataBS500, order(country, dateB)),]

coronadataBSfill <- coronadataBS500 %>%
  group_by(country) %>%
  fill(StringencyIndexB1, .direction = "down")

describe(coronadataBSfill)

# Which countries are there in the dataset?
coronadataBSfill$country <- as_factor(coronadataBSfill$country)
levels(coronadataBSfill$country)


# Number of people in different age groups
countrytableage <- coronadataBSfill %>%
  dplyr::filter(!is.na(age)) %>% 
  dplyr::group_by(country, age) %>%
  dplyr::summarise(N=n())

# Descriptives of emotions
coronadataBSfill <- as_tibble(coronadataBSfill)

countrytableaff <- coronadataBSfill %>%
  dplyr::filter(!is.na(country)) %>% 
  dplyr::group_by(country) %>%
  dplyr::summarise_each(funs(mean(., na.rm=TRUE), sd(., na.rm=TRUE)), 
                        affAnx, affBor, affCalm, affContent, affDepr, affEnerg, 
                        affExc, affNerv, affExh, affInsp, affRel, happy, N)

countrytableaff<- countrytableaff[c("country", "affAnx_mean", "affAnx_sd", "affBor_mean", "affBor_sd", 
                                    "affCalm_mean", "affCalm_sd", "affContent_mean", "affContent_sd", "affDepr_mean", 
                                    "affDepr_sd", "affEnerg_mean", "affEnerg_sd", 
                                    "affExc_mean", "affExc_sd", "affNerv_mean", "affNerv_sd", 
                                    "affExh_mean", "affExh_sd", "affInsp_mean", "affInsp_sd", "affRel_mean",
                                    "affRel_sd", "happy_mean", "happy_sd")] 

countrytableaff[,-1] <- round(countrytableaff[,-1], digits=2)

write.table(countrytableaff, file="countrytableaff.txt",row.names= FALSE, col.names = TRUE)

save(countrytableaff, file = "countrytableaff.Rdata")

#Missing data
missing.values <- coronadataBSfill %>%
  gather(key = "key", value = "val") %>%
  dplyr::mutate(is.missing = is.na(val)) %>%
  dplyr::group_by(key, is.missing) %>%
  dplyr::summarise(num.missing = n()) %>%
  dplyr::filter(is.missing==T) %>%
  dplyr::select(-is.missing) %>%
  dplyr::arrange(desc(num.missing))

save(missing.values, file="missing.values.Rdata")

missing.values %>%
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity') +
  labs(x='variable', y="number of missing values", title='Number of missing values') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####################### Centering #######################
# Centering Stringency Index
install.packages("rockchalk")
library(rockchalk)

coronadataBSfill <- gmc(coronadataBSfill, "StringencyIndexB1", "country", FUN = mean, suffix = c("_mn", "_dev"),
    fulldataframe = TRUE)

save(coronadataBSfill, file="coronadataBSfill.Rdata")

####################### Anxiety #######################
#Multilevel anxiety random intercept
model.anx0 <- lmer(affAnx ~ 1 + (1 | country), data=coronadataBSfill)
summary(model.anx0)

# Calculate variance accounted for by country
# 0.09672 + 1.45879 = 1.55551
# 0.09672 / 1.55551 = 0.06217896
# Variance in anxiety explained by country is pretty low
# Only 6.2% of the total variance of the random effects is attributed to the nested effect
# 1.45879 / 1.55551 = 0.937821
# Residual variance between individuals is quite high, 93.78%

# Predicted by stringency
model.anx1 <- lmer(affAnx ~ StringencyIndexB1_dev + (1 | country), data=coronadataBSfill)
summary(model.anx1)

anova(model.anx0, model.anx1)

# Age
model.anx2 <- lmer(affAnx ~ StringencyIndexB1_dev + age + (1 | country), data=coronadataBSfill)
summary(model.anx2)

anova(model.anx1, model.anx2)
                               
####################### Bored #######################
#Multilevel bored random intercept
model.bor0 <- lmer(affBor ~ 1 + (1 | country), data=coronadataBSfill)
summary(model.bor0)

# Predicted by stringency
model.bor1 <- lmer(affBor ~ StringencyIndexB1_dev + (1 | country), data=coronadataBSfill)
summary(model.bor1)

anova(model.bor0, model.bor1)

# Age
model.bor2 <- lmer(affBor ~ StringencyIndexB1_dev + age + (1 | country), data=coronadataBSfill)
summary(model.bor2)

anova(model.bor1, model.bor2)

####################### Calm #######################
#Multilevel calm random intercept
model.calm0 <- lmer(affCalm ~ 1 + (1 | country), data=coronadataBSfill)
summary(model.calm0)

# Predicted by stringency
model.calm1 <- lmer(affCalm ~ StringencyIndexB1_dev + (1 | country), data=coronadataBSfill)
summary(model.calm1)

anova(model.calm0, model.calm1)

# Age
model.calm2 <- lmer(affCalm ~ StringencyIndexB1_dev + age + (1 | country), data=coronadataBSfill)
summary(model.calm2)

anova(model.calm1, model.calm2)

####################### Content #######################
#Multilevel content random intercept
model.content0 <- lmer(affContent ~ 1 + (1 | country), data=coronadataBSfill)
summary(model.content0)

# Predicted by stringency
model.content1 <- lmer(affContent ~ StringencyIndexB1_dev + (1 | country), data=coronadataBSfill)
summary(model.content1)

anova(model.content0, model.content1)

# Age
model.content2 <- lmer(affContent ~ StringencyIndexB1_dev + age + (1 | country), data=coronadataBSfill)
summary(model.content2)

anova(model.content1, model.content2)

####################### Depressed #######################
#Multilevel depression random intercept
model.dep0 <- lmer(affDepr ~ 1 + (1 | country), data=coronadataBSfill)
summary(model.dep0)

# Predicted by stringency
model.dep1 <- lmer(affDepr ~ StringencyIndexB1_dev + (1 | country), data=coronadataBSfill)
summary(model.dep1)

anova(model.dep0, model.dep1)

# Age
model.dep2 <- lmer(affDepr ~ StringencyIndexB1_dev + age + (1 | country), data=coronadataBSfill)
summary(model.dep2)

anova(model.dep1, model.dep2)

####################### Energetic #######################

####################### Excited #######################

####################### Exhausted #######################

####################### Inspired #######################

####################### Nervous #######################

####################### Relaxed #######################

####################### Happy #######################

install.packages("devtools")
install.packages("finalfit") 
library(finalfit)
 
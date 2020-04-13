# Test of a relationship between spatial working memory & perception of midline

# packages ----------------------------------------------------------------

install.packages("tidyverse")
library(tidyverse)
install.packages("nycflights13")
library(nycflights13)
install.packages("multcomp")
library(multcomp)
install.packages("stringr")
library(stringr)
install.packages("foreign")
library(foreign)

# Analyses of 4-to 6-year-olds --------------------------------------------

### SWM directional errors

swm <- read.csv(file = "swm.csv", header = TRUE)

swm3 <- swm %>% 
  select(Subject:gender, DEGREE, DNERR) %>% 
  group_by(Subject,Age,DEGREE) %>% 
  summarise(meanerr = mean(DNERR,na.rm =TRUE))

as_tibble(swm3)

swm3$DEGREE <- as.factor(swm3$DEGREE)
swm3$Age <- as.factor(swm3$Age)
swm3$Subject <- as.factor(swm3$Subject)

# -- ANOVA
res.aov <- aov(meanerr ~ Age * DEGREE, data = swm3)
summary(res.aov)

# -- Multiple comparison
summary(glht(res.aov, linfct = mcp(DEGREE = "Tukey")))
TukeyHSD(res.aov)


# -- Age by target location interaction
res.aov <- aov(`60` ~ Age, data = swm3)

df <- swm %>% 
  select(Subject:gender, DEGREE, DNERR) %>% 
  group_by(Subject,age_yrs,DEGREE) %>% 
  summarise(meanerr = mean(DNERR,na.rm =TRUE))

df <- df %>%
  spread(key = DEGREE, value = meanerr)

### Performance in discrim
discrim <- read.csv(file = "discrim.csv", header = TRUE)

discrim <- discrim %>% 
  select(Subject:ALL0_654) %>% 
  gather('ALL5_13', 'ALL3_88', 'ALL2_60', 'ALL1_30', 'ALL0_654', key = 'Location', value = 'pcorrect')

discrim <- discrim %>%
  mutate(age = stringr::str_sub(age_yrs, 1, 1)) %>% 
  select(Subject, Location, pcorrect, age)

as_tibble(discrim)

discrim$Location <- as.factor(discrim$Location)
discrim$age <- as.factor(discrim$age) 
discrim$Subject <- as.factor(discrim$Subject)

# -- ANOVA
res.aov <- aov(pcorrect ~ age + Location, data = discrim)
summary(res.aov)

# -- Multiple comparison
summary(glht(res.aov, linfct = mcp(Location = "Tukey")))
TukeyHSD(res.aov, "age")
TukeyHSD(res.aov, "Location")

#-- gglot

swm4 <- swm3 %>%
  left_join(discrim, by = "Subject")

ggplot(data = swm4, mapping = aes (x = pcorrect, y = meanerr, linetype = DEGREE))+
  geom_smooth(se = FALSE)+
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data = swm4, mapping = aes (x = pcorrect, y = meanerr, color = DEGREE))+
  geom_smooth(se = FALSE)+
  theme_bw() + theme(panel.border = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



# Analyses of 3-year-olds -------------------------------------------------

#swm
swm_3yrs <- read.csv(file = "swm_3yrs.csv", header = TRUE)

as_tibble(swm_3yrs)
as_tibble(df3)

swm_3yrs$Subject <- as.factor(swm_3yrs$Subject)
swm_3yrs$degree <- as.factor(swm_3yrs$degree)

swm_3yrs %>% 
  select(Subject:errdir) %>% 
  group_by(degree) %>% 
  summarise(meanerr = mean(errdir,na.rm =TRUE))

df3 <- swm_3yrs %>% 
  select(Subject:errdir) %>% 
  group_by(Subject, degree) %>% 
  summarise(meanerr = mean(errdir))

df3 <- df3 %>%
  spread(key = degree, value = meanerr)

df3 <- df3 %>% 
  mutate (target5 =`5`, target10 = `10`,
          target20 = `20`, target40 =`40`,
          target60 = `60`)

## one-sample t test
t.test(df3$target5,  mu=0, alternative="two.sided", conf.level=0.95)
t.test(df3$target10, mu=0, alternative="two.sided", conf.level=0.95)
t.test(df3$target20, mu=0, alternative="two.sided", conf.level=0.95)
t.test(df3$target40, mu=0, alternative="two.sided", conf.level=0.95)
t.test(df3$target60, mu=0, alternative="two.sided", conf.level=0.95)

# perception of midline

discrim__3yrs <- read.csv(file = "discrim__3yrs.csv", header = TRUE)

as_tibble(discrim__3yrs)

discrim__3yrs$ID <- as.factor(discrim__3yrs$ID)

## one-sample t test
t.test(discrim__3yrs$dis5,   mu=0.5, alternative="two.sided", conf.level=0.95)
t.test(discrim__3yrs$dis3,   mu=0.5, alternative="two.sided", conf.level=0.95)
t.test(discrim__3yrs$dis2,   mu=0.5, alternative="two.sided", conf.level=0.95)
t.test(discrim__3yrs$dis1,   mu=0.5, alternative="two.sided", conf.level=0.95)
t.test(discrim__3yrs$dis06,  mu=0.5, alternative="two.sided", conf.level=0.95)


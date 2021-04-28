options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

titanic %>% filter(!is.na(Age)) %>%
  ggplot(aes(Age, y=..count..)) +
  geom_density(aes(fill=Sex), alpha=0.2, position = "stack")

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>% filter(!is.na(Age)) %>% ggplot(aes(sample=Age)) +
  geom_qq(dparams = params) + 
  geom_abline()

titanic %>% ggplot(aes(Survived, fill=Sex))  + 
  geom_bar(position = position_dodge())

titanic %>% filter(!is.na(Age)) %>% ggplot(aes(x=Age,y=..count..)) +
  geom_density(aes(fill=Survived), alpha=0.2, position = "stack")

titanic %>% filter(Fare!=0) %>% ggplot(aes(Survived,Fare)) +
  geom_boxplot() +
  geom_jitter(width=0.1, alpha=0.2) +
  scale_y_continuous(trans="log2")

titanic %>% ggplot(aes(Pclass)) +
  geom_bar(aes(fill=Survived), position = position_fill())

titanic %>% ggplot(aes(Survived)) +
  geom_bar(aes(fill=Pclass),position = position_fill())

titanic %>% filter(!is.na(Age)) %>% ggplot(aes(x=Age, y=..count..)) +
  geom_density(aes(fill=Survived), alpha=0.2) +
  facet_grid(Sex~Pclass)

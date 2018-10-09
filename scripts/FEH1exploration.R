install.packages("emmeans")
install.packages("ggplot2")
install.packages("tidyverse")
library(emmeans)
library(ggplot2)
library(tidyverse)
install.packages("lmerTest")
library(lmerTest)

herb <- read.csv("data/herbicide_raw_data.csv")

herb$Block <- factor(herb$Block)
herb$Innoculation <- factor(herb$Innoculation, c("RRI128", "washed", "herb", "O-N", "O+N"))
herb$Herbicide <- factor(herb$Herbicide, c("NoHerbicide", "Herbicide"))

str(herb)
View(herb)

herb$totalN <- herb$shootN + herb$bgN

# create a tibble called herb2 where shootN and bgN are summed to give totalN
herb2 <- read_csv("data/herbicide_raw_data.csv")

View(herb2)

# create a tibble called herb3 without uninoculated
herb3 <- filter(herb2, Innoculation %in% c("RRI128", "herb", "washed"))
View(herb3)

str(herb)

herb4 <- filter(herb, Innoculation %in% c("RRI128", "herb", "washed"))
str(herb4)


# some plots for visualisation

ggplot(herb, aes(Variety, shootDM + bgDM, colour = Herbicide)) +
  geom_point() +
  geom_boxplot() +
  facet_wrap("Innoculation")


ggplot(herb, aes(Variety, shootN, colour = Herbicide)) +
  geom_point() +
  geom_boxplot() +
  facet_wrap("Innoculation")

ggplot(herb, aes(shootDM + bgDM, shootN + bgN, colour = Innoculation)) +
  geom_smooth(aes(fill = Innoculation)) +
  geom_point(size = 3) +
  theme(panel.grid = element_blank(),
         panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA))


ggplot(herb3, aes(shootDM, shootN, colour = Herbicide)) +
  geom_point() +
  facet_wrap("Variety")

ggplot(herb3, aes(bgDM, bgN, colour = Herbicide)) +
  geom_point() +
  facet_wrap("Variety")

ggplot(herb3, aes(Variety, shootDM / bgDM + shootDM, colour = Herbicide)) +
  geom_point() +
  geom_boxplot() +
  facet_wrap("Innoculation")

# look at the relationship between ara and nodules, roots, shoots
ggplot(herb3, aes(shootDM, ara, colour = Herbicide)) +
  geom_point() +
  facet_wrap("Variety")

ggplot(herb3, aes(ara, bgDM + shootDM, colour = Herbicide)) +
  geom_point() +
  facet_wrap("Variety")

ggplot(herb3, aes(shootDM + bgDM, shootDM, colour = Herbicide)) +
  geom_point() +
  facet_wrap("Variety")

ggplot(herb3, aes(bgDM - pinknodmass - nonpinknodmass, pinknodmass, colour = Herbicide)) +
  geom_point() +
  facet_wrap("Variety")

ggplot(herb3, aes(bgDM - pinknodmass - nonpinknodmass, pinknods + nonpinknods, colour = Herbicide)) +
  geom_point() +
  facet_wrap("Variety")

# some analyses

aralm <- lm(ara~Variety*Herbicide*Innoculation + Block, data = herb3)
anova(aralm)
plot(aralm, which=1)

aralm1 <- lm(log(ara+1)~Variety*Herbicide*Innoculation + Block, data = herb3)
anova(aralm1)
plot(aralm1, which=1)

aralm2 <- lm(sqrt(ara)~Variety*Herbicide*Innoculation + Block, data = herb3)
anova(aralm2)
plot(aralm2, which =1)
emmeans(aralm2, ~Variety*Herbicide)
emmeans(aralm2, ~Variety*Herbicide, type = "response")

aralmer <- lmer(sqrt(ara)~Variety*Herbicide*Innoculation + (1|Block), data=herb3)
anova(aralmer)
summary(aralmer)
emmeans(aralmer, ~Variety*Herbicide, type = "response")

rm(list=ls())
# Load tidyverse for data manipulation (dplyr, ggplot2, etc.)
library(tidyverse)
library(ggridges)
library(boot)
# load the pres/abs data for some exploratory work
presAbsDF <- readRDS(file = "presAbsDF.rds")
names(presAbsDF)
ggplot(data=presAbsDF,
       mapping = aes(x = AnnualMeanTmp, y = SpeciesStatus,
                     fill = SpeciesStatus,
                     height = after_stat(density))) +
  geom_density_ridges(stat = "density", alpha=0.5) +
  scale_fill_viridis_d() +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_ridges() + theme(legend.position = "none")

pDens <- presAbsDF %>%
  select(-CategoricalAspect) %>%
  pivot_longer(-SpeciesStatus) %>%
  ggplot(mapping = aes(x = value, y = SpeciesStatus,
                     fill = SpeciesStatus,
                     height = after_stat(density))) +
  geom_density_ridges(stat = "density", alpha=0.5) +
  scale_fill_viridis_d() +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_ridges() + theme(legend.position = "none") +
  facet_wrap(~name,scales = "free_x")

pdf(file = "figs/edf_predictors.pdf",width = 10,height = 10)
pDens
dev.off()


# get bootstraps for means of all numeric predictors
m <- 1000
predictorBootstrapMeans <- list()
for(i in 1:m){
predictorBootstrapMeans[[i]] <- presAbsDF %>%
  slice_sample(prop=1,replace=TRUE,by=SpeciesStatus) %>%
  summarise(across(where(is.numeric), mean, .names = "{.col}"),
            .by = SpeciesStatus)
}
predictorBootstrapMeans <- bind_rows(predictorBootstrapMeans)

predictorBootstrapMeans %>%
  ggplot(mapping = aes(x=SpeciesStatus,
                       y=AnnualMeanTmp,
                       color = SpeciesStatus)) +
  geom_jitter(width=0.2,alpha=0.25, show.legend = FALSE)

pMeans <- predictorBootstrapMeans %>%
  pivot_longer(-SpeciesStatus) %>%
  ggplot(mapping = aes(x=SpeciesStatus,y=value,
                     color=name)) +
  geom_jitter(width=0.2,alpha=0.25, show.legend = FALSE) +
  facet_wrap(~name,scales="free_y") +
  labs(x = element_blank(), y = element_blank(),
       title = "Bootstrapped Sample Means",
       subtitle = "1000 realizations of sample means via bootstrap") +
  theme_minimal()

pdf(file = "figs/means_predictors.pdf",width = 10,height = 10)
pMeans
dev.off()


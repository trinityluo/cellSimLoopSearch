# calculate flux from decomposed results
library(stringr)
library(data.table)
library(ggplot2)
library(scales)
decopseList <- lapply(list.files('data/processed/gamma', full.names = T), readRDS)
dfName <- list.files('data/processed/gamma', full.names = T) %>% 
  str_extract('0.[0-9]{2}')
  
names(decopseList) <- dfName

gFlux <- lapply(decopseList, summarise, avg.flux = mean(weight), bio.flux = max(weight)) %>% 
  rbindlist(idcol = 'gamma') %>% 
  mutate(gamma = as.numeric(gamma))



g.flux <- ggplot(data = gFlux, aes(x = gamma, y = bio.flux)) +
  geom_line() +
  geom_point() +
  labs(title = expression(paste('c=0.001, ', mu, '=5, Varying ', gamma)), 
       x = expression(gamma), y = 'flux') +
  scale_x_continuous(labels = percent_format(), breaks = c(0.01, 0.1, 0.2, 0.3)) +
  scale_y_continuous(labels = scientific_format()) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave('figs/g.flux.eps', g.flux)
ggsave('figs/g.flux.pdf', g.flux)


# freq vs g
powspec.c0.01 <- read.table('data/raw/powspe/a7.0.b5.0.c0.01.powspe', col.names = c('count', 'value')) %>% 
  filter(count != Inf) %>% 
  mutate(freq = 1/count)

ggplot(powspec.c0.01, aes(x = freq, y = value)) +
  geom_line() +
  #geom_point()+
  xlim(0, NA) +
  theme_classic()


# freq vs gamma
fileName <- list.files('data/raw/powspe', full.names = T, pattern = '.powspe')
powspecList <- lapply(fileName, read.table, col.names = c('count', 'value')) %>% 
  lapply(mutate, freq = 1/count) %>% 
  lapply(filter, count != Inf)

dfName <- fileName %>% 
  str_extract('0.[0-9]{2}')

names(powspecList) <- dfName

gMaxFreq <- lapply(powspecList, filter, value == max(value), freq >= 0) %>% 
  lapply(select, freq) %>% 
  rbindlist(idcol = 'gamma') %>% 
  mutate(gamma = as.numeric(gamma)) %>% 
  left_join(gFlux, by = 'gamma')

plot.gMaxFreq <- ggplot(gMaxFreq, aes(x = gamma, y = freq)) +
  geom_point() +
  geom_line() +
  labs(title = expression(paste('c=0.001, ', mu, '=5, Varying ', gamma)), 
       x = expression(gamma), y = 'frequency of the most prominent oscillation') +
  scale_x_continuous(labels = percent, breaks = c(0.01, 0.1, 0.2, 0.3)) +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave('figs/g.freq.eps', plot.gMaxFreq)
ggsave('figs/g.freq.pdf', plot.gMaxFreq)


# freq vs flux
plot.fluxMaxFreq <- ggplot(gMaxFreq, aes(x = bio.flux, y = freq)) +
  geom_point() +
  geom_line() +
  labs(title = expression(paste('c=0.001, ', mu, '=5, Varying ', gamma)), 
       x = 'flux', y = 'frequency of the most prominent oscillation') +
  scale_x_continuous(labels = scientific) +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave('figs/g.flux.freq.eps', plot.fluxMaxFreq)
ggsave('figs/g.flux.freq.pdf', plot.fluxMaxFreq)


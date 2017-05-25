# calculate flux from decomposed results
library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)
library(cowplot)

myThemes <- theme(
  axis.title = element_text(family = "Helvetica", size = rel(0.8)),
  axis.text.x = element_text(size = rel(0.8)),
  plot.title = element_text(face = 'bold',
                            size = rel(1), family = 'Helvetica', hjust = 0.5),
    panel.border = element_rect(colour = "black", fill = NA, size = rel(1))
)

# get data ready b=5
decopseList.b5 <- lapply(list.files('data/processed/gamma.b5', full.names = T), readRDS)
dfName.b5 <- list.files('data/processed/gamma.b5', full.names = T) %>% 
  str_extract('0.[0-9]{2}')
  
names(decopseList.b5) <- dfName.b5

gFlux.b5 <- lapply(decopseList.b5, summarise, avg.flux = mean(weight), bio.flux = max(weight)) %>% 
  rbindlist(idcol = 'gamma') %>% 
  mutate(gamma = as.numeric(gamma))

# freq vs gamma data
fileName.b5 <- list.files('data/raw/powspe', full.names = T, pattern = '.powspe')
powspecList.b5 <- lapply(fileName.b5, read.table, col.names = c('count', 'value')) %>% 
  lapply(mutate, freq = 1/count) %>% 
  lapply(filter, count != Inf)

dfName.b5 <- fileName.b5 %>% 
  str_extract('0.[0-9]{2}')

names(powspecList.b5) <- dfName.b5

# freq vs gamma data
df.g.freq.b5 <- lapply(powspecList.b5, filter, value == max(value), freq >= 0) %>% 
  lapply(select, count, freq) %>% 
  rbindlist(idcol = 'gamma') %>% 
  mutate(gamma = as.numeric(gamma)) %>% 
  left_join(gFlux.b5, by = 'gamma')

# epr data
g.epr.b5 <- read.table('data/raw/ga7.b5.c.01.to.c0.3.out', 
                       col.names = c('c', 'miu', 'gamma', 'RR', 'epr', 'PG1', 'Ppath', 'CFlux')) %>% 
  left_join(df.g.freq.b5, by = 'gamma') %>% 
  mutate(eprByPeriod = epr * count)

g.epr.b0.8 <- read.table('data/raw/ga7.b0.8.c.01.to.c0.3.out', 
                         col.names = c('c', 'miu', 'gamma', 'RR', 'epr', 'PG1', 'Ppath', 'CFlux')) %>% 
  left_join(gFlux.b5, by = 'gamma')

df.merged.b5 <- g.epr.b5 %>% filter(gamma != 0.01)


# get data ready b=0.8
decopseList.b0.8 <- lapply(list.files('data/processed/gamma.b0.8', full.names = T), readRDS)
dfName.b0.8 <- list.files('data/processed/gamma.b0.8', full.names = T) %>% 
  str_extract('0.[0-9]{2}')

names(decopseList.b0.8) <- dfName.b0.8

gFlux.b0.8 <- lapply(decopseList.b0.8, summarise, avg.flux = mean(weight), bio.flux = max(weight)) %>% 
  rbindlist(idcol = 'gamma') %>% 
  mutate(gamma = as.numeric(gamma))

# freq vs gamma data
fileName.b0.8 <- list.files('data/raw/powspe', full.names = T, pattern = '.powspe')
powspecList.b0.8 <- lapply(fileName.b0.8, read.table, col.names = c('count', 'value')) %>% 
  lapply(mutate, freq = 1/count) %>% 
  lapply(filter, count != Inf)

dfName.b0.8 <- fileName.b0.8 %>% 
  str_extract('0.[0-9]{2}')

names(powspecList.b0.8) <- dfName.b0.8

# freq vs gamma data
df.g.freq.b0.8 <- lapply(powspecList.b0.8, filter, value == max(value), freq >= 0) %>% 
  lapply(select, count, freq) %>% 
  rbindlist(idcol = 'gamma') %>% 
  mutate(gamma = as.numeric(gamma)) %>% 
  left_join(gFlux.b0.8, by = 'gamma')

# epr data
g.epr.b0.8 <- read.table('data/raw/ga7.b0.8.c.01.to.c0.3.out', 
                       col.names = c('c', 'miu', 'gamma', 'RR', 'epr', 'PG1', 'Ppath', 'CFlux')) %>% 
  left_join(df.g.freq.b0.8, by = 'gamma') %>% 
  mutate(eprByPeriod = epr * count)

g.epr.b0.8 <- read.table('data/raw/ga7.b0.8.c.01.to.c0.3.out', 
                         col.names = c('c', 'miu', 'gamma', 'RR', 'epr', 'PG1', 'Ppath', 'CFlux')) %>% 
  left_join(gFlux.b0.8, by = 'gamma')

df.merged.b0.8 <- g.epr.b0.8 %>% filter(gamma != 0.01)




# g vs flux fig.2
plot.g.flux.b5 <- ggplot(data = df.merged.b5, aes(x = gamma, y = bio.flux)) +
  geom_line() +
  geom_point() +
  labs( 
       x = expression(gamma), y = 'flux') +
  #scale_x_continuous(labels = percent_format(), breaks = c(0.01, 0.1, 0.2, 0.3)) +
  scale_x_continuous(, breaks = pretty_breaks()) +
  scale_y_continuous(labels = scientific_format()) +
  theme_classic() +
  myThemes

ggsave('figs/g.flux.b5.eps', plot.g.flux.b5)
ggsave('figs/g.flux.b5.pdf', plot.g.flux.b5)

# g vs flux fig.2 miu=0.8
plot.g.flux.b0.8 <- ggplot(data = df.merged.b0.8, aes(x = gamma, y = bio.flux)) +
  geom_line() +
  geom_point() +
  labs( 
    x = expression(gamma), y = 'flux') +
  #scale_x_continuous(labels = percent_format(), breaks = c(0.01, 0.1, 0.2, 0.3)) +
  scale_x_continuous(, breaks = pretty_breaks()) +
  scale_y_continuous(labels = scientific_format()) +
  theme_classic() +
  myThemes

ggsave('figs/g.flux.b0.8.eps', plot.g.flux.b0.8)
ggsave('figs/g.flux.b0.8.pdf', plot.g.flux.b0.8)


# freq vs g
# powspec.c0.01 <- read.table('data/raw/powspe/a7.0.b5.0.c0.01.powspe', col.names = c('count', 'value')) %>% 
#   filter(count != Inf) %>% 
#   mutate(freq = 1/count)
# 
# ggplot(powspec.c0.01, aes(x = freq, y = value)) +
#   geom_line() +
#   #geom_point()+
#   xlim(0, NA) +
#   theme_classic()




# freq vs gamma fig.1
plot.g.freq.b5 <- ggplot(df.merged.b5, aes(x = gamma, y = freq)) +
  geom_point() +
  geom_line() +
  labs( 
       x = expression(gamma), y = 'frequency of the most prominent oscillation') +
  #scale_x_continuous(labels = percent, breaks = c(0.01, 0.1, 0.2, 0.3)) +
  scale_x_continuous(, breaks = pretty_breaks()) +
  scale_y_continuous(limits = c(0.04, 0.08), breaks = pretty_breaks()) +
  theme_classic() +
  myThemes

ggsave('figs/g.freq.b5.eps', plot.g.freq.b5)
ggsave('figs/g.freq.b5.pdf', plot.g.freq.b5)


# flux vs freq fig.4
plot.flux.freq.b5 <- ggplot(df.merged.b5, aes(x = bio.flux, y = freq)) +
  geom_point() +
  geom_line() +
  labs( 
       x = 'flux', y = 'frequency of the most prominent oscillation') +
  scale_x_continuous(labels = scientific) +
  scale_y_continuous(limits = c(0.04, 0.08), breaks = pretty_breaks()) +
  theme_classic() +
  myThemes

ggsave('figs/g.flux.freq.b5.eps', plot.flux.freq.b5)
ggsave('figs/g.flux.freq.b5.pdf', plot.flux.freq.b5)



# g vs epr fig.3
# b=5
plot.g.epr.b5 <- ggplot(df.merged.b5, aes(x = gamma, y = epr)) +
  geom_point() +
  geom_line() +
  labs(
       x = expression(gamma), y = 'dS/dt') +
  #scale_x_continuous(labels = percent) +
  scale_x_continuous(, breaks = pretty_breaks()) +
  theme_classic() +
  myThemes

ggsave('figs/g.epr.b5.eps', plot.g.epr.b5)
ggsave('figs/g.epr.b5.pdf', plot.g.epr.b5)

# b=0.8 fig.3
plot.g.epr.b0.8 <- ggplot(df.merged.b5, aes(x = gamma, y = epr)) +
  geom_point() +
  geom_line() +
  labs(
       x = expression(gamma), y = 'dS/dt') +
  scale_x_continuous(labels = percent) +
  
  theme_classic() +
  myThemes

ggsave('figs/g.epr.b0.8.eps', plot.g.epr.b0.8)
ggsave('figs/g.epr.b0.8.pdf', plot.g.epr.b0.8)

# flux vs epr fig.5
# b=5
plot.flux.epr.b5 <- ggplot(df.merged.b5, aes(x = bio.flux, y = epr)) +
  geom_point() +
  geom_line() +
  labs(
       x = 'flux', y = 'dS/dt') +
  scale_x_continuous(labels = scientific) +
  theme_classic() +
  myThemes

ggsave('figs/g.flux.epr.b5.eps', plot.flux.epr.b5)
ggsave('figs/g.flux.epr.b5.pdf', plot.flux.epr.b5)

# epr*period vs flux fig.6
plot.flux.eprByPeriod.b5 <- ggplot(df.merged.b5, aes(x = bio.flux, y = eprByPeriod)) +
  geom_point() +
  geom_line() +
  labs(
       x = 'flux', y = 'EPR x period') +
  scale_x_continuous(labels = scientific) +
  scale_y_continuous(breaks = pretty_breaks()) +
  theme_classic() +
  myThemes

ggsave('figs/g.flux.eprByPeriod.b5.eps', plot.flux.eprByPeriod.b5)
ggsave('figs/g.flux.eprByPeriod.b5.pdf', plot.flux.eprByPeriod.b5)


# combine figs
# pdf('figs/g.combined.pdf')
# grid.arrange(plot.g.freq.b5, plot.g.flux.b5, plot.g.epr.b5,
#              plot.flux.freq.b5, plot.flux.epr.b5, plot.flux.eprByPeriod.b5,
#              ncol = 3, nrow = 2)
# dev.off()
# 
# ggsave('figs/g.combined.pdf', 
#        arrangeGrob(plot.g.freq.b5, plot.g.flux.b5, plot.g.epr.b5, 
#                    plot.flux.freq.b5, plot.flux.epr.b5, plot.flux.eprByPeriod.b5,
#                    ncol = 3, nrow = 2))

p <- plot_grid(plot.g.freq.b5, plot.g.flux.b5, plot.g.epr.b5, 
          plot.flux.freq.b5, plot.flux.epr.b5, plot.flux.eprByPeriod.b5,
          ncol = 3, nrow = 2, labels = c('(a)', '(b)','(c)','(d)','(e)','(f)'),
          label_size = 9, hjust = -12, vjust = 31.5, scale = 0.9)
  #ggtitle(expression(paste('c=0.001, ', mu, '=5, Varying ', gamma, ' from 1% to 30%')))
ggsave('figs/g.combined.pdf', p, width = 10.4, height = 6.5)
ggsave('figs/g.combined.eps', p, width = 10.4, height = 6.5)

#setEPS()
#postscript('figs/spectrum.eps', width = 8, height = 4)
# cairo_ps('figs/spectrum.eps', width = 8, height = 4)
# grid.arrange(p.landscape, p.flux, ncol = 2)
# dev.off()

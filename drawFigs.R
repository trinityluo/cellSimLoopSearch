library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

rm(list = ls())

options(bitmapType="cairo")

Dflux.a7.0.b5.0.c0.60 <- read_delim("data/raw/Dflux.a7.0.b5.0.c0.60.out", 
                                    ':', col_names = c('prob', 'loop')) %>%
  select(prob) %>% 
  mutate(masterEq.flux = -log(prob), id = 1:nrow(.), ref = 2)

prob.a7.0.b5.0.c0.60 <- read_csv("data/raw/prob.a7.0.b5.0.c0.60.out", 
                                 col_names = 'prob') %>% 
  mutate(landscape.flux = -log(prob), id = 1:nrow(.), ref = 2)

#bioPath <- c('4', '132', '130', '138', '154', '19', '101', '100', '612', '356')
bioPath <- c(4, 132, 130, 138, 154, 19, 101, 100, 612, 356) %>% + 1

myThemes <- theme(
  axis.title = element_text(colour="steelblue4",family = "Helvetica",
                            size = rel(1.5)),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  #panel.grid.major = element_line(colour="grey",size = rel(0.5)),
  panel.background = element_rect(fill = "whitesmoke"),
  axis.line = element_line(size = 1, linetype = 'solid', colour = 'black'),
  plot.title = element_text(colour = 'steelblue4', face = 'bold',
                            size = rel(2), family = 'Helvetica', hjust = 0.5)
)

p.landscape <- ggplot(prob.a7.0.b5.0.c0.60, aes(x = ref, y = landscape.flux)) +
  geom_hline(yintercept = prob.a7.0.b5.0.c0.60$landscape.flux, alpha = 0.5, size = 0.1) +
  geom_hline(yintercept = prob.a7.0.b5.0.c0.60[bioPath, ]$landscape.flux, 
             colour = 'green', size = 0.1, alpha = 0.5) +
  scale_y_continuous(limits = c(0, 45)) +
  scale_x_continuous(limits = c(0,2)) +
  myThemes +
  annotate('text', x = 1.7, y = 0.5, label = 'Bio-Path', size = 5, colour = 'red') +
  geom_segment(aes(x = 1.2), y = 1.9, xend = 1.4, yend = 0.5, colour = 'red',
               size = 0.4, arrow = arrow(length = unit(0.4, "cm"))) +
  ggtitle('Landscape Spectrum') +
  labs(y = 'U', x = NULL)
p.landscape

p.flux <- ggplot() +
  geom_hline(yintercept = Dflux.a7.0.b5.0.c0.60$masterEq.flux, alpha = 0.3, size = 0.1) +
  geom_hline(yintercept = Dflux.a7.0.b5.0.c0.60[1, ]$masterEq.flux, colour = 'green',
             size = 0.1, alpha = 0.5) +
  scale_y_continuous(limits = c(0, 190)) +
  scale_x_continuous(limits = c(0,2)) +
  myThemes + 
  annotate('text', x = 1.7, y = 0.1, label = 'Bio-Path', size = 5, colour = 'red') +
  geom_segment(aes(x = 1.2), y = 1.9, xend = 1.4, yend = 0.1, colour = 'red',
               size = 0.5, arrow = arrow(length = unit(0.4, "cm"))) +
  ggtitle('Flux Spectrum') +
  labs(y = '-log(flux)', x = NULL)
p.flux

pdf('figs/spectrum.pdf', width = 8, height = 4)
grid.arrange(p.landscape, p.flux, ncol = 2)
dev.off()

#setEPS()
#postscript('figs/spectrum.eps', width = 8, height = 4)
cairo_ps('figs/spectrum.eps', width = 8, height = 4)
grid.arrange(p.landscape, p.flux, ncol = 2)
dev.off()


# RR vs simTime
singleRRInfo <- read_delim("data/processed/singleRRInfo.txt", delim = ' '
                                    ) %>%
  select(-X3, -simTime) %>% 
  mutate(id = seq(1:nrow(.))) %>% 
  mutate(simStep = id * 100 * 1e5 + 6586572) %>% 
  select(id, RR, simStep)

p.RR <- ggplot(singleRRInfo, aes(x = simStep, y = RR)) +
  geom_line() +
  scale_x_continuous(trans = 'log10')

p.RR



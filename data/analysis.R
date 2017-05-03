# calculate potential max loop

potentialLoop <- choose(1024, 3) + choose(1024, 4)*factorial(3)+ choose(1024, 5)*factorial(4) +
choose(1024, 6)*factorial(5)+ choose(1024, 7)*factorial(6)+ choose(1024, 8)*factorial(7) +
choose(1024, 9)*factorial(8)+ choose(1024, 10)*factorial(9)

singleRRInfo <- read_delim("data/processed/singleRRInfo.txt", delim = ' '
) %>%
  select(-X3, -simTime) %>% 
  mutate(id = seq(1:nrow(.))) %>% 
  mutate(simStep = id * 100 * 1e5 + 6586572) %>% 
  select(id, RR, simStep)

singleLoopInfo.a7.b5.c0.6 <- read_rds('data/processed/singleloop.a7.b5.c0.6.Rds')

totalSimStep <- sum(singleLoopInfo.a7.b5.c0.6$count * singleLoopInfo.a7.b5.c0.6$steps)

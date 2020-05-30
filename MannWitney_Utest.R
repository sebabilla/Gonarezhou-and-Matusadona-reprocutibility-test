#Table 4 means & ranks
meansG <- raw %>% filter(Park == "G") %>% select(A1:A14) %>%
  colMeans()
ranksG <- rank(-meansG)
meansM <- raw %>% filter(Park == "M") %>% select(A1:A14) %>%
  colMeans()
ranksM <- floor(rank(-meansM))

#Table 4 Mann-Whitney U tests (are the 2 samples independant)
entryG <- raw %>% filter(Park == "G") %>% select(A1:A14)
entryM <- raw %>% filter(Park == "M") %>% select(A1:A14)
UtestW <- sapply(1:14, function(col) wilcox.test(unlist(entryM[,col]),unlist(entryG[,col]), correct = FALSE)$statistic)
Utestp <- sapply(1:14, function(col) wilcox.test(unlist(entryM[,col]),unlist(entryG[,col]), correct = FALSE)$p.value)

#Table 4 many values so CLT and p.value instead of Z (abs(Z)>2 == p<0.5)
tibble(names[2:15,2],names[2:15,1],meansG,ranksG,meansM,ranksM,N=rep(128,14),U=UtestW,round(Utestp, 2))

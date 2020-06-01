#Exploratory Factor analysis
EFAdata <- raw %>% select(A1:A14)

#EFA possible if data are normally sorted
par(mfrow=c(3,5))
for (i in 1:14) {
  x <- EFAdata[,i] %>% unlist
  title <- paste("A",i, sep = "")
  hist(x, main=title)
}
par(mfrow=c(1,1))
#How to group Data?
round(cor(EFAdata),2)
#before starting check that 2 variables are not too corelated - Avoid part of cofounding 
colSums(cor(EFAdata)>=0.9) 
#does the level between variables seems different?
colSums(cor(EFAdata)>=0.4) # in other papers it's 0.3?, but OK, the more is better
#is it appropriate to do an EFA?
KMO(cor(EFAdata)) # > 0.6  same value as paper = Kaiser-Meyer-Oklin
cortest.bartlett(EFAdata) # 91 df but chisq only 658 -> low variability, but p < 0.001, OK
#how many groups to do?
fa.parallel(EFAdata, fa = 'pc')

#do the EFA using PCA 
noms <-  names[2:15,2] %>% pull
colnames(EFAdata) <- noms
pc <- principal(EFAdata, nfactors=3, rotate="none")
pck <- kaiser(pc, rotate = "oblimin")
fa.plot(pck, title = "position of factors along PCx, PCy axes")
print.psych(pck, cut = 0.42, sort = TRUE) #contrary to the paper I have to do a cut at 0.42 (not 0.32)
fa.diagram(pck, cut = 0.42, digits = 2, simple = TRUE)
#Table 3 : it looks like Factors/items and factor load, but slightly different 

Paperfactors <- tibble(Rawcode = names[2:15,1]$Raw_code, 
                       Paperfactors = c("Appreciating",
                                        "Appreciating",
                                        "Appreciating",
                                        "Feeling",
                                        "Feeling",
                                        "Appreciating",
                                        NA,
                                        "Recreation",
                                        "Recreation",
                                        "Recreation",
                                        "Recreation",
                                        "Recreation",
                                        "Recreation",
                                        "Recreation"))
#Table 3 : mean by doing mean of columns then group -> fail
means_col_raw <- raw %>% select(A1:A14) %>%
  colMeans()
tibble(Rawcode = names(means_col_raw), means = means_col_raw) %>%
  left_join(Paperfactors) %>%
  group_by(Paperfactors) %>%
  summarize(means = mean(means))

#Table 3 : mean by park, the agregate (to test disymetry) -> fail 
mean(c(mean(c(4.52,4.75)),mean(c(4.51,4.61)))) 

#Table 3 : Cronbach alpha -> this too is slightly different
find_alpha <- function(x){
select_col <- Paperfactors %>% filter(Paperfactors == x) %>%
  select(Rawcode) %>% pull
alpha <- raw %>% select(select_col) %>%
  alpha() %>% .$total %>%
  select(mean, std.alpha) %>% unlist %>% round(., digits = 2)
alpha
}
fact <- c("Recreation","Appreciating","Feeling")
sapply(fact, find_alpha) %>%
  t() %>% as.data.frame() %>% rownames_to_column() %>% 
  tableGrob(rows = NULL) %>% grid.arrange()


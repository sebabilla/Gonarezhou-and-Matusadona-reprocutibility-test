#Kruskal-Willis

rawG <- raw %>% filter(Park == "G") %>% select(A1:A14, `J2-Age`:`J5-Income`)
rawG %>% select(1, 15) %>% kruskal.test()

GAJ <- sapply(c(15,17,18,16), function(x){
allA <- sapply(1:14, function(y){
  dat <- rawG %>% select(x=x, y=y) 
    kruskal.test(y~x, dat) %>% .$statistic
})
allA
})
GAJ <- t(GAJ) %>% round(.,2)
colnames(GAJ) <- names(rawG)[1:14]
rownames(GAJ) <- names(rawG)[c(15,17,18,16)]

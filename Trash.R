#EFA
#same kind of results using specific pca function
factors3 <- fa(EFAdata,nfactors = 2,rotate = "none",fm="pa")
factors3k <- kaiser(factors3, rotate = "oblimin") #According to the documentation, results will be different in SPSS and R whatsoever https://www.personality-project.org/r/html/kaiser.html
print.psych(factors3k, cut = 0.32, sort = TRUE)
fa.diagram(factors3k)

alpha(EFAdata)
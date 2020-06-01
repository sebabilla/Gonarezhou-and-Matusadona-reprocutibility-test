#ordinal logistic regression
#https://www.r-bloggers.com/how-to-perform-ordinal-logistic-regression-in-r/
ranked_levels <- c("1","2","3","4","5") 

reg_input <- raw %>% #filter(Park == "G") %>%
  mutate(Appr = (A1 + A2  +A3 + A6)/4*0.815, 
         Feel = (A5 + A6)/2*0.482, 
         Recr = (A8 + A9 + A10 + A11 + A12 + A13 + A14)/6*0.783,
         Intepretation = (C1 + C2 + C3 + C4)/4,
         Interaction = (D1 + D2 + D3 + D4)/4) %>%
  #mutate(Intepretation = ifelse(Park == "G", Intepretation*0.72, Intepretation*0.69),
  #       Interaction = ifelse(Park == "G", Interaction*0.76, Interaction*0.66)) %>%
  mutate(Intepretation = as.factor(Intepretation),
         Interaction = as.factor(Interaction))

model_interpretation <- polr(Intepretation ~ Recr + Appr + Feel, data = reg_input, Hess = TRUE)
odds <- exp(confint(model_interpretation)) %>% as.data.frame() %>%
  mutate(odd = exp(model_interpretation$coefficients))
tableup <- summary(model_interpretation)$coefficients[1:3,1:2] %>% 
  as.data.frame() %>% data.frame(., odds) %>% round(., 2)
colnames(tableup) <- c("coef", "sd", "inf2.5", "odd", "sup2.5")
model_interpretation2 <- stepAIC(model_interpretation, ~.^2)
chisq_up <- anova(model_interpretation, model_interpretation2)


model_interaction <- polr(Interaction ~ Recr + Appr + Feel, data = reg_input, Hess = TRUE)
odds <- exp(confint(model_interaction)) %>% as.data.frame() %>%
  mutate(odd = exp(model_interaction$coefficients))
tabledown <- summary(model_interaction)$coefficients[1:3,1:2] %>% 
  as.data.frame() %>% data.frame(., odds) %>% round(., 2)
colnames(tabledown) <- c("coef", "sd", "inf2.5", "odd", "sup2.5")
model_interaction2 <- stepAIC(model_interaction, ~.^2)
chisq_down <- anova(model_interaction, model_interaction2)

#figure 5
chisq_up$`Pr(Chi)`[2]
tableup
chisq_down$`Pr(Chi)`[2]
tabledown

reg_input_bis <- reg_input %>% filter(Park == "G")
model_all <- polr(Intepretation ~ A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A9 + A10 + A11 + A12 + A13 + A14, data = reg_input_bis, Hess = TRUE)
odds <- exp(confint(model_all)) %>% as.data.frame() %>%
  mutate(odd = exp(model_all$coefficients))
tableleft <- summary(model_all)$coefficients[1:14,1:2] %>% 
  as.data.frame() %>% data.frame(., odds) %>% round(., 2)
colnames(tableleft) <- c("coef", "sd", "inf2.5", "odd", "sup2.5")
#model_all2 <- stepAIC(model_all, ~.^2)
#chisq_left <- anova(model_all, model_all2)

#table6
#chisq_left$`Pr(Chi)`[2]
tableleft
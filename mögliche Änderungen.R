
  #extract focal variable
  all.data <-  cleanGSS(var) %>%
    mutate(y1 = w1, y2 = w2, y3 = w3, y4 = w3) %>% #simulate a fourth wave out of the third
    select(-c(w1, w2, w3)) 
  
  #Unconstrained model
  nls1 <- nls(y4 ~ a + b*p*y3 + b*p*y2 + b*p*y1, #combined AUM & SDM 
              data = all.data,
              start = list(a = 0, b = 1, p = 1),
              weights = wtpannr123)
  # scheint basierend auf der start values nicht zu funktionieren
  bic1 <- BIC(nls1)
  
  #Model that constrains phi to .5
  nls2 <- nls(y3 ~ a + b*.5*y2 + b*(.5)*y1, #This model should generate a better fit for data, if SDM is a preferable explanation.
              data = all.data,
              start = list(a = 0, b = 1),
              weights = wtpannr123)
  bic2 <- BIC(nls2)
  
  #store results in list
  results[[i]] <- data.frame(var = var, bic1 = bic1, bic2 = bic2,
                             p = coef(nls1)[3], 
                             b1 = coef(nls1)[2], b2 = coef(nls2)[2])
  
  #print to spot errors
  print(paste(var, i, sep = " "))
  
  #clean out before the next iteration
  rm(var, bic1, bic2, nls1, nls2)
  
}

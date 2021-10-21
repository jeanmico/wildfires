tmp = births
expvars = c('binary_exposed')

for (i in 2:15) {
  mdf = births_trn %>% dplyr::select(c(all_of(fullcov[-i])), expvars[[1]])
  
  tmpmdl = glm(outcome ~ . , data = mdf, family = 'poisson')
  
  print(i)
  a = tidy(tmpmdl)
  print(a[a$term == "binary_exposedTRUE",])
  }

a = tidy(tmpmdl)
a
a <- a %>% filter(term == "binary_exposedTRUE")
a
fullcov

mdl = glm(outcome ~ binary_exposed + race + ipi + edu + insurance + pn_care + wic + mom_age + 
            mental_disorder  + nulliparous + drug_abuse + alcohol_abuse,
          data = tmp, family = 'poisson')
a = tidy(mdl)
print(a[a$term == "binary_exposedTRUE",])
 
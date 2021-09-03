

# Re-analysis Waldron et al.
# Author: DN Borg
# Date: April 2021

# Load data and filter out data from Shvartz
d = read_xlsx("data-jb-db-extraction.xlsx") %>%
  filter(!study_id %in% c("Shvartz (1977)2"))

# Number of unique studies
length(unique(d$study_id))

# Data sets for models
d1 = d %>% filter(!study_id == "Chen (2013)")
d2 = d %>% filter(!study_id == "Gore (1997)1")
d3 = d %>% filter(!study_id == "Gore (1997)2")
d4 = d %>% filter(!study_id == "Karlsen (2015)")
d5 = d %>% filter(!study_id == "Keiser (2015)")
d6 = d %>% filter(!study_id == "Lorenzo (2010)")
d7 = d %>% filter(!study_id == "Mikkelsen (2019)")
d8 = d %>% filter(!study_id == "Rivas (2017)")
d9 = d %>% filter(!study_id == "Ronnestad (2020)")
d10 = d %>% filter(!study_id == "Takeno (2001)")
d11 = d %>% filter(!study_id == "Van de Velde (2016)")
d12 = d %>% filter(!study_id == "Waldron (2019)-post4")
d13 = d %>% filter(!study_id == "Willmott (2018)1")
d14 = d %>% filter(!study_id == "Willmott (2018)2")

# Run meta-analysis for each dataset
data_list <- list(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14)

results_list <- list()

for(i in 1:length(data_list)){
  
  # fit new model
  new_model <- metacont(data = data_list[[i]], n.c = n_con, mean.c = mu_con, sd.c = sd_con, n.e = n_exp, mean.e = mu_exp, sd.e = sd_exp, studlab = paste(study_id), comb.fixed = F, comb.random = T, prediction = T, sm = "SMD", method.smd = "Hedges", method.tau = "SJ")
  # save results to list
  results_list <- c(results_list, list(new_model))
}

# Pull results
d = results_list[[1]]; omit_chen = cbind("omit_chen", d$TE.random, d$lower.random, d$upper.random, d$lower.predict, d$upper.predict, d$I2, d$lower.I2, d$upper.I2, d$tau2, d$lower.tau2, d$upper.tau2)
d = results_list[[2]]; omit_gore1 = cbind("omit_gore1", d$TE.random, d$lower.random, d$upper.random, d$lower.predict, d$upper.predict, d$I2, d$lower.I2, d$upper.I2, d$tau2, d$lower.tau2, d$upper.tau2)
d = results_list[[3]]; omit_gore2 = cbind("omit_gore2", d$TE.random, d$lower.random, d$upper.random, d$lower.predict, d$upper.predict, d$I2, d$lower.I2, d$upper.I2, d$tau2, d$lower.tau2, d$upper.tau2)
d = results_list[[4]]; omit_karlsen = cbind("omit_karlsen", d$TE.random, d$lower.random, d$upper.random, d$lower.predict, d$upper.predict, d$I2, d$lower.I2, d$upper.I2, d$tau2, d$lower.tau2, d$upper.tau2)
d = results_list[[5]]; omit_keiser = cbind("omit_keiser", d$TE.random, d$lower.random, d$upper.random, d$lower.predict, d$upper.predict, d$I2, d$lower.I2, d$upper.I2, d$tau2, d$lower.tau2, d$upper.tau2)
d = results_list[[6]]; omit_lorenzo = cbind("omit_lorenzo", d$TE.random, d$lower.random, d$upper.random, d$lower.predict, d$upper.predict, d$I2, d$lower.I2, d$upper.I2, d$tau2, d$lower.tau2, d$upper.tau2)
d = results_list[[7]]; omit_mikkelsen = cbind("omit_mikkelsen", d$TE.random, d$lower.random, d$upper.random, d$lower.predict, d$upper.predict, d$I2, d$lower.I2, d$upper.I2, d$tau2, d$lower.tau2, d$upper.tau2)
d = results_list[[8]]; omit_rivas = cbind("omit_rivas", d$TE.random, d$lower.random, d$upper.random, d$lower.predict, d$upper.predict, d$I2, d$lower.I2, d$upper.I2, d$tau2, d$lower.tau2, d$upper.tau2)
d = results_list[[9]]; omit_ronnestad = cbind("omit_ronnestad", d$TE.random, d$lower.random, d$upper.random, d$lower.predict, d$upper.predict, d$I2, d$lower.I2, d$upper.I2, d$tau2, d$lower.tau2, d$upper.tau2)
d = results_list[[10]]; omit_takeno = cbind("omit_takeno", d$TE.random, d$lower.random, d$upper.random, d$lower.predict, d$upper.predict, d$I2, d$lower.I2, d$upper.I2, d$tau2, d$lower.tau2, d$upper.tau2)
d = results_list[[11]]; omit_van = cbind("omit_van", d$TE.random, d$lower.random, d$upper.random, d$lower.predict, d$upper.predict, d$I2, d$lower.I2, d$upper.I2, d$tau2, d$lower.tau2, d$upper.tau2)
d = results_list[[12]]; omit_waldron = cbind("omit_waldron", d$TE.random, d$lower.random, d$upper.random, d$lower.predict, d$upper.predict, d$I2, d$lower.I2, d$upper.I2, d$tau2, d$lower.tau2, d$upper.tau2)
d = results_list[[13]]; omit_willmott1 = cbind("omit_willmott1", d$TE.random, d$lower.random, d$upper.random, d$lower.predict, d$upper.predict, d$I2, d$lower.I2, d$upper.I2, d$tau2, d$lower.tau2, d$upper.tau2)
d = results_list[[14]]; omit_willmott2 = cbind("omit_willmott2", d$TE.random, d$lower.random, d$upper.random, d$lower.predict, d$upper.predict, d$I2, d$lower.I2, d$upper.I2, d$tau2, d$lower.tau2, d$upper.tau2)

sum = rbind(omit_chen,
            omit_gore1,
            omit_gore2,
            omit_karlsen,
            omit_keiser,
            omit_lorenzo,
            omit_mikkelsen,
            omit_rivas,
            omit_ronnestad,
            omit_takeno,
            omit_van,
            omit_waldron,
            omit_willmott1,
            omit_willmott2) %>%
  as.data.frame() %>%
  clean_names() %>%
  mutate(
    study = as.factor(v1),
    g = v2,
    lower_g = v3,
    upper_g = v4,
    pi_lower = v5,
    pi_upper = v6,
    i2 = v7,
    i2_lower = v8,
    i2_upper = v9,
    tau2 = v10,
    tau2_lower = v11,
    tau2_upper = v12
  ) %>%
  select(-v1:-v12) %>%
  mutate_if(is.character,as.numeric) %>%
  #mutate(across(where(is.numeric), round, 2)) %>%
  mutate_at(2:9, round, 2) %>%
  mutate_at(10:12, round, 3) %>%
  arrange(g)

write.csv(sum, file = "leave-one-out-results.csv", row.names = F)









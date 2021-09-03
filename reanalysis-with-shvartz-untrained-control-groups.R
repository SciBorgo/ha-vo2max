
#### Waldron et al.
# Meta-analysis
# Author: DN Borg
# Date: August 2021

# Load data
d_therm = read_xlsx("data-jb-db-extraction.xlsx")

# Check data
vis_miss(d_therm)

# Making sure only unique studies
d_therm %>% group_by(study_id) %>%
  summarise(count = n())

# Number of unique studies
length(unique(d_therm$study_id))

# Look at names
sort(names(d_therm_drop))

# Meta analysis
# Details: https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/random.html
meta_fit <- metacont(n.c = n_con,
                     mean.c = mu_con,
                     sd.c = sd_con,
                     n.e = n_exp,
                     mean.e = mu_exp,
                     sd.e = sd_exp, 
                     data = d_therm_drop,
                     studlab = paste(study_id),
                     comb.fixed = F,
                     comb.random = T,
                     prediction = T,
                     sm = "SMD",
                     method.smd = "Hedges",
                     method.tau = "SJ")
meta_fit
summary(meta_fit)

png(file = 'vo2max-perf-thermo-with-shvartz.png', width = 10, height = 5, res = 600, units = "in") 
forest(meta_fit, 
       sortvar = TE,
       xlim = c(-2,2),
       xlab ="Favours Con         SMD         Favours HA",
       rightlabs = c("g","95% CI","Weight"),
       leftlabs = c("Author (year)", "N","Mean","SD","N","Mean","SD"),
       lab.e = "HA",
       lab.c = "Control",
       pooled.totals = T,
       smlab = "",
       text.random = "Overall effect",
       print.tau2 = T,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict = "black",
       print.I2.ci = T,
       digits.sd = 1,
       digits.mean = 1,
       mlab = "",
       ilab.xpos = 8,
       ilab.pos = 8,
       showweights = T)
dev.off()



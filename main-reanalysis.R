
# Re-analysis Waldron et al.
# Author: DN Borg
# Date: April 2021

# Load data
d_therm = read_xlsx("data-jb-db-extraction.xlsx")

# Check data
vis_miss(d_therm)

# Making sure only unique studies
d_therm %>% group_by(study_id) %>% summarise(count = n())

# Number of unique studies
length(unique(d_therm$study_id))

# Drop studies
d_therm_drop = d_therm %>% filter(!study_id == "Shvartz (1977)2")

# Look at names
sort(names(d_therm_drop))

# Meta analysis
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
summary(meta_fit)

png(file = 'figure-1.png', width = 10, height = 5, res = 600, units = "in") 
forest(meta_fit, 
       sortvar = TE,
       xlim = c(-2,2),
       xlab ="Favours Control   SMD         Favours HA",
       rightlabs = c("g","95% CI","Weight"),
       leftlabs = c("Author (year)", "N","Mean","SD","N","Mean","SD"),
       lab.e = "HA",
       lab.c = "Control",
       pooled.totals = T,
       smlab = "",
       text.random = "Overall effect",
       print.tau2 = T,
       print.tau2.ci = T,
       col.diamond = "gray",
       col.diamond.lines = "black",
       col.predict = "black",
       print.I2 = F,
       print.I2.ci = F,
       digits.tau2 = 3,
       digits.sd = 1,
       digits.mean = 1,
       mlab = "",
       ilab.xpos = 8,
       ilab.pos = 8,
       showweights = T)
dev.off()



# Data extraction for non-parametric prediction interval
names = meta_fit$studlab
te = meta_fit$TE
se = meta_fit$seTE

cbind(names, te, se) %>%
        write.csv(file = "data-for-nonparametric-prediction-interval-calculation.csv",
                  row.names = F)






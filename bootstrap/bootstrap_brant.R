# 2 methods for bootstrapping the correction factor
#
# 1. "by hand"
# 2. R package boot 
#




# 1. by hand bootstrap sampling for correction factor variance

for (i in 1:1000){
  
# Construct data frame of sample sizes of validated photos per replicate
  
  cf.n = brant_merge %>%
    filter(!(is.na(manual_density))) %>%
    group_by(rep) %>%
    summarize(n=n()) %>%
    ungroup()

# Get a new sample of size cf.n[rep] of validated counts
  
  cf.sample = brant_merge %>%
    filter(!(is.na(manual_density))) %>%
    group_by(rep) %>%
    nest() %>%
    ungroup() %>%
    mutate(n=cf.n$n) %>%
    mutate(samp=map2(data, n, ~sample_n(.x,size=.y, replace=TRUE))) %>%
    select(-data) %>%
    unnest(samp)
  
# Compute new cf by rep
  
  cf = cf.sample %>%
    group_by(rep) %>%
    summarize(CF = sum(manual_count)/sum(yolo_count)) %>%
    ungroup()
  
  
  
  
  if(i == 1){
    bootstrap = cf
  }
  
  if(i > 1){
    bootstrap = rbind(bootstrap, cf)
    
  }
  
  
}

boot_cf = bootstrap %>%
  group_by(rep) %>%
  summarize(mCF = mean(CF),
            CF.var = var(CF),
            year = 2022)

est22 = read.csv("est22.csv", header=TRUE, stringsAsFactors = FALSE)

# correct the 2022 estimate and propagate errors
corrected = left_join(est22, boot_cf) %>%
  mutate(corrected = est * mCF, 
         corrected.se = sqrt(CF.var*var + (mCF^2)*var + (est^2)*CF.var),
         year=2022)


# 2. boot package
library(boot)

# define reps
reps=unique(brant_merge$rep)

# define statistic process function
corrfactor = function(sample, ind){
  cf = sum(sample$manual_count[ind])/sum(sample$yolo_count[ind])
  return(cf)
}

# iterate through

boot_list = list()

for(i in 1:length(reps)){
  
  cf.sample = brant_merge %>%
    filter(!(is.na(manual_density))) %>%
    filter(rep==reps[i]) 
  
  boot = boot(cf.sample, corrfactor, R=1000)
  boot_list[[i]] = boot

  }

boot_list[[2]]


# Visualize
library(ggplot2)

for(i in 1:5){
if(i == 1){
  boot_results <- as.data.frame(boot_list[[i]]$t)
  boot_results$rep=1
  boot_results$mode = "boot"
}
if(i != 1){
  temp=as.data.frame(boot_list[[i]]$t)
  temp$rep=i
  temp$mode="boot"
  boot_results <- rbind(boot_results, temp)
}
}

colnames(boot_results)[1]="CF"

bootstrap$mode="hand"

boot_results = rbind(boot_results, bootstrap)

ggplot() + geom_density(data=boot_results,aes(x=CF, color=mode), linewidth=.5, alpha=.1) +
  facet_wrap(~rep)


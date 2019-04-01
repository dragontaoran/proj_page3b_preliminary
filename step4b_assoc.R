rm(list=ls())
gc()

#### input files ###################################################################
### covariate
fn.cov = "/fs0/straubp/iMega/final_qced_set/12182018_MEGA.GRID.RACE.GEN.batch.PCs.covariates.txt"

### phenotype
fn.hdl = "/data/coxvgi/dennisj/shared/rtao/lipids_2019-03-27/HDL-C/20190318_HDL-C_Medians_and_Age_at_Median.txt"
fn.ldl = "/data/coxvgi/dennisj/shared/rtao/lipids_2019-03-27/LDL-C/20190318_LDL-C_Medians_and_Age_at_Median.txt"
fn.tg = "/data/coxvgi/dennisj/shared/rtao/lipids_2019-03-27/Trigs/20190318_Trigs_Medians_and_Age_at_Median.txt"

### prs
fn.prs.hdl.ea = "/data/taor2/page3b_preliminary_20190328/step3_EU_HDL/plink.profile"
fn.prs.hdl.aa = "/data/taor2/page3b_preliminary_20190328/step3_AA_HDL/plink.profile"

fn.prs.ldl.ea = "/data/taor2/page3b_preliminary_20190328/step3_EU_LDL/plink.profile"
fn.prs.ldl.aa = "/data/taor2/page3b_preliminary_20190328/step3_AA_LDL/plink.profile"

fn.prs.tg.ea = "/data/taor2/page3b_preliminary_20190328/step3_EU_TG/plink.profile"
fn.prs.tg.aa = "/data/taor2/page3b_preliminary_20190328/step3_AA_TG/plink.profile"

### output file
fn.out = "step4b_summary.tab"
#### input files ###################################################################



#### covariate data ################################################################
fi.cov = read.table(fn.cov, header=TRUE, as.is=TRUE)
fi.cov = fi.cov[which(fi.cov$GENDER %in% c("F", "M") & fi.cov$RACE %in% c("B", "W")),]
#### covariate data ################################################################



#### HDL ###########################################################################
fi.hdl = read.table(fn.hdl, header=TRUE, as.is=TRUE)
dat.hdl = merge(fi.hdl, fi.cov, by.x="grid", by.y="GRID")

### EA
fi.prs.hdl.ea = read.table(fn.prs.hdl.ea, header=TRUE, as.is=TRUE)
dat.hdl.ea = merge(dat.hdl, fi.prs.hdl.ea, by.x="grid", by.y="IID")
dat.hdl.ea$Yr = NA
resiY = residuals(lm(indiv_median~median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=dat.hdl.ea))
dat.hdl.ea$Yr[as.numeric(names(resiY))] = resiY
dat.hdl.ea$Pr = NA
resiP = residuals(lm(SCORESUM~median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=dat.hdl.ea)) 
dat.hdl.ea$Pr[as.numeric(names(resiP))] = resiP
cor_Yr_Pr_hdl_ea = cor(dat.hdl.ea$Yr, dat.hdl.ea$Pr)
N_hdl_ea = nrow(dat.hdl.ea)
prs_mean_hdl_ea = mean(dat.hdl.ea$SCORESUM)
prs_sd_hdl_ea = sd(dat.hdl.ea$SCORESUM)

### AA
fi.prs.hdl.aa = read.table(fn.prs.hdl.aa, header=TRUE, as.is=TRUE)
dat.hdl.aa = merge(dat.hdl, fi.prs.hdl.aa, by.x="grid", by.y="IID")
dat.hdl.aa$Yr = NA
resiY = residuals(lm(indiv_median~median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=dat.hdl.aa))
dat.hdl.aa$Yr[as.numeric(names(resiY))] = resiY
dat.hdl.aa$Pr = NA
resiP = residuals(lm(SCORESUM~median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=dat.hdl.aa)) 
dat.hdl.aa$Pr[as.numeric(names(resiP))] = resiP
cor_Yr_Pr_hdl_aa = cor(dat.hdl.aa$Yr, dat.hdl.aa$Pr)
N_hdl_aa = nrow(dat.hdl.aa)
prs_mean_hdl_aa = mean(dat.hdl.aa$SCORESUM)
prs_sd_hdl_aa = sd(dat.hdl.aa$SCORESUM)

### All
dat.hdl.all = rbind(dat.hdl.ea, dat.hdl.aa)
dat.hdl.all$Yr = NA
resiY = residuals(lm(indiv_median~median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+RACE, data=dat.hdl.all))
dat.hdl.all$Yr[as.numeric(names(resiY))] = resiY
dat.hdl.all$Pr = NA
resiP = residuals(lm(SCORESUM~median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+RACE, data=dat.hdl.all)) 
dat.hdl.all$Pr[as.numeric(names(resiP))] = resiP
cor_Yr_Pr_hdl_all = cor(dat.hdl.all$Yr, dat.hdl.all$Pr)
N_hdl_all = nrow(dat.hdl.all)
prs_mean_hdl_all = mean(dat.hdl.all$SCORESUM)
prs_sd_hdl_all = sd(dat.hdl.all$SCORESUM)
#### HDL ###########################################################################



#### LDL ###########################################################################
fi.ldl = read.table(fn.ldl, header=TRUE, as.is=TRUE)
dat.ldl = merge(fi.ldl, fi.cov, by.x="grid", by.y="GRID")

### EA
fi.prs.ldl.ea = read.table(fn.prs.ldl.ea, header=TRUE, as.is=TRUE)
dat.ldl.ea = merge(dat.ldl, fi.prs.ldl.ea, by.x="grid", by.y="IID")
dat.ldl.ea$Yr = NA
resiY = residuals(lm(indiv_median~median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=dat.ldl.ea))
dat.ldl.ea$Yr[as.numeric(names(resiY))] = resiY
dat.ldl.ea$Pr = NA
resiP = residuals(lm(SCORESUM~median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=dat.ldl.ea)) 
dat.ldl.ea$Pr[as.numeric(names(resiP))] = resiP
cor_Yr_Pr_ldl_ea = cor(dat.ldl.ea$Yr, dat.ldl.ea$Pr)
N_ldl_ea = nrow(dat.ldl.ea)
prs_mean_ldl_ea = mean(dat.ldl.ea$SCORESUM)
prs_sd_ldl_ea = sd(dat.ldl.ea$SCORESUM)

### AA
fi.prs.ldl.aa = read.table(fn.prs.ldl.aa, header=TRUE, as.is=TRUE)
dat.ldl.aa = merge(dat.ldl, fi.prs.ldl.aa, by.x="grid", by.y="IID")
dat.ldl.aa$Yr = NA
resiY = residuals(lm(indiv_median~median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=dat.ldl.aa))
dat.ldl.aa$Yr[as.numeric(names(resiY))] = resiY
dat.ldl.aa$Pr = NA
resiP = residuals(lm(SCORESUM~median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=dat.ldl.aa)) 
dat.ldl.aa$Pr[as.numeric(names(resiP))] = resiP
cor_Yr_Pr_ldl_aa = cor(dat.ldl.aa$Yr, dat.ldl.aa$Pr)
N_ldl_aa = nrow(dat.ldl.aa)
prs_mean_ldl_aa = mean(dat.ldl.aa$SCORESUM)
prs_sd_ldl_aa = sd(dat.ldl.aa$SCORESUM)

### All
dat.ldl.all = rbind(dat.ldl.ea, dat.ldl.aa)
dat.ldl.all$Yr = NA
resiY = residuals(lm(indiv_median~median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+RACE, data=dat.ldl.all))
dat.ldl.all$Yr[as.numeric(names(resiY))] = resiY
dat.ldl.all$Pr = NA
resiP = residuals(lm(SCORESUM~median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+RACE, data=dat.ldl.all)) 
dat.ldl.all$Pr[as.numeric(names(resiP))] = resiP
cor_Yr_Pr_ldl_all = cor(dat.ldl.all$Yr, dat.ldl.all$Pr)
N_ldl_all = nrow(dat.ldl.all)
prs_mean_ldl_all = mean(dat.ldl.all$SCORESUM)
prs_sd_ldl_all = sd(dat.ldl.all$SCORESUM)
#### LDL ###########################################################################



#### TG ############################################################################
fi.tg = read.table(fn.tg, header=TRUE, as.is=TRUE)
dat.tg = merge(fi.tg, fi.cov, by.x="grid", by.y="GRID")

### EA
fi.prs.tg.ea = read.table(fn.prs.tg.ea, header=TRUE, as.is=TRUE)
dat.tg.ea = merge(dat.tg, fi.prs.tg.ea, by.x="grid", by.y="IID")
dat.tg.ea$Yr = NA
resiY = residuals(lm(indiv_median~median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=dat.tg.ea))
dat.tg.ea$Yr[as.numeric(names(resiY))] = resiY
dat.tg.ea$Pr = NA
resiP = residuals(lm(SCORESUM~median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=dat.tg.ea)) 
dat.tg.ea$Pr[as.numeric(names(resiP))] = resiP
cor_Yr_Pr_tg_ea = cor(dat.tg.ea$Yr, dat.tg.ea$Pr)
N_tg_ea = nrow(dat.tg.ea)
prs_mean_tg_ea = mean(dat.tg.ea$SCORESUM)
prs_sd_tg_ea = sd(dat.tg.ea$SCORESUM)

### AA
fi.prs.tg.aa = read.table(fn.prs.tg.aa, header=TRUE, as.is=TRUE)
dat.tg.aa = merge(dat.tg, fi.prs.tg.aa, by.x="grid", by.y="IID")
dat.tg.aa$Yr = NA
resiY = residuals(lm(indiv_median~median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=dat.tg.aa))
dat.tg.aa$Yr[as.numeric(names(resiY))] = resiY
dat.tg.aa$Pr = NA
resiP = residuals(lm(SCORESUM~median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=dat.tg.aa)) 
dat.tg.aa$Pr[as.numeric(names(resiP))] = resiP
cor_Yr_Pr_tg_aa = cor(dat.tg.aa$Yr, dat.tg.aa$Pr)
N_tg_aa = nrow(dat.tg.aa)
prs_mean_tg_aa = mean(dat.tg.aa$SCORESUM)
prs_sd_tg_aa = sd(dat.tg.aa$SCORESUM)

### All
dat.tg.all = rbind(dat.tg.ea, dat.tg.aa)
dat.tg.all$Yr = NA
resiY = residuals(lm(indiv_median~median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+RACE, data=dat.tg.all))
dat.tg.all$Yr[as.numeric(names(resiY))] = resiY
dat.tg.all$Pr = NA
resiP = residuals(lm(SCORESUM~median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+RACE, data=dat.tg.all)) 
dat.tg.all$Pr[as.numeric(names(resiP))] = resiP
cor_Yr_Pr_tg_all = cor(dat.tg.all$Yr, dat.tg.all$Pr)
N_tg_all = nrow(dat.tg.all)
prs_mean_tg_all = mean(dat.tg.all$SCORESUM)
prs_sd_tg_all = sd(dat.tg.all$SCORESUM)
#### TG ############################################################################



#### output ########################################################################
trait = rep(c("HDL", "LDL", "TG"), each=3)
race = rep(c("EA", "AA", "All"), 3)
cor_Yr_Pr = c(cor_Yr_Pr_hdl_ea, cor_Yr_Pr_hdl_aa, cor_Yr_Pr_hdl_all,
			  cor_Yr_Pr_ldl_ea, cor_Yr_Pr_ldl_aa, cor_Yr_Pr_ldl_all,
			  cor_Yr_Pr_tg_ea, cor_Yr_Pr_tg_aa, cor_Yr_Pr_tg_all)
N = c(N_hdl_ea, N_hdl_aa, N_hdl_all,
	  N_ldl_ea, N_ldl_aa, N_ldl_all,
	  N_tg_ea, N_tg_aa, N_tg_all)
prs_mean = c(prs_mean_hdl_ea, prs_mean_hdl_aa, prs_mean_hdl_all,
	  prs_mean_ldl_ea, prs_mean_ldl_aa, prs_mean_ldl_all,
	  prs_mean_tg_ea, prs_mean_tg_aa, prs_mean_tg_all)
prs_sd = c(prs_sd_hdl_ea, prs_sd_hdl_aa, prs_sd_hdl_all,
	  prs_sd_ldl_ea, prs_sd_ldl_aa, prs_sd_ldl_all,
	  prs_sd_tg_ea, prs_sd_tg_aa, prs_sd_tg_all)
res = data.frame(trait, race, cor_Yr_Pr, N, prs_mean, prs_sd)
write.table(res, file=fn.out, sep="\t", row.names=FALSE, quote=FALSE)
#### output ########################################################################

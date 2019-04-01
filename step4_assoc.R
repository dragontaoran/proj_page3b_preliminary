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
fn.out = "step4_summary.tab"
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
res.hdl.ea = summary(lm(indiv_median~SCORESUM+median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=dat.hdl.ea))

### AA
fi.prs.hdl.aa = read.table(fn.prs.hdl.aa, header=TRUE, as.is=TRUE)
dat.hdl.aa = merge(dat.hdl, fi.prs.hdl.aa, by.x="grid", by.y="IID")
res.hdl.aa = summary(lm(indiv_median~SCORESUM+median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=dat.hdl.aa))
#### HDL ###########################################################################



#### LDL ###########################################################################
fi.ldl = read.table(fn.ldl, header=TRUE, as.is=TRUE)
dat.ldl = merge(fi.ldl, fi.cov, by.x="grid", by.y="GRID")

### EA
fi.prs.ldl.ea = read.table(fn.prs.ldl.ea, header=TRUE, as.is=TRUE)
dat.ldl.ea = merge(dat.ldl, fi.prs.ldl.ea, by.x="grid", by.y="IID")
res.ldl.ea = summary(lm(indiv_median~SCORESUM+median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=dat.ldl.ea))

### AA
fi.prs.ldl.aa = read.table(fn.prs.ldl.aa, header=TRUE, as.is=TRUE)
dat.ldl.aa = merge(dat.ldl, fi.prs.ldl.aa, by.x="grid", by.y="IID")
res.ldl.aa = summary(lm(indiv_median~SCORESUM+median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=dat.ldl.aa))
#### LDL ###########################################################################



#### TG ############################################################################
fi.tg = read.table(fn.tg, header=TRUE, as.is=TRUE)
dat.tg = merge(fi.tg, fi.cov, by.x="grid", by.y="GRID")

### EA
fi.prs.tg.ea = read.table(fn.prs.tg.ea, header=TRUE, as.is=TRUE)
dat.tg.ea = merge(dat.tg, fi.prs.tg.ea, by.x="grid", by.y="IID")
res.tg.ea = summary(lm(indiv_median~SCORESUM+median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=dat.tg.ea))

### AA
fi.prs.tg.aa = read.table(fn.prs.tg.aa, header=TRUE, as.is=TRUE)
dat.tg.aa = merge(dat.tg, fi.prs.tg.aa, by.x="grid", by.y="IID")
res.tg.aa = summary(lm(indiv_median~SCORESUM+median_age+GENDER+batch+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10, data=dat.tg.aa))
#### TG ############################################################################



#### output ########################################################################
trait = rep(c("HDL", "LDL", "TG"), each=2)
race = rep(c("EA", "AA"), 3)
effect = c(res.hdl.ea$coef[2,1],
           res.hdl.aa$coef[2,1],
           res.ldl.ea$coef[2,1],
           res.ldl.aa$coef[2,1],
           res.tg.ea$coef[2,1],
           res.tg.aa$coef[2,1])
se = c(res.hdl.ea$coef[2,2],
       res.hdl.aa$coef[2,2],
       res.ldl.ea$coef[2,2],
       res.ldl.aa$coef[2,2],
       res.tg.ea$coef[2,2],
       res.tg.aa$coef[2,2])
pvalue = c(res.hdl.ea$coef[2,4],
           res.hdl.aa$coef[2,4],
           res.ldl.ea$coef[2,4],
           res.ldl.aa$coef[2,4],
           res.tg.ea$coef[2,4],
           res.tg.aa$coef[2,4])
n = c(nrow(dat.hdl.ea),
      nrow(dat.hdl.aa),
      nrow(dat.ldl.ea),
      nrow(dat.ldl.aa),
      nrow(dat.tg.ea),
      nrow(dat.tg.aa))
model_r2 = c(res.hdl.ea$r.squared,
             res.hdl.aa$r.squared,
             res.ldl.ea$r.squared,
             res.ldl.aa$r.squared,
             res.tg.ea$r.squared,
             res.tg.aa$r.squared)
corr_prs = c(cor(dat.hdl.ea$indiv_median, dat.hdl.ea$SCORESUM),
             cor(dat.hdl.aa$indiv_median, dat.hdl.aa$SCORESUM),
             cor(dat.ldl.ea$indiv_median, dat.ldl.ea$SCORESUM),
             cor(dat.ldl.aa$indiv_median, dat.ldl.aa$SCORESUM),
             cor(dat.tg.ea$indiv_median, dat.tg.ea$SCORESUM),
             cor(dat.tg.aa$indiv_median, dat.tg.aa$SCORESUM))
res = data.frame(trait, race, effect, se, pvalue, n, model_r2, corr_prs)
write.table(res, file=fn.out, sep="\t", row.names=FALSE, quote=FALSE)
#### output ########################################################################

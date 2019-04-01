1. Download SNP list SNP_list_10traits_MinusMissingInPAGE.txt from Bien, Stephanie A (sbien@fredhutch.org)'s email on 3/27/2019.  
2. Create trait-specific SNP list in plink format.  
3. Create individual-level prs scores.  
4. Run PRS association analysis.  
4b. Redo step 4 according to Charles' email on 3/31/2019.  
	4b1.	for each phenotype there are some adjustment covariates (like age,  and maybe some others, I think that Stephanie defined them).  
	4b2.	let Y be the true phenotype and P be the PRS. Regress Y ~ covariates, and let Yr be the residuals; regress P ~ covariates and let Pr be the residuals.  
	4b3.	then compute cor(Yr,Pr)  
	4b4.	do this separate for each minority, for whites, for all minorities combined, and for all samples combined.  
	4b5.	also give me N for each analysis, and the mean score and the SD of the scores (before the regression) for each of these strata.  
5. Copy the .nopred files generated in Step 3 to the parent folder  

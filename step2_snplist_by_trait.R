fn = "SNP_list_10traits_MinusMissingInPAGE.txt"
traits = c("HDL", "LDL", "TG")

fi = read.table(fn, header=TRUE, as.is=TRUE, sep="\t")
dim(fi)
head(fi)

for (trait in traits) {
	fn_out = paste0("step2_", trait, ".txt")
	res = fi[which(fi$trait == trait), c("SNP", "effect_allele", "beta")]
	dim(res)
	write.table(res, file=fn_out, sep="\t", row.names=FALSE, col.names=FALSE, quote=FALSE)
}


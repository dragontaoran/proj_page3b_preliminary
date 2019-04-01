#!/bin/bash

traits=("HDL" "LDL" "TG")
races=("AA" "EU")
wd="/data/taor2/page3b_preliminary_20190328"

for trait in ${traits[@]};
do
	for race in ${races[@]};
	do
		subdir="${wd}/step3_${race}_${trait}"
		mkdir -p ${subdir}
		cd ${subdir}
		plink --bfile /fs0/straubp/iMega/final_qced_set/12182018_biallelic_mega_batches1-15.chr1-22.grid.${race} --score ${wd}/step2_${trait}.txt sum --allow-no-sex
	done
done

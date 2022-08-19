* Analyze the merged prescriber-level data, and merge it with PAC ID and data on group practices

clear all
set more off
cd "C:\Users\khuon\Dropbox (Harvard University)\Physician Altruism\Altruism\"
*cd "/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Code"

program main
	partd_partb_regression
end


********************************************************************************************************
** Do regressions of Part D + Part B doc behavior on various determinants
****************************************************************************
program partd_partb_regression
	local doc_chars c.ehr_adopt_post12 c.ehr_adopt_pre12 c.urban i.male i.top20_medschool c.exp##c.exp bene_avg_risk_scre bene_avg_age bene_pct_male bene_pct_nonwhite bene_pct_dual 
	local doc_chars_time_trend c.yearsince2014 c.yearsince2014#(c.ehr_adopt_post12 c.ehr_adopt_pre12 c.urban c.grpsize_lessthan50) 

	local grp_chars grp_academic c.grp_pct_ehr_adopt_post12 c.grp_pct_ehr_adopt_pre12
	local grp_patientmix grp_bene_avg_age grp_bene_pct_male grp_bene_pct_nonwhite grp_bene_pct_dual grp_bene_avg_risk_scre 
	local grpsize_vars c.grpsize_1 c.grpsize_2to15 c.grpsize_16to50 c.grpsize_51to200 c.grpsize_201to999  

	local pcsa_hh_interact_chars c.pcsa_high_hh_per100doc c.pcsa_high_hh_per100doc#(c.ehr_adopt i.top20_medschool c.grpsize_lessthan50)
	local pcsa_highoffice_interact_chars c.pcsa_high_office_top c.pcsa_high_office_top#(c.ehr_adopt c.urban c.grpsize_lessthan50) 
	local pcsa_opioid_interact_chars c.pcsa_opioid_prscrb_rate c.pcsa_opioid_prscrb_rate #(c.ehr_adopt c.urban c.grpsize_lessthan50)

	local date 63022

	***********************************
	** Part B analysis
	**********************************

	*************************************
	***** NPI-group-year level analysis
	************************************
	local em_cat office hosp_obs hosp_inpat emerg nurs_facil domic home
	use  "data\PartB\ProviderandService\PartBProvider_AllServiceCounts20132019.dta", clear
	drop if merge_emcode_counts == 1 
	drop if merge_dac == 1
	drop if merge_pcsa == 1

	egen pcsaID = group(pcsa)
	egen groupID = group(grouppracticepacid)
	egen npiID = group(npi) 
	egen yearID = group(year)
	egen specialtyID = group(specialty)

	foreach cat of local em_cat{
		gen pct_`cat'_top1 = `cat'_top1 / `cat'
		gen pct_`cat'_top2 = `cat'_top2 / `cat'
		gen pct_`cat'_top3 = `cat'_top3 / `cat'
		capture gen pct_`cat'_top4 = `cat'_top4 / `cat'
		capture gen pct_`cat'_top5 = `cat'_top5 / `cat'
	}
	gen pct_office_top = pct_office_top1 + pct_office_top2

	* Create PCSA home health variables
	gen pcsa_hh_cert_per100doc = pcsa_tot_hh_cert/ pcsa_tot_partbproviders * 100
	gen pcsa_hh_sprvsn_per100doc = pcsa_tot_hh_sprvsn/ pcsa_tot_partbproviders * 100
	gen pcsa_hh_per100doc = pcsa_hh_cert_per100doc + pcsa_hh_sprvsn_per100doc
	bys year: egen pcsa_med_hh_per100doc = median(pcsa_hh_per100doc)
	gen pcsa_high_hh_per100doc = (pcsa_hh_per100doc > pcsa_med_hh_per100doc)
	* Create pcsa office variables
	gen pcsa_pct_office_top = pcsa_tot_office_top / pcsa_tot_office
	bys year: egen pcsa_med_pct_office_top = median(pcsa_pct_office_top)
	gen pcsa_high_office_top = (pcsa_pct_office_top >  pcsa_med_pct_office_top)
	
	* Create doc-level number of home health services per 100 patients
	gen hh_per100benes = hh/total_benes * 100
	gen hh_cert_per100benes = hh_cert/total_benes * 100
	gen hh_sprvsn_per100benes = hh_sprvsn/total_benes * 100

	* Crete PCSA opioid rate variable
	gen pcsa_opioid_prscrb_rate = pcsa_tot_opioid_clms / pcsa_tot_clms

	* Create patient mix variables
	gen bene_pct_male = bene_male_cnt / total_benes
	gen bene_pct_nonwhite = (total_benes - bene_race_wht_cnt) / total_benes
	gen bene_pct_dual = bene_dual_cnt / total_benes
	gen grp_bene_pct_male = grp_bene_male_cnt / grp_total_benes
	gen grp_bene_pct_nonwhite = (grp_total_benes - grp_bene_race_wht_cnt) / grp_total_benes
	gen grp_bene_pct_dual = grp_bene_dual_cnt / grp_total_benes

	drop if missing(bene_pct_male)
	drop if missing(bene_pct_nonwhite) 
	drop if missing(bene_pct_dual) | (bene_dual_cnt == 0 & bene_ndual_cnt == 0) 

	gen yearsince2014 = year-2014
	gen lgrp_size = log(grp_size)
	gen grpsize_lessthan50 = !(grpsize_51to200 |  grpsize_201to999 | grpsize_morethan1000)

	* Set group variable to a constant for solo practices
	foreach char of varlist grp*{
		if "`char'" != "grpsize_1" & "`char'" != "grp_size"{
			quietly replace `char' = 0 if grpsize_1 == 1
		}
	}

	****** Office regresion
	reghdfe pct_office_top i.year  `doc_chars'  `grpsize_vars' `grp_chars' if !grp_apm_flag [aw = office_benes], absorb(specialtyID pcsaID) vce(cluster npi)
	* Add mean of dependent variables, for scale
	quietly summ pct_office_top
	outreg2 using "\output\tables\partb_doc_year_reg_`date'.xls", replace ctitle(Office, Yr-FE, pcsa-FE) addstat(Mean, r(mean), SD, r(sd)) addtext() adjr2 

	* Includes time trends 
	reghdfe pct_office_top i.year  `doc_chars' `doc_chars_time_trend'  `grpsize_vars' `grp_chars' if !grp_apm_flag  [aw = office_benes], absorb(specialtyID pcsaID) vce(cluster npi)
	outreg2 using "\output\tables\partb_doc_year_reg_`date'.xls", append ctitle(Office, Time Trends, Yr-FE, pcsa-FE) addtext() adjr2 
	**** Plot marginal effects of experience
	quietly margins, at(exp=(0 5 10 15 20 25 30 35 40 45))
	marginsplot
	graph export "\output\figures\doc_reg_expeffect\expeffect_office_`date'.pdf", replace

	reghdfe pct_office_top i.year  `doc_chars' `doc_chars_time_trend'  `grpsize_vars' `grp_chars' `grp_patientmix' if !grp_apm_flag   [aw = office_benes] if !grp_apm_flag, absorb(specialtyID pcsaID) vce(cluster npi)
	outreg2 using "\output\tables\partb_doc_year_reg_`date'.xls", append ctitle(Office, Time Trends, Yr-FE, pcsa-FE) addtext() adjr2 

	reghdfe pct_office_top i.year  `doc_chars' `doc_chars_time_trend'  `grpsize_vars' `grp_chars'  `pcsa_hh_interact_chars' if !grp_apm_flag [aw = office_benes], absorb(specialtyID pcsaID) vce(cluster npi)
	outreg2 using "\output\tables\partb_doc_year_reg_`date'.xls", append ctitle(Office, Time Trends, Yr-FE, pcsa-FE, pcsa-HH-Interact) addtext() adjr2 

	reghdfe pct_office_top i.year  `doc_chars' `doc_chars_time_trend'  `grpsize_vars' `grp_chars' `pcsa_opioid_interact_chars' if !grp_apm_flag [aw = office_benes], absorb(specialtyID pcsaID) vce(cluster npi)
	outreg2 using "\output\tables\partb_doc_year_reg_`date'.xls", append ctitle(HH Cert, Yr-FE, pcsa-FE, pcsa-Opioid-Interact) addtext() adjr2 


	********* Home health regression
	reghdfe hh_cert_per100benes i.year  `doc_chars' `grpsize_vars' `grp_chars' if !grp_apm_flag [aw = office_benes], absorb(specialtyID pcsaID) vce(cluster npi)
	quietly summ hh_cert_per100benes
	outreg2 using "\output\tables\partb_doc_year_reg_`date'.xls", append ctitle(HH Cert, Yr-FE, pcsa-FE)  addstat(Mean, r(mean), SD, r(sd))  addtext() adjr2 

	reghdfe hh_cert_per100benes i.year  `doc_chars' `doc_chars_time_trend'  `grpsize_vars' `grp_chars' if !grp_apm_flag [aw = office_benes], absorb(specialtyID pcsaID) vce(cluster npi)
	outreg2 using "\output\tables\partb_doc_year_reg_`date'.xls", append ctitle(HH Cert, Yr-FE, pcsa-FE) addtext() adjr2 
	**** Plot marginal effects of experience
	quietly margins, at(exp=(0 5 10 15 20 25 30 35 40 45))
	marginsplot
	graph export "\output\figures\doc_reg_expeffect\expeffect_hhcert_`date'.pdf", replace

	reghdfe hh_cert_per100benes i.year  `doc_chars' `doc_chars_time_trend'  `grpsize_vars' `grp_chars' `grp_patientmix' if !grp_apm_flag [aw = office_benes], absorb(specialtyID pcsaID) vce(cluster npi)
	outreg2 using "\output\tables\partb_doc_year_reg_`date'.xls", append ctitle(HH Cert, Yr-FE, pcsa-FE) addtext() adjr2 

	reghdfe hh_cert_per100benes i.year  `doc_chars' `doc_chars_time_trend'  `grpsize_vars' `grp_chars' `pcsa_highoffice_interact_chars' if !grp_apm_flag [aw = office_benes], absorb(specialtyID pcsaID) vce(cluster npi)
	outreg2 using "\output\tables\partb_doc_year_reg_`date'.xls", append ctitle(HH Cert, Yr-FE, pcsa-FE, pcsa-Office-Interact) addtext() adjr2 

	reghdfe hh_cert_per100benes i.year  `doc_chars' `doc_chars_time_trend'  `grpsize_vars' `grp_chars' `pcsa_opioid_interact_chars' if !grp_apm_flag [aw = office_benes], absorb(specialtyID pcsaID) vce(cluster npi)
	outreg2 using "\output\tables\partb_doc_year_reg_`date'.xls", append ctitle(HH Cert, Yr-FE, pcsa-FE, pcsa-Opioid-Interact) addtext() adjr2 



	***************************
	** Group level MIPS analysis
	***************************
	* Get group-level beneficiary characteristics
	preserve
		duplicates drop grouppracticepacid year, force
		keep grouppracticepacid grp_bene* grp_total_benes year
		collapse (mean) grp_bene_avg* grp_bene_pct* (rawsum) grp_total_benes grp*cnt* [aweight=grp_total_benes], by(grouppracticepacid)
		tempfile grp_benes_chars
		save `grp_benes_chars'
	restore

	* Get group-level physician characteristics
	use "data\DAC_NationalDownloadableFiles\group_characteristics.dta", clear
	collapse (mean) grp_exp grp_pct_ehr* [aweight = grp_size], by(grouppracticepacid)
	tempfile grp_docs_chars
	save `grp_docs_chars'

	* Get other time invariant group characteristics
	use "data\DAC_NationalDownloadableFiles\group_characteristics.dta", clear
	gen grpsize_lessthan50 = !(grpsize_51to200 |  grpsize_201to999 | grpsize_morethan1000)
	collapse (mean) *2019 *2020 grp_tag* grp_full_mips_info grp_academic grp_size grpsize_* (max) grp_apm_flag (firstnm) organizationlegalname, by(grouppracticepacid)
	merge 1:1 grouppracticepacid using `grp_benes_chars'
	* Drop groups that don't appear in Part B data (34,549 matched groups; 92,057 did not appear in Part B data)
	drop if _merge == 1
	drop _merge
	merge 1:1 grouppracticepacid using `grp_docs_chars'
	drop _merge
	
	drop if missing(grouppracticepacid)
	drop if grp_size == 1

	* Group-level MIPS regressions
	local scores final_mips_score quality_category_score pi_category_score ia_category_score 
	foreach score of local scores{
		preserve
			reshape long grp_`score', i(grouppracticepacid) j(year)

			reg grp_`score' i.grp_apm_flag##i.year `grpsize_vars' `grp_chars' `grp_patientmix', vce(robust)
			quietly sum grp_`score'
			if "`score'" == "final_mips_score"{
				outreg2 using "\output\tables\GrpMIPSreg_`date'.xls", replace ctitle(PartB Grp `score') addstat(Mean, r(mean), SD, r(sd)) addtext()

				reg grp_`score' i.grp_apm_flag##i.year `grpsize_vars' `grp_chars' `grp_patientmix' if grp_full_mips_info == 1, vce(robust)
				quietly sum grp_`score' if grp_full_mips_info
				outreg2 using "\output\tables\GrpMIPSreg_`date'.xls", append ctitle(PartB Grp`score' Full Info) addstat(Mean, r(mean), SD, r(sd)) addtext()
			}
			else{
				outreg2 using "\output\tables\GrpMIPSreg_`date'.xls", append ctitle(PartB Grp `score') addstat(Mean, r(mean), SD, r(sd)) addtext()
			}
		restore
	}

	drop if missing(grp_final_mips_score2020) & missing(grp_final_mips_score2019)
	unique grouppracticepacid if grp_apm_flag == 0 
	unique grouppracticepacid if grp_apm_flag == 1 
	unique grouppracticepacid  if grp_final_mips_score2019 < 30 & grp_apm_flag == 0
	unique grouppracticepacid  if grp_final_mips_score2019 < 30 & grp_apm_flag == 1
	unique grouppracticepacid  if grp_final_mips_score2020 < 45 & grp_apm_flag == 0
	unique grouppracticepacid  if grp_final_mips_score2020 < 45 & grp_apm_flag == 1
	unique grouppracticepacid  if grp_final_mips_score2019 == 30 & grp_apm_flag == 0
	unique grouppracticepacid  if grp_final_mips_score2019 == 30 & grp_apm_flag == 1
	unique grouppracticepacid if grp_final_mips_score2020 == 45 & grp_apm_flag == 0
	unique grouppracticepacid  if grp_final_mips_score2020 == 45 & grp_apm_flag == 1
	
 	***********************************************************************************
	** PART D ANALYSIS
	***********************************************************************************

	************************************
	***** NPI-group-year level analysis
	************************************
	local drugs opioid benzo_all antbtc antpsyct
	use "data\PartD\ProviderandDrug\PartDProvider_AllDrugCtgrs20132019.dta", clear
	drop if merge_drug_categories == 1 
	drop if merge_dac == 1
	drop if merge_pcsa == 1

	* Create the outcome variable
	* For each drug category of interest, compute average claims
	foreach drug of local drugs{
		gen avg_`drug'_clms_per100ben = total_`drug'_claims/ total_benes * 100 
	}


	* Create patient mix variables
	gen bene_pct_male = bene_male_cnt / total_benes
	gen bene_pct_nonwhite = (total_benes - bene_race_wht_cnt) / total_benes
	gen bene_pct_dual = bene_dual_cnt / total_benes 
	gen grp_bene_pct_male = grp_bene_male_cnt / grp_total_benes
	gen grp_bene_pct_nonwhite = (grp_total_benes - grp_bene_race_wht_cnt) / grp_total_benes
	gen grp_bene_pct_dual = grp_bene_dual_cnt / grp_total_benes


	drop if missing(bene_pct_male)
	drop if missing(bene_pct_nonwhite) 
	drop if missing(bene_pct_dual) | (bene_dual_cnt == 0 & bene_ndual_cnt == 0) 

	egen npiID = group(npi) 
	egen pcsaID = group(pcsa)
	egen groupID = group(grouppracticepacid)
	egen yearID = group(year)
	egen specialtyID = group(specialty)

	gen yearsince2014 = year-2014
	gen lgrp_size = log(grp_size)
	gen grpsize_lessthan50 = !(grpsize_51to200 |  grpsize_201to999 | grpsize_morethan1000)

	* Set group variable to a constant for solo practices
	foreach char of varlist grp*{
		if "`char'" != "grpsize_1" & "`char'" != "grp_size"{
			replace `char' = 0 if grpsize_1 == 1
		}
	}

	****** Opioid regressions
	* No Time Trends
	reghdfe avg_opioid_clms_per100ben i.year  `doc_chars' `grpsize_vars'  `grp_chars'  if !grp_apm_flag [aw=total_opioid_benes], absorb(specialtyID pcsaID) vce(cluster npiID)
	quietly summ avg_opioid_clms_per100ben 
	outreg2 using "\output\tables\partd_doc_year_reg_`date'.xls", replace ctitle(Opioid, Yr-FE, pcsa-FE) addstat(Mean, r(mean), SD, r(sd)) addtext() adjr2

	* With time trends
	reghdfe avg_opioid_clms_per100ben i.year  `doc_chars' `doc_chars_time_trend' `grpsize_vars'  `grp_chars' if !grp_apm_flag  [aw=total_opioid_benes], absorb(specialtyID pcsaID) vce(cluster npiID)
	outreg2 using "\output\tables\partd_doc_year_reg_`date'.xls", append ctitle(Opioid, Time Trends, Yr-FE, pcsa-FE) addtext() adjr2
	**** Plot marginal effects of experience
	quietly margins, at(exp=(0 5 10 15 20 25 30 35 40 45))
	marginsplot
	graph export "\output\figures\doc_reg_expeffect\expeffect_opioidspartd_`date'.pdf", replace

	reghdfe avg_opioid_clms_per100ben i.year  `doc_chars' `doc_chars_time_trend' `grpsize_vars'  `grp_chars'  `grp_patientmix' if !grp_apm_flag [aw=total_opioid_benes], absorb(specialtyID pcsaID) vce(cluster npiID)
	outreg2 using "\output\tables\partd_doc_year_reg_`date'.xls", append ctitle(Opioid, Time Trends, Yr-FE, pcsa-FE) addtext() adjr2
	
	
	********** Benzo regressions
	reghdfe avg_benzo_all_clms_per100ben i.year  `doc_chars'  `grpsize_vars'  `grp_chars' if !grp_apm_flag [aw=total_opioid_benes] , absorb(specialtyID pcsaID) vce(cluster npiID)
	quietly summ avg_benzo_all_clms_per100ben
	outreg2 using "\output\tables\partd_doc_year_reg_`date'.xls", append ctitle(Benzo, Yr-FE, pcsa-FE) addstat(Mean, r(mean), SD, r(sd)) addtext() adjr2

	reghdfe avg_benzo_all_clms_per100ben i.year  `doc_chars' `doc_chars_time_trend' `grpsize_vars'  `grp_chars' if !grp_apm_flag  [aw=total_opioid_benes], absorb(specialtyID pcsaID) vce(cluster npiID)
	outreg2 using "\output\tables\partd_doc_year_reg_`date'.xls", append ctitle(Benzo, Time Trends, Yr-FE, pcsa-FE) addtext() adjr2
	**** Plot marginal effects of experience
	quietly margins, at(exp=(0 5 10 15 20 25 30 35 40 45))
	marginsplot
	graph export "\output\figures\doc_reg_expeffect\expeffect_benzopartd_`date'.pdf", replace

	reghdfe avg_benzo_all_clms_per100ben i.year  `doc_chars' `doc_chars_time_trend' `grpsize_vars'  `grp_chars'  `grp_patientmix'  if !grp_apm_flag  [aw=total_opioid_benes], absorb(specialtyID pcsaID) vce(cluster npiID)
	outreg2 using "\output\tables\partd_doc_year_reg_`date'.xls", append ctitle(Benzo, Time Trends, Yr-FE, pcsa-FE) addtext() adjr2

	************ Antibiotics regressions
	reghdfe avg_antbtc_clms_per100ben i.year  `doc_chars'  `grpsize_vars'  `grp_chars' if !grp_apm_flag [aw=total_opioid_benes] , absorb(specialtyID pcsaID) vce(cluster npiID)
	quietly summ avg_antbtc_clms_per100ben
	outreg2 using "\output\tables\partd_doc_year_reg_`date'.xls", append ctitle(Antibiotics, Yr-FE, pcsa-FE) addstat(Mean, r(mean), SD, r(sd)) addtext() adjr2

	reghdfe avg_antbtc_clms_per100ben i.year  `doc_chars' `doc_chars_time_trend' `grpsize_vars'  `grp_chars'  if !grp_apm_flag [aw=total_opioid_benes], absorb(specialtyID pcsaID) vce(cluster npiID)
	outreg2 using "\output\tables\partd_doc_year_reg_`date'.xls", append ctitle(Antibiotics, Time Trends, Yr-FE, pcsa-FE) addtext() adjr2
	**** Plot marginal effects of experience
	quietly margins, at(exp=(0 5 10 15 20 25 30 35 40 45))
	marginsplot
	graph export "\output\figures\doc_reg_expeffect\expeffect_antibioticspartd_`date'.pdf", replace


	************ Antipsychotics regressions
	reghdfe avg_antpsyct_clms_per100ben i.year  `doc_chars'  `grpsize_vars'  `grp_chars' if !grp_apm_flag [aw=total_opioid_benes] , absorb(specialtyID pcsaID) vce(cluster npiID)
	quietly summ avg_antpsyct_clms_per100ben
	outreg2 using "\output\tables\partd_doc_year_reg_`date'.xls", append ctitle(Antipsychotics, Yr-FE, pcsa-FE) addstat(Mean, r(mean), SD, r(sd)) addtext() adjr2

	reghdfe avg_antpsyct_clms_per100ben i.year  `doc_chars' `doc_chars_time_trend' `grpsize_vars'  `grp_chars' if !grp_apm_flag  [aw=total_opioid_benes], absorb(specialtyID pcsaID) vce(cluster npiID)
	outreg2 using "\output\tables\partd_doc_year_reg_`date'.xls", append ctitle(Antipsychotics, Time Trends, Yr-FE, pcsa-FE) addtext() adjr2
	**** Plot marginal effects of experience
	quietly margins, at(exp=(0 5 10 15 20 25 30 35 40 45))
	marginsplot
	graph export "\output\figures\doc_reg_expeffect\expeffect_antipsychoticspartd_`date'.pdf", replace

	***************************
	** Group level MIPS analysis
	***************************
	* Get group-level beneficiary characteristics
	preserve
		duplicates drop grouppracticepacid year, force
		keep grouppracticepacid grp_bene* grp_total_benes year
		collapse (mean) grp_bene_avg* grp_bene_pct* (rawsum) grp_total_benes grp*cnt* [aweight=grp_total_benes], by(grouppracticepacid)
		tempfile grp_benes_chars
		save `grp_benes_chars'
	restore

	* Get other time invariant group characteristics
	use "data\DAC_NationalDownloadableFiles\group_characteristics.dta", clear
	gen grpsize_lessthan50 = !(grpsize_51to200 |  grpsize_201to999 | grpsize_morethan1000)
	collapse (mean) *2019 *2020 grp_tag* grp_full_mips_info grp_academic grp_size grpsize_* (max) grp_apm_flag (firstnm) organizationlegalname, by(grouppracticepacid)

	merge 1:1 grouppracticepacid using `grp_benes_chars'
	* Drop groups that don't appear in Part D data (37,684 matched groups; 88,922 did not appear in Part B data)
	drop if _merge == 1
	drop _merge
	merge 1:1 grouppracticepacid using `grp_docs_chars'
	drop _merge
	
	drop if missing(grouppracticepacid)
	drop if grp_size == 1

	* Group-level MIPS regressions
	local scores final_mips_score quality_category_score pi_category_score ia_category_score 
	foreach score of local scores{
		preserve
			reshape long grp_`score', i(grouppracticepacid) j(year)

			reg grp_`score' i.grp_apm_flag##i.year `grpsize_vars' `grp_chars' `grp_patientmix', vce(robust)
			quietly sum grp_`score'
			if "`score'" == "final_mips_score"{
				outreg2 using "\output\tables\GrpMIPSreg_`date'.xls", append ctitle(PartD Grp `score') addstat(Mean, r(mean), SD, r(sd)) addtext()

				reg grp_`score' i.grp_apm_flag##i.year `grpsize_vars' `grp_chars' `grp_patientmix' if grp_full_mips_info == 1, vce(robust)
				quietly sum grp_`score' if grp_full_mips_info
				outreg2 using "\output\tables\GrpMIPSreg_`date'.xls", append ctitle(PartD Grp`score' Full Info) addstat(Mean, r(mean), SD, r(sd)) addtext()
			}
			else{
				outreg2 using "\output\tables\GrpMIPSreg_`date'.xls", append ctitle(PartD Grp `score') addstat(Mean, r(mean), SD, r(sd)) addtext()
			}	
		restore
	}
	
	drop if missing(grp_final_mips_score2020) & missing(grp_final_mips_score2019)
	unique grouppracticepacid if grp_apm_flag == 0 
	unique grouppracticepacid if grp_apm_flag == 1 
	unique grouppracticepacid  if grp_final_mips_score2019 < 30 & grp_apm_flag == 0
	unique grouppracticepacid  if grp_final_mips_score2019 < 30 & grp_apm_flag == 1
	unique grouppracticepacid  if grp_final_mips_score2020 < 45 & grp_apm_flag == 0
	unique grouppracticepacid  if grp_final_mips_score2020 < 45 & grp_apm_flag == 1
	unique grouppracticepacid  if grp_final_mips_score2019 == 30 & grp_apm_flag == 0
	unique grouppracticepacid  if grp_final_mips_score2019 == 30 & grp_apm_flag == 1
	unique grouppracticepacid if grp_final_mips_score2020 == 45 & grp_apm_flag == 0
	unique grouppracticepacid  if grp_final_mips_score2020 == 45 & grp_apm_flag == 1


end 

***********************************************************************
*** Function to investigate what kind of docs are missing MIPs components
************************************************************************
program mips_missing_check
	use "data\DAC_NationalDownloadableFiles\mips_perf20192020.dta", clear
	keep if source == "individual"
	gen tag20missing = 1 if (final_mips_score == 45 & year == 2020)
	gen tag19missing = 1 if (final_mips_score == 30 & year == 2019)
	gen tag20has = 1 if year == 2020
	gen tag19has = 1 if year == 2019
	collapse (mean) *score (max) tag*, by(npi)

	* Full mips info includes (1) those who have info for both years, neither of which are missing in all components
	** (2) those who have info for 2020 but not 2019, and 2020 isn't missing in all components
	** (3) those who hav einfo for 2019 but not 2020, and 2019 isn't missing in all components
	gen full_mips_info = 0
	replace full_mips_info = 1 if !missing(tag20has) & !missing(tag19has) & missing(tag19missing) & missing(tag20missing)
	replace full_mips_info = 1 if  & missing(tag19has)  & !missing(tag20has) & missing(tag20missing)
	replace full_mips_info = 1 if  & missing(tag20has)  & !missing(tag19has) & missing(tag19missing)

	* Count those who have MIPS scores for both years
	count if !missing(tag20has) & !missing(tag19has)
	count if !missing(tag20has) & !missing(tag19has) & missing(tag20missing) 
	count if !missing(tag20has) & !missing(tag19has) & missing(tag19missing)
	count if !missing(tag20has) & !missing(tag19has) & missing(tag19missing) & missing(tag20missing)
end

**************************************************************************
** Function to see how many NPIs are affiliated with academic institutions
**************************************************************************
program academic_tab
	* Part B
	use  "data\PartB\ProviderandService\PartBProvider_AllServiceCounts20132019.dta", clear
	drop if merge_emcode_counts == 1 
	drop if merge_dac == 1
	drop if merge_hrr == 1
	collapse (max) academic grp_academic, by(npi)
	sum

	use "data\PartD\ProviderandDrug\PartDProvider_AllDrugCtgrs20132019.dta", clear
	drop if merge_drug_categories == 1 
	drop if merge_dac == 1
	drop if merge_hrr == 1
	collapse (max) academic grp_academic, by(npi)
	sum
end
*************************************************************************

** Function to create correlation matrix between outcome variables
program correlation_matrix_outcomes
	** Load Part B Data
	use "data\PartB\ProviderandService\PartBProvider_AllServiceCounts20132019.dta", clear
	local em_cat office

	* Do correlation at the doc level
	collapse (firstnm) *name state zip5 specialty male graduationyear (sum) total* office* hosp* emerg* nurs* domic* home* rev_delta hh hh_*   (mean) urban ehr_adopt* *2019 *2020, by(npi grouppracticepacid)

	foreach cat of local em_cat{
		gen pct_`cat'_top1 = `cat'_top1 / `cat'
		gen pct_`cat'_top2 = `cat'_top2 / `cat'
		gen pct_`cat'_top3 = `cat'_top3 / `cat'
		capture gen pct_`cat'_top4 = `cat'_top4 / `cat'
		capture gen pct_`cat'_top5 = `cat'_top5 / `cat'
	}
	gen pct_office_top = pct_office_top1 + pct_office_top2

	gen hh_per100benes = hh/total_benes * 100
	gen hh_cert_per100benes = hh_cert/total_benes * 100
	gen hh_sprvsn_per100benes = hh_sprvsn/total_benes * 100

	collapse (mean) hh*per100benes pct_office_top  final_mips_score2019 quality_category_score2019 pi_category_score2019 final_mips_score2020 quality_category_score2020 pi_category_score2020 ehr_adopt*,  by(npi)

	tempfile ptb_npis
	save `ptb_npis'

	** Load Part D Data
	use "data\PartD\ProviderandDrug\PartDProvider_AllDrugCtgrs20132019.dta", clear

	* Collapse to the NPI-group level
	collapse (firstnm) *name state zip5 specialty male graduationyear (sum) total*  bene*cnt (mean) bene_avg_risk_scre bene_avg_age urban ehr_adopt* *2019 *2020, by(npi grouppracticepacid)
	local drugs opioid benzo_all antbtc antpsyct
	drop if total_claims_categories == 0

	* For each drug category of interest, compute average claims per 100 bene
	foreach drug of local drugs{
		gen avg_`drug'_clms_per100ben = total_`drug'_claims/ total_benes * 100 
	}


	collapse (mean) avg*clms_per100ben  final_mips_score2019 quality_category_score2019 pi_category_score2019 final_mips_score2020 quality_category_score2020 pi_category_score2020 ehr_adopt*, by(npi)
	merge 1:1 npi using `ptb_npis'

	egen final_mips_score = rmean(final_mips_score2019 final_mips_score2020)
	egen pi_category_score = rmean(pi_category_score2019 pi_category_score2020)
	egen quality_category_score = rmean(quality_category_score2019 quality_category_score2020)

	* Save correlation matrix
	label variable ehr_adopt "EHR"
	label variable ehr_adopt_pre12 "EHR Pre12"
	label variable ehr_adopt_post12 "EHR Post12"
	label variable avg_opioid "OpioidAvgClaims"
	label variable avg_benzo "BenzoAvgClaims"
	label variable avg_antbtc "AntibioticsAvgClaims"
	label variable avg_antpsyct "AntipsychoticsAvgClaims"
	label variable final_mips_score "MIPS (final)"
	label variable quality_category_score "MIPS (quality)"
	label variable pi_category_score "MIPS (PI)"
	label variable hh_per100benes "HomeHealth/100benes"
	label variable hh_cert "HomeHealthCert/100benes"
	label variable hh_sprvsn "HomeHealthCert/100benes"
	label variable pct_office_top "%OfficeTop"
	drop _merge  hh_cert hh_sprvsn *2019 *2020
	asdoc pwcorr, label save(\output\tables\outcomes_corrmat) replace

end



*******************************************************************************************
** Flag all the NPIs that appear in Part D and Part B - and those that are given MIPS scores
program combine_partd_partb_mips
	use  "data\PartB\ProviderandService\PartBProvider_AllServiceCounts20132019.dta", clear
	keep npi
	duplicates drop
	gen partb_flag = 1
	tempfile partb
	save `partb'

	use "data\PartD\ProviderandDrug\PartDProvider_AllDrugCtgrs20132019.dta", clear
	keep npi
	duplicates drop
	gen partd_flag = 1
	tempfile partd
	save `partd'

	use "data\DAC_NationalDownloadableFiles\mips_perf20192020.dta", clear
	keep if source == "individual" | source == "apm"
	collapse (mean) final_mips_score, by(npi)
	gen mips_flag = 1
	tempfile mips
	save `mips'

	use "data\DAC_NationalDownloadableFiles\DAC_NationalDownloadableFiles20142020.dta", clear
	keep if primary_care == 1
	keep npi
	duplicates drop
	gen dac_flag = 1

	* Begin merging
	merge 1:1 npi using `partb'
	drop _merge
	merge 1:1 npi using `partd'
	drop _merge
	merge 1:1 npi using `mips'
	drop _merge

	replace partb_flag = 0 if missing(partb_flag)
	replace partd_flag = 0 if missing(partd_flag)
	replace mips_flag = 0 if missing(mips_flag)
	replace dac_flag = 0 if missing(dac_flag)

	save "data\merged\PartDPartBMIPS_flags.dta", replace
end





* Visualize the spatial distribution of variables of interest on a map
* Tutorial: https://www.stata.com/support/faqs/graphics/spmap-and-maps/
program draw_map
	cd "data\MappingData\hrr-shapefile\"

	shp2dta using Hrr98Bdry_AK_HI_unmodified, database(hrrdb) coordinates(hrrcoord) genid(hrrid) replace

	* Clean the shapefile data a little bit
	use hrrdb, clear
	tostring hrrnum, replace
	describe

	* Drop Hawaii and Alaska from shapefile make graphs look nicer
	drop if hrrnum == "150" | hrrnum == "10"

	save hrrdb, replace
	
	************** Merge Part D Z Scores with shapefile
	use   "data\ZipCrosswalks\HRR_characteristics.dta", clear
	collapse (sum) hrr_tot_hh_cert 	hrr_tot_hh_sprvsn hrr_tot_srvc hrr_tot_partbproviders, by(hrrnum)

	* Merg ewith shapefile data
	merge m:1 hrrnum using "data\MappingData\hrr-shapefile\hrrdb"
	keep if _merge == 3
	drop _merge

	gen hrr_hh_cert_per100doc = hrr_tot_hh_cert/ hrr_tot_partbproviders * 100
	gen hrr_hh_sprvsn_per100doc = hrr_tot_hh_sprvsn/ hrr_tot_partbproviders * 100

	* Plot z- and mips score maps
	spmap hrr_hh_cert_per100doc using hrrcoord, id(hrrid) fcolor(Blues)
	graph export "\output\figures\hrr_hh_cert_per100doc.pdf", replace
	spmap hrr_hh_sprvsn_per100doc using hrrcoord, id(hrrid) fcolor(Blues)
	graph export "\output\figures\hrr_hh_sprvsn_per100doc.pdf", replace


end

**************************************************************************************
** Check the different merge results between data set, restricted to overlapping years
** Done at the NPI level to see which NPIs are in which data sets and not others
***************************************************************************************
program check_merge_results
	** Get the HealthIT data at the provider level
	import delimited "data\HealthIT\healthit_attest.csv", stringcols(1 2 5) clear 
	keep if attestation_year >= 2013
	gen ehr_adopt = 1
	collapse (mean) ehr_adopt, by(npi)
	tempfile healthit 
	save `healthit'

	** Get DAC data
	use "data\DAC_NationalDownloadableFiles\DAC_NationalDownloadableFiles20142020.dta", clear
	keep if year < 2020

	gen primary_care = 0
	replace primary_care = 1 if specialty_dac == "PEDIATRIC MEDICINE" | specialty_dac == "FAMILY PRACTICE" | specialty_dac == " GERIATRIC MEDICINE" | specialty_dac == "GENERAL PRACTICE" | specialty_dac == "INTERNAL MEDICINE" | specialty_dac == " OBSTETRICS/GYNECOLOGY" | specialty_dac == "FAMILY MEDICINE" 
	drop if primary_care == 0
	collapse (mean) exp, by(npi)
	tempfile dac_data
	save `dac_data'

	** Get the MIPS performance data at the provider level
	use "data\DAC_NationalDownloadableFiles\mips_perf20192020.dta", clear 
	rename org_pac_id grouppracticepacid
	* Get individual scores
	keep if source == "individual" | source == "apm"
	keep if year == 2019
	collapse (mean) final_mips_score, by(npi)
	tempfile mips_score
	save `mips_score'


	************************** Begin combining the Part D provider-level data
	forvalues y=2013/2019{
		import delimited "data\PartD\Provider\Provider`y'.csv", stringcols(1 6) clear

		keep prscrbr_npi prscrbr_last_org_name prscrbr_first_name prscrbr_crdntls prscrbr_gndr prscrbr_ent_cd prscrbr_state_abrvtn prscrbr_zip5 prscrbr_ruca  prscrbr_cntry prscrbr_type tot_clms tot_benes opioid_tot_clms opioid_tot_benes opioid_la_tot_clms opioid_la_tot_benes bene_avg_risk_scre

		rename (prscrbr_npi prscrbr_last_org_name prscrbr_first_name prscrbr_crdntls prscrbr_gndr prscrbr_ent_cd prscrbr_state_abrvtn prscrbr_zip5 prscrbr_ruca prscrbr_cntry prscrbr_type) (npi last_org_name first_name credentials gender entity_code state zip5 ruca country specialty )

		replace specialty = "Obstetrics/Gynecology" if specialty == "Obstetrics & Gynecology"
		* Create indicator for primary care physicians
		gen primary_care = 0
		replace primary_care = 1 if specialty == "Pediatric Medicine" | specialty == "Family Practice" | specialty == "Geriatric Medicine" | specialty == "General Practice" | specialty == "Internal Medicine" | specialty == "Obstetrics/Gynecology" | specialty == "Family Medicine" 
		drop if primary_care == 0 

		gen year = `y'

		tempfile tempdatad`y'
		save `tempdatad`y''
	}

	** Check merge results with Health IT data
	use `tempdatad2013', clear
	* Combine all years into a single data set
	forvalues y=2014/2017{
		append using `tempdatad`y''
	}

	collapse (mean) tot_clms tot_benes, by(npi)
	tempfile tempdatad20132017
	save `tempdatad20132017'
	merge 1:1 npi using `healthit'

	** Check merge results with DAC data
	use `tempdatad2013', clear
	* Combine all years into a single data set
	forvalues y=2014/2019{
		append using `tempdatad`y'', force
	}
	collapse (mean) tot_clms tot_benes, by(npi)
	tempfile tempdatad20132019
	save `tempdatad20132019'
	merge 1:1 npi using `dac_data'

	** Check merge results with MIPS data
	use `tempdatad2019', clear
	merge 1:1 npi using `mips_score'

	*************************** Begin combining the Part B provider-level data
	forvalues y=2013/2019{
		import delimited "data\PartB\Provider\Provider`y'.csv", stringcols(1 6) clear

		keep rndrng_npi rndrng_prvdr_last_org_name rndrng_prvdr_first_name rndrng_prvdr_crdntls rndrng_prvdr_gndr rndrng_prvdr_ent_cd rndrng_prvdr_state_abrvtn rndrng_prvdr_zip5 rndrng_prvdr_ruca rndrng_prvdr_cntry rndrng_prvdr_type tot_benes tot_srvcs drug_tot_srvcs med_tot_srvcs bene_avg_risk_scre

		rename (rndrng_npi rndrng_prvdr_last_org_name rndrng_prvdr_first_name rndrng_prvdr_crdntls rndrng_prvdr_gndr rndrng_prvdr_ent_cd rndrng_prvdr_state_abrvtn rndrng_prvdr_zip5 rndrng_prvdr_ruca rndrng_prvdr_cntry rndrng_prvdr_type) (npi last_org_name first_name credentials gender entity_code state zip5 ruca country specialty )

		replace specialty = "Obstetrics/Gynecology" if specialty == "Obstetrics & Gynecology"
		* Create indicator for primary care physicians
		gen primary_care = 0
		replace primary_care = 1 if specialty == "Pediatric Medicine" | specialty == "Family Practice" | specialty == "Geriatric Medicine" | specialty == "General Practice" | specialty == "Internal Medicine" | specialty == "Obstetrics/Gynecology" | specialty == "Family Medicine" 
		drop if primary_care == 0 

		gen year = `y'
		tempfile tempdatab`y'
		save `tempdatab`y''
	}

	** Check merge results with Health IT data
	use `tempdatab2013', clear
	* Combine all years into a single data set
	forvalues y=2014/2017{
		append using `tempdatab`y''
	}
	collapse (mean) tot_srvcs tot_benes, by(npi)
	tempfile tempdatab20132017
	save `tempdatab20132017'
	merge 1:1 npi using `healthit'


	** Check merge results with DAC data
	use `tempdatab2013', clear
	forvalues y=2014/2019{
		append using `tempdatab`y''
	}
	collapse (mean) tot_srvcs tot_benes, by(npi)
	tempfile tempdatab20132019
	save `tempdatab20132019'
	merge 1:1 npi using `dac_data'
	

	** Check merge results with MIPS data
	use `tempdatab2019', clear
	merge 1:1 npi using `mips_score'

	********************************* Combining Part D and Part B together ***************
	* Check merge results with Health IT data
	use `tempdatab20132017', clear
	merge 1:1 npi using `tempdatad20132017'
	collapse (mean) tot_benes, by(npi)
	merge 1:1 npi using `healthit'


	* Check merge results with DAC data
	use `tempdatab20132019', clear
	merge 1:1 npi using `tempdatad20132019'
	collapse (mean) tot_benes, by(npi)
	merge 1:1 npi using `dac_data'


	* Check merge results with MIPS data
	use `tempdatad2019', clear
	merge 1:1 npi using `tempdatab2019', force 
	collapse (mean) tot_benes, by(npi)
	merge 1:1 npi using `mips_score'


end


**************************************************************
** Tabulate the frequnecies of diffrent E/M coding intensities
**************************************************************
program tabulate_emcodes_bysite
	use "data\PartB\ProviderandService\ProviderSrvc_EMCodeCounts.dta", clear
	collapse (sum) office* hosp* emerg* nurs* domic* home* total*, by(npi)

	local em_cat office hosp_obs hosp_inpat emerg nurs_facil domic home
	foreach cat of local em_cat{
		gen pct_`cat'_top1 = `cat'_top1 / `cat'
		gen pct_`cat'_top2 = `cat'_top2 / `cat'
		gen pct_`cat'_top3 = `cat'_top3 / `cat'
		capture gen pct_`cat'_top4 = `cat'_top4 / `cat'
		capture gen pct_`cat'_top5 = `cat'_top5 / `cat'
	}

	foreach cat of local em_cat{
		sum pct_`cat'*
	}
end

main

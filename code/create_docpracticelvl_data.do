***************
* Change log: Now compute PCA first component instead of z-scores for prescribing and upcoding
***************

clear all
set more off
cd "C:\Users\khuon\Dropbox (Harvard University)\Physician Altruism\Altruism\"
*cd "/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Code/"


program main
	*import_providerservice_partb
	* join_dac_ndfs
	* get_grppractice_chars
	import_provider_partd
	import_provider_partb
	* create_partd_zscores
	*create_partb_zscores
end

program join_dac_ndfs
	forvalues y=2014/2020{
		import delimited "data\DAC_NationalDownloadableFiles\National_Downloadable_File_`y'.csv", stringcols(1 2 14) clear
		keep npi pacid graduationyear medicalschoolname primaryspecialty organizationlegalname grouppracticepacid numberofgrouppracticemembers
		destring numberofgrouppracticemembers, force replace
		destring graduationyear, force replace
		rename primaryspecialty specialty


		* Drop duplicates - treating the different locations of the same organization as duplicates
		* Also, since an organization might have different numbers of practice members in different months of the same year, just pick one as the "representative" number
		duplicates drop npi grouppracticepacid, force

		gen year = `y'
		tempfile tempdata`y'
		save `tempdata`y''
	}
	use `tempdata2014', clear
	* Combine all years into a single data set
	forvalues y=2015/2020{
		append using `tempdata`y''
	}

	* Clean up grouppracticepacid and wrongly formatted data
	replace grouppracticepacid = "" if grouppracticepacid == "NA"
	drop if medicalschoolname == " "

	rename numberofgrouppracticemembers grp_size
	* Missing group sizes = solo practice?
	replace grp_size = 1 if missing(grp_size)
	replace grouppracticepacid = "" if grp_size == 1

	* Get providers' years of experience
	gen exp = year - graduationyear
	
	**** IMPORTANT NOTES ****
	* A few (not a lot) are from defunct medical schools listed in https://en.wikipedia.org/wihtki/List_of_defunct_medical_schools_in_the_United_States, many have dubious graduation year (before 1900)
	* Relevant artcle: https://www.yahoo.com/entertainment/2015-07-31-phantom-med-schools-are-contributing-to-medicare-data-21216499.html?guccounter=1&guce_referrer=aHR0cHM6Ly93d3cuZ29vZ2xlLmNvbS8&guce_referrer_sig=AQAAADnOTaod5rBJCXW_n4Y5Zy3In9EISuafzZLWrC4MlAs0nc_5X1c0E-ItEwid4EGQAbiBBD_zUMZemKQIs

	* Indicator for graduates of top 20 USNEWS medical schools
	gen top20_medschool = 0
	replace top20_medschool = 1 if strpos(medicalschoolname, "HARVARD") | strpos(medicalschoolname, "NEW YORK UNIVERSITY") | strpos(medicalschoolname, "DUKE UNIVERSITY") | strpos(medicalschoolname, "COLUMBIA UNIVERSITY") |   strpos(medicalschoolname, "SAN FRANCISCO") | strpos(medicalschoolname, "JOHNS HOPKINS") | strpos(medicalschoolname, "STANFORD UNIVERSITY") | strpos(medicalschoolname, "UNIVERSITY OF WASHINGTON") | strpos(medicalschoolname, "UNIVERSITY OF PENNSYLVANIA") | strpos(medicalschoolname, "YALE UNIVERSITY") | strpos(medicalschoolname, "WASHINGTON UNIVERSITY") | strpos(medicalschoolname, "MAYO") | strpos(medicalschoolname, "UNIVERSITY OF PITTSBURGH") | strpos(medicalschoolname, "VANDERBILT UNIVERSITY") | strpos(medicalschoolname, "UNIVERSITY OF MICHIGAN MEDICAL SCHOOL") | strpos(medicalschoolname, "NORTHWESTERN UNIVERSITY") | strpos(medicalschoolname, "ICAHN SCHOOL") | strpos(medicalschoolname, "UNIVERSITY OF CHICAGO") | strpos(medicalschoolname, "SAN DIEGO")  | strpos(medicalschoolname, "CORNELL UNIVERSITY")
	* In case multiple med schools are reported, designate them as a top 20 graduate if one of the schools is top 20
	bys npi: egen top20_med = max(top20_medschool)
	drop top20_medschool
	rename top20_med top20_medschool

	* Indicator for graduates of foreign medical schools, including those tagged as "OTHER"
	gen foreign_other_medschool = 0
	replace foreign_other_medschool = 1 if strpos(medicalschoolname, "CANADIAN") | strpos(medicalschoolname, "DALHOUSIE") | strpos(medicalschoolname, "FOREIGN") |  strpos(medicalschoolname, "MCGILL") | strpos(medicalschoolname, "MCMASTER") | strpos(medicalschoolname, "MIDDLESEX UNIVERSITY SCHOOL OF MEDICINE") | strpos(medicalschoolname, "NATIONAL MEDICAL UNIVERSITY") | strpos(medicalschoolname, "NATIONAL UNIVERSITY OF ARTS") | strpos(medicalschoolname, "PACIFIC MEDICAL COLLEGE") | strpos(medicalschoolname, "QUEEN'S SCHOOL") | strpos(medicalschoolname, "THE GENERAL MEDICAL COLLEGE") | strpos(medicalschoolname, "BRITISH COLUMBIA")  | strpos(medicalschoolname, "MANITOBA")  |  strpos(medicalschoolname, "WESTERN ONTORIO")  |  strpos(medicalschoolname, "MONTREAL") | strpos(medicalschoolname, "UNIVERSITE LAVAL") | strpos(medicalschoolname, "UNIVERSITY COLLEGE OF MEDICINE") |  strpos(medicalschoolname, "CALGARY")  | strpos(medicalschoolname, "OTTAWA")  | strpos(medicalschoolname, "SASKATCHEWAN") | strpos(medicalschoolname, "TORONTO")  | strpos(medicalschoolname, "WATERLOO") |   strpos(medicalschoolname, "WESTERN UN") | strpos(medicalschoolname, "OTHER") 


	* Create indicator for primary care physicians
	gen primary_care = 0
	replace primary_care = 1 if specialty == "PEDIATRIC MEDICINE" | specialty == "FAMILY PRACTICE" | specialty == "GERIATRIC MEDICINE" | specialty == "GENERAL PRACTICE" | specialty == "INTERNAL MEDICINE" | specialty == "OBSTETRICS/GYNECOLOGY" | specialty == "FAMILY MEDICINE" 


	save "data\DAC_NationalDownloadableFiles\DAC_NationalDownloadableFiles20142020.dta", replace

	**** Create variable keeping track of the number of practices an NPI is affiliated with in a given year
	* Drop solo practices
	drop if missing(grouppracticepacid)
	bysort npi year: gen num_affil_grps = _N

	
	* Create variable indicating the doc never participates in more than one group in any given year
	bysort npi (num_affil_grps): gen always_one_grp = (num_affil_grps[_N] == 1)

	* For docs that never partcipates in multiple groups, create indicator variable for group changes
	bysort npi (year): gen changed_grp = (grouppracticepacid != grouppracticepacid[_n-1]) if _n!= 1 & always_one_grp 
	egen ever_changed_grp = max(changed_grp), by(npi)

	order npi year grouppracticepacid num_affil_grps always_one_grp changed_grp ever_changed_grp
	sort npi year grouppracticepacid

	drop pacid medicalschoolname graduationyear specialty foreign_other_medschool top20_medschool exp

	save "data\DAC_NationalDownloadableFiles\DAC_NDF20142020_grpchanges.dta", replace
	
	* Do some NPI-level summary stats on changes
	collapse (max) always_one_grp ever_changed_grp primary_care, by(npi)
	keep if primary_care
	sum always_one_grp
	keep if always_one_grp
	sum ever_changed_grp
end



* Merge different years of DAC data on MIPS scores and CAHPS ratings and health IT 
program clean_mips_healthit
	*************************************************
	*** Merge MIPS scores data
	forvalues y=2019/2020{
		import delimited "data\DAC_NationalDownloadableFiles\mips_perf`y'.csv", stringcols(1 2) clear
		tempfile mips`y'
		gen year = `y'
		save `mips`y''
	}

	use `mips2019', clear
	forvalues y=2020/2020{
		append using `mips`y'', force
	}
	tempfile mips_all
	save `mips_all'

	********************************************************************************************
	* Create separate MIPS files for individuals, groups, and APMs as well as "full info" tags
	*** Individual MIPS
	* Impute PI scores to be zero if other categories are non-zero (due to existence Hardship Exception applications)
	replace pi_category_score = 0 if missing(pi_category_score) & !missing(quality_category_score) & !missing(ia_category_score)

	keep if source == "individual" 
	drop facility* *nm org_pac_id source
	duplicates drop

	foreach var of varlist *score*{
		forvalues yr = 2019/2020{
				gen `var'`yr' = `var' if year == `yr'
		}
	}

	*** Mark who is missing all the components of MIPS 
	gen tag20nocomponent = 1 if year == 2020 & missing(quality_category_score) & missing(pi_category_score) & missing(ia_category_score) 
	gen tag19nocomponent = 1 if year == 2019 & missing(quality_category_score) & missing(pi_category_score) & missing(ia_category_score) 
	gen tag20hasfinal = 1 if year == 2020
	gen tag19hasfinal = 1 if year == 2019

	preserve
		collapse (max) tag*, by(npi)
	
		* Full mips info includes (1) those who have info for both years, neither of which are missing in all components
		** (2) those who have info for 2020 but not 2019, and 2020 isn't missing in all components
	    ** (3) those who have info for 2019 but not 2020, and 2019 isn't missing in all components
		gen full_mips_info = 0
		replace full_mips_info = 1 if tag20hasfinal==1 & tag19hasfinal==1 & missing(tag19nocomponent) & missing(tag20nocomponent)
		replace full_mips_info = 1 if tag19hasfinal==1   & missing(tag20hasfinal) & missing(tag19nocomponent)
		replace full_mips_info = 1 if  tag20hasfinal==1  & missing(tag19hasfinal) & missing(tag20nocomponent)
	
		tempfile full_mips_individual
		save `full_mips_individual'
	restore

	collapse (max) tag* *2019 *2020 , by(npi)
	merge 1:1 npi using `full_mips_individual'
	drop _merge
	drop if missing(npi)

	save "data\DAC_NationalDownloadableFiles\mips_perf20192020_individual.dta", replace

	*** Group + APM MIPS
	local sources group apm
	foreach s of local sources{
		use `mips_all', clear

		replace pi_category_score = 0 if missing(pi_category_score) & !missing(quality_category_score) & !missing(ia_category_score)

		keep if source == "`s'" 
		drop facility* *nm 
		rename org_pac_id grouppracticepacid

		if "`s'" == "apm"{
			drop grouppracticepacid
			egen grouppracticepacid = group(quality_category_score pi_category_score ia_category_score final_mips_score), missing
			tostring grouppracticepacid, replace
			gen grp_apm_flag = 1

			egen grp_size = count(npi), by(grouppracticepacid year)
			forvalues yr = 2019/2020{
				gen grp_size_apm`yr' = grp_size if year == `yr'
			}
		}

		foreach var of varlist *score*{
			forvalues yr = 2019/2020{
					gen `var'`yr' = `var' if year == `yr'
			}
		}

		*** Mark who is missing all the components of MIPS 
		gen tag20nocomponent = 1 if year == 2020 & missing(quality_category_score) & missing(pi_category_score) & missing(ia_category_score) 
		gen tag19nocomponent = 1 if year == 2019 & missing(quality_category_score) & missing(pi_category_score) & missing(ia_category_score) 
		gen tag20hasfinal = 1 if year == 2020
		gen tag19hasfinal = 1 if year == 2019
	
		preserve
			collapse (max) *2019 *2020  tag*, by(grouppracticepacid)
	
			* Full mips info includes (1) those who have info for both years, neither of which are missing in all components
			** (2) those who have info for 2020 but not 2019, and 2020 isn't missing in all components
	    	** (3) those who hav einfo for 2019 but not 2020, and 2019 isn't missing in all components
			gen full_mips_info = 0
			replace full_mips_info = 1 if tag20hasfinal==1 & tag19hasfinal==1 & missing(tag19nocomponent) & missing(tag20nocomponent)
			replace full_mips_info = 1 if tag19hasfinal==1   & missing(tag20hasfinal) & missing(tag19nocomponent)
			replace full_mips_info = 1 if  tag20hasfinal==1  & missing(tag19hasfinal) & missing(tag20nocomponent)
	
			tempfile group_info_`s'
			save `group_info_`s''
		restore
	
		if "`s'" == "apm"{
			collapse (mean) grp_size (max) grp_apm_flag (firstnm) source, by(npi grouppracticepacid year)
			tempfile npi_apm_map
			save `npi_apm_map'
			collapse (max)  grp_apm_flag (firstnm) source, by(npi grouppracticepacid)
		} 
		else if "`s'" == "group"{
			collapse (firstnm) source, by(npi grouppracticepacid)
		}

		* Merge with the full_mips_info variable
		merge m:1 grouppracticepacid using `group_info_`s''
		drop _merge
		
		drop if missing(grouppracticepacid)
		save "data\DAC_NationalDownloadableFiles\mips_perf20192020_`s'.dta", replace
	}
	
	********************************************************
	*** Create an NPI-group/APM linkage file (basically, a DAC_NationalDownloadableFiles with APM as "groups")
	use `npi_apm_map', clear
	drop source
  	append using "data\DAC_NationalDownloadableFiles\DAC_NationalDownloadableFiles20142020.dta"

	* Create practice size bins
	gen grpsize_1 = (grp_size == 1)
	gen grpsize_2to15 = grp_size > 1 & grp_size <=15 
	gen grpsize_16to50 =  grp_size >15 & grp_size <=50 
	gen grpsize_51to200 = grp_size >50 & grp_size <=200
	gen grpsize_201to999 = grp_size >200 & grp_size <=999
	gen grpsize_morethan1000 = grp_size >999

	replace grp_apm_flag = 0 if missing(grp_apm_flag)

	* Fill the missing values
	bys npi (graduationyear): replace graduationyear = graduationyear[_n-1] if missing(graduationyear) &  _n > 1 
	bys npi: replace specialty = specialty[_n-1] if missing(specialty) &  _n > 1 
	bys npi (top20_medschool): replace top20_medschool = top20_medschool[_n-1] if missing(top20_medschool) &  _n > 1 
	bys npi: replace medicalschoolname = medicalschoolname[_n-1] if missing(medicalschoolname) &  _n > 1  
	bys npi (primary_care): replace primary_care = primary_care[_n-1] if missing(primary_care) &  _n > 1 
	bys npi: replace pacid = pacid[_n-1] if missing(pacid) &  _n > 1  
	bys npi (foreign_other_medschool): replace foreign_other_medschool =foreign_other_medschool[_n-1] if missing(foreign_other_medschool) &  _n > 1  
	replace exp = year - graduationyear if missing(exp)

	
	* Create indicator for academic institution	
	gen academic_institution = 1 if strpos(organizationlegalname, "UNIVERSITY") | strpos(organizationlegalname, "COLLEGE")
	replace academic_institution = 0 if missing(academic_institution)

	save "data\DAC_NationalDownloadableFiles\DAC_apm_grp_linkage.dta", replace
	

	*** Health IT data
	** Get the HealthIT data at the provider level
	import delimited "data\HealthIT\healthit_attest.csv", stringcols(1 2 5) clear 
	keep npi attestation_year provider_type
	gen ehr_adopt = 1
	collapse (min) ehr_adopt first_ityear=attestation_year, by(npi)
	gen ehr_adopt_post12 = (first_ityear > 2012) == 1
	gen ehr_adopt_pre12 = (first_ityear <= 2012) == 1
	duplicates drop

	save "data\HealthIT\healthit_attest_cleaned.dta", replace

end

* Obtain group practice-level characteristics, such as the number of group members, % graduates of top 20 medical schools, average years of experience, patient experience, etc.
program get_grppractice_chars
	* Clean data on MIPS performance, group level
	use "data\DAC_NationalDownloadableFiles\mips_perf20192020_group.dta", clear 
	collapse (mean) *2019 *2020 (max) tag* full_mips_info , by(grouppracticepacid)
	tempfile mips_score_grp 
	save `mips_score_grp'

	* Clean data on MIPS performance, APM level
	use "data\DAC_NationalDownloadableFiles\mips_perf20192020_apm.dta", clear 
	collapse (mean) *2019 *2020 (max) tag* full_mips_info , by(grouppracticepacid)
	append using `mips_score_grp'
	tempfile mips_score_all
	save `mips_score_all'

	** Get data on group practice sizes
	use "data\DAC_NationalDownloadableFiles\DAC_apm_grp_linkage.dta", clear

	merge m:1 grouppracticepacid using `mips_score_all'
	drop if _merge == 2
	rename _merge merge_mips_all

	* Merge with Health IT data
	merge m:1 npi using "data\HealthIT\healthit_attest_cleaned.dta"
	drop if _merge == 2
	rename _merge merge_healthit

	replace ehr_adopt = 0 if missing(ehr_adopt)
	replace ehr_adopt_post12 = 0 if missing(ehr_adopt_post12)
	replace ehr_adopt_pre12 = 0 if missing(ehr_adopt_pre12)

	drop first_ityear
	drop if missing(grouppracticepacid)

	* Collapse to the group level
	collapse (mean) grp_exp=exp grp_quality_category_score2019=quality_category_score2019 grp_pi_category_score2019=pi_category_score2019 grp_ia_category_score2019=ia_category_score2019 grp_cost_category_score2019=cost_category_score2019 grp_final_mips_score2019=final_mips_score2019  grp_quality_category_score2020=quality_category_score2020 grp_pi_category_score2020=pi_category_score2020 grp_ia_category_score2020=ia_category_score2020 grp_cost_category_score2020=cost_category_score2020 grp_final_mips_score2020=final_mips_score2020 grp_final_mips_score_wo_cpb2020=final_mips_score_without_cpb2020 grp_top20_medschool=top20_medschool grp_pct_ehr_adopt=ehr_adopt grp_pct_ehr_adopt_post12=ehr_adopt_post12 grp_pct_ehr_adopt_pre12=ehr_adopt_pre12 grpsize* grp_size grp_full_mips_info=full_mips_info grp_tag20component=tag20nocomponent grp_tag19component=tag19nocomponent grp_tag20hasfinal=tag20hasfinal grp_tag19hasfinal=tag19hasfinal (max) grp_apm_flag grp_academic=academic_institution (firstnm) organizationlegalname, by(grouppracticepacid year)

	save "data\DAC_NationalDownloadableFiles\group_characteristics.dta", replace
end

* Join the zipcode-hsa-hrr crosswalks from the Dartmouth Atlas together
program join_zip5_crosswalks
	* HRR level
	use "data\ZipCrosswalks\ziphsahrr13.dta", clear 

	* Combine all years into a single data set
	forvalues y=14/19{
		append using "data\ZipCrosswalks\ziphsahrr`y'.dta"
	}

	* Add leading zeros to zip codes
	destring zip5, replace
	gen str5 zip = string(zip5, "%05.0f")
	drop zip5
	rename zip zip5

	save "data\ZipCrosswalks\ziphsahrr20132019.dta", replace

end


*  Get opioid prescription summary statistics at the zip level
program clean_opioid_region_rates 
	import delimited "data\PartD\OpioidRates\opioid_rates_region.csv", stringcols(3 5) clear 
	keep if prscrbr_geo_lvl == "ZIP"
	rename prscrbr_geo_cd zip5

	keep year zip5 tot* opioid* la*
	save "data\PartD\OpioidRates\opioid_rates_region.dta", replace
end

* Obtain PCSA-level characteristics
program get_pcsa_chars
	* Aggrete opioid rates data to the PCSA level
	use  "data\PartD\OpioidRates\opioid_rates_region.dta", clear

	* Merge the zipcode-PCSA crosswalk data from Dartmouth Atlas
	merge m:1 zip5 using "data\ZipCrosswalks\zippcsa.dta"
	drop if _merge == 2
	drop _merge

	replace la_tot_opioid_clms = 5 if missing(la_tot_opioid_clms)
	replace tot_opioid_clms = 5 if missing(tot_opioid_clms)
	replace tot_opioid_clms = tot_opioid_clms + la_tot_opioid_clms
	
	collapse (rawsum) pcsa_tot_partdprescribers=tot_prscrbrs pcsa_tot_opioidprescribers=tot_opioid_prscrbrs pcsa_tot_clms=tot_clms pcsa_tot_opioid_clms=tot_opioid_clms, by(year pcsa)

	drop if pcsa == ""

	tempfile pcsa_opioid_rate
	save `pcsa_opioid_rate'

	forvalues y=2013/2019{
		import delimited "data\PartB\ProviderandService\ProviderSrvc`y'.csv", stringcols(1) clear 

		* Keep only variables of interest
		keep rndrng_npi hcpcs_cd tot_srvcs tot_benes rndrng_prvdr_zip5

		gen tot_hh_cert_srvc = tot_srvcs if inlist(hcpcs_cd, "G0179", "G0180") 
		gen tot_hh_sprvsn_srvc = tot_srvcs if inlist(hcpcs_cd, "G0181", "G0182") 

		gen tot_office = tot_srvcs if inlist(hcpcs_cd,  "99201", "99202", "99203", "99204", "99205")
		replace tot_office = tot_srvcs if inlist(hcpcs_cd, "99211", "99212", "99213", "99214", "99215")		
		gen tot_office_top = tot_srvcs if inlist(hcpcs_cd, "99205", "99215", "99204", "99214")

		gen tot_office_benes = tot_benes if inlist(hcpcs_cd,  "99201", "99202", "99203", "99204", "99205")
		replace tot_office_benes = tot_benes if inlist(hcpcs_cd, "99211", "99212", "99213", "99214", "99215")	

		collapse (firstnm) rndrng_prvdr_zip5 (sum) tot_hh_cert_srvc tot_hh_sprvsn_srvc tot_srvcs tot_office tot_office_top tot_office_benes, by(rndrng_npi)

		gen year=`y'

		tempfile tempdata`y'
		save `tempdata`y''
	}

	use `tempdata2013', clear
	* Combine all years into a single data set
	forvalues y=2014/2019{
		append using `tempdata`y''
	}

	rename rndrng_prvdr_zip5 zip5
	tostring zip5, force replace
	* Merge the zipcode-PCSA crosswalk data from Dartmouth Atlas
	merge m:1 zip5 using "data\ZipCrosswalks\zippcsa.dta"
	drop if _merge == 2
	drop _merge

	* Count the number of unique NPIs per PCSA per year
	egen tag = tag(rndrng_npi pcsa year)

	collapse (sum) pcsa_tot_hh_cert=tot_hh_cert_srvc  pcsa_tot_hh_sprvsn=tot_hh_sprvsn_srvc  pcsa_tot_srvc=tot_srvcs pcsa_tot_office=tot_office pcsa_tot_office_top=tot_office_top pcsa_tot_office_benes=tot_office_benes  (count) pcsa_tot_partbproviders=tag, by(year pcsa)
	drop if pcsa == ""

	merge 1:1 pcsa year using `pcsa_opioid_rate'
	drop _merge
	save  "data\ZipCrosswalks\PCSA_characteristics.dta", replace
end


program import_providerdrug_partd
	* Import the raw Part D Provider-Drug files and aggregate all years into one file
	forvalues y=2013/2019{
		import delimited "C:\Users\khuon\\Dropbox\Research\Summer 2021\Altruismdata\PartD\ProviderandDrug\ProviderDrug`y'.csv", stringcols(1 6) clear

		* Keep only variables of interest
		keep prscrbr_npi brnd_name gnrc_name tot_clms tot_benes

		replace brnd_name = upper(brnd_name)
		replace gnrc_name = upper(gnrc_name)

		* Impute # beneficiaries to be 5 if censored due to being under 11
		replace tot_benes = 5 if missing(tot_benes)

		gen year=`y'

		tempfile partdproviderdrug`y'
		save `partdproviderdrug`y''
	}

	* Prepare the NDC-drug name-drug category mapping to merge with the Part D Provider-Drug files 
	use "data\PartD\ndc_map\ndc_allatc4_categories_bnn_gn_map.dta", clear
	keep brnd_name *flag
	duplicates drop brnd_name, force
	tempfile temp_drugcats
	save `temp_drugcats'

	* Aggregate the total claims and benficiaries in each drug categories for each year
	local drugs benzo_all stimulant barbiturate glucocorticoid plain_steroid hypnotic bladder_relax wtloss_drug
	local dep_vars claims benes

	forvalues y=2013/2019{
		use `partdproviderdrug`y'', clear

		* Merge with the generic name - drug catgories mapping
		merge m:1 brnd_name using `temp_drugcats', keepusing(*flag)

		rename (prscrbr_npi tot_clms tot_benes) (npi claims benes)

		foreach var of local dep_vars{
			foreach drug of local drugs{
				gen `drug'_`var' = `var'*`drug'_flag
				egen total_`drug'_`var' = total(`drug'_`var'), by(npi)
				drop `drug'_`var'
			}
		}	

		keep npi total_*
		collapse (mean) total_*, by(npi)

		gen year = `y'

		tempfile temp_drug`y'
		save `temp_drug`y''
	}

	* Append the years together
	use `temp_drug2013'
	forvalues y==2014/2019{
		append using `temp_drug`y''

	}

	save "data\PartD\ProviderandDrug\PartDProviderDrugCtgrs20132019.dta", replace
end

* Get Part D provider-level characteristics
program import_provider_partd
	** Begin combining the provider-level data
	forvalues y=2013/2019{
		import delimited "data\PartD\Provider\Provider`y'.csv", stringcols(1 6) clear

		keep prscrbr_npi prscrbr_last_org_name prscrbr_first_name prscrbr_crdntls prscrbr_gndr prscrbr_ent_cd prscrbr_state_abrvtn prscrbr_zip5 prscrbr_ruca  prscrbr_cntry prscrbr_type tot_clms tot_benes opioid_tot_clms opioid_tot_benes opioid_la_tot_clms opioid_la_tot_benes antbtc_tot_clms antbtc_tot_benes antpsyct_ge65_tot_clms antpsyct_ge65_tot_benes bene*

		rename (prscrbr_npi prscrbr_last_org_name prscrbr_first_name prscrbr_crdntls prscrbr_gndr prscrbr_ent_cd prscrbr_state_abrvtn prscrbr_zip5 prscrbr_ruca prscrbr_cntry prscrbr_type) (npi last_org_name first_name credentials gender entity_code state zip5 ruca country specialty )

		* Impute # beneficiaries that's been censored
		replace tot_benes = 5 if missing(tot_benes) 
		replace opioid_tot_benes = 5 if missing(opioid_tot_benes) & opioid_tot_clms != 0 
		replace opioid_tot_benes = 0 if missing(opioid_tot_benes) & opioid_tot_clms == 0 
		replace opioid_la_tot_benes = 5 if missing(opioid_la_tot_benes) & opioid_la_tot_clms != 0
		replace opioid_la_tot_benes = 0 if missing(opioid_la_tot_benes) & opioid_la_tot_clms == 0

		replace antbtc_tot_benes = 5 if missing(antbtc_tot_benes) & antbtc_tot_clms != 0
		replace antbtc_tot_benes = 0 if missing(antbtc_tot_benes) & antbtc_tot_clms == 0 
		replace antpsyct_ge65_tot_benes = 5 if missing(antpsyct_ge65_tot_benes) & antpsyct_ge65_tot_clms != 0
		replace antpsyct_ge65_tot_benes = 0 if missing(antpsyct_ge65_tot_benes) & antpsyct_ge65_tot_clms == 0 

		* Impute # prescriptions under 11 also
		replace opioid_tot_clms = 5 if missing(opioid_tot_clms) 
		replace opioid_la_tot_clms = 5 if missing(opioid_la_tot_clms)
		replace antbtc_tot_clms = 5 if missing(antbtc_tot_clms) 
		replace antpsyct_ge65_tot_clms = 5 if missing(antpsyct_ge65_tot_clms)

		replace opioid_tot_clms  = opioid_tot_clms  + opioid_la_tot_clms 
		replace opioid_tot_benes = opioid_tot_benes + opioid_la_tot_benes

		gen year = `y'

		tempfile tempdata`y'
		save `tempdata`y''
	}

	use `tempdata2013', clear
	* Combine all years into a single data set
	forvalues y=2014/2019{
		append using `tempdata`y''
	}

	gen str5 zip = string(zip5, "%05.0f")
	drop zip5
	rename zip zip5

	* Rename some specialties to match each other
	replace specialty = "Allergy/Immunology" if specialty == "Allergy/ Immunology"
	replace specialty = "Anesthesiologist Assistants" if specialty == "Anesthesiology Assistant" 
	replace specialty = "Behavior Analyst" if specialty == "Behavioral Analyst"
	replace specialty = "Cardiology" if specialty == "Cardiovascular Disease (Cardiology)"
	replace specialty = "Case Management" if specialty == "Case Manager/Care Coordinator"
	replace specialty = "Clinical Cardiac Electrophysiology" if specialty == "Clinical Cardiatric Electrophysiology"
	replace specialty = "Gynecological Oncology" if specialty == "Gynecological/Oncology"
	replace specialty = "Hematology/Oncology" if specialty == "Hematology-Oncology"
	replace specialty = "Hematology/Oncology" if specialty == "Hematology"
	replace specialty = "Independent Diagnostic Testing Facility" if specialty == "Independent Diagnostic Testing Facility (IDTF)"
	replace specialty = "Mammography Center" if specialty == "Mammographic Screening Center"
	replace specialty = "Mass Immunization Roster Biller" if specialty == "Mass Immunizer Roster Biller"
	replace specialty = "Medical Genetics" if specialty == "Medical Genetics, Ph.D. Medical Genetics" 
	replace specialty = "Obstetrics/Gynecology" if specialty == "Obstetrics & Gynecology"
	replace specialty = "Oral Surgery (dentists only)" if specialty == "Oral Surgery (Dentist only)"
	replace specialty = "Oral Surgery (dentists only)" if specialty == "Oral Surgery (Dentists only)"
	replace specialty = "Orthopedic Surgery" if specialty == "Orthopaedic Surgery"
	replace specialty = "Physical Medicine and Rehabilitation" if specialty == "Physical Medicine & Rehabilitation"
	replace specialty = "Public Health Welfare Agency" if specialty == "Public Health or Welfare Agency"
	replace specialty = "Registered Dietician/Nutrition Professional" if specialty == "Registered Dietitian or Nutrition Professional"
	replace specialty = "Respiratory Therapist" if specialty == "Respiratory Therapist, Registered"
	replace specialty = "Respiratory Therapist" if specialty == "Respiratory Therapist, Certified"
	replace specialty = "Thoracic Surgery" if specialty == "Thoracic Surgery (Cardiothoracic Vascular Surgery)"
	replace specialty = "Voluntary Health or Charitable Agency" if specialty == "Voluntary Health or Charitable Agencies"

	* Replace specialty of NPI with the most common specialty
	bysort npi specialty : gen specialty_count = _N
	bys npi (specialty_count): replace specialty = specialty[_N]
	drop specialty_count

	* Create indicator for primary care physicians
	gen primary_care = (specialty == "Pediatric Medicine" | specialty == "Family Practice" | specialty == "Geriatric Medicine" | specialty == "General Practice" | specialty == "Internal Medicine" | specialty == "Obstetrics/Gynecology" | specialty == "Family Medicine")

	* Keep only primary care docs
	keep if primary_care == 1
	
	* Merge the zipcode-PCSA crosswalk data from Dartmouth Atlas
	merge m:1 zip5 using "data\ZipCrosswalks\zippcsa.dta"
	drop if _merge == 2
	rename _merge merge_zippcsa_xwalks

	gen urban = 0
	replace urban = 1 if ruca == 1 | ruca == float(1.1) | ruca == 2 | ruca == float(2.1) | ruca == 3 | ruca == float(4.1) | ruca == float(5.1) | ruca == float(7.1) | ruca == float(8.1) | ruca == float(10.1)

	gen male = (gender == "M")
	* Use the last-reported gender identity as the gender identity
	bys npi (year): replace male = male[_N]
    drop gender

	* Merge with Drug Categories Mapping
	merge m:1 npi year using "data\PartD\ProviderandDrug\PartDProviderDrugCtgrs20132019.dta"
	drop if _merge == 2
	rename _merge merge_drug_categories

	rename (opioid_tot_clms opioid_tot_benes opioid_la_tot_clms opioid_la_tot_benes antbtc_tot_clms antbtc_tot_benes antpsyct_ge65_tot_clms antpsyct_ge65_tot_benes tot_benes tot_clms) (total_opioid_claims total_opioid_benes total_opioid_la_claims total_opioid_la_benes total_antbtc_claims total_antbtc_benes total_antpsyct_claims total_antpsyct_benes total_benes total_all_claims) 

	* Sum up the total number of claims and beneficiaries over all the drug categories of interest
	egen total_claims_categories = rowtotal(total_opioid_claims total_benzo_all_claims total_stimulant_claims total_barbiturate_claims total_glucocorticoid_claims total_plain_steroid_claims total_hypnotic_claims total_bladder_relax_claims total_wtloss_drug_claims total_antbtc_claims total_antpsyct_claims)
	egen total_benes_categories = rowtotal(total_opioid_benes total_benzo_all_benes total_stimulant_benes total_barbiturate_benes total_glucocorticoid_benes total_plain_steroid_benes total_hypnotic_benes total_bladder_relax_benes total_wtloss_drug_benes total_antbtc_benes total_antpsyct_benes)

	* Create an "into drugs" measure which encapsulates the tendency of a provider to prescribe drugs
	gen total_claims_noncategories = total_all_claims - total_claims_categories 
	gen partd_into_money_measure = total_claims_noncategories / total_benes

	* Merge with health it attestation data
	merge m:1 npi  using  "data\HealthIT\healthit_attest_cleaned.dta"
	drop if _merge == 2
	rename _merge merge_healthit
	replace ehr_adopt = 0 if missing(ehr_adopt)
	replace ehr_adopt_pre12 = 0 if missing(ehr_adopt_pre12)
	replace ehr_adopt_post12 = 0 if missing(ehr_adopt_post12)

	* Merge with MIPS score data
	merge m:1 npi using "data\DAC_NationalDownloadableFiles\mips_perf20192020_individual.dta"
	drop if _merge == 2
	rename _merge merge_mips

	* Merge with DAC data
	merge 1:m npi year using "data\DAC_NationalDownloadableFiles\DAC_apm_grp_linkage.dta"
	drop if _merge == 2
	rename _merge merge_dac_apm_grp

	* Merge with group-level data
	merge m:1 grouppracticepacid year using  "data\DAC_NationalDownloadableFiles\group_characteristics.dta"
	drop if _merge == 2
	rename _merge merge_grp

	* Create group-level demographic variables 
	local dem_vars_cnt bene_age_lt_65_cnt bene_age_gt_84_cnt bene_age_65_74_cnt bene_age_75_84_cnt bene_feml_cnt bene_male_cnt bene_race_wht_cnt bene_race_black_cnt bene_race_api_cnt bene_race_hspnc_cnt bene_race_natind_cnt bene_race_othr_cnt  bene_dual_cnt bene_ndual_cnt  total_benes
	local dem_vars_avg bene_avg_risk_scre bene_avg_age

	foreach var of local dem_vars_cnt{
		bys grouppracticepacid year: egen grp_`var'_sample = sum(`var') 
	}

	foreach var of local dem_vars_avg{
		bys grouppracticepacid year: egen grp_`var'_sample = sum(`var' * total_benes)
		replace grp_`var'_sample = grp_`var'_sample / grp_total_benes_sample 
	}

	* Merge with PCSA-level data
	merge m:1 year pcsa using "data\ZipCrosswalks\PCSA_characteristics.dta"
	drop if _merge == 2
	rename _merge merge_pcsa

	drop if npi == ""

	* Drop US territories and foreign countries
	drop if state == "PR" | state == "AS" | state == "FM" | state == "GU" | state == "MH" | state == "MP" | state == "PW" | state == "PW" | state == "VI" | state == "UM"
	keep if country == "US"

	save  "data\PartD\ProviderandDrug\PartDProvider_AllDrugCtgrs20132019.dta", replace
end


program import_providerservice_partb
	* Evaluation and Management (E/M) codes 
	* From https://oig.hhs.gov/oei/reports/oei-04-10-00180.pdf
	forvalues y=2013/2019{
		import delimited "data\PartB\ProviderandService\ProviderSrvc`y'.csv", stringcols(1) clear 
		disp `y'
		unique rndrng_npi

		* Keep only variables of interest
		keep rndrng_npi hcpcs_cd tot_srvcs tot_benes place_of_srvc

		* Impute the number of beneficiaries in under 11
		replace tot_benes = 5 if missing(tot_benes)

		* Flag the E&M code
		gen em_flag = 1 if inlist(hcpcs_cd, "99201", "99202", "99203", "99204", "99205", "99211", "99212", "99213", "99214")
		replace em_flag = 1 if inlist(hcpcs_cd, "99215", "99218", "99219", "99220", "99221", "99222", "99223", "99231", "99232")
		replace em_flag = 1 if inlist(hcpcs_cd, "99233", "99234", "99235", "99236", "99281", "99282", "99283", "99284", "99285")
		replace em_flag = 1 if inlist(hcpcs_cd, "99304", "99305", "99306", "99307", "99308", "99309", "99310", "99324", "99325")
		replace em_flag = 1 if inlist(hcpcs_cd, "99326", "99327", "99328", "99334", "99335", "99336", "99337", "99341", "99342")
		replace em_flag = 1 if inlist(hcpcs_cd, "99343", "99344", "99345", "99347", "99348", "99349", "99350", "99224", "99225")
		replace em_flag = 1 if inlist(hcpcs_cd, "99226")

		gen hh_flag = 1 if inlist(hcpcs_cd, "G0179", "G0180", "G0181", "G0182")
		
		keep if em_flag == 1 | hh_flag == 1
		

		unique rndrng_npi

		collapse (sum) tot_benes tot_srvcs, by(rndrng_npi hcpcs_cd)

		gen year=`y'

		tempfile tempdata`y'
		save `tempdata`y''
	}

	use `tempdata2013', clear
	* Combine all years into a single data set
	forvalues y=2014/2019{
		append using `tempdata`y''
	}

	******** Create flags for home health services
	gen hh_cert_flag = 0
	replace hh_cert_flag = 1 if inlist(hcpcs_cd, "G0179", "G0180")
	gen hh_sprvsn_flag = 0
	replace hh_sprvsn_flag = 1 if inlist(hcpcs_cd, "G0181", "G0182")
	gen hh_cert = tot_srvcs * hh_cert_flag 
	gen hh_sprvsn = tot_srvcs * hh_sprvsn_flag 
	gen hh_cert_benes = tot_benes * hh_cert_flag
	gen hh_sprvsn_benes = tot_benes * hh_sprvsn_flag
	gen hh = hh_cert + hh_sprvsn
	gen hh_benes = hh_cert_benes + hh_sprvsn_benes

	******** Create flags for various E/M categories
	* Office patient visits
	gen init_office_flag = 0
	replace init_office_flag = 1 if inlist(hcpcs_cd,  "99201", "99202", "99203", "99204", "99205")
	gen subsq_office_flag = 0
	replace subsq_office_flag = 1 if inlist(hcpcs_cd, "99211", "99212", "99213", "99214", "99215")
	gen office_top1_flag = 0
	replace office_top1_flag = 1 if inlist(hcpcs_cd, "99205", "99215")
	gen office_top2_flag = 0
	replace office_top2_flag = 1 if inlist(hcpcs_cd, "99204", "99214")
	gen office_top3_flag = 0 
	replace office_top3_flag = 1 if inlist(hcpcs_cd, "99203", "99213")
	gen office_top4_flag = 0
	replace office_top4_flag = 1 if inlist(hcpcs_cd, "99202", "99212")
	gen office_top5_flag = 0
	replace office_top5_flag = 1 if inlist(hcpcs_cd, "99201", "99211")

	* Observation care
	gen init_obs_flag = 0
	replace init_obs_flag = 1 if inlist(hcpcs_cd, "99218", "99219", "99220")
	gen subsq_obs_flag = 0
	replace subsq_obs_flag = 1 if inlist(hcpcs_cd, "99224", "99225", "99226")
	gen hosp_obs_top1_flag = 0
	replace hosp_obs_top1_flag = 1 if inlist(hcpcs_cd, "99220", "99226")
	gen hosp_obs_top2_flag = 0
	replace hosp_obs_top2_flag = 1 if inlist(hcpcs, "99219", "99225")
	gen hosp_obs_top3_flag = 0
	replace hosp_obs_top3_flag = 1 if inlist(hcpcs, "99218", "99224")

	* Inpatient care
	gen init_inpat_flag = 0
	replace init_inpat_flag = 1 if inlist(hcpcs_cd, "99221", "99222", "99223")
	gen subsq_inpat_flag = 0
	replace subsq_inpat_flag = 1 if inlist(hcpcs_cd, "99231", "99232", "99233")
	gen inpat_obs_flag = 0
	replace inpat_obs_flag = 1 if inlist(hcpcs_cd, "99234", "99235", "99236")
	gen hosp_inpat_top1_flag = 0
	replace hosp_inpat_top1_flag = 1 if inlist(hcpcs_cd, "99223", "99233", "99236")
	gen hosp_inpat_top2_flag = 0
	replace hosp_inpat_top2_flag = 1 if inlist(hcpcs_cd, "99222", "99232", "99235")
	gen hosp_inpat_top3_flag = 0
	replace hosp_inpat_top3_flag = 1 if inlist(hcpcs_cd, "99221", "99231", "99234")


	* Emergency
	gen emerg_flag = 0
	replace emerg_flag = 1 if inlist(hcpcs_cd, "99281", "99282", "99283", "99284", "99285")
	gen emerg_top1_flag = 0
	replace emerg_top1_flag = 1 if hcpcs_cd == "99285"
	gen emerg_top2_flag = 0
	replace emerg_top2_flag = 1 if hcpcs_cd == "99284"
	gen emerg_top3_flag = 0
	replace emerg_top3_flag = 1 if hcpcs_cd == "99283"
	gen emerg_top4_flag = 0
	replace emerg_top4_flag = 1 if hcpcs_cd == "99282"
	gen emerg_top5_flag = 0
	replace emerg_top5_flag = 1 if hcpcs_cd == "99281"


	* Nursing facility
	gen init_nurs_facil_flag = 0
	replace init_nurs_facil_flag = 1 if inlist(hcpcs_cd, "99304", "99305", "99306")
	gen subsq_nurs_facil_flag = 0
	replace subsq_nurs_facil_flag = 1 if inlist(hcpcs_cd, "99307", "99308", "99309", "99310")
	gen nurs_facil_top1_flag = 0
	replace nurs_facil_top1_flag = 1 if inlist(hcpcs_cd, "99306", "99310")
	gen nurs_facil_top2_flag = 0
	replace nurs_facil_top2_flag = 1 if inlist(hcpcs_cd, "99305", "99309")
	gen nurs_facil_top3_flag = 0
	replace nurs_facil_top3_flag = 1 if inlist(hcpcs_cd, "99304", "99308")
	gen nurs_facil_top4_flag = 0
	replace nurs_facil_top4_flag = 1 if hcpcs_cd == "99307"

	* Domiciliary 
	gen init_domic_flag = 0
	replace init_domic_flag = 1 if inlist(hcpcs_cd, "99324", "99325", "99326", "99327", "99328")
	gen subsq_domic_flag = 0
	replace subsq_domic_flag = 1 if inlist(hcpcs_cd, "99334", "99335","99336","99337")
	gen domic_top1_flag = 0
	replace domic_top1_flag = 1 if inlist(hcpcs_cd, "99328", "99337")
	gen domic_top2_flag = 0
	replace domic_top2_flag = 1 if inlist(hcpcs_cd, "99327", "99336")
	gen domic_top3_flag = 0
	replace domic_top3_flag = 1 if inlist(hcpcs_cd, "99326", "99335")
	gen domic_top4_flag = 0
	replace domic_top4_flag = 1 if inlist(hcpcs_cd, "99325", "99334")
	gen domic_top5_flag = 0
	replace domic_top5_flag = 1 if hcpcs_cd == "99324"

	* Home visits
	gen init_home_flag = 0
	replace init_home_flag = 1 if inlist(hcpcs_cd, "99341", "99342", "99343", "99344","99345")
	gen subsq_home_flag = 0
	replace subsq_home_flag = 1 if inlist(hcpcs_cd, "99347", "99348", "99349", "99350")
	gen home_top1_flag = 0
	replace home_top1_flag = 1 if inlist(hcpcs_cd, "99345", "99350")
	gen home_top2_flag = 0
	replace home_top2_flag = 1 if inlist(hcpcs_cd, "99344", "99349")
	gen home_top3_flag = 0
	replace home_top3_flag = 1 if inlist(hcpcs_cd, "99343", "99348")
	gen home_top4_flag = 0
	replace home_top4_flag = 1 if inlist(hcpcs_cd, "99342", "99347")
	gen home_top5_flag = 0
	replace home_top5_flag = 1 if hcpcs_cd == "99341"


	** Create variables for total counts of different categories of E/M codes + total beneficiaries
	* Total count of office patient visits + beneficiaries + # top codes
	gen office = tot_srvcs * (init_office_flag + subsq_office_flag)
	gen office_benes = tot_benes * (init_office_flag + subsq_office_flag)
	gen office_top1 = tot_srvcs * office_top1_flag
	gen office_top2 = tot_srvcs * office_top2_flag
	gen office_top3 = tot_srvcs * office_top3_flag
	gen office_top4 = tot_srvcs * office_top4_flag
	gen office_top5 = tot_srvcs * office_top5_flag

	* Total count of hospital observation visits + beneficiaries + # top codes
	gen hosp_obs = tot_srvcs * (init_obs_flag + subsq_obs_flag)
	gen hosp_obs_benes = tot_benes * (init_obs_flag + subsq_obs_flag)
	gen hosp_obs_top1 = tot_srvcs * hosp_obs_top1_flag
	gen hosp_obs_top2 = tot_srvcs * hosp_obs_top2_flag
	gen hosp_obs_top3 = tot_srvcs * hosp_obs_top3_flag


	* Total count of  hospital inpatient visits + beneficiaries + # top codes
	gen hosp_inpat = tot_srvcs * (init_inpat_flag + subsq_inpat_flag + inpat_obs_flag)
	gen hosp_inpat_benes = tot_benes * (init_inpat_flag + subsq_inpat_flag + inpat_obs_flag)
	gen hosp_inpat_top1 = tot_srvcs * hosp_inpat_top1_flag
	gen hosp_inpat_top2 = tot_srvcs * hosp_inpat_top2_flag
	gen hosp_inpat_top3 = tot_srvcs * hosp_inpat_top3_flag

	* Total count of emergency visits + beneficiaries
	gen emerg = tot_srvcs * emerg_flag
	gen emerg_benes = tot_benes * emerg_flag
	gen emerg_top1 = tot_srvcs * emerg_top1_flag
	gen emerg_top2 = tot_srvcs * emerg_top2_flag
	gen emerg_top3 = tot_srvcs * emerg_top3_flag
	gen emerg_top4 = tot_srvcs * emerg_top4_flag
	gen emerg_top5 = tot_srvcs * emerg_top5_flag

	* Total count of nursing facility visits + beneficiaries + # top codes
	gen nurs_facil = tot_srvcs *  (init_nurs_facil_flag + subsq_nurs_facil_flag)
	gen nurs_facil_benes = tot_benes *  (init_nurs_facil_flag + subsq_nurs_facil_flag)
	gen nurs_facil_top1 = tot_srvcs * nurs_facil_top1_flag
	gen nurs_facil_top2 = tot_srvcs * nurs_facil_top2_flag
	gen nurs_facil_top3 = tot_srvcs * nurs_facil_top3_flag
	gen nurs_facil_top4 = tot_srvcs * nurs_facil_top4_flag

	* Total count of domiciliary patient visits + beneficiaries + # top codes
	gen domic = tot_srvcs *  (init_domic_flag + subsq_domic_flag)
	gen domic_benes = tot_benes *  (init_domic_flag + subsq_domic_flag)
	gen domic_top1 = tot_srvcs * domic_top1_flag
	gen domic_top2 = tot_srvcs * domic_top2_flag
	gen domic_top3 = tot_srvcs * domic_top3_flag
	gen domic_top4 = tot_srvcs * domic_top4_flag
	gen domic_top5 = tot_srvcs * domic_top5_flag

	* Total count of home visits + beneficiaries + # top codes
	gen home = tot_srvcs * (init_home_flag + subsq_home_flag) 
	gen home_benes = tot_benes * (init_home_flag + subsq_home_flag)
	gen home_top1 = tot_srvcs * home_top1_flag
	gen home_top2 = tot_srvcs * home_top2_flag
	gen home_top3 = tot_srvcs * home_top3_flag
	gen home_top4 = tot_srvcs * home_top4_flag
	gen home_top5 = tot_srvcs * home_top5_flag

	* Count the total number of E/M codes being billed per NPI
	egen total_emcodes = rowtotal(office hosp_obs hosp_inpat emerg nurs_facil domic home)
	egen total_emcodes_benes = rowtotal(office_benes hosp_obs_benes hosp_inpat_benes emerg_benes nurs_facil_benes domic_benes home_benes)

	drop *flag 
	rename rndrng_npi npi
	collapse (sum) office* hosp_obs* hosp_inpat* emerg* nurs_facil* domic* home* total* hh*, by(npi year)


	save "data\PartB\ProviderandService\ProviderSrvc_EMCodeCounts.dta", replace
end


* Obtain prices paid to physicians for Part B codes aggregated at the state level (note that prices are different for place of service == "facility" vs == "office")
program get_partb_prices	
	* Get the state-level yearly prices for the medium-intensity codes only
	local medium_em_codes 99203 99213 99219 99225 99222 99232 99235 99283 99305 99308 99326 99335 99343 99348

	forvalues y=2013/2019{
		import delimited "data\PartB\Service\Service`y'.csv", clear
		keep if rndrng_prvdr_geo_lvl == "State"
		keep rndrng_prvdr_geo_desc hcpcs_cd place_of_srvc avg_mdcr_alowd_amt	

		* Convert state names to abbreviation
		statastates, name(rndrng_prvdr_geo_desc)
		keep if _merge == 3
		drop state_fips rndrng_prvdr_geo_desc _merge

		rename (state_abbrev avg_mdcr_alowd_amt) (state state_avg_mdcr_alowd_amt) 

		foreach code of local medium_em_codes{
			gen state_allowed_amt_`code' = state_avg_mdcr_alowd_amt if hcpcs_cd == "`code'" 
			egen  state_avg_alowd_amt_`code' = total(state_allowed_amt_`code'), by(state place_of_srvc)

			drop state_allowed_amt_`code'
		}
	
		collapse (mean) state_avg_alowd_amt*, by(state place_of_srvc)

		gen year = `y'

		tempfile tempdata`y'
		save `tempdata`y''
	}
	
	use `tempdata2013', clear
	* Combine all years into a single data set
	forvalues y=2014/2019{
		append using `tempdata`y''
	}

	save "data\PartB\Service\StateLevel_MediumSrvcPrices.dta", replace
end


**************************************************************************
*** For each E/M codes, construct an "into money" measure for each NPI
* 1. To do this, we must first obtain price for each service by aggregating the payments received by all NPIs for that service code in a given geographic unit (e.g. HRR/state)
* 2. Using these prices, we then compute the difference between the NPI's actual revenue from E/M codes versus her hypothetical revenue if she coded everything at the "mid-intensity" (e.g. coded all E/M new patient office visits at 99203)
* NOTE: prices are different if the service took place at an office vs a facility 
program create_partb_intomoney_measure
	forvalues y=2013/2019{
		disp `y'
		import delimited "data\PartB\ProviderandService\ProviderSrvc`y'.csv", stringcols(1) clear 

		* Flag the E&M code
		gen em_flag = 1 if inlist(hcpcs_cd, "99201", "99202", "99203", "99204", "99205", "99211", "99212", "99213", "99214")
		replace em_flag = 1 if inlist(hcpcs_cd, "99215", "99218", "99219", "99220", "99221", "99222", "99223", "99231", "99232")
		replace em_flag = 1 if inlist(hcpcs_cd, "99233", "99234", "99235", "99236", "99281", "99282", "99283", "99284", "99285")
		replace em_flag = 1 if inlist(hcpcs_cd, "99304", "99305", "99306", "99307", "99308", "99309", "99310", "99324", "99325")
		replace em_flag = 1 if inlist(hcpcs_cd, "99326", "99327", "99328", "99334", "99335", "99336", "99337", "99341", "99342")
		replace em_flag = 1 if inlist(hcpcs_cd, "99343", "99344", "99345", "99347", "99348", "99349", "99350", "99224", "99225")
		replace em_flag = 1 if inlist(hcpcs_cd, "99226")
		keep if em_flag == 1

		* Keep only variables of interest
		keep rndrng_npi rndrng_prvdr_state_abrvtn hcpcs_cd tot_srvcs place_of_srvc avg_mdcr_alowd_amt
		rename (rndrng_npi rndrng_prvdr_state_abrvtn) (npi state)

		gen year = `y'
		* Merge with state-level prices for medium-intensity services
		merge m:1 state year place_of_srvc using "data\PartB\Service\StateLevel_MediumSrvcPrices.dta"		
		* Merge results: Matched 2627974, not matched from master 9976 (physicians from outside of the 50 states + DC). 
		keep if _merge == 3
		drop _merge


		* Office patient visits
		gen init_office_flag = 0
		replace init_office_flag = 1 if inlist(hcpcs_cd,  "99201", "99202", "99203", "99204", "99205")
		gen subsq_office_flag = 0
		replace subsq_office_flag = 1 if inlist(hcpcs_cd, "99211", "99212", "99213", "99214", "99215")

		* Observation care
		gen init_obs_flag = 0
		replace init_obs_flag = 1 if inlist(hcpcs_cd, "99218", "99219", "99220")
		gen subsq_obs_flag = 0
		replace subsq_obs_flag = 1 if inlist(hcpcs_cd, "99224", "99225", "99226")

		* Inpatient care
		gen init_inpat_flag = 0
		replace init_inpat_flag = 1 if inlist(hcpcs_cd, "99221", "99222", "99223")
		gen subsq_inpat_flag = 0
		replace subsq_inpat_flag = 1 if inlist(hcpcs_cd, "99231", "99232", "99233")
		gen inpat_obs_flag = 0
		replace inpat_obs_flag = 1 if inlist(hcpcs_cd, "99234", "99235", "99236")

		* Emergency
		gen emerg_flag = 0
		replace emerg_flag = 1 if inlist(hcpcs_cd, "99281", "99282", "99283", "99284", "99285")

		* Nursing facility
		gen init_nurs_facil_flag = 0
		replace init_nurs_facil_flag = 1 if inlist(hcpcs_cd, "99304", "99305", "99306")
		gen subsq_nurs_facil_flag = 0
		replace subsq_nurs_facil_flag = 1 if inlist(hcpcs_cd, "99307", "99308", "99309", "99310")

		* Domiciliary 
		gen init_domic_flag = 0
		replace init_domic_flag = 1 if inlist(hcpcs_cd, "99324","99325","99326","99327","99328")
		gen subsq_domic_flag = 0
		replace subsq_domic_flag = 1 if inlist(hcpcs_cd, "99334", "99335","99336","99337")

		* Home visits
		gen init_home_flag = 0
		replace init_home_flag = 1 if inlist(hcpcs_cd, "99341", "99342", "99343", "99344","99345")
		gen subsq_home_flag = 0
		replace subsq_home_flag = 1 if inlist(hcpcs_cd, "99347", "99348", "99349", "99350")

		* Generate variables for actual revenues associated with the different categories
		* Generate variable for counterfactual revenues associated with the different categories, if the physician had coded at medium intensity
		gen office_rev_actual = (init_office_flag + subsq_office_flag)*(tot_srvcs * avg_mdcr_alowd_amt)
		gen office_rev_counterfactual = init_office_flag*tot_srvcs*state_avg_alowd_amt_99203 + subsq_office_flag*tot_srvcs*state_avg_alowd_amt_99213

		gen hosp_obs_rev_actual = (init_obs_flag + subsq_obs_flag)*(tot_srvcs * avg_mdcr_alowd_amt)
		gen hosp_obs_rev_counterfactual = init_obs_flag*tot_srvcs*state_avg_alowd_amt_99219 + subsq_obs_flag*tot_srvcs*state_avg_alowd_amt_99225
		
		gen hosp_inpat_rev_actual = (init_inpat_flag + subsq_inpat_flag + inpat_obs_flag)*(tot_srvcs * avg_mdcr_alowd_amt)
		gen hosp_inpat_rev_counterfactual = init_inpat_flag*tot_srvcs*state_avg_alowd_amt_99222 + subsq_inpat_flag*tot_srvcs*state_avg_alowd_amt_99232 + inpat_obs_flag*tot_srvcs*state_avg_alowd_amt_99235
		
		gen emerg_rev_actual = emerg_flag*tot_srvcs*avg_mdcr_alowd_amt
		gen emerg_rev_counterfactual = emerg_flag*tot_srvcs*state_avg_alowd_amt_99283

		gen nurs_facil_rev_actual = (init_nurs_facil_flag + subsq_nurs_facil_flag)*(tot_srvcs * avg_mdcr_alowd_amt)
		gen nurs_facil_rev_counterfactual = init_nurs_facil_flag*tot_srvcs*state_avg_alowd_amt_99305 + subsq_nurs_facil_flag*tot_srvcs*state_avg_alowd_amt_99308
		
		gen domic_rev_actual = (init_domic_flag + subsq_domic_flag)*(tot_srvcs * avg_mdcr_alowd_amt)
		gen domic_rev_counterfactual = init_domic_flag*tot_srvcs*state_avg_alowd_amt_99326 + subsq_domic_flag*tot_srvcs*state_avg_alowd_amt_99335
		
		gen home_rev_actual = (init_home_flag + subsq_home_flag)*(tot_srvcs * avg_mdcr_alowd_amt)
		gen home_rev_counterfactual = init_home_flag*tot_srvcs*state_avg_alowd_amt_99343 + subsq_home_flag*tot_srvcs*state_avg_alowd_amt_99348

		* Sum up total revenues across the E/M categories
		collapse (sum) *rev_actual *rev_counterfactual (firstnm) year, by(npi)
	
		egen total_rev_actual = rowtotal(office_rev_actual hosp_obs_rev_actual hosp_inpat_rev_actual  emerg_rev_actual nurs_facil_rev_actual  domic_rev_actual home_rev_actual)
		egen total_rev_counterfactual = rowtotal(office_rev_counterfactual hosp_obs_rev_counterfactual hosp_inpat_rev_counterfactual  emerg_rev_counterfactual nurs_facil_rev_counterfactual  domic_rev_counterfactual home_rev_counterfactual)

		gen rev_delta = total_rev_actual - total_rev_counterfactual 

		tempfile tempdata`y'
		save `tempdata`y''
	}

	use `tempdata2013', clear
	* Combine all years into a single data set
	forvalues y=2014/2019{
		append using `tempdata`y''
	}
		
	save "data\PartB\EMRevenues.dta", replace

end

* Get provider-level characteristics
program import_provider_partb
	** Begin combining the provider-level data
	forvalues y=2013/2019{
		import delimited "data\PartB\Provider\Provider`y'.csv", stringcols(1 6) clear

		keep rndrng_npi rndrng_prvdr_last_org_name rndrng_prvdr_first_name rndrng_prvdr_crdntls rndrng_prvdr_gndr rndrng_prvdr_ent_cd rndrng_prvdr_state_abrvtn rndrng_prvdr_zip5 rndrng_prvdr_ruca rndrng_prvdr_cntry rndrng_prvdr_type tot_benes tot_srvcs drug_tot_srvcs med_tot_srvcs bene*
		drop bene_cc*

		* Impute # beneficiaries that's been censored
		replace tot_benes = 5 if missing(tot_benes) & tot_srvcs != 0 

		rename (rndrng_npi rndrng_prvdr_last_org_name rndrng_prvdr_first_name rndrng_prvdr_crdntls rndrng_prvdr_gndr rndrng_prvdr_ent_cd rndrng_prvdr_state_abrvtn rndrng_prvdr_zip5 rndrng_prvdr_ruca rndrng_prvdr_cntry rndrng_prvdr_type) (npi last_org_name first_name credentials gender entity_code state zip5 ruca country specialty )

		gen year = `y'

		disp `y'
		unique npi

		tempfile tempdata`y'
		save `tempdata`y''
	}

	use `tempdata2013', clear
	* Combine all years into a single data set
	forvalues y=2014/2019{
		append using `tempdata`y''
	}

	tostring zip5, force replace

	* Rename some specialties to match each other
	replace specialty = "Allergy/Immunology" if specialty == "Allergy/ Immunology"
	replace specialty = "Anesthesiologist Assistants" if specialty == "Anesthesiology Assistant" 
	replace specialty = "Behavior Analyst" if specialty == "Behavioral Analyst"
	replace specialty = "Cardiology" if specialty == "Cardiovascular Disease (Cardiology)"
	replace specialty = "Case Management" if specialty == "Case Manager/Care Coordinator"
	replace specialty = "Clinical Cardiac Electrophysiology" if specialty == "Clinical Cardiatric Electrophysiology"
	replace specialty = "Gynecological Oncology" if specialty == "Gynecological/Oncology"
	replace specialty = "Hematology/Oncology" if specialty == "Hematology-Oncology"
	replace specialty = "Hematology/Oncology" if specialty == "Hematology"
	replace specialty = "Independent Diagnostic Testing Facility" if specialty == "Independent Diagnostic Testing Facility (IDTF)"
	replace specialty = "Mammography Center" if specialty == "Mammographic Screening Center"
	replace specialty = "Mass Immunization Roster Biller" if specialty == "Mass Immunizer Roster Biller"
	replace specialty = "Medical Genetics" if specialty == "Medical Genetics, Ph.D. Medical Genetics" 
	replace specialty = "Obstetrics/Gynecology" if specialty == "Obstetrics & Gynecology"
	replace specialty = "Oral Surgery (dentists only)" if specialty == "Oral Surgery (Dentist only)"
	replace specialty = "Oral Surgery (dentists only)" if specialty == "Oral Surgery (Dentists only)"
	replace specialty = "Orthopedic Surgery" if specialty == "Orthopaedic Surgery"
	replace specialty = "Physical Medicine and Rehabilitation" if specialty == "Physical Medicine & Rehabilitation"
	replace specialty = "Public Health Welfare Agency" if specialty == "Public Health or Welfare Agency"
	replace specialty = "Registered Dietician/Nutrition Professional" if specialty == "Registered Dietitian or Nutrition Professional"
	replace specialty = "Thoracic Surgery" if specialty == "Thoracic Surgery (Cardiothoracic Vascular Surgery)"
	replace specialty = "Voluntary Health or Charitable Agency" if specialty == "Voluntary Health or Charitable Agencies"
	
	* Replace specialty of NPI with the most common specialty
	bysort npi specialty : gen specialty_count = _N
	bys npi (specialty_count): replace specialty = specialty[_N]
	drop specialty_count


	* Create indicator for primary care physicians
	gen primary_care = (specialty == "Pediatric Medicine" | specialty == "Family Practice" | specialty == "Geriatric Medicine" | specialty == "General Practice" | specialty == "Internal Medicine" | specialty == "Obstetrics/Gynecology" | specialty == "Family Medicine")

	* Keep only primary care docs
	keep if primary_care == 1


	* Merge the zipcode-PCSA crosswalk data from Dartmouth Atlas
	merge m:1 zip5 using "data\ZipCrosswalks\zippcsa.dta"
	drop if _merge == 2
	rename _merge merge_zippcsa_xwalks

	gen urban = 0
	replace urban = 1 if ruca == 1 | ruca == float(1.1) | ruca == 2 | ruca == float(2.1) | ruca == 3 | ruca == float(4.1) | ruca == float(5.1) | ruca == float(7.1) | ruca == float(8.1) | ruca == float(10.1)

	gen male = (gender == "M")
	* Use the last-reported gender identity as the gender identity
	bys npi (year): replace male = male[_N]
	drop gender


	* Merge with data on E/M code counts
	merge m:1 npi year using "data\PartB\ProviderandService\ProviderSrvc_EMCodeCounts.dta"
	drop if _merge == 2
	rename _merge merge_emcode_counts

	rename (tot_benes tot_srvcs drug_tot_srvcs med_tot_srvcs) (total_benes total_all_srvcs total_drug_srvcs total_med_srvcs)

	* Merge with the Part B counterfactual revenues data set
	merge 1:1 npi year using "data\PartB\EMRevenues.dta"
	drop if _merge == 2
	drop _merge

	gen partb_into_money_measure = rev_delta/total_emcodes

	* Merge with Health IT EHR adoption data
	merge m:1 npi using "data\HealthIT\healthit_attest_cleaned.dta"
	drop if _merge == 2
	rename _merge merge_healthit
	replace ehr_adopt = 0 if missing(ehr_adopt)
	replace ehr_adopt_pre12 = 0 if missing(ehr_adopt_pre12)
	replace ehr_adopt_post12 = 0 if missing(ehr_adopt_post12)

	* Merge with MIPS score data
	merge m:1 npi using "data\DAC_NationalDownloadableFiles\mips_perf20192020_individual.dta"
	drop if _merge == 2
	rename _merge merge_mips

	* Merge with DAC data
	merge 1:m npi year using "data\DAC_NationalDownloadableFiles\DAC_apm_grp_linkage.dta"
	drop if _merge == 2
	rename _merge merge_dac_apm_grp

	* Merge with group-level data
	merge m:1 grouppracticepacid year using  "data\DAC_NationalDownloadableFiles\group_characteristics.dta"
	drop if _merge == 2
	rename _merge merge_grp

	* Create group-level demographic variables 
	local dem_vars_cnt bene_age_lt_65_cnt bene_age_gt_84_cnt bene_age_65_74_cnt bene_age_75_84_cnt bene_feml_cnt bene_male_cnt bene_race_wht_cnt bene_race_black_cnt bene_race_api_cnt bene_race_hspnc_cnt bene_race_natind_cnt bene_race_othr_cnt  bene_dual_cnt bene_ndual_cnt  total_benes
	local dem_vars_avg bene_avg_risk_scre bene_avg_age

	foreach var of local dem_vars_cnt{
		bys grouppracticepacid year: egen grp_`var'_sample = sum(`var') 
	}

	foreach var of local dem_vars_avg{
		bys grouppracticepacid year: egen grp_`var'_sample = sum(`var' * total_benes)
		replace grp_`var'_sample = grp_`var'_sample / grp_total_benes_sample 
	}

	* Merge with PCSA-level data
	merge m:1 year pcsa using "data\ZipCrosswalks\PCSA_characteristics.dta"
	drop if _merge == 2
	rename _merge merge_pcsa

	drop if npi == ""

	* Drop US territories and foreign countries
	drop if state == "PR" | state == "AS" | state == "FM" | state == "GU" | state == "MH" | state == "MP" | state == "PW" | state == "PW" | state == "VI" | state == "UM"
	keep if country == "US"


	save  "data\PartB\ProviderandService\PartBProvider_AllServiceCounts20132019.dta", replace
end



main

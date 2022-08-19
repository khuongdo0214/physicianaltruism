* Match each NDC to ATC classes

clear all
set more off
cd "C:\Users\khuon\Dropbox (Harvard University)\Physician Altruism\Altruism\"
* cd "/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Code"

*** EXTERNALS ****
global data_path "../data"
global temp_path "../temp"
global figs_path "../output/figures"
global tables_path "../output/tables"

program main
	* get_ndc_from_pde
	* clean_up_ndc_atc_map
	* get_bn_gnn_from_pde
	* merge_ndc_drug_names
end


* Clean up the NBER's PDE files to collect a list of all NDCs that appear in each year from 2013 to 2017
program get_ndc_from_pde
	forvalues y=2013/2017{
		use "/disk/aging/partd/cutler-DUA28717/20pct/pde/`y'/pde`y'.dta", clear
		rename prdsrvid ndc
		keep ndc 

		* Remove duplicates
		duplicates drop

		* Have to save as .csv files in order for the query .R script to read
		outsheet ndc using "/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/PDE/ndc_from_pde`y'.csv", comma
	}
end

* Clean up the pre-made ndc_atc_map files, keeping only the ndc column and combining all ATCs for an NDC into a single row
program clean_up_ndc_atc_map
	forvalues y=2013/2017{
		import delimited "data\PartD\ndc_map\ndc_atc_map`y'.csv", stringcols(1 2) clear
		tempfile tempdata`y'
		save `tempdata`y''
	}
	use `tempdata2013', clear
	* Combine all years into a single data set
	forvalues y=2014/2017{
		append using `tempdata`y''
	}
	duplicates drop ndc atc4, force


	* Concatenate all ATC4 corresponding to NDC
	sort ndc, stable
	by ndc: gen all_atc4 = atc4[1]
	by ndc: replace all_atc4 = all_atc4[_n-1] + "," + atc4 if _n > 1
	by ndc: replace all_atc4 = all_atc4[_N]

	drop atc4 rxcui atc4_name

	duplicates drop 

	******************************************************************
	* Put the NDCs into drug categories based on the associatd ATC4s
	******************************************************************
	* Create lists of ATC4s corresponding to the categories of drugs we are interested in
	local benzodiapezine_derivatives "N03AE N05BA N05CD"
	local benzodiapezine_rel_drugs "N05CF"
	* These are centrally acting sympathomimetics, and seemingly include all FDA-approved oral drugs for ADHD/narcolepsy
	local adhd_narcolepsy_drugs "N06BA" 
	local urinary_freq_incontinence_drugs "G04BD"
	local weightloss_drugs "A08A"
	local barbiturates "N05CA N05CB"
	local all_hypnotics_sedatives "N05C"
	local glucocorticoids "H02AB R03BA"
	* Plain corticosteroids (no combinations with antiseptics, antibiotics, etc.)
	local corticosteroids_plain "D07A" 

	* Create lists of ATC4s corresponding to the categories of drugs we are interested in
	local benzodiapezine_derivatives "N03AE N05BA N05CD"
	local benzodiapezine_rel_drugs "N05CF"
	* These are centrally acting sympathomimetics, and seemingly include all FDA-approved oral drugs for ADHD/narcolepsy
	local adhd_narcolepsy_drugs "N06BA" 
	local urinary_freq_incontinence_drugs "G04BD"
	local weightloss_drugs "A08A"
	local barbiturates "N05CA N05CB"
	local all_hypnotics_sedatives "N05C"
	local glucocorticoids "H02AB R03BA"
	* Plain corticosteroids (no combinations with antiseptics, antibiotics, etc.)
	local corticosteroids_plain "D07A" 


	gen benzo_derivs_flag = 0
	foreach atc of local benzodiapezine_derivatives{
		replace benzo_derivs_flag = 1 if strpos(all_atc4,"`atc'") == 1
	}

	gen benzo_related_flag = 0
	foreach atc of local benzodiapezine_rel_drugs{
		replace benzo_related_flag = 1 if strpos(all_atc4,"`atc'") == 1
	}

	* Combine benzodiapezine derivatives with benzodiazepine related drugs into a single category
	gen benzo_all_flag = 0
	replace benzo_all_flag = benzo_derivs_flag + benzo_related_flag
	replace benzo_all_flag = 1 if benzo_all_flag == 2

	gen stimulant_flag = 0
	foreach atc of local adhd_narcolepsy_drugs{
		replace stimulant_flag = 1 if strpos(all_atc4,"`atc'") == 1
	}

	gen bladder_relax_flag = 0
	foreach atc of local urinary_freq_incontinence_drugs{
		replace bladder_relax_flag = 1 if strpos(all_atc4,"`atc'") == 1
	}

	gen wtloss_drug_flag = 0
	foreach atc of local weightloss_drugs{
		replace wtloss_drug_flag = 1 if strpos(all_atc4,"`atc'") == 1
	}

	gen barbiturate_flag = 0
	foreach atc of local barbiturates{
		replace barbiturate_flag = 1 if strpos(all_atc4,"`atc'") == 1
	}
	****************************************************************************
	* replace barbiturate_flag = 1 if strpos(genericname,"butabarbital") == 1
	****************************************************************************

	gen hypnotic_flag = 0
	foreach atc of local all_hypnotics_sedatives{
		replace hypnotic_flag = 1 if strpos(all_atc4,"`atc'") == 1
	}

	gen glucocorticoid_flag = 0
	foreach atc of local glucocorticoids{
		replace glucocorticoid_flag = 1 if strpos(all_atc4,"`atc'") == 1
	}

	gen plain_steroid_flag = 0
	foreach atc of local corticosteroids_plain{
		replace plain_steroid_flag = 1 if strpos(all_atc4,"`atc'") == 1
	}
	
	************************************************************************************
	* replace plain_steroid_flag = 1 if genericname == "fluticasone propion/salmeterol"
	************************************************************************************

	save "data\PartD\ndc_map\ndc_allatc4_categories_map.dta", replace
end


* Clean up the NBER's PDE files to collect a list of all NDCs that appear in each year from 2013 to 2017 as well as the associated brand and generic names
program get_bn_gnn_from_pde
	forvalues y=2013/2017{
		use "/disk/aging/partd/cutler-DUA28717/20pct/pde/`y'/pde`y'.dta", clear
		rename prdsrvid ndc
		keep ndc bn gnn

		* Remove duplicates
		duplicates drop

		tempfile tempdata`y'
		save `tempdata`y''
	}
	use `tempdata2013', clear
	* Combine all years into a single data set
	forvalues y=2014/2017{
		append using `tempdata`y''
	}

	duplicates drop ndc, force
	save "/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/PDE/ndc_bn_gnn_from_pdefiles.dta", replace
end


* Merge the NDC - ATC4 - drug categories map with variables on generic and brand names
program merge_ndc_drug_names
	use "data\PartD\ndc_map\ndc_allatc4_categories_map.dta", clear
	merge 1:1 ndc using "data\PartD\ndc_map\ndc_bn_gnn_from_pdefiles.dta" 
	* The match is 100%, as expected as we created both data sets from the same PDE files

	***** WORK IN PROGRESS *******
	* Manually correct for some classiication failures... 
	replace barbiturate_flag = 1 if strpos(gnn,"BUTABARBITAL") == 1
	replace plain_steroid_flag = 1 if strpos(gnn,"FLUTICASONE PROPIONATE")
	replace plain_steroid_flag = 1 if strpos(gnn,"SALMETEROL")
	

	order ndc all_atc4 bn gnn
	rename bn brnd_name
	rename gn gnrc_name 
	save "data\PartD\ndc_map\ndc_allatc4_categories_bnn_gn_map.dta", replace
end


*EXECUTE ANALYSIS
main
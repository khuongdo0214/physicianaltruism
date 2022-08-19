* Preprocss the 20% claims data. 

forvalues y=2010/2018{
	display "`y'"
	use "/disk/aging/medicare/data/harm/20pct/car/`y'/carl`y'.dta", clear
	keep prf_npi tax_num
	rename (prf_npi tax_num) (npi tin)

	drop if missing(npi)

	* Count the number of times an NPI bills under a given TIN
	disp "Counting # times NPI bills under a TIN"
	bys npi tin: gen n = _N
	duplicates drop

	* Selecting the top 2 TINs per npi
	gsort npi -n
	by npi: keep if _n <= 2 

	save "/homes/nber/lucasdo-dua28717/cutler-DUA28717/lucasdo-dua28717/Altruism/Data/TIN-PRACID-xwalk/claims20/carl`y'_npitin.dta", replace
}

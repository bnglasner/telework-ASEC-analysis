clear
set more off


global data_path = ""
global output_path = ""


* Open data set
import delimited "$data_path/metro_housing_and_telework_rf.csv", delimiter(comma) varnames(1) clear

gen med_home_value_2019_thou = median_home_value_2019/1000


* Compute fully/hybrid ratios 
gen fully_hybrid_ratio = fully_remote_share_2022/hybrid_remote_share_2022


* Percentile of ratio
egen sumwt = sum(pop_share_2022)
gen rsum = sum(pop_share_2022)
gen fully_hybrid_ratio_ptile = 100*rsum/sumwt


* Scatterplot 
twoway (scatter d_median_home_value_pct fully_hybrid_ratio [aw = pop_share_2022], xlabel(, grid) msymbol(circle_hollow)) ///
		(lfit d_median_home_value_pct fully_hybrid_ratio [aw = pop_share_2022], lpattern(dash)), ///
		legend(off) ///
		xtitle("Ratio of Fully Remote to Hybrid Workers") ///
		ytitle("Change in Median Home Value (%)") ///
		plotregion(color(white)) ///
		graphregion(color(white)) 
		
graph export "$output_path/fully_hybrid_ratio_vs_homeval_change.png", replace


twoway (scatter fully_hybrid_ratio med_home_value_2019_thou [aw = pop_share_2022], xlabel(, grid) msymbol(circle_hollow)) ///
		(lfit fully_hybrid_ratio med_home_value_2019_thou [aw = pop_share_2022], lpattern(dash)), ///
		legend(off) ///
		xtitle("Median Home Value in 2019 (Thousands USD)") ///
		ytitle("Ratio of Fully Remote to Hybrid Workers in 2022") ///
		plotregion(color(white)) ///
		graphregion(color(white)) 
		
graph export "$output_path/fully_hybrid_ratio_vs_homeval.png", replace
		
		

* Fully remote share conditional on any remote work
gen full_remote_share_condit_2022 = 100*fully_remote_2022/(fully_remote_2022 + hybrid_remote_2022)
gen fully_remote_resid = fully_remote_2022 - fully_remote_pred
gen fully_remote_resid_share_condit = 100*fully_remote_resid/(fully_remote_2022 + hybrid_remote_2022)
gen full_remote_share_pred_condit = 100*fully_remote_pred/(fully_remote_2022 + hybrid_remote_2022)


twoway (scatter full_remote_share_condit_2022 med_home_value_2019_thou [aw = pop_share_2022], xlabel(, grid) msymbol(circle_hollow)) ///
		(lfit full_remote_share_condit_2022 med_home_value_2019_thou [aw = pop_share_2022], lpattern(dash)), ///
		legend(off) ///
		xtitle("Median Home Value in 2019 (Thousands USD)", size(small)) ///
		ytitle("Share of Remote Workers Who are Fully Remote", size(small)) ///
		plotregion(color(white)) ///
		graphregion(color(white)) 
		
		
graph export "$output_path/fully_share_of_remote_vs_2019_homeval.png", replace
		
		
twoway (scatter d_median_home_value_pct full_remote_share_condit_2022 [aw = pop_share_2022], xlabel(, grid) msymbol(circle_hollow)) ///
		(lfit d_median_home_value_pct full_remote_share_condit_2022 [aw = pop_share_2022], lpattern(dash)), ///
		legend(off) ///
		xtitle("Share of Remote Workers Who are Fully Remote") ///
		ytitle("Change in Median Home Value (%)") ///
		plotregion(color(white)) ///
		graphregion(color(white)) 		
		
		
twoway (scatter fully_remote_resid_share_condit med_home_value_2019_thou [aw = pop_share_2022] if fully_remote_resid_share_condit > -100, xlabel(, grid) msymbol(circle_hollow)) ///
		(lfit fully_remote_resid_share_condit med_home_value_2019_thou [aw = pop_share_2022] if fully_remote_resid_share_condit > -100, lpattern(dash)), ///
		legend(off) ///
		xtitle("Median Home Value in 2019 (Thousands USD)", size(small)) ///
		ytitle("Ratio of Fully Remote Residual to All Remote Workers in 2022", size(small)) ///
		plotregion(color(white)) ///
		graphregion(color(white)) 
		
		
twoway (scatter full_remote_share_pred_condit med_home_value_2019_thou [aw = pop_share_2022] if full_remote_share_pred_condit < 100, xlabel(, grid) msymbol(circle_hollow)) ///
		(lfit full_remote_share_pred_condit med_home_value_2019_thou [aw = pop_share_2022] if full_remote_share_pred_condit < 100, lpattern(dash)), ///
		legend(off) ///
		xtitle("Median Home Value in 2019 (Thousands USD)", size(small)) ///
		ytitle("Ratio of Predicted Fully Remote to All Remote Workers in 2022", size(small)) ///
		plotregion(color(white)) ///
		graphregion(color(white)) 


	

* Regressions: Dependent variable is percent change in home values
* Indep vars:
* 1. Any remote
* 2. Fully remote (actual), hybrid remote (actual)
* 3. Fully remote (predicted), hybrid remote (predicted)
* 4. Fully predicted, fully residual, hybrid predicted, hybrid residual 

eststo clear
quietly eststo: reg d_median_home_value_pct any_remote_share_2022 [aw = pop_share_2022]
quietly eststo: reg d_median_home_value_pct fully_remote_share_2022 hybrid_remote_share_2022 [aw = pop_share_2022]
quietly eststo: reg d_median_home_value_pct fully_remote_share_pred hybrid_remote_share_pred [aw = pop_share_2022]
quietly eststo: reg d_median_home_value_pct fully_remote_share_pred fully_remote_share_diff hybrid_remote_share_pred hybrid_remote_share_diff [aw = pop_share_2022]
quietly eststo: reg d_median_home_value_pct fully_hybrid_ratio [aw = pop_share_2022]


esttab using "$output_path/telework_regression_results.csv", se b(2) r2 ar2 star varwidth(25) replace



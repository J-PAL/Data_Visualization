************************************************
* Title: LaTeX-Stata Integration - Example
* 
* Date Created: Oct 12th, 2023
* Date Updated: Jan 5th, 2023
* Purpose: This exercise demonstrates how to create a final, reproducible document through Stata-Latex integration
* Author: Diana Horvath
*************************************************

*************************************************
*			Set Stata Environment				*
*************************************************
clear all
set more off

* Change working directory: 
global path "[SET DIRECTORY HERE]" 

	* Set sub-directories 
	global data "$path/data"
	global figures "$path/figures"
	global output "$path/output"

***************************************************************
*  			STEP 1: DATA CLEANING  	     		  			  *
***************************************************************
use "$data/baroda_0102_1obs.dta", clear
drop if pre_tot == . | post_tot == . 

* Dummy var for grades: 
gen gr3 = (std == 3) 
gen gr4 = (std == 4) 

* Clean Variable Labels:
label variable bal "Treatment"
label variable female "Female"
label variable bigschool "Big School"
label variable std "Grade"
label variable gr3 "Grade 3"
label variable gr4 "Grade 4"
label variable pre_tot "Pre-test Score"
label variable post_tot "Post-test Score"



***************************************************************
*  		STEP 2: EXPORTING 1 TABLE AT A TIME USING ESTTAB      *
***************************************************************

***************** Table 1. Regression Results *****************

* Run regression and store estimates
reg post_tot bal 
est sto reg1

* Export with pre-formatted Latex code
esttab reg1 using "$output/reg1.tex",  ///
	label title (Regression Results) replace
	
	
***************************************************************
* 	 	STEP 3A: STORING ESTIMATES FOR A FULL REPORT     	  *
***************************************************************

**************** Table 2. Descriptive Statistics **************

* Store average share in locals 
local stats pre_tot post_tot female gr3 gr4 bigschool 

foreach var in `stats' {
	// treatment
	sum `var' if bal == 1 
	local `var'_count_t = `r(N)'  
	local `var'_mean_t = round(`r(mean)', .01) 
	// control
	sum `var' if bal == 0 
	local `var'_count_c = `r(N)'  
	local `var'_mean_c = round(`r(mean)', .01) 
}




*** Table 3. Regression Results (with and without covariates) ***

* Regression without covariates
reg post_tot bal 

* Store regression results in locals:
local b_reg1 = round(_b[bal],0.001) 
local N_reg1 = e(N)
local R_reg1 = round(e(r2), 0.001)
local se_reg1 = round(_se[bal], 0.001)
	
* Calculate P-value manually and store in local:
local p = 2 * ttail(e(df_r), abs(_b[bal]/_se[bal]))
local p_reg1 = string(round(`p', 0.001), "%3.2f" )

* Regression with covariates
local controls = "female bigschool std"
reg post_tot bal `controls'

* Store regression results in locals:
	// Treatment 
	local b_reg2 = round(_b[bal],0.001) 
	local N_reg2 = e(N)
	local R_reg2 = round(e(r2), 0.001)
	local se_reg2 = round(_se[bal], 0.001)
	local p = 2 * ttail(e(df_r), abs(_b[bal]/_se[bal]))
	local p_reg2 = string(round(`p', 0.001), "%3.2f" ) 	
	di "`p_reg2'"
	
	// Covariates
	foreach var in `controls' {
		local b_reg2_`var' = round(_b[`var'],0.001) 
		local se_reg2_`var' = round(_se[`var'],0.001) 
		local p = 2 * ttail(e(df_r), abs(_b[`var']/_se[`var']))
		local p_reg2_`var' = string(round(`p', 0.001), "%3.2f" ) 
	}
	


**** Figure 1. Gains in test scores, by pre-test score group *****

* Calculate difference in scores and bin by 10 groups
gen diff_scores = post_tot - pre_tot
gen bins  = 10 * floor(pre_tot/10)
gen _freq = 1

preserve
collapse (mean) pre_tot diff_scores (sum) _freq, by(bins bal)


* Scatterplot
twoway (scatter diff_scores pre_tot  if bal == 0 [fw = _freq],        /// 
     mcolor("228 92 36")  msymbol(circle_hollow) msize(vsmall))       ///
     (scatter diff_scores pre_tot if bal == 1  [fw = _freq],          /// 
     mcolor("44 172 156") msymbol(circle_hollow) msize(vsmall)),      ///
	 legend(order(1 "Non-Balsakhi" 2 "Balsakhi") size(small)) 	      ///								
	 xlabel(, grid gstyle(linestyle(color(white))) labsize(small))    ///
	 ysc(r(-10 20)) ylabel(-10 "-10"  0 "0" 10 "10" 20 "20")	      ///
	 ylabel(, grid gstyle(linestyle(color(white))) labsize(small)     /// 
	 angle(0)) plotregion(color(white))  			   			   	  ///
	 graphregion(fcolor(white) ifcolor(white) color(white) 		   	  ///
	 icolor(white)) ytitle("Average Test Score Improvement (in Bin)", ///
	 margin(small) size(small)) 									  ///
	 xtitle("Pre-test Scores" "(Scaled by Number of Students in Bin)", ///
	 margin(small) size(small))			
restore

* Export figure: 
graph export "$figures/figure_1.png", as(png) replace
	


***************************************************************
*  		STEP 3B: EXPORT STORED ESTIMATES TO TEX FILE	      *
***************************************************************


	* Program for adding a new line in your tex file: 
	cap program drop line
	program define line
	file write Report "`1'" _n 
	end
	
	* Open Tex document: 
	file open Report using "$output/Report.tex", write replace

	* Set your document class 
	line "\documentclass{article}"

	* Load Latex packages	
	line "\usepackage{graphicx, caption, placeins, color, hyperref}" 
	line "\usepackage{booktabs, array, morefloats, tabularx, titling}"
	line "\usepackage{multirow, subfig, longtable, threeparttable, wrapfig}" 


	* Create document title, author & date 
	line "\title{Stata-LaTeX Integration Example}"
	line "\author{J-PAL}"
	line "\date{Last Updated: January 2024}"
	
	* Begin document
	line "\begin{document}"
	line "\maketitle"

	* Table 2: Descriptive Statistics
	line "\begin{center}"
	line "\begin{threeparttable}"
	line "\caption{Descriptive Statistics}"
	line "\begin{tabular}{p{4.5cm}ccc}"
	line "\toprule \toprule"
	line "& Treatment & Control  \\"
	line "\midrule" 
	
	* Place stored locals into table:
	foreach var in `stats' {
		line " `: var lab `var'' & ``var'_mean_t' & ``var'_mean_c' \\ "
	}
	
	* Add the number of observations
	line "\midrule"
	line "Observations & `post_tot_count_t'  & `post_tot_count_c'  \\ " 
	line "\bottomrule"
	
	* End table & add note
	line "\end{tabular}"	
	line "\multicolumn{5}{l}{\footnotesize \emph{Notes:} Test scores can range from 0-100. }" 
	line "\end{threeparttable}"
	line "\end{center}" 

	
	* Table 3: Regression with and without controls  
	line "\begin{center}"
	line "\begin{threeparttable}"	
	line "\caption{Regression Results}"
	
	line "\begin{tabular}{p{3cm}ccccc}"
	line "\toprule \toprule"
	
	line "& & Post-test Score & \\"	
	line "& Coefficient & P-value & Coefficient & P-value \\"
	line "\midrule"
    line "Balsakhi Treatment &  `b_reg1'  & `p_reg1' & `b_reg2' & `p_reg2' \\"	
	line "& (`se_reg1')  &     &  (`se_reg2') &  \\"

	* Loop for exporting local controls into table
	foreach var in `controls' {
	line " `: var lab `var''  &   & &  `b_reg2_`var'' & `p_reg2_`var'' \\ "
	line "& & &  (`se_reg2_`var'')  &  \\"
	}
	
	line "\midrule"
	line "Observations & `N_reg1'  & &  `N_reg2' & \\ "
	line "R^2 & `R_reg2'  & &  `R_reg2' & \\ "
	line "\bottomrule"
	
	* End Table 3
	line "\end{tabular}"	
	line "\multicolumn{5}{l}{\footnotesize \emph{Notes:} Standard errors in parentheses. }"
	line "\end{threeparttable}"
	line "\end{center}" 

	
	* Write figure to file 
	line "\begin{figure}[h]"
	line "\centering"
	line "\caption[online]{Improvement in test scores following program intervention, by pre-test score bin}"
	line "\begin{minipage}{1\textwidth}"
	line "\includegraphics[width=1\linewidth]{./figures/figure_1.png}"
	line "{\footnotesize \emph{Notes:} Students are divided into 10 bins based on their pre-test scores. Markers are scaled by the number of students in each bin.\par}"
	line "\end{minipage}"
	line "\end{figure}"

	
	* End your document
	line "\end{document}"
	file close Report 		// close tex file
	




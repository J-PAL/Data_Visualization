**********************************************
* Title: Data Visualization Sample Figure Code
*
* Date Created: Sep 25th, 2023
* Purpose: Exercise in creating and customizing figures in Stata
* Author: Diana Horvath 
**********************************************
clear all 
set more off

* Set directories in globals
global main "[SET MAIN DIRECTORY HERE]"

	* sub-directories
	global data "$main/Data"
	global output "$main/Figures"


*****************************************************
* 	Scatterplot of Pre-Test Scores and Test Gains	*
*****************************************************
* Import Data and drop missing values
use "$data/baroda_0102_1obs.dta", clear
drop if post_tot == . | pre_tot == .

* Calculate difference between pre- and post-test scores
gen diff_scores = post_tot - pre_tot

* Create bins of total pre-test scores in intervals of 10
gen bins  = 10 * floor(pre_tot/10)

* Create a frequency variable (count of students in each category) to use as a weight
gen _freq = 1

* Average pre-test scores and differences in scores, by bin and treatment
collapse (mean) pre_tot diff_scores (sd) sd_scores = diff_scores (sum) _freq, by(bins bal)



* Generate confidence intervals
gen lb_score = (diff_scores - invttail(_freq-1,0.025)*(sd_scores / sqrt(_freq))) 
gen ub_score = (diff_scores + invttail(_freq-1,0.025)*(sd_scores / sqrt(_freq))) 
replace lb_score = -12 if lb_score < -10




* Scatterplot
twoway (scatter diff_scores pre_tot if bal == 0 [fw = _freq],  	 		/// non-treated group
     mcolor("228 92 36")  msymbol(circle_hollow) msize(vsmall))  		///
	 (rcap lb_score ub_score pre_tot if bal == 0, color("228 92 36"))	///
     (scatter diff_scores pre_tot if bal == 1  [fw = _freq],    		/// treated group
     mcolor("44 172 156") msymbol(circle_hollow) msize(vsmall))      	/// 
	 (rcap lb_score ub_score pre_tot if bal == 1, color("44 172 156")),	///
	 legend(order(1 "Non-Balsakhi" 3 "Balsakhi") size(small) 		 	///
	 region(lstyle(none)) rows(2) position(3)) 		 				 	///								
	 xlabel(, grid gstyle(linestyle(color(white))) labsize(small)) 	 	///
	 ysc(r(-12 20)) ylabel(-10 "-10"  0 "0" 10 "10" 20 "20")		 	///
	 ylabel(, grid gstyle(linestyle(color(white))) labsize(small) 	 	///
	 angle(0)) plotregion(color("white") margin(zero)) graphregion(fcolor(white)) ///
	 ytitle("Average Test Score Improvement (by Bin)", margin(small) 	///
	 size(small)) xtitle("Pre-test Scores", margin(small) size(small))	///
	 note("Note: Markers are scaled by the number of students in each bin. The average bin size is 421 students." ///
	 , size(vsmall)) xsize(7)
	 


* Export figure: 
graph export "$output/Figure 1 - Stata.png", as(png) replace






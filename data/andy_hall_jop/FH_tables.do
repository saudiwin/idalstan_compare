*************************************************************************
* PROGRAM :			FH_tables.do
* AUTHOR :			James Feigenbaum
* DATE WRITTEN :	30 April 2015
*************************************************************************

/*********************************************
Replication code for tables in Feigenbaum and Hall
How Legislators Respond To Localized Economic Shocks: Evidence From Chinese Import Competition
*********************************************/

clear
program drop _all

/*********************************************
Load and prepare data
*********************************************/

use fh_final_analysis, clear

*scale cato scores
sum pro_ft_barrier if decade == 1, d
replace pro_ft_barrier = pro_ft_barrier - r(p50) if decade == 1
sum pro_ft_barrier if decade == 2, d
replace pro_ft_barrier = pro_ft_barrier - r(p50) if decade == 2

/*********************************************
Table 1
*********************************************/

xi: reg tradescore x dem_share i.decade, cluster(state_cluster)
	local x_ols = _b[x]
	local x_ols_se = _se[x]
	local dem_share_ols = _b[dem_share]
	local dem_share_ols_se = _se[dem_share]
	local n_ols = e(N)

xi: reg x z dem_share i.decade, cluster(state_cluster)
	local z_first = _b[z]
	local dem_share_first = _b[dem_share]
	local z_first_se = _se[z]
	local dem_share_first_se = _se[dem_share]
	local n_first = e(N)

xi: ivregress 2sls tradescore (x = z) dem_share i.decade, cluster(state_cluster)
	local x_iv = _b[x]
	local x_iv_se = _se[x]
	local dem_share_iv = _b[dem_share]
	local dem_share_iv_se = _se[dem_share]
	local n_iv = e(N)

xi: ivregress 2sls pro_ft_barrier (x = z) dem_share i.decade, cluster(state_cluster)
	local x_cato = _b[x]
	local x_cato_se = _se[x]
	local dem_share_cato = _b[dem_share]
	local dem_share_cato_se = _se[dem_share]
	local n_cato = e(N)

quietly {
	capture log close
	log using table1.tex, text replace
	noisily display ""
	noisily display "\begin{table}[t] \caption{{\bf Economic Shocks and Congressional Voting, U.S. House 1990--2010.} Localized economic shocks from trade cause more protectionist roll-call voting. } \label{tab:main}"
	noisily display "\vspace{-4mm}"
	noisily display "\begin{center}"
	noisily display "\footnotesize"
	noisily display "\begin{tabular}{l*{4}{c}}"
	noisily display "\toprule\toprule"
	noisily display "& \multicolumn{3}{c}{District Trade Score} & Cato Score \\[2mm]"
	noisily display " & OLS & First Stage & IV & IV \\"
	noisily display " \midrule "
	noisily display "Import Exposure Per Worker (IPW) & " %4.2f `x_ols' " &  & " %4.2f `x_iv' " & " %4.2f `x_cato' " \\"
	noisily display " & (" %4.2f `x_ols_se' ") & & (" %4.2f `x_iv_se' ") & (" %4.2f `x_cato_se' ") \\[2mm]"
	noisily display " IPW Non-US & & " %4.2f `z_first' " & & \\"
	noisily display " & & (" %4.2f `z_first_se' ") & & \\[2mm]"
	noisily display " Dem Share & " %4.2f `dem_share_ols' " & " %4.2f `dem_share_first' " & " %4.2f `dem_share_iv' " & " %4.2f `dem_share_cato' " \\ "
	noisily display " & (" %4.2f `dem_share_ols_se' ") & (" %4.2f `dem_share_first_se' ") & (" %4.2f `dem_share_iv_se' ") & (" %4.2f `dem_share_cato_se' ") \\[2mm] "
	noisily display "\midrule"
	noisily display " N & " %5.0f `n_ols' " & " %5.0f `n_first' " & " %5.0f `n_iv' " & " %5.0f `n_cato' " \\ "
	noisily display " Decade Fixed Effect & Yes & Yes & Yes & Yes \\"
	noisily display " \bottomrule \bottomrule "
	noisily display "\multicolumn{5}{p{.7\textwidth}}{State-decade clustered standard errors in parentheses."
	noisily display "IPW measured in thousands of U.S. Dollars.}"
	noisily display "\end{tabular}"
	noisily display "\end{center}"
	noisily display "\end{table}"
	log off
}

/*********************************************
Table 2
*********************************************/

xi: ivregress 2sls contested (x = z) i.decade, cluster(state_cluster)
	local x1 = _b[x]
	local sex1 = _se[x]
	local n1 = e(N)

xi: ivregress 2sls serious_chall  (x = z) i.decade, cluster(state_cluster)
	local x2 = _b[x]
	local sex2 = _se[x]
	local n2 = e(N)

xi: ivregress 2sls inc_share  (x = z)  i.decade, cluster(state_cluster)
	local x3 = _b[x]
	local sex3 = _se[x]
	local n3 = e(N)

xi: ivregress 2sls inc_win (x = z) i.decade, cluster(state_cluster)
	local x4 = _b[x]
	local sex4 = _se[x]
	local n4 = e(N)

quietly {
	capture log close
	set linesize 255
	log using table2.tex, text replace
	noisily display ""
	noisily display "\begin{table}[tbp] \caption{{\bf Economic Shocks and Incumbent Electoral Outcomes, U.S. House 1990--2010.}  Shocks have no effect on primary or general-election outcomes, in equilibrium. \label{tab:inc_vote}}"
	noisily display "\begin{center}"
	noisily display "\footnotesize"
	noisily display "\begin{tabular}{l*{4}{c}}"
	noisily display "\toprule\toprule"
	noisily display " & IV & IV & IV & IV \\"
	noisily display " & Contested Primary & Serious Primary & Ave. Incumbent Vote & Ave. Incumbent Win \\"
	noisily display " \midrule \\[-3mm]"
	noisily display " \shortstack{Import Exposure \\ Per Worker (IPW)} & " %4.3f `x1' " & " %4.3f `x2' " & " %4.3f `x3'  " & " %4.3f `x4' " \\"
	noisily display " & (" %4.2f `sex1' ") & (" %4.2f `sex2' ") & (" %4.2f `sex3' ") & (" %4.2f `sex4' ") \\[2mm]"
	noisily display "\midrule"
	noisily display " N & " %5.0f `n1' " & " %5.0f `n2' " & " %5.0f `n3' " & " %5.0f `n4'  " \\ "
	noisily display " Decade Fixed Effect & Yes & Yes & Yes & Yes \\"
	noisily display " \bottomrule \bottomrule "
	noisily display "\multicolumn{5}{p{.9\textwidth}}{State-decade clustered standard errors in parentheses."
	noisily display "Estimated from 2SLS as described in equation 5.  IPW measured in thousands of U.S. Dollars.  Outcome variables are share variables running from 0 to 1.}"
	noisily display "\end{tabular}"
	noisily display "\end{center}"
	noisily display "\end{table}"
	log off
}

/*********************************************
Table 3
*********************************************/

xi: ivregress 2sls nontradescore (x = z) dem_share i.decade, cluster(state_cluster)
	local x_iv = _b[x]
	local x_iv_se = _se[x]
	local dem_share_iv = _b[dem_share]
	local dem_share_iv_se = _se[dem_share]
	local n_placebo_iv = e(N)

xi: ivregress 2sls pro_ft_sub (x = z) dem_share i.decade, cluster(state_cluster)
	local x_sub = _b[x]
	local x_sub_se = _se[x]
	local dem_share_sub = _b[dem_share]
	local dem_share_sub_se = _se[dem_share]
	local n_placebo_sub = e(N)

xi: ivregress 2sls trade_deviation (x = z) dem_share i.decade, cluster(state_cluster)
	local x_dev = _b[x]
	local x_dev_se = _se[x]
	local n_placebo_dev = e(N)
	local dem_share_dev = _b[dem_share]
	local dem_share_dev_se = _se[dem_share]

xi: ivregress 2sls tradescore (x = z) dem_share i.decade if always_same_inc==1, cluster(state_cluster)
	local x_al = _b[x]
	local x_al_se = _se[x]
	local n_placebo_al = e(N)
	local dem_share_al = _b[dem_share]
	local dem_share_al_se = _se[dem_share]

xi: ivregress 2sls pro_ft_barrier (x = z) dem_share i.decade if always_same_inc==1, cluster(state_cluster)
	local x_al1 = _b[x]
	local x_al_se1 = _se[x]
	local n_placebo_al1 = e(N)
	local dem_share_al1 = _b[dem_share]
	local dem_share_al_se1 = _se[dem_share]

quietly {
	capture log close
	set linesize 255
	log using table3.tex, text replace
	noisily display ""
	noisily display "\begin{table}[tbp] \caption{\bf Economic Shocks and Congressional Voting: Testing Theories of Legislative Behavior, U.S. House 1990--2010 \label{tab:placebo}}"
	noisily display "\begin{center}"
	noisily display "\footnotesize"
	noisily display "\begin{tabular}{l*{5}{c}}"
	noisily display "\toprule\toprule"
	noisily display " & & & & \multicolumn{2}{c}{\bf Districts with} \\"
	noisily display " & \bf All Districts & \bf All Districts & \bf All Districts & \multicolumn{2}{c}{\bf Same Incumbent} \\[0.1in]"
	noisily display " & IV & IV & IV & \multicolumn{2}{c}{IV} \\"
	noisily display "  & Non Trade Score & Subsidy Score & Trade Minus Non-trade & Trade Score & Cato Score \\"
	noisily display " \midrule "
	noisily display " \shortstack{Import Exposure \\ Per Worker (IPW)} & " %4.2f `x_iv' " & " %4.2f `x_sub' " & " %4.2f `x_dev'  " & " %4.2f `x_al' " & " %4.2f `x_al1' " \\"
	noisily display " & (" %4.2f `x_iv_se' ") & (" %4.2f `x_sub_se' ") & (" %4.2f `x_dev_se' ") & (" %4.2f `x_al_se' ") & (" %4.2f `x_al_se1' ") \\[2mm]"
	noisily display " Dem Share & " %4.2f `dem_share_iv' " & " %4.2f `dem_share_sub' " & " %4.2f `dem_share_dev' " & " %4.2f `dem_share_al' " & " %4.2f `dem_share_al1' " \\ "
	noisily display " & (" %4.2f `dem_share_iv_se' ") & (" %4.2f `dem_share_sub_se' ") & (" %4.2f `dem_share_dev_se' ") & (" %4.2f `dem_share_al_se' ") & (" %4.2f `dem_share_al_se1' ") \\[2mm] "
	noisily display "\midrule"
	noisily display " N & " %5.0f `n_placebo_iv' " & " %5.0f `n_placebo_sub' " & " %5.0f `n_placebo_dev' " & " %5.0f `n_placebo_al' " & " %5.0f `n_placebo_al1' " \\ "
	noisily display " Decade Fixed Effect & Yes & Yes & Yes & Yes & Yes \\"
	noisily display " \bottomrule \bottomrule "
	noisily display "\multicolumn{6}{p{.9\textwidth}}{State-decade clustered standard errors in parentheses."
	noisily display "Estimated from 2SLS as described in equation 5.  IPW measured in thousands of U.S. Dollars.}"
	noisily display "\end{tabular}"
	noisily display "\end{center}"
	noisily display "\end{table}"
	log off
}

/*********************************************
Table 4
*********************************************/

xi: ivregress 2sls tradescore (x = z) dem_share i.decade if safe_repub_dist == 1, cluster(state_cluster)
	local x_ols = _b[x]
	local x_ols_se = _se[x]
	local dem_share_ols = _b[dem_share]
	local dem_share_ols_se = _se[dem_share]
	local n_ols = e(N)

xi: ivregress 2sls pro_ft_barrier (x = z) dem_share i.decade if safe_repub_dist == 1, cluster(state_cluster)
	local bar1 = _b[x]
	local bar1se = _se[x]
	local bar1_dem = _b[dem_share]
	local bar1_dem_se = _se[dem_share]
	local n_bar1 = e(N)

xi: ivregress 2sls tradescore (x = z) dem_share i.decade if safe_dem_dist == 1, cluster(state_cluster)
	local z_first = _b[x]
	local dem_share_first = _b[dem_share]
	local z_first_se = _se[x]
	local dem_share_first_se = _se[dem_share]
	local n_first = e(N)

xi: ivregress 2sls pro_ft_barrier (x = z) dem_share i.decade if safe_dem_dist == 1, cluster(state_cluster)
	local bar2 = _b[x]
	local bar2se = _se[x]
	local bar2_dem = _b[dem_share]
	local bar2_dem_se = _se[dem_share]
	local n_bar2 = e(N)

xi: ivregress 2sls tradescore (x = z) dem_share i.decade if competitive_dist == 1, cluster(state_cluster)
	local x_iv = _b[x]
	local x_iv_se = _se[x]
	local dem_share_iv = _b[dem_share]
	local dem_share_iv_se = _se[dem_share]
	local n_iv = e(N)

xi: ivregress 2sls pro_ft_barrier (x = z) dem_share i.decade if competitive_dist == 1, cluster(state_cluster)
	local bar3 = _b[x]
	local bar3se = _se[x]
	local bar3_dem = _b[dem_share]
	local bar3_dem_se = _se[dem_share]
	local n_bar3 = e(N)

quietly {
	capture log close
	log using table3.tex, text replace
	noisily display ""
	noisily display "\begin{table}[tbp] \caption{\bf Economic Shocks and Congressional Voting Across District Normal Vote: U.S. House, 1990--2010} \label{tab:norm}"
	noisily display "\footnotesize"
	noisily display "\begin{center}"
	noisily display "\begin{tabular}{cllllll}"
	noisily display "\toprule\toprule"
	noisily display " & \multicolumn{2}{c}{\bf Safe Rep District} & \multicolumn{2}{c}{\bf Safe Dem District} & \multicolumn{2}{c}{\bf Competitive District} \\"
	noisily display " &  \multicolumn{2}{c}{IV} & \multicolumn{2}{c}{IV} & \multicolumn{2}{c}{IV} \\"
	noisily display " & Trade Score & Cato Score & Trade Score & Cato Score & Trade Score & Cato Score \\"
	noisily display " \midrule "
	noisily display "\shortstack{Import Exposure \\ Per Worker (IPW)} & " %4.2f `x_ols' " & " %4.2f `bar1' " & " %4.2f `z_first' " & " %4.2f `bar2' " & " %4.2f `x_iv' " & " %4.2f `bar3' " \\"
	noisily display " & (" %4.2f `x_ols_se' ") & (" %4.2f `bar1se' ") & (" %4.2f `z_first_se' ") & (" %4.2f `bar2se' ")& (" %4.2f `x_iv_se' ") & (" %4.2f `bar3se' ") \\[2mm]"
	noisily display " Dem Share & " %4.2f `dem_share_ols' " & " %4.2f `bar1_dem' " & " %4.2f `dem_share_first' " & " %4.2f `bar2_dem' " & " %4.2f `dem_share_iv' " & " %4.2f `bar3_dem' " \\ "
	noisily display " & (" %4.2f `dem_share_ols_se' ") & (" %4.2f `bar1_dem_se' ") & (" %4.2f `dem_share_first_se' ") & (" %4.2f `bar2_dem_se' ") & (" %4.2f `dem_share_iv_se' ") & (" %4.2f `bar3_dem_se' ") \\[4mm] "
	noisily display "\midrule"
	noisily display " N & " %5.0f `n_ols' " & " %5.0f `n_bar1' " & " %5.0f `n_first' " & " %5.0f `n_bar2' " & " %5.0f `n_iv' " & " %5.0f `n_bar3' " \\ "
	noisily display " Decade Fixed Effect & Yes & Yes & Yes & Yes & Yes & Yes \\"
	noisily display " \bottomrule \bottomrule "
	noisily display "\multicolumn{7}{p{.8\textwidth}}{State-decade clustered standard errors in parentheses."
	noisily display "Estimated from 2SLS as described in equation 5.  IPW measured in thousands of U.S. Dollars.}"
	noisily display "\end{tabular}"
	noisily display "\end{center}"
	noisily display "\end{table}"
	log off
}

/*********************************************
Table 5
*********************************************/

quietly {
	cap log close
	set linesize 255
	log using "table5.tex", text replace

	noisily dis "\begin{table}[h]"
	noisily dis "\centering"
	noisily dis "\footnotesize"
	noisily dis "\caption{\bf Summary Statistics, U.S. House 1990--2010. \label{tab:sum}}"
	noisily dis "\begin{tabular}{lccc}"
	noisily dis "\toprule \toprule"
	noisily dis " Variable & All Years & 1990s & 2000s \\"
	noisily dis "\midrule"
	foreach var in tradescore pro_ft_barrier x z dem_share {
		if "`var'" == "tradescore" {
			local name = "Trade Score (\%)"
		}
		else if "`var'" == "pro_ft_barrier" {
			local name = "Cato Score (\%)"
		}
		else if "`var'" == "x" {
			local name = "Import Exposure Per Worker, US (\\\$1,000)"
		}
		else if "`var'" == "z" {
			local name = "Import Exposure Per Worker, Non-US (\\\$1,000)"
		}
		else {
			local name = "Dem Share (Decade)"
		}
		sum `var'
		local mean0 = r(mean)
		local n0 = r(N)

		sum `var' if decade == 1
		local mean1 = r(mean)
		local n1 = r(N)
		
		sum `var' if decade == 2
		local mean2 = r(mean)
		local n2 = r(N)
		
		noisily dis "`name' & " %5.2f `mean0' " & " %5.2f `mean1' " & " %5.2f `mean2' " \\ "
		noisily dis " & [" %3.0f `n0' "] & [" %3.0f `n1' "] & [" %3.0f `n2' "] \\[2mm]"
	}

	noisily dis "\bottomrule \bottomrule"
	noisily dis "\multicolumn{4}{p{.7\textwidth}}{Sample sizes in brackets.  Trade Scores and Cato Scores are probabilities of voting in free trade direction relative to the median district.}"
	noisily dis "\end{tabular}"
	noisily dis "\end{table}"

	log close
}

/*********************************************
Table 6
*********************************************/

*reload data and merge with congruence data
use fh_final_analysis, clear
sort state dist decade
merge state dist decade using congruence_merge
keep if _merge == 3

sum congruence, d
local c1 = 0.25
local c2 = 0.75

xi: ivregress 2sls tradescore (x=z) dem_share i.decade if congruence < `c1', cluster(state_cluster)
	local trade1 = _b[x]
	local tradese1 = _se[x]
	local n1 = e(N)

xi: ivregress 2sls tradescore (x=z) dem_share i.decade if congruence > `c2', cluster(state_cluster)
	local trade2 = _b[x]
	local tradese2 = _se[x]
	local n2 = e(N)

xi: ivregress 2sls pro_ft_barrier (x=z) dem_share i.decade if congruence < `c1', cluster(state_cluster)
	local cato3 = _b[x]
	local catose3 = _se[x]
	local n3 = e(N)

xi: ivregress 2sls pro_ft_barrier (x=z) dem_share i.decade if congruence > `c2', cluster(state_cluster)
	local cato4 = _b[x]
	local catose4 = _se[x]
	local n4 = e(N)

quietly {
	capture log close
	set linesize 255
	log using table6.tex, text replace
	noisily display ""
	noisily display "\begin{table}[tbp] \caption{{\bf Economic Shocks and Incumbent Electoral Outcomes Across Levels of Media Coverage, U.S. House 1990--2010.} \label{tab:media}}"
	noisily display "\begin{center}"
	noisily display "\footnotesize"
	noisily display "\begin{tabular}{l*{4}{c}}"
	noisily display "\toprule\toprule"
	noisily display " & \multicolumn{2}{c}{Trade Score} & \multicolumn{2}{c}{Cato Score} \\"
	noisily display " & Low Media Coverage & High Media Coverage & Low Media Coverage & High Media Coverage \\"
	noisily display " \midrule \\[-3mm]"
	noisily display " \shortstack{Import Exposure \\ Per Worker (IPW)} & " %4.3f `trade1' " & " %4.3f `trade2' " & " %4.3f `cato3'  " & " %4.3f `cato4' " \\"
	noisily display " & (" %4.2f `tradese1' ") & (" %4.2f `tradese2' ") & (" %4.2f `catose3' ") & (" %4.2f `catose4' ") \\[2mm]"
	noisily display "\midrule"
	noisily display " N & " %5.0f `n1' " & " %5.0f `n2' " & " %5.0f `n3' " & " %5.0f `n4'  " \\ "
	noisily display " Decade Fixed Effect & Yes & Yes & Yes & Yes \\"
	noisily display " \bottomrule \bottomrule "
	noisily display "\multicolumn{5}{p{.9\textwidth}}{State-decade clustered standard errors in parentheses."
	noisily display "Estimated from 2SLS as described in equation 5.  IPW measured in thousands of U.S. Dollars.}"
	noisily display "\end{tabular}"
	noisily display "\end{center}"
	noisily display "\end{table}"
	log off
}

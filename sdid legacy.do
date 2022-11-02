clear all
cd "/Users/constanzazepedadiaz/Documents/Personal/Tesis/SPD-BCP/Proj_tesisME/"

* Globals 
global resultados 	"resultados4/"
global insumos 		"insumos intermedios4/"

********************************************************************************
****************************** Data mensual ************************************
********************************************************************************

use "${insumos}panel_month.dta", clear
egen g=group(gcod)
gen monthly_date = mofd(date_seq)
format monthly_date %tm
xtset g monthly_date
format %tm monthly_date
ren n_del cant_del

* Params
global reps = 50 
global seed = 123

* local variables
local rcode 0 
local metric dens cant 
local mcode 0
local delito del com_ile rob_hur rob_veh rob_vio
foreach r of local metric {
	local rcode = `rcode' + 1
	
	foreach m of local delito {
    local mcode = `mcode' + 1
	display "`rcode'.`mcode' : `r'_`m'"
	
	#delimit ;
	sdid `r'_`m' g monthly_date trat, vce(placebo) reps($reps) seed($seed) // covariates(dummy_cuar , optimized)
		graph g1_opt(xtitle("") scheme(plotplainblind)) 
		g2_opt( xtitle("") scheme(plotplainblind))
	graph_export(${resultados}/freq mensual/$reps reps/`r'/sdid_`r'_`m'_, .png);
	#delimit cr

	} 

}


********************************************************************************
****************************** Data semanal ************************************
********************************************************************************

use "${insumos}panel_week.dta", clear
egen g=group(gcod)
* gen monthly_date = mofd(date_seq)
* format monthly_date %tm
xtset g date_seq
format %td date_seq
ren n_del cant_del

* Params
global reps = 50 
global seed = 123

* local variables
local rcode 0 
local metric dens cant 
local mcode 0
local delito del com_ile rob_hur rob_veh rob_vio
foreach r of local metric {
	local rcode = `rcode' + 1
	
	foreach m of local delito {
    local mcode = `mcode' + 1
	display "`rcode'.`mcode' : `r'_`m'"
	
	#delimit ;
	sdid `r'_`m' g date_seq trat, vce(placebo) reps($reps) seed($seed) // covariates(dummy_cuar , optimized)
		graph g1_opt(xtitle("") scheme(plotplainblind)) 
		g2_opt( xtitle("") scheme(plotplainblind))
	graph_export(${resultados}/freq semanal/$reps reps/`r'/sdid_`r'_`m'_, .png);
	#delimit cr

	} 

}



********************************************************************************
*************************** Data mensual comercial *****************************
********************************************************************************

use "${insumos}panel_comer_month.dta", clear
egen g=group(gcod)
gen monthly_date = mofd(date_seq)
format monthly_date %tm
xtset g monthly_date
format %tm monthly_date
ren n_del cant_del

* Params
global reps = 50 
global seed = 123

* local variables
local rcode 0 
local metric dens cant 
local mcode 0
local delito del com_ile rob_hur rob_veh rob_vio
foreach r of local metric {
	local rcode = `rcode' + 1
	
	foreach m of local delito {
    local mcode = `mcode' + 1
	display "`rcode'.`mcode' : `r'_`m'"
	
	#delimit ;
	sdid `r'_`m' g monthly_date trat, vce(placebo) reps($reps) seed($seed) // covariates(dummy_cuar , optimized)
		graph g1_opt(xtitle("") scheme(plotplainblind)) 
		g2_opt( xtitle("") scheme(plotplainblind))
	graph_export(${resultados}/freq mensual/$reps reps comer/`r'/sdid_`r'_`m'_, .png);
	#delimit cr

	} 

}

********************************************************************************
*************************** Data mensual spillovers ****************************
********************************************************************************



********************************************************************************
*************************** Data mensual antes/dps *****************************
********************************************************************************


* 
* ***
* foreach r of local metric {
* 	local rcode = `rcode' + 1
* 	display "`rcode': `r'"
* 	
* 	foreach m of local delito {
*     local mcode = `mcode' + 1
* 	display "`rcode': `r'"
* 	display "`mcode': `m'"
* 	
* 	
* 	} 
* 
* }
* 
* local metric dens cant 
* foreach r of local metric {
* 
* 	display "`r'"
* 
* }
* 
* ******
* *Codigo iterador 50 reps
* * Cantidad 
* local mcode 0
* local delito n_del cant_com_ile cant_rob_hur cant_rob_veh cant_rob_vio
* foreach m of local delito {
*     local mcode = `mcode' + 1
* 	
* 	display "`mcode': `m'"
* 	
* 	#delimit ;
* 	sdid `m' g monthly_date trat, vce(placebo) reps(50) seed(123) // covariates(dummy_cuar , optimized)
* 		graph g1_opt(xtitle("") scheme(plotplainblind)) 
* 		g2_opt( xtitle("") scheme(plotplainblind))
* 	graph_export(${resultados}/freq mensual/50 reps/cant/sdid_`m'_, .png);
* 	#delimit cr
* 
* 	} 
* 	
* * Densidad 
* local mcode 0
* local delito dens_del dens_com_ile dens_rob_hur dens_rob_veh dens_rob_vio
* foreach m of local delito {
*     local mcode = `mcode' + 1
* 	
* 	display "`mcode': `m'"
* 	
* 	#delimit ;
* 	sdid `m' g mensual tratados, vce(placebo) reps(50) seed(123) //covariates(dummy_cuar , optimized)
* 		graph g1_opt(xtitle("") scheme(plotplainblind)) 
* 		g2_opt( xtitle("") scheme(plotplainblind))
* 	graph_export(${resultados}/freq mensual/50 reps/dens/sdid_`m'_, .png);
* 	#delimit cr
* 
* 	} 	
* 	
* *****
* 
* 
* 
* 
* 	
* *****
* 	
* 
* #delimit ;
* sdid total_deli g mensual tratados, vce(placebo) reps(3) seed(123) covariates(dummy_cuar , optimized)
* 	graph g1_opt(xtitle("total delitops") scheme(plotplainblind)) 
*           g2_opt( xtitle("total delitos") scheme(plotplainblind))
*     graph_export(sdid_, .png);
* #delimit cr
* 
* matrix omega=e(omega)
* svmat omega
* 
* mat list e(omega)
* covariates(r, projected)
* matlist e(beta)
* 
* 
* 
* 
* 
* sdid total_deli g mensual tratados, vce(bootstrap) reps(50) seed(123) covariates(dummy_cuar , optimized)
* sdid total_deli g mensual tratados, vce(jackknife) covariates(dummy_cuar , optimized)
* 
* 
* 
* 
* 
* 	
* 	
* 

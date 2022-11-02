clear all
cd "/Users/constanzazepedadiaz/Documents/Personal/Tesis/SPD-BCP/Proj_tesisME/"
*net install sdid, from("https://raw.githubusercontent.com/daniel-pailanir/sdid/master") replace

* Globals 
global resultados 	"resultados5/"
global insumos 		"insumos intermedios5/"

********************************************************************************
****************************** Data mensual ************************************
********************************************************************************

use "${insumos}denuncias_panel_mensual.dta", clear
egen g=group(gcod)
gen monthly_date = mofd(date_seq)
format monthly_date %tm
xtset g monthly_date
format %tm monthly_date
* ren n_del cant_del

* Params
global reps = 5 
global seed = 123
global reps_base = 50 


* tratamiento simultaneo
gen date_trat_sim = tm(2020-03)
format date_trat_sim %tm

gen trat_sim = 0
replace trat_sim = 1 if barrio_id != 0 & monthly_date >= date_trat_sim


*	* Con covariables de cuarentena tratamiento simultaneo *	*

* local variables
local rcode 0 
local metric dens //cant 
local mcode 0
* local delito del inc //amenaza com_ile con_alc dannyos desorde ebrieda rob_hur rob_ve2 rob_veh rob_vio rui_mol
local delito   inc // del //armas homicidio // drogas
foreach r of local metric {
	local rcode = `rcode' + 1
	
	foreach m of local delito {
    local mcode = `mcode' + 1
	display "`rcode'.`mcode' : `r'_`m'"
	
	#delimit ;
	sdid `r'_`m' g monthly_date trat_sim, vce(placebo) reps($reps) seed($seed)  covariates(cuarentena , optimized)
		graph g2_opt( xtitle("") scheme(plotplainblind))
	graph_export(${resultados}/freq mensual denuncias/$reps_base reps cov simult/`r'/sdid_`r'_`m'_, .png);
	#delimit cr

	} 

}


*	* Con covariables de cuarentena tratamiento escalonado *	*

* local variables
local rcode 0 
local metric dens //cant 
local mcode 0
* local delito del inc //amenaza com_ile con_alc dannyos desorde ebrieda rob_hur rob_ve2 rob_veh rob_vio rui_mol
local delito   homicidio //drogas armas
foreach r of local metric {
	local rcode = `rcode' + 1
	
	foreach m of local delito {
    local mcode = `mcode' + 1
	display "`rcode'.`mcode' : `r'_`m'"
	
	#delimit ;
	sdid `r'_`m' g monthly_date trat, vce(placebo) reps($reps) seed($seed)  covariates(cuarentena , optimized)
		graph g2_opt( xtitle("") scheme(plotplainblind))
	graph_export(${resultados}/freq mensual denuncias/$reps_base reps cov/`r'/sdid_`r'_`m'_, .png);
	#delimit cr

	} 

}


* 
* * 	* Sin covariables de cuarentena * 	*
* 
* * local variables
* local rcode 0 
* local metric dens //cant 
* local mcode 0
* local delito del com_ile rob_hur rob_veh rob_vio
* foreach r of local metric {
* 	local rcode = `rcode' + 1
* 	
* 	foreach m of local delito {
*     local mcode = `mcode' + 1
* 	display "`rcode'.`mcode' : `r'_`m'"
* 	
* 	#delimit ;
* 	sdid `r'_`m' g monthly_date trat, vce(placebo) reps($reps) seed($seed) // covariates(cuarentena , optimized)
* 		graph g2_opt( xtitle("") scheme(plotplainblind))
* 	graph_export(${resultados}/freq mensual/$reps reps sin cov/`r'/sdid_`r'_`m'_, .png);
* 	#delimit cr
* 
* 	} 
* 
* }
* 

* 	* Filtro zonas comerciales, incl covariables * 	*

use "${insumos}denuncias_panel_mensual.dta", clear
egen g=group(gcod)
gen monthly_date = mofd(date_seq)
format monthly_date %tm
xtset g monthly_date
format %tm monthly_date
* ren n_del cant_del

* Params
global reps = 5
global seed = 123

keep if comercial == 1 

* tratamiento simultaneo
gen date_trat_sim = tm(2020-03)
format date_trat_sim %tm

gen trat_sim = 0
replace trat_sim = 1 if barrio_id != 0 & monthly_date >= date_trat_sim



* local variables
local rcode 0 
local metric dens //cant 
local mcode 0
* local delito del inc // amenaza com_ile con_alc dannyos desorde ebrieda rob_hur rob_ve2 rob_veh rob_vio rui_mol
local delito drogas armas homicidio
foreach r of local metric {
	local rcode = `rcode' + 1
	
	foreach m of local delito {
    local mcode = `mcode' + 1
	display "`rcode'.`mcode' : `r'_`m'"
	
	#delimit ;
	sdid `r'_`m' g monthly_date trat_sim, vce(placebo) reps($reps) seed($seed)  covariates(cuarentena , optimized)
		graph g2_opt( xtitle("") scheme(plotplainblind))
	graph_export(${resultados}/freq mensual denuncias/$reps_base reps cov comer/`r'/sdid_`r'_`m'_, .png);
	#delimit cr

	} 

}

* 
* * 	* Filtro zonas comerciales, sin incl covariables * 	*
* 
* * keep if comercial == 1 
* 
* * local variables
* local rcode 0 
* local metric dens //cant 
* local mcode 0
* local delito del com_ile rob_hur rob_veh rob_vio
* foreach r of local metric {
* 	local rcode = `rcode' + 1
* 	
* 	foreach m of local delito {
*     local mcode = `mcode' + 1
* 	display "`rcode'.`mcode' : `r'_`m'"
* 	
* 	#delimit ;
* 	sdid `r'_`m' g monthly_date trat, vce(placebo) reps($reps) seed($seed) // covariates(cuarentena , optimized)
* 		graph g2_opt( xtitle("") scheme(plotplainblind))
* 	graph_export(${resultados}/freq mensual/$reps reps sin cov comer/`r'/sdid_`r'_`m'_, .png);
* 	#delimit cr
* 
* 	} 
* 
* }
* 
********************************************************************************
****************** Data mensual: Antes y despues de cuarentenas *****************
********************************************************************************

* * 	* Antes y despues de las cuarentenas, incl covariables * 	*
* 
* use "${insumos}denuncias_panel_mensual.dta", clear
* egen g=group(gcod)
* gen monthly_date = mofd(date_seq)
* format monthly_date %tm
* xtset g monthly_date
* format %tm monthly_date
* * ren n_del cant_del
* 
* * Gen fecha inicio de tratamiento "ficticia"
* 
* gen fecha_pre_pandemic = tm(2020-03)
* format fecha_pre_pandemic %tm
* 
* gen fecha_post_pandemic = tm(2021-07)
* format fecha_post_pandemic %tm
* 
* gen trat_pos_pandemic = 0 
* replace trat_pos_pandemic = 1 if trat == 1 & monthly_date > fecha_post_pandemic
* 
* keep if monthly_date > fecha_post_pandemic | monthly_date < fecha_pre_pandemic
* 
* 
* * xtset g monthly_date
* 
* * Re crear variable temporal 
* egen w=group(monthly_date)
* 
* xtset g w
* 
* * Params
* global reps = 50 
* global seed = 123
* 
* 
* * local variables
* local rcode 0 
* local metric dens //cant 
* local mcode 0
* local delito del inc //amenaza com_ile con_alc dannyos desorde ebrieda rob_hur rob_ve2 rob_veh rob_vio rui_mol
* foreach r of local metric {
* 	local rcode = `rcode' + 1
* 	
* 	foreach m of local delito {
*     local mcode = `mcode' + 1
* 	display "`rcode'.`mcode' : `r'_`m'"
* 	
* 	#delimit ;
* 	sdid `r'_`m' g w trat_pos_pandemic, vce(placebo) reps($reps) seed($seed) covariates(cuarentena , optimized)
* 		graph g2_opt( xtitle("") scheme(plotplainblind))
* 	graph_export(${resultados}/freq mensual denuncias/$reps reps antes dps cov/`r'/sdid_`r'_`m'_, .png);
* 	#delimit cr
* 
* 	} 
* 
* }
* 

********************************************************************************
*********************** Data mensual: Casos de exito ***************************
********************************************************************************

* a) Coordinadores sin rotacion 


use "${insumos}denuncias_panel_mensual.dta", clear
egen g=group(gcod)
gen monthly_date = mofd(date_seq)
format monthly_date %tm
xtset g monthly_date
format %tm monthly_date
* ren n_del cant_del

* Mantengo barrios de exito
keep if (barrio_id == 0 | barrio_id == 10 | barrio_id == 12)

* Params
global reps = 5 
global seed = 123

* tratamiento simultaneo
gen date_trat_sim = tm(2020-03)
format date_trat_sim %tm

gen trat_sim = 0
replace trat_sim = 1 if barrio_id != 0 & monthly_date >= date_trat_sim



* local variables
local rcode 0 
local metric dens //cant 
local mcode 0
* local delito del inc //amenaza com_ile con_alc dannyos desorde ebrieda rob_hur rob_ve2 rob_veh rob_vio rui_mol
local delito drogas armas homicidio
foreach r of local metric {
	local rcode = `rcode' + 1
	
	foreach m of local delito {
    local mcode = `mcode' + 1
	display "`rcode'.`mcode' : `r'_`m'"
	
	#delimit ;
	sdid `r'_`m' g monthly_date trat_sim, vce(placebo) reps($reps) seed($seed)  covariates(cuarentena , optimized)
		graph g2_opt( xtitle("") scheme(plotplainblind))
	graph_export(${resultados}/freq mensual denuncias/$reps_base reps cov exito coord/`r'/sdid_`r'_`m'_, .png);
	#delimit cr

	} 

}




* b) Casos de exito "cualitativo"


use "${insumos}denuncias_panel_mensual.dta", clear
egen g=group(gcod)
gen monthly_date = mofd(date_seq)
format monthly_date %tm
xtset g monthly_date
format %tm monthly_date
* ren n_del cant_del

* Mantengo barrios de exito
keep if (barrio_id == 0 | barrio_id == 10 | barrio_id == 9)

* Params
global reps = 50 
global seed = 123

* tratamiento simultaneo
gen date_trat_sim = tm(2020-03)
format date_trat_sim %tm

gen trat_sim = 0
replace trat_sim = 1 if barrio_id != 0 & monthly_date >= date_trat_sim



* local variables
local rcode 0 
local metric dens //cant 
local mcode 0
*local delito del inc //amenaza com_ile con_alc dannyos desorde ebrieda rob_hur rob_ve2 rob_veh rob_vio rui_mol
local delito drogas armas homicidio
foreach r of local metric {
	local rcode = `rcode' + 1
	
	foreach m of local delito {
    local mcode = `mcode' + 1
	display "`rcode'.`mcode' : `r'_`m'"
	
	#delimit ;
	sdid `r'_`m' g monthly_date trat_sim, vce(placebo) reps($reps) seed($seed)  covariates(cuarentena , optimized)
		graph g2_opt( xtitle("") scheme(plotplainblind))
	graph_export(${resultados}/freq mensual denuncias/$reps reps cov exito programa/`r'/sdid_`r'_`m'_, .png);
	#delimit cr

	} 

}


* fin data mensual 

********************************************************************************
****************************** Data semanal ************************************
********************************************************************************



use "${insumos}denuncias_panel_semanal.dta", clear
egen g=group(gcod)
egen w=group(date_seq)
* gen monthly_date = mofd(date_seq)
* format monthly_date %tm

gen weekly_date_seq = wofd(date_seq) 
format weekly_date_seq %tw


* drop date_3 weekly_date_3
* gen date_2 = date_seq
gen date = date_seq 
replace date = date_seq + 7 if date_seq >= td("31/12/2018")

* format date_2 %td
format date %td

* gen weekly_date_2 = wofd(date_2) 
* format weekly_date_2 %tw

gen weekly_date = wofd(date) 
format weekly_date %tw


* replace date_2  = td("01/01/2019") if date_seq == td("31/12/2018")
* format date_2 %td

* gen weekly_date_2 = wofd(date_2) 
* format weekly_date_2 %tw


xtset g weekly_date
format %td date_seq
* ren n_del cant_del



* Params
global reps = 50 
global seed = 123


*	* Con covariables de cuarentena *	*

* local variables
local rcode 0 
local metric dens //cant 
local mcode 0
local delito del inc //amenaza com_ile con_alc dannyos desorde ebrieda rob_hur rob_ve2 rob_veh rob_vio rui_mol
foreach r of local metric {
	local rcode = `rcode' + 1
	
	foreach m of local delito {
    local mcode = `mcode' + 1
	display "`rcode'.`mcode' : `r'_`m'"
	
	#delimit ;
	sdid `r'_`m' g weekly_date trat, vce(placebo) reps($reps) seed($seed)  covariates(cuarentena , optimized)
		graph g2_opt( xtitle("") scheme(plotplainblind))
	graph_export(${resultados}/freq semanal denuncias/$reps reps cov/`r'/sdid_`r'_`m'_, .png);
	#delimit cr

	} 

}

* 
* * 	* Sin covariables de cuarentena * 	*
* 
* * local variables
* local rcode 0 
* local metric dens //cant 
* local mcode 0
* local delito del com_ile rob_hur rob_veh rob_vio
* foreach r of local metric {
* 	local rcode = `rcode' + 1
* 	
* 	foreach m of local delito {
*     local mcode = `mcode' + 1
* 	display "`rcode'.`mcode' : `r'_`m'"
* 	
* 	#delimit ;
* 	sdid `r'_`m' g weekly_date trat, vce(placebo) reps($reps) seed($seed) // covariates(cuarentena , optimized)
* 		graph g2_opt( xtitle("") scheme(plotplainblind))
* 	graph_export(${resultados}/freq semanal/$reps reps sin cov/`r'/sdid_`r'_`m'_, .png);
* 	#delimit cr
* 
* 	} 
* 
* }
* 
* 	* Filtro zonas comerciales, incl covariables * 	*
* 
* keep if comercial == 1 
* 
* * local variables
* local rcode 0 
* local metric dens //cant 
* local mcode 0
* local delito del com_ile rob_hur rob_veh rob_vio
* foreach r of local metric {
* 	local rcode = `rcode' + 1
* 	
* 	foreach m of local delito {
*     local mcode = `mcode' + 1
* 	display "`rcode'.`mcode' : `r'_`m'"
* 	
* 	#delimit ;
* 	sdid `r'_`m' g weekly_date trat, vce(placebo) reps($reps) seed($seed)  covariates(cuarentena , optimized)
* 		graph g2_opt( xtitle("") scheme(plotplainblind))
* 	graph_export(${resultados}/freq semanal/$reps reps cov comer/`r'/sdid_`r'_`m'_, .png);
* 	#delimit cr
* 
* 	} 
* 
* }
* 
* * 	* Filtro zonas comerciales, sin incl covariables * 	*
* 
* * keep if comercial == 1 
* 
* * local variables
* local rcode 0 
* local metric dens //cant 
* local mcode 0
* local delito del com_ile rob_hur rob_veh rob_vio
* foreach r of local metric {
* 	local rcode = `rcode' + 1
* 	
* 	foreach m of local delito {
*     local mcode = `mcode' + 1
* 	display "`rcode'.`mcode' : `r'_`m'"
* 	
* 	#delimit ;
* 	sdid `r'_`m' g weekly_date trat, vce(placebo) reps($reps) seed($seed) // covariates(cuarentena , optimized)
* 		graph g2_opt( xtitle("") scheme(plotplainblind))
* 	graph_export(${resultados}/freq semanal/$reps reps sin cov comer/`r'/sdid_`r'_`m'_, .png);
* 	#delimit cr
* 
* 	} 
* 
* }
* 
* 
* *****
* * 
* * Params
* global reps = 50
* global seed = 123
* 
* xtset g w

* * local variables
* local rcode 0 
* local metric dens cant 
* local mcode 0
* local delito del com_ile rob_hur rob_veh rob_vio
* foreach r of local metric {
* 	local rcode = `rcode' + 1
* 	
* 	foreach m of local delito {
*     local mcode = `mcode' + 1
* 	display "`rcode'.`mcode' : `r'_`m'"
* 	
* 	#delimit ;
* 	sdid `r'_`m' g w trat, vce(placebo) reps($reps) seed($seed) // covariates(dummy_cuar , optimized)
* 		graph g1_opt(xtitle("") scheme(plotplainblind)) 
* 		g2_opt( xtitle("") scheme(plotplainblind));
* 	#delimit cr
* 
* 	} 
* 
* }
* 
* 

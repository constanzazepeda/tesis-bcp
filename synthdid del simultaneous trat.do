clear all
cd "/Users/constanzazepedadiaz/Documents/Personal/Tesis/SPD-BCP/Proj_tesisME/"
*net install sdid, from("https://raw.githubusercontent.com/daniel-pailanir/sdid/master") replace

* Globals 
global resultados 	"resultados5/"
global insumos 		"insumos intermedios6 - drogas armas y homicidios/"

********************************************************************************
****************************** Data mensual************************************
********************************************************************************

use "${insumos}panel_mensual.dta", clear
egen g=group(gcod)
gen monthly_date = mofd(date_seq)
format monthly_date %tm
xtset g monthly_date
format %tm monthly_date
* ren n_del cant_del

* Params
global reps = 50 
global seed = 123

gen date_trat_sim = tm(2020-03)
format date_trat_sim %tm

gen trat_sim = 0
replace trat_sim = 1 if barrio_id != 0 & monthly_date >= date_trat_sim

*	* Con covariables de cuarentena *	*

* local variables
local rcode 0 
local metric dens //cant 
local mcode 0
* local delito del inc amenaza com_ile con_alc dannyos desorde ebrieda rob_hur rob_ve2 rob_veh rob_vio rui_mol
local delito dah drogas armas homicid
foreach r of local metric {
	local rcode = `rcode' + 1
	
	foreach m of local delito {
    local mcode = `mcode' + 1
	display "`rcode'.`mcode' : `r'_`m'"
	
	#delimit ;
	sdid `r'_`m' g monthly_date trat_sim, vce(placebo) reps($reps) seed($seed)  covariates(cuarentena , optimized)
		graph g2_opt( xtitle("") scheme(plotplainblind))
	graph_export(${resultados}/freq mensual delitos simult/$reps reps cov/`r'/sdid_`r'_`m'_, .png);
	#delimit cr

	} 

}

********************************************************************************
****************************** Robustez ************************************
********************************************************************************



* 	* Filtro zonas comerciales, incl covariables * 	*

use "${insumos}panel_mensual.dta", clear
egen g=group(gcod)
gen monthly_date = mofd(date_seq)
format monthly_date %tm
xtset g monthly_date
format %tm monthly_date
* ren n_del cant_del

* Params
global reps = 50 
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
local delito del inc dah drogas armas homicid
foreach r of local metric {
	local rcode = `rcode' + 1
	
	foreach m of local delito {
    local mcode = `mcode' + 1
	display "`rcode'.`mcode' : `r'_`m'"
	
	#delimit ;
	sdid `r'_`m' g monthly_date trat_sim, vce(placebo) reps($reps) seed($seed)  covariates(cuarentena , optimized)
		graph g2_opt( xtitle("") scheme(plotplainblind))
	graph_export(${resultados}/freq mensual delitos simult/$reps reps cov comer/`r'/sdid_`r'_`m'_, .png);
	#delimit cr

	} 

}



********************************************************************************
****************** Data mensual: Antes y despues de cuarentenas *****************
********************************************************************************

* 	* Antes y despues de las cuarentenas, incl covariables * 	*

use "${insumos}panel_mensual.dta", clear
egen g=group(gcod)
gen monthly_date = mofd(date_seq)
format monthly_date %tm
xtset g monthly_date
format %tm monthly_date
* ren n_del cant_del

* tratamiento simultaneo (en este caso no se utiliza porque ya es tratamiento simultaneo post pandemia)
gen date_trat_sim = tm(2020-03)
format date_trat_sim %tm

gen trat_sim = 0
replace trat_sim = 1 if barrio_id != 0 & monthly_date >= date_trat_sim


* Gen fecha inicio de tratamiento "ficticia"

gen fecha_pre_pandemic = tm(2020-03)
format fecha_pre_pandemic %tm

gen fecha_post_pandemic = tm(2021-07)
format fecha_post_pandemic %tm

gen trat_pos_pandemic = 0 
replace trat_pos_pandemic = 1 if trat == 1 & monthly_date > fecha_post_pandemic

keep if monthly_date > fecha_post_pandemic | monthly_date < fecha_pre_pandemic


* xtset g monthly_date

* Re crear variable temporal 
egen w=group(monthly_date)

xtset g w

* Params
global reps = 50 
global seed = 123


* local variables
local rcode 0 
local metric dens //cant 
local mcode 0
* local delito del inc //amenaza com_ile con_alc dannyos desorde ebrieda rob_hur rob_ve2 rob_veh rob_vio rui_mol
local delito del inc dah drogas armas homicid
foreach r of local metric {
	local rcode = `rcode' + 1
	
	foreach m of local delito {
    local mcode = `mcode' + 1
	display "`rcode'.`mcode' : `r'_`m'"
	
	#delimit ;
	sdid `r'_`m' g w trat_pos_pandemic, vce(placebo) reps($reps) seed($seed) covariates(cuarentena , optimized)
		graph g2_opt( xtitle("") scheme(plotplainblind))
	graph_export(${resultados}/freq mensual delitos simult/$reps reps antes dps cov/`r'/sdid_`r'_`m'_, .png);
	#delimit cr

	} 

}


********************************************************************************
*********************** Data mensual: Casos de exito ***************************
********************************************************************************

* a) Coordinadores sin rotacion 


use "${insumos}panel_mensual.dta", clear
egen g=group(gcod)
gen monthly_date = mofd(date_seq)
format monthly_date %tm
xtset g monthly_date
format %tm monthly_date
* ren n_del cant_del

* Mantengo barrios de exito
keep if (barrio_id == 0 | barrio_id == 10 | barrio_id == 12)

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
* local delito del inc //amenaza com_ile con_alc dannyos desorde ebrieda rob_hur rob_ve2 rob_veh rob_vio rui_mol
local delito del inc dah drogas armas homicid
foreach r of local metric {
	local rcode = `rcode' + 1
	
	foreach m of local delito {
    local mcode = `mcode' + 1
	display "`rcode'.`mcode' : `r'_`m'"
	
	#delimit ;
	sdid `r'_`m' g monthly_date trat_sim, vce(placebo) reps($reps) seed($seed)  covariates(cuarentena , optimized)
		graph g2_opt( xtitle("") scheme(plotplainblind))
	graph_export(${resultados}/freq mensual delitos simult/$reps reps cov exito coord/`r'/sdid_`r'_`m'_, .png);
	#delimit cr

	} 

}


* b) Casos de exito "cualitativo"


use "${insumos}panel_mensual.dta", clear
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
* local delito del inc //amenaza com_ile con_alc dannyos desorde ebrieda rob_hur rob_ve2 rob_veh rob_vio rui_mol
local delito dah drogas armas homicid // queda pendiente del inc 
foreach r of local metric {
	local rcode = `rcode' + 1
	
	foreach m of local delito {
    local mcode = `mcode' + 1
	display "`rcode'.`mcode' : `r'_`m'"
	
	#delimit ;
	sdid `r'_`m' g monthly_date trat_sim, vce(placebo) reps($reps) seed($seed)  covariates(cuarentena , optimized)
		graph g2_opt( xtitle("") scheme(plotplainblind))
	graph_export(${resultados}/freq mensual delitos simult/$reps reps cov exito programa/`r'/sdid_`r'_`m'_, .png);
	#delimit cr

	} 

}


* fin data mensual 





********************************************************************************
****************************** Data semanal ************************************
********************************************************************************



use "${insumos}panel_semanal.dta", clear
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

* gen weekly_date = wofd(date) 
* format weekly_date %tw


* replace date_2  = td("01/01/2019") if date_seq == td("31/12/2018")
* format date_2 %td

* gen weekly_date_2 = wofd(date_2) 
* format weekly_date_2 %tw


* xtset g weekly_date
* xtset g date
xtset g w 
* format %td date_seq
* ren n_del cant_del



* Params
global reps = 50 
global seed = 123

* tratamiento simultaneo
gen date_trat_sim = td(1feb2020)
format date_trat_sim %td

gen trat_sim = 0
replace trat_sim = 1 if barrio_id != 0 & date >= date_trat_sim


*	* Con covariables de cuarentena *	*

* local variables
local rcode 0 
local metric dens //cant 
local mcode 0
local delito del inc dah drogas armas homicid  // amenaza com_ile con_alc dannyos desorde ebrieda rob_hur rob_ve2 rob_veh rob_vio rui_mol
foreach r of local metric {
	local rcode = `rcode' + 1
	
	foreach m of local delito {
    local mcode = `mcode' + 1
	display "`rcode'.`mcode' : `r'_`m'"
	
	#delimit ;
	sdid `r'_`m' g w trat_sim, vce(placebo) reps($reps) seed($seed)  covariates(cuarentena , optimized)
		graph g2_opt( xtitle("") scheme(plotplainblind))
	graph_export(${resultados}/freq semanal delitos simult/$reps reps cov/`r'/sdid_`r'_`m'_, .png);
	#delimit cr

	} 

}




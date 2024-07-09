

******CREATE VARIABLES FOR TABLE 10*****
foreach v in ex_rate_regime us_integ trade_open vuln_index fin_depth dollar_exp dollar_debt{

*Step 1: Standardize each exposure variable, i.e. subtract mean and divide by std. dev.

	sum `v' 
	gen `v'_std = (`v' - r(mean)) / r(sd)
	
*Step 2: Construct a logistic transformation of the standardized variable

	gen `v'_log =  exp(`v'_std) / (1 + exp(`v'_std))
	
*Step 3: Re-center the logistic variable in terms of distance between 25th and 75th percentiles

	sum `v'_log, detail
	gen `v'_exposure = (`v'_log - r(p25)) / (r(p75) - r(p25))
	

*Step 4: Construct interaction term with shocks

	gen `v'_mps_int = `v'_exposure * mp
	
	gen `v'_cbi_int = `v'_exposure * cbi
	
	

}	 

*Drop U.S.
drop if ccode==49



*For robustness, can change variable ordering with loops
foreach y in yield10y {	
	
	foreach a in trade_open {

	foreach b in ex_rate_regime {
		 
	foreach c in fin_depth {
			
	
				

		*Keep emerging countries only
		*keep if adv==0

		keep if `y'_2day!=. & `a'!=. & `b'!=. & `c'!=.
				
			*First Variable
			reg `a'_mps_int mp cbi
			predict `a'_mp_orthog, residual

			reg  `a'_cbi_int mp cbi `a'_mp_orthog
			predict `a'_cbi_orthog, residual
			
			*Second Variable
			reg `b'_mps_int mp cbi `a'_mp_orthog `a'_cbi_orthog
			predict `b'_mp_orthog, residual

			reg  `b'_cbi_int mp cbi `a'_mp_orthog `a'_cbi_orthog `b'_mp_orthog
			predict `b'_cbi_orthog, residual
				
			*Third Variable
			reg `c'_mps_int mp cbi `a'_mp_orthog `a'_cbi_orthog `b'_mp_orthog `b'_cbi_orthog
			predict `c'_mp_orthog, residual

			reg  `c'_cbi_int mp cbi `a'_mp_orthog `a'_cbi_orthog `b'_mp_orthog `b'_cbi_orthog `c'_mp_orthog
			predict `c'_cbi_orthog, residual
			


	}
	}
	}
}
reghdfe yield10y_2day mp cbi fin_depth_cbi_orthog fin_depth_mp_orthog trade_open_cbi_orthog trade_open_mp_orthog ex_rate_regime_mp_orthog ex_rate_regime_cbi_orthog mpu mpu_level us_yield, noabsorb cluster(ccode date)
*capture outreg2 using "tables\table9.xml", `replace' se bdec(3) bracket e(r2_a) label 
*capture eststo 



clear

//Set location of the data to use
use "C:\Users\jorge\Desktop\Nova SBE\M1\Empirical Methods\Case 1\ibes_ps1.dta"

//Install packages
ssc install egenmore

//Exercise 1 - Sample Selection and Variable Definition
//Drop missing values 

drop if missing(eps, forecast, cusip)

//create variable coverage defined as the number of analysts covering a given earnings announcement
egen coverage = nvals(analystcode), by(epsdate cusip) 

//generate variable fatigue
sort date analystcode time
foreach analyst of varlist analystcode{
   generate fatigue = 1 if analyst != analyst[_n-1] 
   replace fatigue = fatigue[_n-1] + 1 if analyst == analyst[_n-1]
}

//generate forecast error, fe
gen fe = abs(eps - forecast)

//generate relative forecast accuracy, relacc
egen median = median(fe)
egen stdev = sd(fe)
gen relacc = (median - fe)/stdev
drop median
drop stdev

//generate number of firms an analyst is covering in a given year, followed
gen year = year(date)
sort analystcode
egen followed = nvals(cusip), by(analystcode year)

//generate variable experience, exper
sort analystcode cusip year
gen exper = 0
local i = 1
while `i' <=4 {
	replace exper = exper[_n-1] if (year == year[_n-1] & cusip == cusip[_n-1] & analystcode == analystcode[_n-1])
	replace exper = exper[_n-1] + 1 if (year != year[_n-1] & cusip == cusip[_n-1] & analystcode == analystcode[_n-1])
	local i = `i' + 1
 }


sort exper
//Keep only forecasts made in 2020
keep if year == 2020

//winsorize relacc at the 1% level
ssc install winsor2 
winsor2 relacc, replace cuts(1 99)

//Exercise 2 - Summary Statistics and Plots

//earnings announcements left
sort epsdate cusip
generate nvals = 1 if cusip != cusip[_n-1]
count if nvals == 1
drop nvals 

//unique analysts left in the sample
sort analystcode
generate nvals = 1 if analystcode != analystcode[_n-1]
count if nvals == 1
drop nvals 

//On average, how many firms does an analyst follow in 2020? Provide also minimum number, maximum number, median and standard deviation
sort analystcode cusip
gen nvals = followed if analystcode != analystcode[_n-1]
sum nvals, d 
drop nvals

//On average, how many analysts follow a given earnings event? Provide also minimum number, maximum number, median and standard deviation
sort epsdate cusip
gen nvals = coverage if cusip != cusip[_n-1]
sum nvals, d 
drop nvals 

//On average, how many forecast does an analyst make during one day? Provide also minimum number, maximum number, median and standard deviation
sort analystcode date
egen n_forecasts = nvals(forecast), by(analystcode date)
gen daily_forecasts = n_forecasts if date != date[_n-1]
sum daily_forecasts, d

//Exercise 3 - Analysis

//Estimate the univariate model
reg relacc fatigue

//Estimate the same model but with log(fatigue)
gen ln_fatigue = ln(fatigue)
reg relacc ln_fatigue

//Estimate the multivariate model
reg relacc ln_fatigue exper followed
pwcorr ln_fatigue exper followed

//Regress coverage on fatigue
reg coverage fatigue

//Estimate the model with dummy variables

gen dummy_2 = 0
replace dummy_2 = 1 if fatigue == 2
gen dummy_3 = 0
replace dummy_3 = 1 if fatigue == 3
gen dummy_4 = 0
replace dummy_4 = 1 if fatigue == 4
gen dummy_plus = 0
replace dummy_plus = 1 if fatigue >= 5

reg relacc dummy_2 dummy_3 dummy_4 dummy_plus

//Hypothesis testing - first forecast twice as accurate forecast forecast made fifth or higher

test _b[_cons] - 2 * ( _b[dummy_plus] + _b[_cons] ) = 0 

//predicted values and residuals
predict relacc_pred
predict relacc_resid, residuals
scatter relacc_resid relacc_pred
pwcorr relacc_resid relacc_pred



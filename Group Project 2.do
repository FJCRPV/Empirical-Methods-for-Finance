*Do-file for the Assignment 2
*Alexandre Melo 39214, Francisco Perestrello 39001, Jorge Gouveia 39215

clear
cd "C:\Users\Francisco\Desktop\Nova SBE\Masters\S1\T2\Empirical Methods for Finance\Case II"
use eventstudy.dta

*1. Generate a dummy variable split that is equal to one for companies that do a stock split and those that don't. How many companies in the sample announce a split in 2006 and how many do not? 
generate split = 1 if et != .

generate split_unique = 1 if et == 0
count if split_unique == 1
sort permno
generate nvals = 1 if permno != permno[_n-1]
count if nvals == 1 // This value is the total number of companies
//Answer for how many companies didn't announce a stock split in 2006: total number of companies - companies that announced a split in 2006
drop nvals

*2. Compute companies' market capitalization (marketcap).
generate marketcap = (prc * shrout)

*3. Compute separately for companies that do split and those that don't the average market capitalization, beta, stock return in 2005 (BHR05) and stock return in 2006 (BHR06).
generate BHR05_unique = BHR05 if permno != permno[_n-1] // generating variables so that we have only one value of each of these variables per company, since different companies have different nummbers of observations
generate BHR06_unique = BHR06 if permno != permno[_n-1]
generate beta_unique = beta if permno != permno[_n-1]

summarize if split == 1
summarize if split == .

*4. Focus on the difference in average annual return in 2006. Is this difference statistically significant? 
replace split = 0 if split == .
ttest BHR06_unique, by(split)

*6. Drop companies that do not announce any stock split in 2006.
drop if et == .

*7. Generate a dummy variable eventwindow for the 21 days around the split announcement (-10,+10). Generate a dummy variable estimationwindow that indicates a 30 days period ending 20 days before the announcement (-50,-20). 
sort permno et
generate eventwindow = 1 if et == 10
local i = 1
while `i' <=20 {
	replace eventwindow = 1 if eventwindow[_n+1] == 1
	local i = `i' + 1
}

generate estimationwindow = 1 if et == -20
local i = 1
while `i' <=30 {
	replace estimationwindow = 1 if estimationwindow[_n+1] == 1
	local i = `i' + 1
}

*8. For each stock compute the average return (avgret) during the estimation window. 
generate estimation_values = ret if estimationwindow == 1
by permno: egen avgret = mean(estimation_values)

keep if eventwindow == 1 // cleaning up the data, dropping observations that are no longer needed
drop split eventwindow estimationwindow estimation_values BHR05_unique BHR06_unique beta_unique // cleaning up the data, dropping variables that are no longer needed

*9.  Compute abnormal returns according to three different definitions
*a) aret1 = ret − avgret
generate aret1 = ret - avgret

*b) aret2 = ret − vwret
generate aret2 = ret - vwret

*c) aret3 = ret − βmvwret
generate aret3 = ret - beta*vwret

*10. For each of the three definitions of abnormal returns, compute the cumulative abnormal returns (cumaret).
by permno: egen cumaret1 = total(aret1)
by permno: egen cumaret2 = total(aret2)
by permno: egen cumaret3 = total(aret3)

*11. For each of the three definitions of abnormal returns, test if the average eventwindow cumaret is statistically different from zero. 
replace cumaret1 = . if et != 10 // We are doing this so we have less observations, thus less degrees of freedom, for the t-tests. We can do this as every firm had the same cumaret value repeated for every day in the 21-day event window. 
replace cumaret2 = . if et != 10 // This way, each firm only has one observation of its cumaret.
replace cumaret3 = . if et != 10
ttest cumaret1 == 0
ttest cumaret2 == 0
ttest cumaret3 == 0

*12. Produce a plot of the average cumaret computed using the market model over the 21-days event window.del" 
sort et permno
by et: egen cumaret = sum(aret3)
collapse (mean) cumaret, by(et)
tw connected cumaret et, xtitle("Event Time") ytitle("Average Cumulative Abnormal Returns") xline(0)
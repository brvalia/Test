* TT book club - Missing data imputation 
* 12/05/2018 
* Doug Johnson/Niklas Heusch

* Comment from Eric *


gl data "/Users/dougjohnson/Documents/IDinsight/Tech Team/TT book club/mi/" 
gl data "C:\Users\Niklas\Dropbox (IDinsight)\TT book club"
* capture ssc install st0263 // install midiagplots
use "$data/nhanes.dta", clear 



** SECTIONS
* 1. Setup: draw a sample of 500 observations
* 2. Generate missing data for the sample
* 3. Multiple imputation: if outcomes are missing
* 4. Multiple imputation: if covariates are missing
* 5. Multiple imputation: if both are missing



*********************************
*	1. Draw an SRS of size 500 and setup
*********************************

set seed 109239
g rand=uniform() 
sort rand
keep if _n<=500
drop rand

  generate	region = 1 if region1 == 1
  replace	region = 2 if region2 == 1
  replace 	region = 3 if region3 == 1
  replace	region = 4 if region4 == 1
  order region, before(region1)

  
  
***************************************************************************************************
*	2. Generate missing data for the sample 
***************************************************************************************************

 * 1. Set MCAR missing variable 
 g randmiss=uniform() 
 sort randmiss 
 g mcar= _n<=125 
 
 * 2. Set MAR missing variable 
 
  ** MISSING OUTCOMES
  * missing values for blood pressure (if BMI is higher than 75th percentile, then there is a 4/5 chance the value is missing)
	g rand=uniform()
  summ bmi, det
  g bpdiast_d_mar = (bmi >= r(p75) & rand <= 0.8)
	drop rand	
	 
  ** MISSING COVARIATES 
  * missing values for BMI (if blood pressure is between below the 50th percentile, then there is a 1/5 chance the value is missing)
	g rand=uniform()
  summ bpdiast, det
  g bmi_d_mar = (bpdiast <= r(p50) & rand <= 0.2)
	drop rand
	
  * missing value for age (if BMI is lower than 25th percentile, then there is a 3/5 chance that age is missing)
	g rand=uniform()
  summ bmi, det
  g age_d_mar1 = (bmi <= r(p25) & rand <= 0.6)
	drop rand

  * missing value for age (if blood pressure is lower than the 25th percentile, then there is a 2/5 chance that age is missing)
	g rand=uniform()
  summ bpdiast, det
  g age_d_mar2 = (bpdiast <= r(p25) & rand <= 0.4)
	drop rand
	
  g age_d_mar = (age_d_mar1 == 1 | age_d_mar2 == 1)

  
  * 3. Generate the missing values
  g bpdiast_mar = bpdiast	if bpdiast_d_mar == 0
  g bmi_mar = bmi 			if bmi_d_mar == 0
  g age_mar = age 			if age_d_mar == 0

  order bpdiast_mar, after(bpdiast)
  order bmi_mar, after(bmi)
  order age_mar, after(age)

  

***************************************************************************************************
*	3. Multiple imputation (scenario 1: missing values for outcome)
*************************************************************************************************** 

* have a look at missing values
misstable sum bpdiast_mar bmi age, gen(miss_)
misstable patterns bpdiast bmi_mar age_mar 

* what predicts missingness
logit bpdiast_d_mar	bmi age 	female black i.region 		height weight		// surprisingly in a joint regression, bmi is not a significant predictor
logit bpdiast_d_mar	bmi															// but it is in a single one
	
* do the multiple imputation
mi set wide

mi register regular bmi age female black region height weight 
mi register imputed bpdiast_mar

mi describe

mi impute chained (regress) bpdiast_mar = bmi age female black i.region height weight, add(20) rseed(9478)
*mi impute mvn bpdiast_mar = bmi age female black i.region height weight, add(20) rseed(9478)

* run the regression 
mi estimate: reg bpdiast_mar bmi	 age	 female     black i.region     height weight	// with imputed values
			 reg bpdiast	 bmi     age     female     black i.region     height weight	// if there were no missing values
			 reg bpdiast_mar bmi	 age	 female     black i.region     height weight	// complete cases only
			 
* go back to before
mi unset			 
drop miss_bpdiast_mar mi_miss bpdiast_mar_*
  
  
  
***************************************************************************************************
*	4. Multiple imputation (scenario 2: missing values for covariates)
*************************************************************************************************** 

* have a look at missing values
misstable sum bmi_mar age_mar, gen(miss_)
misstable patterns bmi_mar age_mar 

* what predicts missingness
logit bmi_d_mar	bpdiast age_mar	female black i.region 		height weight		// joint regression: low blood pressure predicts BMI is missing
logit bmi_d_mar bpdiast 														
logit bmi_d_mar age_mar

logit age_d_mar bpdiast bmi_mar	female black i.region 		height weight		// joint regression: low blood pressure and low BMI predict age is missing
logit age_d_mar bpdiast 
logit age_d_mar bmi_mar

* do the multiple imputation
mi set wide

mi register regular bpdiast female black region height weight 
mi register imputed bmi_mar age_mar 

mi describe

mi impute chained (regress) bmi_mar age_mar = bpdiast female black i.region height weight, add(20) rseed(9478)
*mi impute mvn bmi_mar age_mar = bpdiast female black i.region height weight, add(20) rseed(9478)

* run the regression 
mi estimate: reg bpdiast	 bmi_mar age_mar female     black i.region     height weight	// with imputed values
			 reg bpdiast	 bmi     age     female     black i.region     height weight	// if there were no missing values
				 
			 reg bpdiast	 bmi_mar age_mar female     black i.region     height weight	// complete cases only

* go back to before
mi unset	
drop miss_bmi_mar miss_age_mar bmi_mar_* age_mar_*
			 
			
			
***************************************************************************************************
*	5. Multiple imputation (scenario 3: missing values for outcomes AND covariates)
*************************************************************************************************** 

* have a look at missing values
misstable sum bpdiast_mar bmi_mar age_mar, gen(miss_)
misstable patterns bpdiast_mar bmi_mar age_mar 

* what predicts missingness
logit bpdiast_d_mar bmi_mar age_mar female black i.region		height weight		// joint regression
logit bpdiast_d_mar bmi_mar age_mar													// BMI is a strong predictor of missing blood pressure

logit bmi_d_mar	bpdiast_mar age_mar	female black i.region 		height weight		// joint regression: low blood pressure predicts BMI is missing
logit bmi_d_mar bpdiast_mar 														
logit bmi_d_mar age_mar																// low age also predicts BMI missing

logit age_d_mar bpdiast_mar bmi_mar	female black i.region 		height weight		// joint regression: low blood pressure and low BMI predict age is missing
logit age_d_mar bpdiast_mar 
logit age_d_mar bmi_mar

* do the multiple imputation
mi set wide

mi register regular female black region height weight 
mi register imputed bpdiast_mar bmi_mar age_mar 

mi describe

*mi impute chained (regress) bpdiast_mar bmi_mar age_mar = female black i.region height weight, add(20) rseed(9479)
mi impute mvn bmi_mar age_mar = bpdiast female black i.region height weight, add(20) rseed(9479)

* run the regression 
mi estimate: reg bpdiast_mar	 bmi_mar age_mar female     black i.region     height weight	// with imputed values
			 reg bpdiast		 bmi     age     female     black i.region     height weight	// if there were no missing values
				 
			 reg bpdiast_mar	 bmi_mar age_mar female     black i.region     height weight	// complete cases only

* go back to before
mi unset	
drop miss_bmi_mar miss_age_mar bmi_mar_* age_mar_*			 
			 
			 	 

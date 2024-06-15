clear all 

* Cleaning Ipums Data for PDR article Urbina et al. 2024.
* Cleaning do file using Chile as an example. 

dis " "
dis " "
dis "--------------------------------------------------------------------------"
dis "============================= CHILE`i' =================================="
dis "--------------------------------------------------------------------------"
dis " "
dis " "

* Upload data set from IPUMS, Chile. 
* cd "/Users/danielar.urbina/Dropbox/ResearchNote_Hypergamy/Data/Ipums_do"

do ipumsi_00029.do 

*******************-SAMPLE 1: EDUCATIONAL TRENDS BY GENDER ******************

* Restrict age

keep if age >= 25

* Complete education information
keep if edattain != 9

* Generate birth cohorts

gen Cohort = year-age

label var Cohort "Birth Cohort"

* Birth Cohorts round to 10

capture gen Cohort_10 = Cohort  - mod(Cohort ,10)
capture gen Cohort_5 = Cohort  - mod(Cohort ,5)

* Generate cohort proportion by education 

label define Education 1 "Less than Primary" 2 "Primary Completed" 3 "Secondary completed" 4 "University Completed" 9 "Unknown"
label values  edattain Education

******* Female Advantage Index ******

* Proportion of women with a primary education in each birth cohort/over the total population in that category


forvalues x=1/4 {
by Cohort_10, sort: egen lpw_`x' = count (edattain) if sex==2
by Cohort_10, sort: egen lpm_`x' = count (edattain)if  sex==1
by Cohort_10, sort: egen lp_women_`x' = count(edattain) if edattain ==`x' & sex==2
by Cohort_10, sort: generate lp_women2_`x' = lp_women_`x'/lpw_`x'
by Cohort_10, sort: egen lp_men_`x' = count(edattain) if edattain ==`x' & sex==1
by Cohort_10, sort: generate lp_men2_`x' = lp_men_`x'/lpm_`x'
}


foreach v of varlist lp_men2_1 lp_men2_2 lp_men2_3 lp_men2_4 lp_women2_1 lp_women2_2 lp_women2_3 lp_women2_4   {
egen `v'_pr = mean(`v'), by(Cohort_10)
}

gen FA_num = (lp_women2_4_pr * (lp_men2_1_pr + lp_men2_2_pr + lp_men2_3_pr)) + (lp_women2_3_pr * (lp_men2_1_pr + lp_men2_2_pr)) + (lp_women2_2_pr * lp_men2_1_pr) 

gen FA_den =  1 - ((lp_women2_1_pr * lp_men2_1_pr) + (lp_women2_2_pr * lp_men2_2_pr) + (lp_women2_3_pr * lp_men2_3_pr) +  (lp_women2_4_pr * lp_men2_4_pr))

gen FA = FA_num/FA_den

drop lp_women_1-lp_men2_4

cd "~/Dropbox/ResearchNote_Hypergamy/Data/Universal_data"
capture drop  __000000                         
save chile_univ.dta, replace
outsheet using chile_univ.csv, comma nolabel replace

**** Generating grouped data set: female advantage index per cohort*******

gen Cohort_W_10 = Cohort_10 if sex == 2 
gen FA_women = FA if sex == 2 

keep FA_women Cohort_W_10 
collapse FA_women, by(Cohort_W_10)


cd "~/Dropbox/ResearchNote_Hypergamy/Data/Universal_data"
capture drop  __000000
save chile_FA.dta, replace
outsheet using chile_FA.csv, comma nolabel replace

*******************-SAMPLE 2: MARRIAGES WITHIN HOUSEHOLDS******************

* Preparing data sets for marital patterns 
clear all 

* Open sample 1 
use "~/Dropbox/ResearchNote_Hypergamy/Data/Universal_data/chile_univ.dta"

* Restricting only to cases with strong spouse link
keep if sprule ==1 | sprule ==2 

* Drop cases that are not married or cohabitating
keep if marstd == 210 | marstd == 220 

* Spouse education is missing.

keep if edattain_sp != 9

* Notes:
* Households are identified by the combination of sample and serial numbers. One household may contain more than one marriage. 
* The variable "related" contains the specific information of each respondent nested in a household in relation to the household head.
* Code is somewhat different in each country because of the "related" categories.
* The goal is to identify marriages and delete "duplicate" unions.

* Step 1: Identyfying marriages between head of households and spouses. 

duplicates tag sample serial if related == 1000 | related == 2100 | related == 2200 , gen (duplicate_spouse)

* We keep cases where we only have the head or spouse or unmarried partner in a sample serial combination. 

gen marr_hh = .
replace marr_hh = 1 if related == 1000 | related == 2100 | related == 2200
egen sex_hh = mode(sex) if duplicate_spouse >= 1, by(sample serial marr_hh)

* Balanced household in terms of sex composition.  Rule only to keep household head, drop the spouse or unmarried partner
drop if duplicate_spouse == 1 & sex == 1 & sex_hh == . //drop MALE respondents in balanced households. Otherwise duplicated unions. 
drop if duplicate_spouse == 3 & sex == 1 & sex_hh == .

* Unbalanced household in terms of sex composition
drop if  duplicate_spouse == 2 & sex == 1 & sex_hh == 2 
drop if  duplicate_spouse == 2 & sex == 2 & sex_hh == 1  

* Step 2: Identyfying other marriages---Child and Child in law.

* Identifying households with potential duplicated marriages between children and children in law. 
duplicates tag sample serial if related == 4300 | related == 3000 | related == 3300, gen (duplicate_child)

* Another way to flag households where there are unions concerning the children of the head of household. 

gen marr_children = .
replace marr_children = 1 if related == 3000 | related == 4300 | related == 3300

* Generating a variable that calculates the modal sex by sample, serial and households where there are married children and children in law (marr_children). This varibale is equal to 1 if the mode in the group is Male and 2 if its Female. If genders are balanced then it generates a missing value. 

egen sex_dom = mode(sex) if duplicate_child >= 1, by(sample serial marr_children)

* Drop Child in law in sex balanced households
drop if related == 4300 & duplicate_child == 1 & sex_dom == . //drop "children in law" respondents in balanced households
drop if related == 4300 & duplicate_child == 3 & sex_dom == . 
drop if related == 4300 & duplicate_child == 5 & sex_dom == . 
drop if related == 4300 & duplicate_child == 7 & sex_dom == . 
drop if related == 4300 & duplicate_child == 9 & sex_dom == . 
drop if related == 4300 &  duplicate_child == 11 & sex_dom == .

* For sex unbalanced households the rule is: we drop the case/s with the non-dominant sex. The assumption is that the spouse information is retained by the sex dominant cases. 

drop if duplicate_child == 2 & sex == 2 & sex_dom == 1 // dropping female respondents if dominant sex is male. 
drop if duplicate_child == 2 & sex == 1 & sex_dom == 2 // dropping male respondents if dominant sex is female. 
drop if duplicate_child == 4 & sex == 2 & sex_dom == 1 // dropping female respondents if dominant sex is male. 
drop if duplicate_child == 4 & sex == 1 & sex_dom == 2 // dropping male respondents if dominant sex is female. 
drop if duplicate_child == 6 & sex == 2 & sex_dom == 1 // dropping female respondents if dominant sex is male. 
drop if duplicate_child == 6 & sex == 1 & sex_dom == 2 // dropping male respondents if dominant sex is female. 
drop if duplicate_child == 8 & sex == 2 & sex_dom == 1 // dropping female respondents if dominant sex is male. 
drop if duplicate_child == 8 & sex == 1 & sex_dom == 2 // dropping male respondents if dominant sex is female. 
drop if duplicate_child == 10 & sex == 2 & sex_dom == 1 // dropping female respondents if dominant sex is male. 
drop if duplicate_child == 10 & sex == 1 & sex_dom == 2 // dropping male respondents if dominant sex is female. 

* Unbalanced and even households (rare)
drop if duplicate_child == 3 & sex == 2 & sex_dom == 1 // dropping female respondents if dominant sex is male. 
drop if duplicate_child == 3 & sex == 1 & sex_dom == 2 // dropping male respondents if dominant sex is female. 
drop if duplicate_child == 5 & sex == 2 & sex_dom == 1 // dropping female respondents if dominant sex is male. 
drop if duplicate_child == 5 & sex == 1 & sex_dom == 2 // dropping male respondents if dominant sex is female. 
drop if duplicate_child == 7 & sex == 2 & sex_dom == 1 // dropping female respondents if dominant sex is male. 
drop if duplicate_child == 7 & sex == 1 & sex_dom == 2 // dropping male respondents if dominant sex is female. 

* Step 3: Identyfying other marriages--Parent/Parent in law (one category)
* Keep cases where there is only one married parent or parent in law.

duplicates tag sample serial if related == 4200 | related == 4210 | related == 4220, gen (duplicate_parent)

* Another way to flag household where there are unions concerning the parents or parents in law of the head of household. 
gen marr_parents = .
replace marr_parents = 1 if related == 4200 | related == 4210 | related == 4220

* Generating a variable that calculates the modal sex by sample, serial and households where there are married parents/parents in law. This varibale is equal to 1 if the mode in the group is Male and 2 if its Female. If genders are balanced then it generates a missing value. 

egen sex_dom_par = mode(sex) if duplicate_parent >= 1, by(sample serial marr_parents)

* Sex balanced households
drop if duplicate_parent==1 & sex == 1 & sex_dom_par == . //keeping female R
drop if duplicate_parent==3 &  sex == 1 & sex_dom_par == . // keeping female R

* Sex unbalanced household (0 in Chile)

* Step 4: Identyfying other marriages---Sibling/Sibling in Law (one category + separate categories)

duplicates tag sample serial if related == 4400 | related ==4410 | related ==4430, gen (duplicate_sibling)

* Another way to flag household where there are unions concerning siblings of the head of household. 
gen marr_siblings = .
replace marr_siblings = 1 if related == 4400 | related ==4410 | related ==4430

* Generating a variable that calculates the modal sex by sample, serial and households where there are married siblings/siblings in law. This varibale is equal to 1 if the mode in the group is Male and 2 if its Female. If genders are balanced then it generates a missing value. 

egen sex_dom_sib = mode(sex) if duplicate_sibling >= 1, by(sample serial marr_siblings)

* Sex Balanced households
drop if duplicate_sibling==1 & sex == 1 & sex_dom_sib == . //keeping female R
drop if duplicate_sibling==3 & sex == 1 & sex_dom_sib == . 
drop if duplicate_sibling==5 & sex == 1 & sex_dom_sib == . 
drop if duplicate_sibling==7 & sex == 1 & sex_dom_sib == . 
drop if duplicate_sibling==9 & sex == 1 & sex_dom_sib == . 

* Sex Unbalanced households
drop if duplicate_sibling==2 & sex == 1 & sex_dom_sib == 2 // dropping female R is dominant sex is male 
drop if duplicate_sibling==2 & sex == 2 & sex_dom_sib == 1 // dropping male R is dominant sex is female 
drop if duplicate_sibling==4 & sex == 1 & sex_dom_sib == 2 // dropping female R is dominant sex is male 
drop if duplicate_sibling==4 & sex == 2 & sex_dom_sib == 1 // dropping male R is dominant sex is female 
drop if duplicate_sibling==6 & sex == 1 & sex_dom_sib == 2 // dropping female R is dominant sex is male 
drop if duplicate_sibling==6 & sex == 2 & sex_dom_sib == 1 // dropping male R is dominant sex is female 
drop if duplicate_sibling==8 & sex == 1 & sex_dom_sib == 2 // dropping female R is dominant sex is male 
drop if duplicate_sibling==8 & sex == 2 & sex_dom_sib == 1 // dropping male R is dominant sex is female 


***********************Generating variables in terms of husband and wife*******************

local x "edattain yrschool age"

foreach j of local x {
	gen `j'_W = `j' if sex ==2
	gen `j'_H = `j'   if sex ==1
	replace `j'_H = `j'_sp if sex_sp==1
	replace `j'_W = `j'_sp if sex_sp==2
}

gen Educ_H =  edattain_H 
gen Educ_W =  edattain_W



gen Cohort_W = year-age_W
gen Cohort_H= year-age_H

label var Cohort_W "Birth Cohort Wife"
label var Cohort_H "Birth Cohort Husband"

* Birth Cohorts round to 10

foreach i in H W {

	capture gen Cohort_`i'_10 = Cohort_`i'  - mod(Cohort_`i',10)
	*capture gen Cohort_`i'_5 = Cohort_`i'  - mod(Cohort_`i',5)

}


***********************Last filter *******************

* Keeps only couples where education of both partners is known (countries have different ways of indicating missing values)
keep if Educ_H!=. & Educ_W!=.
keep if Educ_H!=9 & Educ_W!=9
drop if Educ_H ==0 | Educ_W==0

* Restricting husbands and wives ages

keep if age_W >=25 
keep if age_H >=25 


* Generate marital status

gen union = 1 if marstd ==210
replace union = 2 if marstd == 220
label define union  1 "Married" 2 "Cohabitation" 
label values union union 

* Crossing 0...4  marriage barriers

foreach i of numlist 0/3 {
	gen crossing_`i' = abs(Educ_H-Educ_W)== `i' & Educ_H!=. & Educ_W!=.
} 


* Crossing -3...3  marriage barriers: hypogramy and hypergamy

foreach i of numlist -3/3 {
	gen hypohyper_`=`i'+4' = (Educ_H-Educ_W)== `i' & Educ_H!=. & Educ_W!=.
} 


gen hypergamy = .
replace hypergamy = 1 if hypohyper_5 ==1 | hypohyper_6 ==1 | hypohyper_7 ==1
replace hypergamy = 0 if hypohyper_1 ==1 | hypohyper_2 ==1 | hypohyper_3 ==1 | hypohyper_4 ==1

gen hypogamy = .
replace hypogamy = 0 if hypohyper_5 ==1 | hypohyper_6 ==1 | hypohyper_7 ==1 | hypohyper_4 ==1
replace hypogamy = 1 if hypohyper_1 ==1 | hypohyper_2 ==1 | hypohyper_3 ==1 

gen homogamy = .
replace homogamy = 1 if hypohyper_4 ==1 
replace homogamy = 0 if hypohyper_4 !=1 

gen mating = .
replace mating =1 if hypergamy ==1
replace mating =2 if hypogamy == 1
replace mating = 3 if homogamy ==1


label define mating 1 "Hypergamy" 2 "Hypogamy" 3 "Homogamy"
label values mating mating 

* Saving Data

drop  age_head sex_head  edattain_head  yrschool_head  edattain_W edattain_H
drop crossing_0 crossing_1 crossing_2 crossing_3 hypohyper_1 hypohyper_2 hypohyper_3 hypohyper_4 hypohyper_5 hypohyper_6 hypohyper_7

* Merge with FA dataset at the aggregate level

merge m:1 Cohort_W_10 using "~/Dropbox/ResearchNote_Hypergamy/Data/Universal_data/chile_FA.dta", force

keep if Cohort_W_10 > 1910

cd "~/Dropbox/ResearchNote_Hypergamy/Data/Married_data"
capture drop  __000000
save chile_un.dta, replace
outsheet using chile_un.csv, comma nolabel replace

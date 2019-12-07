/* Reading the original dataset which we downloaded from the Internet */

proc import datafile='/folders/myfolders/main_file_formatted.csv'
dbms=csv out=project replace;
run;

/* Manipulating the dataset by using different procedure in the SAS */

data project1 (drop= UID e_t p_c_s_D A_D B_I S_I M_I CURRENT_PINCODE_ID D_O_B D_D EMPLOYEE_CODE_ID	
		MOBILENO_AVL_FLAG STATE_ID DRIVING_FLAG	PASSPORT_FLAG PRI_O_A PRI_S_A PRI_D_A 
		SEC_O_A	SEC_S_A	SEC_D_A year_avg month_avg year_credit month_credit);
set project;
	
	/* e_t has missing values filled by Others */
	
	if e_t = ' ' then e_t = 'Others';
	
	/* Converting employment_type to numeric */
	
	if e_t = "Salaried" then e_t_num = 2;
	else if e_t = "Self employed" then e_t_num = 1;
	else e_t_num = 3;
	
	/* Converting Avg Acc. Age to proper format */
	
	year_avg = scan(avg_a_a,1,' ');
	month_avg = scan(avg_a_a,2,' ');
	length year_avg month_avg $10.;
	year_avg = tranwrd(year_avg,'yrs','');
	month_avg = tranwrd(month_avg,'mon','');
	month_avg = tranwrd(month_avg,'mo','');
	Avg_acc_age_in_months = sum(year_avg,month_avg);
	
	/* Converting Credit_history_length into proper format */
	
	Year_Credit = scan(credit_h_l,1,' ');
	Month_Credit = scan(credit_h_l,2,' ');
	length year_credit month_credit $10.;
	Year_Credit = tranwrd(Year_Credit,'yrs','');
	Month_Credit = tranwrd(Month_Credit,'mon','');
	Month_Credit = tranwrd(Month_Credit,'mo','');
	Credit_hist_len_in_months = sum(Year_Credit, Month_Credit);
	
	/* Generating Age column in the table */
	
	Age_yrs = yrdif(d_o_b,today(),'ACT/ACT');
	Age_yrs = int(age_yrs);
run;

/* Reading the same dataset but with the manipulation in the Excel */

proc import datafile='/folders/myfolders/project_file2.csv'
dbms=csv out=project2 replace;
run;


/* Creating binary variable of the Employment Type for further analysis */

data project_final (drop= UID e_t p_c_s_D B_I S_I M_I CURRENT_PINCODE_ID D_O_B D_D EMPLOYEE_CODE_ID	
		MOBILENO_AVL_FLAG STATE_ID DRIVING_FLAG	PASSPORT_FLAG PRI_O_A PRI_S_A PRI_D_A 
		SEC_O_A	SEC_S_A	SEC_D_A Avg_A_A Credit_h_l year_avg month_avg year_credit month_credit);
set project2;

	if e_t_num = 1 then Self_Employed = 1;
	else Self_Employed = 0;
	
	if e_t_num = 2 then Salaried = 1;
	else Salaried = 0;
	
	if e_t_num = 3 then Others = 1;
	else Others = 0;
	
	if Loan = 1 then Loan_char = 'Yes';
	else Loan_char = 'No';
	
	if income >= 100000 then income_int = 1;
	else income_int = 0;
	
	if credit_score >= 648 then credit_score_int = 1;
	else credit_score_int = 0;
	
run;

/* ****************************************** Hypothesis Testing ************************************ */

/* Randomly picking 100 observations from the dataset */

proc surveyselect data=project_final
      method=srs n=100 out=SampleSRS;
run;

/* Keeping income and loan column to measure an effect of income on loan */ 
data salary;
set samplesrs;
keep income loan_char;
run;

proc sort data=salary; by loan_char;
proc means data=salary;by loan_char; var income;
proc ttest data=salary; class loan_char; var income;
run;

/* Keeping credit score and loan to measure an effect on loan */
data credit_score;
set samplesrs;
keep credit_score loan_char;
run;

proc sort data=credit_score; by loan_char;
proc means data=credit_score;by loan_char; var credit_score;
proc ttest data=credit_score; class loan_char; var credit_score;
run;

/* Keeping loan and self employed to measure a relationship on loan */
proc freq data = project_final;
tables self_employed * loan/chisq norow nocol;
title 'Summary statistics of Loan and Self Employed';
run;

/* Income greater than 100k with Loan */

proc freq data=project_final;
tables Loan*income_int/chisq norow nocol;
run;

/* Credit score greater than 648 with loan */

proc freq data=project_final;
tables Loan*credit_score_int/chisq norow nocol;
run;


/* ****************************** SUMMARY STATISTICS ********************************** */

/* Statistics for Continuous variables */
proc means data=project_final maxdec=1; 
var INCOME CREDIT_SCORE PRI_N_O_A PRI_A_A PRI_C_B SEC_N_O_A SEC_A_A SEC_C_B PRI_I_A SEC_I_A NEW_A_I_L_S_M
DEL_A_I_L_S_M NO_O_I Avg_acc_age_in_months Credit_hist_len_in_months age_year_int;
title "Summary Statistics of the necessary variables";
run;

/* Statistics for Categorical Variable And Chi-Square test */
proc freq data=project_final;
tables LOAN*e_t_num/chisq norow nocol;
title "Frequency distribution of Loan and Employment Type";
run;

/* To see which employment type are getting loan, average income, minimum & maximum income. */
ods noproctitle;
proc means data=project_final maxdec=0;
class e_t_num loan;
var income;
title 'Table of Employment Type in which Loan has been sanctioned or not w.r.t Income';
run;

/* To check average credit score require to pass the loan */
ods noproctitle;
proc means data=project_final maxdec=0;
class loan;
var credit_score;
title 'To check average credit_score required to sanctioned Loan';
run;


/* ************************************* Logistic Regression ************************************ */

proc freq; tables loan*(Salaried Self_Employed) Salaried*Self_Employed/chisq;
proc corr data = project_final; var e_t_num credit_score pri_c_b new_a_i_l_s_m del_a_i_l_s_m
			 no_o_i Avg_acc_age_in_months Credit_hist_len_in_months Age_year_int;
proc logistic descending data=project_final; 
model Loan = credit_score income Avg_acc_age_in_months Credit_hist_len_in_months
			Age_year_int del_a_i_l_s_m new_a_i_l_s_m Salaried Self_Employed/selection=stepwise;

proc logistic descending data=project_final;
model Loan = income credit_score DEL_A_I_L_S_M Credit_hist_len_in_months Avg_acc_age_in_months 
				NEW_A_I_L_S_M Age_year_int/selection=score;

proc logistic descending data=project_final;
model loan = income DEL_A_I_L_S_M/lackfit ctable;
run;

/* Issue - If we check collineaity then CREDIT_SCORE is collinear with every variable. So, we are not
		able to take any other combination */
		
/* ************************************ Multiple Linear Regression ********************************** */

/* To retrieve all the rows with the Loan Sanctioned = 'Yes' or 1 */

data regression_dataset;
set project_final;
	if loan = 1;
run;

/* Selecting 100 random obervations from the regression dataset */

proc surveyselect data=regression_dataset
      method=srs n=100 out=SampleSRS1;
run;

/* Summary Statistics & correlation table */

proc means data=regression_dataset; var INCOME PRI_C_B SEC_C_B Avg_acc_age_in_months age_year_int Salaried 
			Self_Employed;
proc freq data=regression_dataset; tables salaried*self_employed/chisq;
proc corr data=regression_datase; var INCOME PRI_C_B SEC_C_B Avg_acc_age_in_months age_year_int Salaried 
			Self_Employed;

/* Intializing the Stepwise process */

proc reg data = samplesrs1; 
model A_D = INCOME PRI_C_B SEC_C_B Avg_acc_age_in_months age_year_int Salaried 
			Self_Employed /selection=stepwise;

proc reg data = samplesrs1;
model A_D = INCOME Salaried age_year_int Avg_acc_age_in_months Self_Employed PRI_C_B/selection=adjrsq;

proc reg data = samplesrs1;
model A_D = INCOME/R influence;
	plot A_D*Income;
	plot r.*npp.;
run;

data sample1;
set project_final;
	if income >= 100000;
	keep income loan_char;
run;

proc sort data=sample1; by loan_char;
proc means data=sample1;by loan_char; var income;
proc ttest data=sample1; class loan_char; var income;
run;

data sample2;
set project_final;
	if credit_score >= 648;
	keep credit_score loan_char;
run;

proc sort data=sample2; by loan_char;
proc means data=sample2;by loan_char; var credit_score;
proc ttest data=sample2; class loan_char; var credit_score;
run;

data sample3;
set project_final;
	if self_employed = 1;
	keep self_employed loan_char;
run;

proc sort data=sample3; by loan_char;
proc means data=sample3;by loan_char; var self_employed;
proc ttest data=sample3; class loan_char; var self_employed;
run;


proc contents data=project;
run;

/*
proc format;
	value	score	0-<19 = 0
					600-<650 = 1
					650-<700 = 2
					700-<800 = 3
					800-High = 4
					Other = 'Missing';
run;
*/
/* Exporting dataset into excel */

/*proc export data=project1
dbms=xlsx outfile='/folders/myfolders/project_file.xlsx' replace;
run;*/


/* F and T test for measuring significant difference between mean salaries */
/* 
data salary_gt_10k;
set project_final;
	if Loan = 1 then Loan_char = "Yes";
	else Loan_char = 'No';
	if Loan_char = 'Yes' and income >= 100000;
	keep loan_char income;
run;

data salary_lt_10k;
set project_final;
	if Loan = 1 then Loan_char = "Yes";
	else Loan_char = 'No';
	if Loan_char = 'No' and income < 100000;
	keep loan_char income;
run;


data joined;
set salary_gt_10k salary_lt_10k;
run;

proc sort; by loan_char;
proc means;by loan_char; var income;
proc ttest; class loan_char; var income;
run;
*/
/* F and T test to measure significant difference between mean score of credit score */
/*
data credit_gt_647;
set project_final;
	if Loan = 1 then Loan_char = 'Yes';
	else Loan_char = 'No';
	if Loan_char = 'Yes' and credit_score > 647;
	keep loan_char credit_score;
run;

data obs_10;
set credit_gt_647 (obs=10);
run;

data credit_lt_648;
set project_final;
	if Loan = 1 then Loan_char = 'Yes';
	else Loan_char = 'No';
	if Loan_char = 'No' and credit_score < 648;
	keep loan_char credit_score;
run;

data obs_lt_10;
set credit_lt_648 (obs=10);
run;


data concat;
set obs_10 obs_lt_10;
run;

proc sort; by loan_char;
proc means;by loan_char; var credit_score;
proc ttest; class loan_char; var credit_score;
run;

*/
/* F and T tests to check self employed got more loan then the other employment type */
/*
data self_employed;
set project_final;
	if e_t_num = 1 then e_t_char = 'Self Employed';
	else e_t_char = 'Others';
	keep e_t_char loan;
run;

data first_50;
set self_employed (obs=50);
run;

proc sort; by e_t_char;
proc means;by e_t_char; var Loan;
proc ttest; class e_t_char; var Loan;
run;

data sample;
set project_final;
	keep credit_score loan;
run;

data obs_first_50;
set sample(obs=50);
run;

proc sort; by loan;
proc means;by loan; var credit_score;
proc ttest; class loan; var credit_score;
run;

data sample2;
set project_final;
	keep income loan;
run;

data obs_first_100;
set sample2(obs=100);
run;

proc sort; by loan;
proc means;by loan; var income;
proc ttest; class loan; var income;
run;
*/
		
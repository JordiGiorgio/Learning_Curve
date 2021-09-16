
%let dir = C:\Users\Jordi\OneDrive\Documents\EDUCATION\METRO COLLEGE OF TECHNOLOGY\DATA SCIENCE\SAS BUSINESS PROJECT\Project Data Files\9. Black Friday Data;
LIBNAME kgb "&dir.";
%include "C:\Users\Jordi\OneDrive\Documents\EDUCATION\METRO COLLEGE OF TECHNOLOGY\DATA SCIENCE\SAS BUSINESS PROJECT\Project Data Files\9. Black Friday Data\SAS PROJECT_BlFr_KGB_DEFINED MACROS.sas";
*** 
Business Questions from datasets:
Dataset: BLACK FRIDAY
a.	Problem statement:
Black Friday is the Friday following the Thanksgiving Day in US which is celebrated on the fourth Thursday of November. For this event, all 	businesses try to offer highly promoted sales by attracting any type of customers, targeting any place of sales in order to maximize their sales during this period. So the analysis of this dataset is to anticipate the expected turnover for the next Black Friday.

b.	Hypothesis:
a.	Is Product_Category_1 be the most purchased product?
b.	Are the people aged 26-35 the best customers?
c.	Is there a relationship between Purchase and people Occupation?
d.	Is there a relationship between Purchase and the City_Category?
e.	Is there a relationship between the gender and the purchase?
f.	Is there a relationship between Age and Purchase?
g.	Is there a relationship between Marital_Status and Purchase?
h.	Is there a relationship between Purchase and Product_Category_1, Product_Category_2 or Product_Category_3?

c.	Study Framework
i.	Dependent variable
y = Purchase
i.	Independent variables
X = Gender, Age, Occupation, City_Category, Product_Category_1, Product_Category_2, Product_Category_3, Marital_Status
***;

* Turning Macro function on;

proc options option = macro;
run;

* Turn on all options;
Options MLogic MPrint SymbolGen;


* Reading the main data file ;

data kgb.trainn;
set kgb.black_friday;
run;
proc print data = kgb.train (obs = 10);
run;

* Converting 2 variables from numeric to character;

data kgb.train;
set kgb.trainn;
Married = put(Marital_Status,1.);
Activity = put(occupation,2.);
drop occupation Marital_Status;
run;

data kgb.test;
set kgb.ffriday2;
Married = put(Marital_Status,1.);
Activity = put(occupation,2.);
drop occupation Marital_Status;
run;

proc print data = kgb.train (obs=10);
run;

proc print data = kgb.test (obs=10);
run;



* Copying the original dataset;

proc sql;
CREATE TABLE kgb.traindup AS
SELECT Gender,
	   Age,
	   City_Category,
	   Stay_In_Current_City_Years,
	   Product_Category_1,
	   Product_Category_2,
	   Product_Category_3,
	   Purchase,
	   Married,
	   Activity
FROM kgb.train;
QUIT;
PROC PRINT DATA = kgb.traindup (obs=10);
RUN;

/**SPLIT INTO NUMERIC AND CHARACTER VARAIBLES
 DISPLAYING TYPES OF VARIABLES **/

*%PUT &NUM_ONLY;
*%PUT &CHAR_ONLY;
* Dataset Contents;

%contents(dataset=kgb.traindup);


PROC SQL;
SELECT NAME INTO : NUM_ONLY SEPARATED BY " "
FROM CONTENTS
WHERE TYPE EQ 1
;
SELECT NAME INTO : CHAR_ONLY SEPARATED BY " "
FROM CONTENTS
WHERE TYPE EQ 2
;
QUIT;



* DATASET PROFILING ;
*MODULAR MACRO;
** Data Profiling for Numeric variables;

ODS PDF FILE = "&DIR.\NUMERIC_SUMMARY_&SYSDATE9..PDF";
%DP_NUMERIC(DSN =kgb.traindup ,VAR = &NUM_ONLY);
ODS PDF CLOSE;

** Data Profiling for categorical variables;
ODS PDF FILE= "&DIR.\CHAR_UNI_ANALYSIS.pdf";
%UNI_ANALYSIS_CHAR (DSN =kgb.traindup, CVAR =&CHAR_ONLY , COLOR=graphdata6 );
ODS PDF CLOSE;




* Exploratory Data Analysis;
*** Variable Identification;
   *** Age : Categorical 
   *** City_Category : Categorical 
   *** Gender : Categorical 
   *** Product_ID : Categorical 
   *** Stay_In_Current_City_Years : Categorical 
   *** Married : Categorical
   *** Activity : Categorical
   *** Purchase : Numeric
   *** Product_Category_1 : Numeric 
   *** Product_Category_2 : Numeric 
   *** Product_Category_3 : Numeric 
   *** User_ID : Numeric;

* Contents of the main train and test datasets;
%contents(dataset =kgb.new_ttrain3);


* Analysis on train dataset;

*** Univariate analysis of all continuous variables;

%DP_NUMERIC(DSN =kgb.train ,num = &NUM_ONLY);

*** Univariate analysis of one continuous variable at a time;

%univarnum(dataset = kgb.train, num =Product_Category_2);

%univarnumplot(dataset = kgb.train, num =Product_Category_3);

%univarnumplot_(dataset = kgb.train, num =Product_Category_3);

%uniboxplot(dataset = kgb.train, num =Product_Category_1);

*** Univariate analysis of all categorical variables;
     %UNI_ANALYSIS_CHAR(DSN = kgb.train ,cat = &CHAR_ONLY, COLOR= red);

*** Univariate analysis of one categorical variable at a time;

     %univarcat(dataset = kgb.train , cat = purchase);

     %univarcatbar(dataset = kgb.train, cat = activity);

     %pie(dftrain = kgb.train, cat = activity);

	 

*****************************************************************


*** Bivariate analysis;
*** *** Purchase vs a categorical variable;
     %bivcontcat(dataset=kgb.train, target = purchase, cat=married);


%bivcontcatt(dataset= kgb.new_ttrain4, target = purchase, cat= product_category, ccat= job, type= percent);

%bivcontcattt(dataset= kgb.new_ttrain4, cat= gender, num=Product_Category_1 , nnum=Product_Category_2);

%matrix(dataset= kgb.train, target= purchase, num=Product_Category_1 , nnum= Product_Category_2);

%bivcontcatttt(dataset=kgb.train , cat=activity , catt=city_category ); 

proc print data = kgb.new_ttrain4 (obs=10);
run;

*** *** Discretization of continuous variable 'Purchase';

	 %binning(dataset=kgb.train, num=purchase);

*** *** Formatting 'Purchase' variable;
PROC FORMAT;
 VALUE amt LOW -8000 = "Silver Class"
		   8000- high = "Gold Class"
;
RUN;

*** *** Looking for some relation between 'Purchase' and categorical variable;

   %bivcontcat2(dataset =kgb.train, target=purchase , cat=activity);



*** *** One Way ANOVA between Purchase and categorical variable;

     %anova(dataset= kgb.train, target=purchase, cat= age);
     %ttest(dataset =kgb.train, target=purchase, cat=gender);


*****************************************************************


*** *** Purchase vs a numeric variable;

     %scattered(num=purchase,nnum=Product_Category_1);
     %Corr1(dataset= kgb.train, target= purchase, num= Product_Category_1);
	 %scatter(dataset = kgb.train, nnum=Product_Category_1, num=purchase)
     %matrixcorr (dataset= kgb.train, num= Product_Category_1, nnum= Product_Category_2);


****************************************************************;



****************************************************************;

* Handling Missing Values in train dataset (only);
*** Normality Test in Continuous variables;

%normality(dataset = kgb.train, num= purchase);

*** Replacing missing values with median or mean in continous variables;

%nmissingnum(dftrain= kgb.train, num= Product_Category_2 , mthd = median);

*** New check-up of missing values in continous variables;

%univarnum(dataset= kgb.train, num = Product_Category_2);

* Handling outliers in continuous variables;

*** *** Removing the outliers;

%outliersremoving(dataset = kgb.train, num=product_category_1);

*** *** Checking after outliers removing;

%outliersremoved(num= Product_Category_1);


* Selection of the last relevant variables;

PROC SQL;
 CREATE TABLE KGB.new_train AS
 SELECT Age,
   City_Category,
   Gender,
   Stay_In_Current_City_Years,
   Activity,
   Purchase,
   Product_Category_1,
   Product_Category_2
FROM KGB.ttttrain 
;
QUIT;


proc print data = KGB.new_train (obs=10);
run;
****************************************************************
* Variable Transformation;
*By analyzing the amount of purchase according the activities we 
can divide the activities into 3 classes : the group from 0 to 
$2.10^8, the second group from $2.10^8 to $4.10^8 and the third 
one beyond $4.10^8;
* We will also consider three groups of age : '0-17', '18-45' and
'46-55+';

data kgb.new_train2;
set kgb.new_train;
if activity in (' 0' ' 4' ' 7' ) then job = 'Job1';
else if activity in (' 1' ' 2' '12' '14' '16' '17' '20') then job = 'Job2';
else job = 'Job3';
if age in ('46-50' '51-55' '55+') then group_age = '46-55+';
else if age in ('18-25' '26-35' '36-45') then group_age = '18-45';
else group_age = '0-17';
run;

proc print data = kgb.new_train2 (obs=10);
run;


title "Transformation of Purchase into Categorical";
proc freq data = kgb.new_train2 order = freq;
table purchase/missing;
format purchase amt.;
run;
title;

title "CHISQUARE TEST PURCHASE vs GENDER";
proc freq data=kgb.new_train2;
tables purchase * gender / chisq;
format purchase amt.;
run;
title;


%anova(dataset= kgb.new_train2, target=purchase, cat= job);
%bivcontcatt(dataset= kgb.new_train2, target = purchase, cat= job, ccat= group_age);



*** ### *** ### *** ### *** ### *** ### *** ### ******************;

* MODELLING;

%linearity(dataset= transformation, target= tpurchase, num=Product_Category_1);





     %univarcat(dataset = kgb.new_train, cat = age);

     %univarcatbar(dataset = kgb.new_train, cat = age);

     %pie(dataset= kgb.new_train , cat= activity, target= purchase);



**************************************************************;


* MODELLING;

* Logistic Regression;

* Verification of assumptions :
*** 2.1. Binary dependent variable 
         The Purchase variable has been transformed into a
         binary categorical variable

*** 2.2. Independency of the explanatory variables 
         The earlier Ttest and ANOVA revealed the relation
         between the target variable, except 'Married';


%chiqs(dataset= kgb.new_train2, cat=Stay_In_Current_City_Years , ccat=gender);

%anova(dataset= kgb.new_train2, target=product_category_2, cat= group_age);

%ttest(dataset =kgb.new_train2, target=product_category_2, cat=gender);
 

*** 2.3. Must not be any collinearity among independent variables;
         
%coll(dataset=kgb.new_train2, target=purchase, num=Product_Category_2, nnum=Product_Category_1);

proc print data = kgb.new_train2 (obs=10);
run;

* The VIF > 1 beween Product_Category_1 and Product_Category_2
infers that there is a collinearity. So because Product_Category 2
contains some missing values, Product_Category_1 will be kept;

* We are therefore tranforming Product_Category_1 into a 
categorical variable;

*The new dataset will be built without the 'Age', the 'Married' and 
the 'Product_Category_2' variables then : ;

data kgb.trainn;
set kgb.new_train2;
Product_Category = put(Product_Category_1,3.);
drop Product_Category_1;
run;

proc print data = kgb.trainn (obs=10);
run;

title "Transformation of Purchase into Categorical";
proc freq data = kgb.train  order = freq;
table Product_Category_1 /missing;
run;
title;


PROC SQL;
 CREATE TABLE kgb.new_ttrain2 AS
 SELECT 
   City_Category,
   Gender,
   Stay_In_Current_City_Years,
   group_age,
   job,
   Purchase,
   Product_Category_2
FROM kgb.new_train2
;
QUIT;



PROC SQL;
 CREATE TABLE kgb.new_ttrain3 AS
 SELECT 
   City_Category,
   Gender,
   Stay_In_Current_City_Years,
   group_age,
   job,
   Purchase,
   Product_Category
FROM kgb.trainn
;
QUIT;

proc print data = kgb.new_ttrain3 (obs=10);
run;



*** 2.4. Linearity of independent variables

    Since there now is only one continuous variable this assumption
    is therefore respected.


*** 2.5. Large sample size.
Our dataset contains not less than 550,668 observations, which 
validates the 5th assumption.;




*** *** Splitting the dataset into trainer 70% , tester 30%;

%traintestsplit(dataset= kgb.new_ttrain4, a=.7 );

*** *** 2.1. Without interactions;

%logit1(target= purchase, dataset=training);

PROC SQL;
 CREATE TABLE kgb.new_ttrain4 AS
 SELECT 
   City_Category,
   Stay_In_Current_City_Years,
   group_age,
   job,
   Purchase,
   Product_Category
FROM kgb.new_ttrain3
where group_age ne '0-17'
;
QUIT;

%logit2(target= purchase, dataset=training);


*** *** 2.2. Model With interactions;

%traintestsplit(dataset= kgb.new_ttrain4, a=.7 );

proc print data = kgb.new_ttrain2 (obs=10);
run;



%logit_interact(target= purchase, dataset= training);


*** Predictions based on selected Model;

title "Black Friday Sales Predictions";
proc logistic data = training outmodel = model;
class group_age(param=ref ref='18-45')
      City_Category(param=ref ref='A')
	  Product_Category(param=ref ref='1');
model &target. (event = 'Gold Class')= group_age | City_Category | Product_Category @2 /
CLODDS=PL SELECTION= BACKWARD SLSTAY=0.1 ;
format purchase amt.;
OUTPUT OUT= TEST P=PROB;
RUN;
QUIT;

PROC LOGISTIC INMODEL = MODEL;
 SCORE DATA = TESTING OUT=TESTING_SCORE01;
RUN;

proc print data = TESTING_SCORE01 (obs=10);
run;

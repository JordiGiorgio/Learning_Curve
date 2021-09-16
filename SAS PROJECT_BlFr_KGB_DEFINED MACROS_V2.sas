* DATASET PROFILING ;
%let dir = C:\Users\Jordi\OneDrive\Documents\EDUCATION\METRO COLLEGE OF TECHNOLOGY\DATA SCIENCE\SAS BUSINESS PROJECT\Project Data Files\9. Black Friday Data;
%PUT &DIR.;
%LET DSN = kgb.traindup;
%let dftrain = kgb.train;
%let dftest = kgb.test;
%let ccat = age;
*LIBNAME kgb "&DIR.";

%macro contents(dataset=);
proc contents data = &dataset out = contents;
run;
%mend contents;


*MODULAR MACRO;
** Data Profiling for Numeric variables;
%MACRO DP_NUMERIC(DSN = ,VAR = );

TITLE "DISTRIBUTION OF NUMERIC VARIABLES:SUMMARY";
PROC MEANS DATA = &DSN N NMISS MIN Q1 Q3 MEAN MEDIAN MAX;
 VAR &NUM_ONLY;
RUN;

%LET N = %SYSFUNC(COUNTW(&VAR));
%DO I= 1 %TO &N ;
  %LET X = %SCAN(&VAR,&I);
TITLE "DISTRIBUTION OF &X.: HISTOGRAM AND DENSITY CURVE";
PROC SGPLOT DATA = &DSN;
 HISTOGRAM &X.;
 DENSITY &X./ TYPE=KERNEL;
 KEYLEGEND / LOCATION=INSIDE POSITION=TOPRIGHT ACROSS=1 NOBORDER;
RUN;
QUIT;
TITLE "DISTRIBUTION OF &X. : VERTICAL BOX-PLOT";
PROC SGPLOT DATA = &DSN;
 VBOX &X.;
yaxis grid;
 xaxis display=(nolabel);
RUN;
QUIT;
%END;
title;
%MEND;


** Data Profiling for categorical variables;

%MACRO UNI_ANALYSIS_CHAR(DSN = ,cat= , COLOR= );
TITLE "COUNT OF ALL CATEGORICAL VARIABLES:SUMMARY";
PROC FREQ DATA= &DSN ORDER=FREQ;
 TABLE &cat/MISSING;
RUN;
%LET N = %SYSFUNC(COUNTW(&CVAR));
	%DO I = 1 %TO &N;
	%LET X = %SCAN(&cat,&I);
TITLE "COUNT BY %UPCASE(&X)";
PROC FREQ DATA = &DSN ORDER=FREQ;
 TABLE &X/MISSING;
RUN;
TITLE "COUNT BY %UPCASE(&X)";
PROC SGPLOT DATA = &DSN;
 VBAR &X/categoryorder=respasc barwidth=0.6 fillattrs= &COLOR;
 xaxis display=(nolabel);
RUN;
QUIT;
PROC TEMPLATE; 
DEFINE STATGRAPH PIE;  
BEGINGRAPH;    
ENTRYTITLE "COUNT BY MARITAL";  
LAYOUT REGION;      
PIECHART CATEGORY=&X / DATALABELLOCATION=OUTSIDE DATASKIN = CRISP  DATALABELCONTENT = ALL CATEGORYDIRECTION = CLOCKWISE START = 180 NAME = 'pie' ; 
DISCRETELEGEND 'pie'; 
ENDLAYOUT;  
ENDGRAPH; 
END; 
RUN; 
PROC SGRENDER DATA = &DSN TEMPLATE = PIE;
RUN;
%END;
title;
%MEND;


** Macros on train dataset;
*** Univariate analysis of continuous variables;
%let target = purchase;
%let num = Product_Category_2;
%let nnum = Product_Category_1;
%let ccat = gender;
%let cat = gender;




%MACRO DP_NUMERIC(DSN = ,num = );
TITLE "DISTRIBUTION OF ALL NUMERIC VARIABLES:SUMMARY";
PROC MEANS DATA = &DSN N NMISS MIN Q1 Q3 MEAN MEDIAN MAX; 
VAR &NUM_ONLY;
RUN;
%LET N = %SYSFUNC(COUNTW(&num));
%DO I= 1 %TO &N ;  
%LET X = %SCAN(&num,&I);
TITLE "DISTRIBUTION OF &X.: HISTOGRAM AND DENSITY CURVE";
PROC SGPLOT DATA = &DSN; 
HISTOGRAM &X.; 
DENSITY &X./ TYPE=KERNEL; KEYLEGEND / LOCATION=INSIDE POSITION=TOPRIGHT ACROSS=1 NOBORDER;
RUN;
QUIT;

TITLE "DISTRIBUTION OF &X. : VERTICAL BOX-PLOT";
PROC SGPLOT DATA = &DSN; 
VBOX &X.;
yaxis grid; 
xaxis display=(nolabel);
RUN;
QUIT;
%END;
%MEND;

*****;


%macro univarnum(dataset = , num =);
proc means data = &dataset maxdec = 2 n nmiss mean std max range;
title "Distribution of &num.";
var &num.;
run;
title;
%mend univarnum;

%macro univarnumplot(dataset = , num =);
proc sgplot data = &dataset;
title "Distribution of &num.";
histogram &num.;
density &num.;
run;
title;
%mend univarnumplot;

%macro univarnumplot_(dataset = , num =);
proc univariate data = &dataset;
title "Distribution of &num.";
var &num.;
histogram &num.;
run;
title;
%mend univarnumplot_;

%macro uniboxplot(dataset = , num =);
proc sgplot data = &dataset;
title "The 5 Number Summary of &num.";
vbox &num.;
run;
title;
%mend uniboxplot;

**/;


*******************************************************


*** Univariate analysis of all categorical variables;

%let cat = gender;


%MACRO UNI_ANALYSIS_CHAR(DSN = ,cat = , COLOR= );
TITLE "COUNT OF ALL CATEGORICAL VARAIBLES:SUMMARY";
PROC FREQ DATA= &DSN ORDER=FREQ; 
TABLE &cat/MISSING;
RUN;
%LET N = %SYSFUNC(COUNTW(&cat));	
%DO I = 1 %TO &N;	
%LET X = %SCAN(&cat,&I);
TITLE "COUNT BY %UPCASE(&X)";PROC FREQ DATA = &DSN ORDER=FREQ; 
TABLE &X/MISSING;
RUN;
TITLE "COUNT BY %UPCASE(&X)";
PROC SGPLOT DATA = &DSN; 
VBAR &X/categoryorder=respasc barwidth=0.6 fillattrs= &COLOR; 
xaxis display=(nolabel);
RUN;
QUIT;

PROC TEMPLATE; 
DEFINE STATGRAPH PIE;  
BEGINGRAPH;    
ENTRYTITLE "COUNT BY MARITAL";  
LAYOUT REGION;      
PIECHART CATEGORY=&X / DATALABELLOCATION=OUTSIDE DATASKIN = CRISP  DATALABELCONTENT = ALL CATEGORYDIRECTION = CLOCKWISE START = 180 NAME = 'pie' ; 
DISCRETELEGEND 'pie'; 
ENDLAYOUT;  
ENDGRAPH; 
END; 
RUN; 
PROC SGRENDER DATA = &DSN TEMPLATE = PIE;
RUN;
%END;
%MEND;


%macro univarcat(dataset = , cat = );
title "Descriptive Analysis : &cat.";
proc freq data = &dataset order = freq;
table &cat./missing;
run;
title;
%mend univarcat;

%macro univarcatbar(dataset = , cat = );
goptions reset = all;
pattern value = solid color = blue;
title "&cat.  Frequencies";
proc gchart data = &dataset;
vbar &cat.;
run;
title;
quit;
%mend univarcatbar;

%macro pie(dataset= , cat= , target= );
goptions reset=all device=png hsize=6 in vsize=4.8 in htext=12pt;

title h=12pt "Purchase Share by &cat.";

legend1 label=none
        shape=bar(2,1);

proc gchart data=&dataset;
  pie &cat. / sumvar= &target.
                value=arrow
                percent=arrow
                noheading
                percent=inside plabel=(height=9pt)
                slice=inside value=none
                name='&cat.';
  format purchase dollar20.2;
run;
quit;
%mend pie;


**************************************************************


*** Bivariate analysis;

*** *** Purchase vs a categorical variable;

%macro bivcontcat(dataset= , target =  , cat= );
proc sgplot data=&dataset;
title "&target. by &cat.";
vbar &cat. / response=&target.;
run;
title;
%mend bivcontcat;

%macro bivcontcatt(dataset= , target = , cat= , ccat= , type= );
title "&Target. vs &cat. and &ccat.";
Proc SGPlot Data=&dataset;
  VBar &cat. / Response=&target. Stat=&type. Group=&ccat. 
               Dataskin=Sheen Transparency=0.4 
               GroupOrder=Ascending;
Run;
title;
%mend bivcontcatt;

%macro bivcontcattt(dataset= , cat= , num= , nnum= );
proc sgplot data=&dataset;
title "&cat. vs &num. and &nnum.";
vbar &cat. / response=&num.; *stat=mean;
vbar &cat. / response=&nnum. barwidth=0.5; *stat=mean;
run;
title;
%mend bivcontcattt;


%macro bivcontcatttt(dataset= , cat= , catt= ); 
proc sort data=&dataset out=kgbtrain;
by &cat.;                     /* sort X categories */
run;
proc freq data=kgbtrain noprint;
by &cat.;                    /* X categories on BY statement */
tables &catt./ out=FreqOut;    /* Y (stacked groups) on TABLES statement */
run;
 
title "Where do Customers buy from ?";
proc sgplot data=FreqOut;
vbar &cat. / response=Percent group= &catt. groupdisplay=stack;
xaxis discreteorder=data;
yaxis grid values=(0 to 100 by 25) label="Percentage of Customers by &cat. and &catt.";
run;
title;
%mend bivcontcatttt;


%macro matrix(dataset= , target= , num= , nnum= );
title 'Scatter Plot Matrix';
ods graphics / reset width=5in height=5.5in imagename='Matrix_4x4_Diag';
proc sgscatter data=&dataset;
  matrix &target. &num. &nnum. / diagonal=(histogram normal);
run;
title;
%mend matrix;


*** *** *** One Way ANOVA between Purchase and categorical variable;
%macro anova(dataset= , target=, cat=);
ods graphics on;
title "&target. vs &cat.";
proc glm data = &dataset plots = diagnostics;
class &cat.;
model &target. = &cat. / ss3;
means &cat. / hovtest;
run;
quit;
title;
ods graphics off;
%mend anova;


*** *** Discretization of continuous variable 'Purchase';
%macro binning(dataset=, num=);
proc hpbin data=&dataset output= binned numbin=3 pseudo_quantile ;
   input &num. / numbin=2;    /* override global NUMBIN= option */
   id Product_ID;
run;
%mend binning;

*** *** Looking for some relation between 'Purchase' and categorical variable;

%macro bivcontcat2(dataset =, target= , cat= );
proc freq data = &dataset;
 table &num. * &cat./ NOCOL NOROW CHISQ ;
 FORMAT purchase amt.;
RUN;
%mend bivcontcat2;


%macro ttest(dataset = , target= , cat= );
proc ttest data=&dataset;
class &cat.;
var &target.;
run;
%mend ttest;



*****************************************************************


*** *** Purchase vs a continuous variable;
*** *** *** Looking for Correlation between Purchase and continuous variable;

%macro Corr1(dataset= , target= , num= );
ods graphics on;
title "Correlation &target. vs &num.";
PROC CORR DATA = &dataset PLOTS(maxpoints=none);
 VAR &num.;
 WITH &target.;
RUN;
title;
ods graphics off;
%mend Corr1;

%macro matrixcorr (dataset= , num= , nnum= );
TITLE "CORRELATION MATRIX &TARGET &NUM. &NNUM.";
PROC CORR DATA = &dataset PLOTS(MAXPOINTS=NONE)=MATRIX(HISTORGRAM);
 VAR  &target. &num. &nnum.;
RUN;
title;
%mend matrixcorr;


%macro scattered(num=, nnum= );
title "Trend &num. vs &nnum.";
proc sgplot data=kgb.train;
 scatter x=&nnum. y=&num. ;
run;
title;
%mend scattered;


%macro scatter(dataset= , nnum=, num=);
title "Purchase vs &nnum. and &ccat.";
proc sgscatter data = &dataset;;
plot &num. * &nnum.;
*/ datalabel = &ccat. group = &ccat.;
run;
title;
%mend scatter;



*******************************************;

** Macros (Dectecting missing values and outliers in test dataset);
*** Univariate analysis of continuous variables;
%let num = Product_Category_2;
%let nnum = Product_Category_1;
%let ccat = gender;
%let cat = gender;

%macro univarnum_test(num =);
proc means data = kgb.test maxdec = 2 n nmiss mean std max range;
title "Distribution of &num.";
var &num.;
run;
title;
%mend univarnum_test;

%macro univarnumplot_test(num=);
proc sgplot data = kgb.test;
title "Distribution of &num.";
histogram &num.;
density &num.;
run;
title;
%mend univarnumplot_test;

%macro univarnumplot_test_(num=);
proc univariate data = kgb.test;
title "Distribution of &num.";
var &num.;
histogram &num.;
run;
title;
%mend univarnumplot_test_;

%macro uniboxplot_test(num=);
proc sgplot data = kgb.test;
title "The 5 Number Summary of &num.";
vbox &num.;
run;
title;
%mend uniboxplot_test;


*** Univariate analysis of categorical variables;

%let cat = gender;

%macro univarcat_test(cat=);
title "Descriptive Analysis : &cat.";
proc freq data = kgb.test order = freq;
table &cat./missing;
run;
title;
%mend univarcat_test;

%macro univarcatbar_test(num=);
goptions reset = all;
pattern value = solid color = blue;
title "&num.  Frequencies";
proc gchart data = kgb.test;
vbar &num.;
run;
title;
quit;
%mend univarcatbar_test;


* Handling the missing values (only in train dataset)
*** *** *** Normality test of continuous variables;
%let num = Product_Category_2;
%macro normality (dataset = , num= );
proc univariate data = &dataset normal;
title "Normality Test of &num.";
var &num.;
probplot / normal (mu=est sigma=est);
histogram &num. / normal (color = red w=5);
run;
title;
%mend normality;

*** Replacing missing values with median or mean in continous variables;

%let num = Product_Category_2;

%macro nmissingnum (dftrain= , num= , mthd = );
PROC STDIZE DATA = &dftrain OUT= kgb.train method = &mthd. reponly;
title "Missing values replaced with median in &X. variable";
 VAR &num.;
RUN;
title;
%mend nmissingnum;

*** Handling outliers in continuous variables;
*** *** Removing the outliers;

%macro outliersremoving (dataset= , num=);
PROC MEANS DATA = &dataset MAXDEC=2 N P25 P75 QRANGE;
  VAR &num.;
OUTPUT OUT= TEMP P25= Q1 P75=Q3 QRANGE=IQR;
RUN;
data temp;
SET TEMP;
 LOWER_LIMIT = Q1- (3*IQR);
  UPPER_LIMIT = Q3 +(3*IQR);
RUN;

PROC SQL;
 CREATE TABLE kgb.ttrain AS
 SELECT A.*,B.LOWER_LIMIT,B.UPPER_LIMIT
 FROM kgb.train2 AS A , TEMP AS B
 ;
 QUIT;

DATA kgb.tttrain;
 SET kgb.ttrain;
 IF &num. LE LOWER_LIMIT THEN &num._RANGE = "BELOW LOWER LIMIT";
 ELSE IF &num. GE UPPER_LIMIT THEN &num._RANGE = "ABOVE UPPER LIMIT";
 ELSE  &num._RANGE = "WITHIN RANGE";
RUN;

PROC SQL;
 CREATE TABLE kgb.ttttrain AS
 SELECT *
 FROM kgb.tttrain
 WHERE &num._RANGE = "WITHIN RANGE";
QUIT;
%mend outliersremoving;

*** *** Checking after outliers removing;

%macro outliersremoved (num=UnansweredCalls);
PROC SGPLOT DATA = KGB.ttttrain ;
title "Outliers removed in the &num. variable";
 VBOX &num. / datalabel = &num.;
RUN; 
title;
%mend outliersremoved;


****************************************************************

* MODELLING;

* 1. Linear Regression;
*** *** Assumptions verification;
*** *** *** 1.1.1.	Linearity between target and independent variables;

%macro linearity(dataset= , target= , num= );
title "Checking Linearity &target. vs &num.";
proc reg data = &dataset;
model &target. = &num.;
plot &target. * &num.;
plot r. * p.;
run;
quit;
title;
%mend linearity;


*** *** *** 1.1.2.	Normal distribution of residuals;
%macro normalresiduals(dataset= , target= , num= , nnum= );
title "Test of Residuals Normality";
proc reg data = &dataset;
model &target. = &num. &nnum. / stb clb;
output out = std_residual p = predict r = residual;
run;
quit;

proc univariate data = std_residual normal;
var residual;
run;
title;
%mend normalresiduals;


*** *** *** 1.1.3.	Checking Colinearity within predictors;
%macro coll(dataset=, target=, num= ,nnum= );
Title "Detecting Colinearity within &num. and &nnum.";
proc reg data = &dataset;
model &target. = &num. &nnum. / VIF;
run;
quit;
title;
%mend coll;


*** *** *** 1.1.4.	Homoscedacity of continuous independent variables;
%let a =100;
%macro hmsdty(dataset= , target= , num= , nnum= , a= );
title "Simple Random Sampling";
proc surveyselect data= &dataset method=srs n=&a.
                  out= dataset_sampled;
run;
title;

title "Homoscedasticity Test for &num. and &nnum.";
proc model data = dataset_sampled;
parms a b c;
purchase = a + b * &num. + c * &nnum.; 
fit &target. / white;
run;
quit;
title;
%mend hmsdty;


*** *** *** 1.1.5.	Autocorrelation Test;
%macro autocorr(dataset= , target= , num= , nnum= );
title "DURBIN WATSON Autocorrelation Test";
proc reg data = kgb.new_train plots(maxpoints=none);
model purchase = Product_Category_1 Product_Category_2 / dw;
run;
quit;
title;
%mend autocorr;



* SPLITTING THE DATASET : TRAINING 70% , TESTING 30%;
%macro traintestsplit(dataset= , a= );
PROC SURVEYSELECT DATA = &dataset OUT=SAMPLE RATE = &a. OUTALL;
RUN;

PROC FREQ DATA = SAMPLE;
 TABLE SELECTED/MISSING;
RUN;

DATA TRAINING TESTING ;
 SET SAMPLE;
 IF SELECTED EQ 1 THEN OUTPUT TRAINING;
 ELSE OUTPUT TESTING;
RUN;
%mend traintestsplit;


/*  Multiple regression : Modelling ‘Purchase’ vs 
2 continuous variables */;

%macro multreg(dataset= , num = , nnum= );
title "Multiple Regression 'Purchase’ vs &num. and &nnum.";
proc reg data = &dataset;
model purchase = &num. &nnum.;
run;
quit;
title;
%mend multreg;


/* 	Multiple regression ‘Purchase’ vs all categorical variables
	Creating dummy variables for regression */;


%macro multregx(dataset = , num = );
title "Multiple Regression 'Purchase’ vs All Other Factors";
proc reg data = &dataset;
model &num. = Product_Category_1 Product_Category_2 
Male n_age1 n_age2 n_age3 n_age4 n_age5 n_age6
cityA cityB Stay_In_City0 Stay_In_City1 Stay_In_City2 
Stay_In_City3 act0 act1 act2 act3 act4 act5 act6 act7 act8 act9
act10 act11 act12 act13 act14 act15 act16 act17 act18 act19;
run;
quit;
title;
%mend multregx;


*** *** Best Model Finding (Fwd, Bkwd, Stpw selection);

%macro findbestreg(dataset= , target= , a= );
Title "Finding The Best Model (Fwd, Bkwd, Stpw selection)";
proc reg data = &dataset;
forward: model &target. = Product_Category_1 Product_Category_2 
Male n_age1 n_age2 n_age3 n_age4 n_age5 n_age6
cityA cityB act0 act1 act2 act3 act4 act5 act6 act7 act8 act9
act10 act11 act12 act13 act14 act15 act16 act17 act18 act19 /
selection = forward;
backward: model &target. = Product_Category_1 Product_Category_2 
Male n_age1 n_age2 n_age3 n_age4 n_age5 n_age6
cityA cityB act0 act1 act2 act3 act4 act5 act6 act7 act8 act9
act10 act11 act12 act13 act14 act15 act16 act17 act18 act19 /
selection = backward;
stepwise: model &target. = Product_Category_1 Product_Category_2 
Male n_age1 n_age2 n_age3 n_age4 n_age5 n_age6
cityA cityB act0 act1 act2 act3 act4 act5 act6 act7 act8 act9
act10 act11 act12 act13 act14 act15 act16 act17 act18 act19 /
selection = stepwise slentry= &a.;
run;
quit;
title;
%mend findbestreg;


*** *** *** Re-running Best model without removed variables;

%macro bestreg(dataset = , target=  , a = );
title "BLACK FRIDAY Best Regression Model";
proc reg data = training;
model purchase = Product_Category_1 Product_Category_2 
Male n_age1 n_age2 n_age4 n_age5 n_age6
cityA cityB act0 act1 act2 act3 act4 act5 act6 act7 act9
act10 act11 act12 act14 act15 act16 act17 act19 /selection = stepwise slentry =.5;
RUN;
QUIT;
title;
%mend bestreg;


*** *** Linear Regression Model Validation;

%macro multregxx(dataset = , num = );
title "Multiple Regression 'Purchase’ vs All Other Factors";
proc reg data = &dataset;
model &num. = Product_Category_1 Product_Category_2 
Male n_age1 n_age2 n_age4 n_age5 n_age6
cityA cityB act3 act4 act6 act7 act9
act10 act12 act14 act15 act16 act17 act19;
run;
quit;
title;
%mend multregxx;




* 2. Logistic Regression;

*** *** 2.1. Logistic Model Without interactions;

%macro chiqs(dataset= , cat= , ccat= );
title "CHISQUARE TEST &CAT. vs &CATT.";
proc freq data=&dataset;
tables &cat. * &ccat. / chisq;
run;
title;
%mend chiqs;


%macro logit1(target= , dataset= );
ods graphics on;
title "BLACK FRIDAY Purchase Modelled by Logistic Reg - No Interaction";
proc logistic data = &dataset plots (only) = (roc oddsratio);
class group_age(param=ref ref='18-45')
      City_Category(param=ref ref='A')
	  Gender(param=ref ref='M')
	  job(param=ref ref='Job2')
	  Product_Category(param=ref ref='1');
model &target. (event = 'Gold Class')= group_age gender job Product_Category / clodds=pl;
format purchase amt.;
run;
quit;
title;
ods graphics off;
%mend logit1;


%macro logit2(target= , dataset= );
ods graphics on;
title "BLACK FRIDAY Purchase Modelled by Logistic Reg - No Interaction";
proc logistic data = &dataset plots (only) = (roc oddsratio);
class group_age(param=ref ref='18-45')
      City_Category(param=ref ref='A')
	  Product_Category(param=ref ref='1');
model &target. (event = 'Gold Class')= group_age Product_Category / clodds=pl;
format purchase amt.;
run;
quit;
title;
ods graphics off;
%mend logit2;



%macro logit_interact(target= , dataset= , );
ods graphics on / discretemax = 13700;
title "BLACK FRIDAY Purchase Modelled by Logistic Reg - With Interaction";
proc logistic data = &dataset plots (maxpoints=none)=(roc oddsratio);
class group_age(param=ref ref='18-45')
      City_Category(param=ref ref='A')
	  Product_Category(param=ref ref='1');
	  model &target. (event = 'Gold Class')= group_age | City_Category | Product_Category @2 /
CLODDS=PL SELECTION= BACKWARD SLSTAY=0.1 ;
format purchase amt.;
*units Product_Category = .5;
oddsratio group_age;
oddsratio City_Category;
oddsratio Product_Category;
run;
quit;
title;
ods graphics off;
%mend logit_interact;

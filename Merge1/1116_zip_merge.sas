/*	Merge original with econ_char by zip*/
/*	Find if obs can get BRFSS elements and Econ_char elements */
/*	BRFSS and Econ_char both have missing values */
/*	11/21, zip with missing value has replaced in Econ_char */


/* Mark zip by name of dataset before merging */
proc sort data=snaped.original2011 nodupkey out=original_2011_2;
	by zip;
	run;
data original_2011_3;
	set original_2011_2;
	Dorig=1;
	run;
data econ_char_2011_2;
	set snaped.econ_char_2011;
	Decon=1;
	run;

/* Merge */
proc sql;
	create table merge1 as /*merge4: n=387 */
	select *
	from econ_char_2011_2 as a  
	left join original_2011_3 as b
	on a.zip=b.zip;
	quit;

proc print data=merge1;
	var zip Dorig Decon;run;

/* Get unique zip that didn't mathc with Econ_char */
data merge5;
	set merge4;
	Dadd=Dorig+Decon;
	run;
proc print data=merge5;
	var zip Dorig Decon;
	where Dadd^=2;run;



proc means data=merge2 nmiss;
	var MeanCashPublicAssistanceIncome
MeanSocialSecurityIncome
MeanSupplementalSecurityIncome
MedianHouseholdIncome
PerCapita
PercentPoverty
PercentSNAP
PercentUnemployed;
	;run;
	
	
/** Create zip1 - replaced zip by matching process **/
data original2013_2;
	set snaped.original2013;
	     if zip=85030 then zip1=85003;
	else if zip=85036 then zip1=85003;
	else if zip=85038 then zip1=85003;
	else if zip=85039 then zip1=85003;
	else if zip=85117 then zip1=85215;
	else if zip=85130 then zip1=85122;
	else if zip=85178 then zip1=85215;
	else if zip=85191 then zip1=85128;
	else if zip=85271 then zip1=85251;
	else if zip=85287 then zip1=85281;
	else if zip=85299 then zip1=85312;
	else if zip=85312 then zip1=85301;
	else if zip=85359 then zip1=85325;
	else if zip=85366 then zip1=85364;
	else if zip=85369 then zip1=85364;
	else if zip=85376 then zip1=85375;
	else if zip=85378 then zip1=85375;
	else if zip=85502 then zip1=85501;
	else if zip=85532 then zip1=85545;
	else if zip=85547 then zip1=85541;
	else if zip=85548 then zip1=85546;
	else if zip=85628 then zip1=85621;
	else if zip=85636 then zip1=85635;
	else if zip=85702 then zip1=85701;
	else if zip=85721 then zip1=85724;
	else if zip=85725 then zip1=85701;
	else if zip=85902 then zip1=85911;
	else if zip=86002 then zip1=86001;
	else if zip=86005 then zip1=86001;
	else if zip=86302 then zip1=86303;
	else if zip=86312 then zip1=86315;
	else if zip=86339 then zip1=86024;
	else if zip=86340 then zip1=86024;
	else if zip=86341 then zip1=86024;
	else if zip=86342 then zip1=86335;
	else if zip=86402 then zip1=86409;
	else if zip=86405 then zip1=86403;
	else if zip=86427 then zip1=86426;
	else if zip=86430 then zip1=86438;
	else if zip=86439 then zip1=86438;
	else if zip=86446 then zip1=86440;
	else if zip=85236 then zip1=85234;
	else if zip=77777 then zip1=.;
	else if zip=99999 then zip1=.;
	else zip1=zip;
	run;
proc means data=original2013_2 n nmiss;
var zip zip1;run;

data snaped2.original2013;
	set original2013_2;run;

proc print data=snaped.original2011;
	where zip=57785 or zip=79424 or zip=87825 or zip=89002 ;
	var zip ctycode1;run;
proc print data=snaped.original2013;
	where zip=54498 or zip=74701 or zip=84040 or zip=87124 or zip=89426 or zip=92374;
	var zip ctycode1;run;
	
	
	
/* Merege original with econ */
/*merged dataset*/
/*zip1 is from original, zip is from econ_char_2013 */
proc sql;
	create table merge as
	select a. *, b. *
	from snaped2.original2013 as a
	left join snaped.econ_char_2013 as b
	on a.zip1=b.zip;
	quit;
/* n=4252, variable 616 */

proc print data=merge;
	var zip medianhouseholdincome;
	where zip=85340 or zip=85321 or zip=85534 or zip=85607 or zip=85713 or zip=85719 or zip=85713 or
zip=85901 or zip=86025 or zip=86035 or zip=86001 or zip=86502 or zip=86024 or zip=86301;
run;

data snaped2.orig_econ_2013;
	set merge;
	run;
	
	
proc export
    data = snaped2.orig_econ_2011
    dbms = csv
    outfile = "C:/Users/asaito1/Downloads/orig.econ.2011.csv"
    replace;
run;
proc export
    data = snaped2.orig_econ_2013
    dbms = csv
    outfile = "/home/asaito10/SNAP-Ed2/orig.econ.2013.csv"
    replace;
run;
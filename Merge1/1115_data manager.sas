/*=======================================*/
/* 		ECONOMIC CHARACTERISTICS 2011	 */
/*=======================================*/

/** Import an XLSX file.  **/

PROC IMPORT DATAFILE="/home/asaito10/SNAP-Ed/Economic_Characteristics2011.xlsx"
		    OUT=snaped.econchara2011_raw
		    DBMS=XLSX
		    REPLACE;
RUN;

/* Convert zip into numeric in econ chara */
data econchara2011_2;
	set snaped.econchara2011_raw;
	zip=input(zipcode,best8.);
run;

	/* delete redundunt zip */
proc sort data=econchara2011_2 nodup out=econchara2011_3;
	by zip;
	run;
proc contents data=econchara2011_3;run;
	/* check redudndunt zip */
proc summary data=econchara2011_2 nway; 
   class zip / descending order=freq;
   output out=count;
   run; 
proc print data=count(obs=110);
   run;
proc print data=econchara2011_2;
	where zip=85621;run;
	
	/* Use econ_char_2011 from now on as Economic Characteristics 2011 data*/
data snaped.econ_char_2011;
	set econchara2011_3;run;	
	

/* Check how the dataset look like! */
proc means data=snaped.econ_char_2011 n mean min max nmiss;
run;
proc contents data=snaped.econ_char_2011;run;

/* Check missing values */
data econchara2011_4;
	set snaped.econ_char_2011;
	if MeanCashPublicAssistanceIncome^=. then D1=1; else D1=0;
	if MeanSocialSecurityIncome^=. then D2=1; else D2=0;
	if MeanSupplementalSecurityIncome^=. then D3=1; else D3=0;
	if MedianHouseholdIncome^=. then D4=1; else D4=0;
	if PerCapita^=. then D5=1; else D5=0;
	if PercentPoverty^=. then D6=1; else D6=0;
	if PercentSNAP^=. then D7=1; else D7=0;
	if PercentUnemployed^=. then D8=1; else D8=0;
	Dadd=D1+D2+D3+D4+D5+D6+D7+D8;
	Missing_value=8-Dadd;
	run;
proc freq data=econchara2011_4;
	tables Missing_value;run;

proc print data=econchara2011_4;
	where Missing_value^=0;run;


/* Zip out of AZ? */	
proc print data=econchara2011_4;
	where zip<85001 or zip>86556; /*Out of AZ */
	var zip;
	run;
	
	
/*=======================================*/
/* 		ORIGINAL DATA 2011	 */
/*=======================================*/

proc contents data=snaped.original2011;
run;
proc means data=snaped.original2011 n mean min max nmiss;
	var age marital educa employ income2 sex _incomg _imprace smokday2 
		ctycode1 foodstamp frlunch wic zip;run;
proc freq data=snaped.original2011;
	tables income2;
	tables _incomg;
	run;

/* unique number of zip */
proc sort data=snaped.original2011 nodupkey out=original_2011_2;
	by zip;
	run;
proc print data=original_2011_2;
	where zip<85001 or zip>86556; /*Out of AZ */
	var zip;
	run;
proc freq data=snaped.original2011;
	where (zip=. or zip=57785 or zip=77777 or zip=79424 or zip=87825 or zip=89002 or zip=99999);
	tables zip;
	run;	
	
	
	
/*=======================================*/
/* 		ECONOMIC CHARACTERISTICS 2013	 */
/*=======================================*/

/* Convert zip into numeric in econ chara */
data econchara2013_2;
	set snaped.econchara2013;
	zip=input(zipcode,best8.);
run;

	/* delete redundunt zip */
proc sort data=econchara2013_2 nodup out=econchara2013_3;
	by zip;
	run;
proc contents data=econchara2013_3;run;
	
	/* Use econ_char_2013 from now on as Economic Characteristics 2013 data*/
data snaped.econ_char_2013;
	set econchara2013_3;run;	

/* Check how the dataset look like! */
proc means data=snaped.econ_char_2013 n mean min max nmiss;
run;


/* Check missing values */
data econchara2013_4;
	set snaped.econ_char_2013;
	if MeanCashPublicAssistanceIncome^=. then D1=1; else D1=0;
	if MeanSocialSecurityIncome^=. then D2=1; else D2=0;
	if MeanSupplementalSecurityIncome^=. then D3=1; else D3=0;
	if MedianHouseholdIncome^=. then D4=1; else D4=0;
	if PerCapita^=. then D5=1; else D5=0;
	if PercentPoverty^=. then D6=1; else D6=0;
	if PercentSNAP^=. then D7=1; else D7=0;
	if PercentUnemployed^=. then D8=1; else D8=0;
	Dadd=D1+D2+D3+D4+D5+D6+D7+D8;
	Missing_value=8-Dadd;
	run;
proc freq data=econchara2013_4;
	tables Missing_value;run;

proc print data=econchara2011_4;
	where Missing_value^=0;run;


/* Zip out of AZ? */	
proc print data=econchara2013_4;
	where zip<85001 or zip>86556; /*Out of AZ */
	var zip;
	run;
	
	
/*=======================================*/
/* 		ORIGINAL DATA 2013	 */
/*=======================================*/

proc contents data=snaped.original2013;
run;
data snaped.original2013;
	set snaped.original2013;
	zip=input(zipcode,best8.);
run;
proc means data=snaped.original2013 n mean min max nmiss;
	var age marital educa employ1 income2 sex _incomg _imprace smokday2 
		ctycode1 aaz3_1 aaz3_2 aaz3_3 zip;run;
proc freq data=snaped.original2013;
	tables income2;
	tables _incomg;
	run;

/* unique number of zip */
proc sort data=snaped.original2013 nodupkey out=original_2013_2;
	by zip;
	run;
proc print data=original_2013_2;
	where zip<85001 or zip>86556; /*Out of AZ */
	var zip;
	run;
proc freq data=snaped.original2013;
	where (zip=. or zip=54498 or zip=74701 or zip=77777 or zip=84040 or zip=87124
			or zip=89426 or zip=92374 or zip=99999);
	tables zip;
	run;	
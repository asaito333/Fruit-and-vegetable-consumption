
/* create 2*2 tables to figure out the sample */

proc freq data=snaped3.snapdata;
	tables year;
	tables year*eligbroad1;
	tables year*fs;
	run;
proc freq data=snaped3.snapdata;
	tables year*fs;
	where eligbroad1=1;
	run;
	
/* create subgroup for eligible people*/
data snaped3.elig;
	set snaped3.snapdata;
	where eligbroad1=1;
	run;
	
/* F&V */
ods graphics on;
proc surveymeans plots=none data=snaped3.snapdata mean stderr clm min max;
	STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;
	title "F&V: whole sample";
	var x_frutsum x_vegesum servings;
	run;	
proc surveymeans plots=none data=snaped3.elig mean stderr clm min max;
	STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;
	title "F&V: eligible sample";
	var x_frutsum x_vegesum servings;
	run;		
	
ods graphics off;	

/* T-test for F&V */
/* elig vs inelig for whole sample */
proc surveyreg data=snaped3.snapdata ;
	STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;
	title "T-test x_frutsum: elig=inelig, for whole sample";
	class eligbroad1;
	model x_frutsum=eligbroad1 /solution vadjust=none;
	run;
proc surveyreg data=snaped3.snapdata;
	STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;
	title "T-test x_vegesum: elig=inelig, for whole sample";
	class eligbroad1;
	model x_vegesum=eligbroad1 /solution vadjust=none;
	run;	
proc surveyreg data=snaped3.snapdata;
	STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;
	title "T-test servings: elig=inelig, for whole sample";
	class eligbroad1;
	model servings=eligbroad1 /solution vadjust=none;
	run;	
/* fs vs nofs for whole sample */
proc surveyreg data=snaped3.snapdata ;
	STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;
	title "T-test x_frutsum: fs=nofs, for whole sample";
	class fs;
	model x_frutsum=fs /solution vadjust=none;
	run;
proc surveyreg data=snaped3.snapdata;
	STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;
	title "T-test x_vegesum: fs=nofs, for whole sample";
	class fs;
	model x_vegesum=fs /solution vadjust=none;
	run;	
proc surveyreg data=snaped3.snapdata;
	STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;
	title "T-test servings: fs=nofs, for whole sample";
	class fs;
	model servings=fs /solution vadjust=none;
	run;		
	
/* fs vs nofs for eligible sample*/
proc surveyreg data=snaped3.elig ;
	STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;
	title "T-test x_frutsum: fs=nofs, for eligible sample";
	class fs;
	model x_frutsum=fs /solution vadjust=none;
	run;
proc surveyreg data=snaped3.elig;
	STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;
	title "T-test x_vegesum: fs=nofs, for eligible sample";
	class fs;
	model x_vegesum=fs /solution vadjust=none;
	run;	
proc surveyreg data=snaped3.elig;
	STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;
	title "T-test servings: fs=nofs, for eligible sample";
	class fs;
	model servings=fs /solution vadjust=none;
	run;	
	
/**** about covariates***/
proc contents data=snaped3.elig;run;

/* macro for descriptive stat*/
%let other =age	distance male female numhhd;
%let econ=PercentUne MedianHous MeanSocial MeanSupple MeanCashPu PercentSNA PerCapita PercentPov;
%let agedummy=age18to24 age25to34 age35to44 age45to54 age55to64 age65over;
%let county=Apache Cochise Coconino Gila Graham Greenlee LaPaz Maricopa Mohave Navajo Pima Pinal StCruz Yavapai Yuma;
%let marital=Married Divorced Widowed Separated Nevermarri Unmarriedc;
%let educa=noschool elementary somehs hsged somecolleg college;
%let employ=Employed Selfemploy Unemployed Homemaker Student Retired Unablework;
%let race=White Black Asian AmericanIn Hispanic;
%let smoke=Everydaysm Somedaysmo nosmoke;
%let income=lesss10000 lesss15000 lesss20000 lesss25000 lesss35000 lesss50000 lesss75000 Moree75000;
%let grocery=AllEstabli groceryden Population ppldensity;
%let snaped=Dsnaped contracter contracte0 contracte1;
%let partner=SNAPpartne;

/* means for eligible group */
proc sort data=snaped3.snapdata;
	by eligbroad1;run;
	
ods graphics on;
proc surveymeans plots=none data=snaped3.snapdata ;
	STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;
	title "covariates for whole sample by eligbroad1";
	var &other &marital &educa &employ &race &income &county &grocery &snaped &partner;
	by eligbroad1;
	run;		
ods graphics off;	

/* T-test for eligible group*/
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test age: fs=nofs, for eligible sample";
	model age=fs /solution vadjust=none;
run;	
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test distance: fs=nofs, for eligible sample";
	model distance=fs /solution vadjust=none;
run;	
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test male: fs=nofs, for eligible sample";
	model male=fs /solution vadjust=none;
run;	

/*marital*/
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test married: fs=nofs, for eligible sample";
	model married=fs /solution vadjust=none;
run;	
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test divorced: fs=nofs, for eligible sample";
	model divorced=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test widowed: fs=nofs, for eligible sample";
	model widowed=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test separated: fs=nofs, for eligible sample";
	model separated=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test nevermarri: fs=nofs, for eligible sample";
	model nevermarri=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test unmarriedc: fs=nofs, for eligible sample";
	model unmarriedc=fs /solution vadjust=none;
run;

/* educa*/
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test noschool: fs=nofs, for eligible sample";
	model noschool=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test elementary: fs=nofs, for eligible sample";
	model elementary=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test somehs: fs=nofs, for eligible sample";
	model somehs=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test hsged: fs=nofs, for eligible sample";
	model hsged=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test somecolleg: fs=nofs, for eligible sample";
	model somecolleg=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test college: fs=nofs, for eligible sample";
	model college=fs /solution vadjust=none;
run;

/* employ*/
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test employed: fs=nofs, for eligible sample";
	model employed=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test selfemploy: fs=nofs, for eligible sample";
	model selfemploy=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test unemployed: fs=nofs, for eligible sample";
	model unemployed=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test homemaker: fs=nofs, for eligible sample";
	model homemaker=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test student: fs=nofs, for eligible sample";
	model student=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test retired: fs=nofs, for eligible sample";
	model retired=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test unablework: fs=nofs, for eligible sample";
	model unablework=fs /solution vadjust=none;
run;

/*race*/
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test white: fs=nofs, for eligible sample";
	model white=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test black: fs=nofs, for eligible sample";
	model black=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test asian: fs=nofs, for eligible sample";
	model asian=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test americanin: fs=nofs, for eligible sample";
	model americanin=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test hispanic: fs=nofs, for eligible sample";
	model hispanic=fs /solution vadjust=none;
run;

/*income*/
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test lesss10000: fs=nofs, for eligible sample";
	model lesss10000=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test lesss15000: fs=nofs, for eligible sample";
	model lesss15000=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test lesss20000: fs=nofs, for eligible sample";
	model lesss20000=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test lesss25000: fs=nofs, for eligible sample";
	model lesss25000=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test lesss35000: fs=nofs, for eligible sample";
	model lesss35000=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test lesss50000: fs=nofs, for eligible sample";
	model lesss50000=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test lesss75000: fs=nofs, for eligible sample";
	model lesss75000=fs /solution vadjust=none;
run;

/* others*/
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test allestablishment: fs=nofs, for eligible sample";
	model allestabli=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test groceryden: fs=nofs, for eligible sample";
	model groceryden=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test population: fs=nofs, for eligible sample";
	model population=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test ppldensity: fs=nofs, for eligible sample";
	model ppldensity=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test dsnaped: fs=nofs, for eligible sample";
	model dsnaped=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test contracter: fs=nofs, for eligible sample";
	model contracter=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test contracte0: fs=nofs, for eligible sample";
	model contracte0=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test contracte1: fs=nofs, for eligible sample";
	model contracte1=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test snappartne: fs=nofs, for eligible sample";
	model snappartne=fs /solution vadjust=none;
run;

/*county*/
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test apache: fs=nofs, for eligible sample";
	model apache=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test cochise: fs=nofs, for eligible sample";
	model cochise=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test coconino: fs=nofs, for eligible sample";
	model coconino=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test gila: fs=nofs, for eligible sample";
	model gila=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test graham: fs=nofs, for eligible sample";
	model graham=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test greenlee: fs=nofs, for eligible sample";
	model greenlee=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test lapaz: fs=nofs, for eligible sample";
	model lapaz=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test maricopa: fs=nofs, for eligible sample";
	model maricopa=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test mohave: fs=nofs, for eligible sample";
	model mohave=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test navajo: fs=nofs, for eligible sample";
	model navajo=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test pima: fs=nofs, for eligible sample";
	model pima=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test pinal: fs=nofs, for eligible sample";
	model pinal=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test stcruz: fs=nofs, for eligible sample";
	model stcruz=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test yavapai: fs=nofs, for eligible sample";
	model yavapai=fs /solution vadjust=none;
run;
proc surveyreg data=snaped3.elig;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class fs;
	title "T-test yuma: fs=nofs, for eligible sample";
	model yuma=fs /solution vadjust=none;
run;

/*=====================================*/
/* means for whole sample */
/* by eligibility status */

/* means for eligible group */
proc sort data=snaped3.elig;
	by fs;run;
	
ods graphics on;
proc surveymeans plots=none data=snaped3.elig ;
	STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;
	title "covariates for eligible group by fs";
	var &other &marital &educa &employ &race &income &county &grocery &snaped &partner;
	by fs;
	run;		
ods graphics off;	

/* T-test for eligible group*/
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test age: eligbroad1=ineligible, for whole sample";
	model age=eligbroad1 /solution vadjust=none;
run;	
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test distance: eligbroad1=ineligible, for whole sample";
	model distance=eligbroad1 /solution vadjust=none;
run;	
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test male: eligbroad1=ineligible, for whole sample";
	model male=eligbroad1 /solution vadjust=none;
run;	

/*marital*/
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test married: eligbroad1=ineligible, for whole sample";
	model married=eligbroad1 /solution vadjust=none;
run;	
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test divorced: eligbroad1=ineligible, for whole sample";
	model divorced=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test widowed: eligbroad1=ineligible, for whole sample";
	model widowed=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test separated: eligbroad1=ineligible, for whole sample";
	model separated=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test nevermarri: eligbroad1=ineligible, for whole sample";
	model nevermarri=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test unmarriedc: eligbroad1=ineligible, for whole sample";
	model unmarriedc=eligbroad1 /solution vadjust=none;
run;

/* educa*/
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test noschool: eligbroad1=ineligible, for whole sample";
	model noschool=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test elementary: eligbroad1=ineligible, for whole sample";
	model elementary=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test somehs: eligbroad1=ineligible, for whole sample";
	model somehs=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test hsged: eligbroad1=ineligible, for whole sample";
	model hsged=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test somecolleg: eligbroad1=ineligible, for whole sample";
	model somecolleg=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test college: eligbroad1=ineligible, for whole sample";
	model college=eligbroad1 /solution vadjust=none;
run;

/* employ*/
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test employed: eligbroad1=ineligible, for whole sample";
	model employed=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test selfemploy: eligbroad1=ineligible, for whole sample";
	model selfemploy=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test unemployed: eligbroad1=ineligible, for whole sample";
	model unemployed=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test homemaker: eligbroad1=ineligible, for whole sample";
	model homemaker=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test student: eligbroad1=ineligible, for whole sample";
	model student=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test retired: eligbroad1=ineligible, for whole sample";
	model retired=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test unablework: eligbroad1=ineligible, for whole sample";
	model unablework=eligbroad1 /solution vadjust=none;
run;

/*race*/
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test white: eligbroad1=ineligible, for whole sample";
	model white=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test black: eligbroad1=ineligible, for whole sample";
	model black=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test asian: eligbroad1=ineligible, for whole sample";
	model asian=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test americanin: eligbroad1=ineligible, for whole sample";
	model americanin=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test hispanic: eligbroad1=ineligible, for whole sample";
	model hispanic=eligbroad1 /solution vadjust=none;
run;

/*income*/
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test lesss10000: eligbroad1=ineligible, for whole sample";
	model lesss10000=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test lesss15000: eligbroad1=ineligible, for whole sample";
	model lesss15000=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test lesss20000: eligbroad1=ineligible, for whole sample";
	model lesss20000=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test lesss25000: eligbroad1=ineligible, for whole sample";
	model lesss25000=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test lesss35000: eligbroad1=ineligible, for whole sample";
	model lesss35000=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test lesss50000: eligbroad1=ineligible, for whole sample";
	model lesss50000=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test lesss75000: eligbroad1=ineligible, for whole sample";
	model lesss75000=eligbroad1 /solution vadjust=none;
run;

/* others*/
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test allestablishment: eligbroad1=ineligible, for whole sample";
	model allestabli=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test groceryden: eligbroad1=ineligible, for whole sample";
	model groceryden=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test population: eligbroad1=ineligible, for whole sample";
	model population=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test ppldensity: eligbroad1=ineligible, for whole sample";
	model ppldensity=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test dsnaped: eligbroad1=ineligible, for whole sample";
	model dsnaped=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test contracter: eligbroad1=ineligible, for whole sample";
	model contracter=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test contracte0: eligbroad1=ineligible, for whole sample";
	model contracte0=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test contracte1: eligbroad1=ineligible, for whole sample";
	model contracte1=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test snappartne: eligbroad1=ineligible, for whole sample";
	model snappartne=eligbroad1 /solution vadjust=none;
run;

/*county*/
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test apache: eligbroad1=ineligible, for whole sample";
	model apache=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test cochise: eligbroad1=ineligible, for whole sample";
	model cochise=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test coconino: eligbroad1=ineligible, for whole sample";
	model coconino=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test gila: eligbroad1=ineligible, for whole sample";
	model gila=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test graham: eligbroad1=ineligible, for whole sample";
	model graham=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test greenlee: eligbroad1=ineligible, for whole sample";
	model greenlee=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test lapaz: eligbroad1=ineligible, for whole sample";
	model lapaz=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test maricopa: eligbroad1=ineligible, for whole sample";
	model maricopa=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test mohave: eligbroad1=ineligible, for whole sample";
	model mohave=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test navajo: eligbroad1=ineligible, for whole sample";
	model navajo=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test pima: eligbroad1=ineligible, for whole sample";
	model pima=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test pinal: eligbroad1=ineligible, for whole sample";
	model pinal=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test stcruz: eligbroad1=ineligible, for whole sample";
	model stcruz=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test yavapai: eligbroad1=ineligible, for whole sample";
	model yavapai=eligbroad1 /solution vadjust=none;
run;
proc surveyreg data=snaped3.snapdata;STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;class eligbroad1;
	title "T-test yuma: eligbroad1=ineligible, for whole sample";
	model yuma=eligbroad1 /solution vadjust=none;
run;


proc surveymeans data=snaped3.snapdata min q1 median q3 max mean stderr clm;
	STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT;
	var distance;
	run;

proc sort data=snaped3.elig; by state;run;	
ods graphics on;
proc surveymeans data=snaped3.elig plots=none; STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT; var servings;
	by state;run;
ods graphics off;	
proc contents data=snaped3.eligp;run;	

ods graphics on;
proc surveyfreq data=snaped3.elig ; STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT; 
   tables mdivorced;
   tables mseparated;
   tables mnevermarr;
   tables fnevermarr;
	run;
ods graphics off;

/* 0424 for presentation */


proc surveymeans data=snaped3.snapdata min q1 median q3 max mean; 
	STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT; 
	var x_frutsum x_vegesum servings;
	run;
data snaped3.snapdata;
	set snaped3.snapdata;
	eligpart=eligbroad1*SNAPpartne;
	eligdistance=eligbroad1*distance;
	run;

proc sort data=snaped3.snapdata;
	by fs;run;
ods graphics on;	
proc surveymeans data=snaped3.snapdata plots=none; 
	STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT; 
	var eligbroad1 eligpart eligdistance;
	
	run;
ods graphics off;

proc surveyfreq data=snaped3.snapdata;
	STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT; 
	tables eligbroad1;
	tables snappartne;
	tables eligbroad1*snappartne;
	run;

proc contents data=snaped3.snapdata;run;

proc surveyfreq data=snaped3.snapdata;
	STRATA X_STSTR; CLUSTER X_PSU; WEIGHT X_LLCPWT; 
	tables fs*eligbroad1;
	run;
	
proc contents data=snaped3.snapdata;run;	

data snaped3.snapdata;
	set snaped3.snapdata;
	logppldensity=log(ppldensity);
	run;
	
proc sgplot data = snaped3.snapdata; 
	scatter x=distance y=ppldensity;
run;

scatter x=distance y=logppldensity; 
	scatter x=distance y=ppldensity;
	
proc freq data=snaped3.snapdata;
	tables marital;
	tables educa;
	tables employ;
	tables income2;
	tables ctycode1;
run;
proc freq data=snaped3.eligp;
	tables marital;
	tables educa;
	tables employ;
	tables income2;
	tables ctycode1;
run;

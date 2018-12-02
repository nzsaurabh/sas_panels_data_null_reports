/* SAS code - Panel graphs */

***********************************************************	;
				/* Panel graphs */ 
***********************************************************	;

* Panel plot of var - SBP and DBP, Weight and BMI, Heart Rate and a1c ;
* Line graphs over visits and compare treatment groups;

* Need Unique PatID, Treatment Group;
* need to remove those withy no vsno => 2;

/*Merge tables treat, a1c, vit*/
/*Keep only required variables*/
* create variable panel to arrange the plots;

PROC SQL;
	create table t as
		select t_astrgp format = treatf. , suinvid* 1000 + subno as subjid, t_acdays
		from treat
		where T_ASTRGP in (36, 901) 
	;
	create table v as
		SELECT suinvid* 1000 + subno as subjid, 
		VARNAME, VARCODE, VSNO, VTDT, VALUE, BASELINE, CHANGE
    from vit
		WHERE VARCODE in ('VS_BPD', 'VTWTKG', 
			'VS_BPS', 'VS_BMI', 'VS_HR');
	;
	CREATE TABLE treatvits as
		select coalesce(t.subjid, v.subjid) as subjid, T_ASTRGP , 
			VARNAME, VARCODE, VSNO, VTDT, VALUE, 
	CASE scan(VARCODE, 1, '')
		WHEN 'VS_BPS' THEN 1
		WHEN 'VTWTKG' THEN 2
		WHEN 'VS_HR'  THEN 3
		WHEN 'VS_BPD' THEN 4
		WHEN 'VS_BMI' THEN 5
	END as Panel format = panelf.

	from t, v
		where t.subjid = v.subjid
		order by t_astrgp, subjid, vsno, VTDT
	;

	create table a as
		select suinvid* 1000 + subno as subjid, 
		VSNO, RETEST, LBSMDT as VTDT, LBVA as Value, LBDMDT,
		put('LBVA', $8. ) as VARCODE,
		put('HbA1c', $60.) as VARNAME,
		6 as Panel format = panelf.
	from a1c
	WHERE VSNO in (1, 2, 5, 7, 8)
	;
	CREATE TABLE treata1c as
		select coalesce(t.subjid, a.subjid) as subjid, T_ASTRGP , 
			VARNAME, VARCODE, VSNO, VTDT, VALUE, Panel, LBDMDT, RETEST
		from t, a
		where t.subjid = a.subjid
		order by t_astrgp, subjid, vsno, VTDT, RETEST
	;
quit;

%nprint(treatvits, 10)

%nprint(treata1c, 30)

%fcontent(treata1c)

*********************************************;

* subset a1c ;

%sortit(treata1c, subjid vsno vtdt RETEST  );

* check retests and neighbours ;

PROC PRINT DATA = treata1c (obs = 20);
	WHERE subjid in (68001, 68002, 71002, 71003, 83007, 83008)  ;
	RUN;


* subset a1c to keep only the last visit date ;
* There were 2506 observations read from the data set WORK.TREATA1C ;
* The data set WORK.A1C_RETEST has 2458 observations and 8 variables ;

DATA a1c_retest;
	SET treata1c;
	BY subjid vsno vtdt RETEST;
	IF value ^= . ;
	IF last.vsno;
	DROP LBDMDT RETEST ;
run;

%nprint(a1c_retest, 10)

* subset treatvits ;

%sortit(treatvits, subjid varcode vsno vtdt);

* There were 19980 observations read from the data set WORK.TREATVITS.;
* data set WORK.SUBVITS has 19777 observations and 8 variables. ;

DATA subvits;
	SET treatvits;
	BY subjid varcode vsno vtdt;
	IF value ^= . ;
	IF last.vsno;
run;


%nprint(subvits, 10)

%fcontent(subvits)

%fcontent(a1c_retest)

* merge a1c and the rest ;

DATA allpanel;
	SET subvits a1c_retest;
RUN;


%sortit(allpanel, subjid varcode vsno vtdt)

%nprint(allpanel, 10)
%fcontent(allpanel)

* The data set WORK.ALLPANEL has 22235 observations and 8 variables. ;
* later need to only keep with atleast 1 vsno > 2;


PROC PRINT data= allpanel (obs = 10);
	WHERE VARCODE = 'LBVA';
RUN;


DATA subpanel;
	SET allpanel;
RUN;

%sortit(subpanel, subjid varcode descending vsno descending vtdt)


DATA subpanel;
	SET subpanel;
	BY subjid varcode descending vsno descending vtdt;
	IF first.varcode THEN lastvisit = vsno;
		RETAIN lastvisit;
RUN;

%nprint(subpanel, 30)

DATA subpanel;
	SET subpanel;
	IF lastvisit > 2;
RUN;


* create macro for sgplot ;
%macro pplot(varc, varn, lab);

TITLE1 &varn; TITLE2; footnote; footnote2; footnote3;

proc SGPLOT data= subpanel (WHERE = (varcode = &varc));
	xaxis type=discrete;
	yaxis label = &lab ;
	VLINE VSNO / response = VALUE stat = mean GROUP= T_ASTRGP GROUPDISPLAY= CLUSTER MARKERS ; 
run;
quit;
TITLE1;
%mend pplot;

%put _user_ ;

******************************************;
* Grid Layout for the plots;
* run macro in each region to generate the plot;


OPTIONS PAPERSIZE = A4 ORIENTATION = LANDSCAPE ;
ods pdf file= "&path\A2\PanelSub_Final20May.pdf" startpage=off style = journal;
ods listing close;
ods html close;
ods pdf startpage=now;

TITLE1 HEIGHT=14pt	'Clinical trial results of new Diabetes treatment';
TITLE2 HEIGHT=12pt 'Change in Vital Signs and HbA1c by Visit Number';
TITLE3 HEIGHT=12pt 'Comparisons by Treatment (New vs Old)';

footnote justify=left " Line graphs and markers have been staggered (shifted horizontally) for easy visibility."; * leave a line space ;
footnote2 justify=left "Report by Saurabh Gupta on %sysfunc(today(),worddate12.) using SAS Version: &sysver";
footnote3 justify=left "Program directory and Name &topdir\A2_sgup072_20May18.sas ";

ods layout gridded y=20pct
			columns = 3 column_widths = (40pct 40pct 40pct)
			column_gutter= 2pct
			rows = 2 row_heights=(30pct 30pct) 
			row_gutter = 2pct
			advance = proc
			;

ods region column= 1 ;

%pplot('VS_BPS', 'Systolic blood pressure', 'Mean SBP')

%pplot('VTWTKG', 'Weight (kg)', 'Mean Weight')

%pplot('VS_HR', 'Heart rate', 'Mean Heart Rate')

%pplot('VS_BPD', 'Diastolic blood pressure', 'Mean DBP' )

%pplot('VS_BMI', 'Body mass index', 'Mean BMI')

%pplot('LBVA', 'HbA1c', 'Mean HbA1c')

ods layout end;
	
ods pdf close;
ods listing;
ODS HTML;

TITLE1; TITLE2;
footnote; footnote2; footnote3;


**********************************************************;
 
/* SAS code - Shift Tables using DATA _null_ steps */


****************************************************;
/* Shift Tables */
****************************************************;


* Need Unique PatID, Treatment Group;
*Keep only required variables;

PROC SQL;
	CREATE TABLE t as
		select t_astrgp format = treatf. , suinvid* 1000 + subno as subjid, t_acdays
		from treat
		where T_ASTRGP in (36, 901)
	;
	CREATE TABLE lbase as
		SELECT suinvid* 1000 + subno as subjid, 
		LBTSCD, LOBDSI, UPBDSI, VSNO, LBSMDT, LBSIVA, LBSIUN, AGE, AGEMN, AGEMX 
    from lab
		WHERE VSNO = 2 and
		LBTSCD in ('BASOPHIL', 'BILITOT', 'CHOLEST', 'CREATIN', 
		'HB', 'PGLUCF', 'PLATELET', 'RBC', 'WBC')
		;
	;
	CREATE TABLE lend as
		SELECT suinvid* 1000 + subno as subjid, 
		LBTSCD, LOBDSI, UPBDSI, VSNO, LBSMDT, LBSIVA, LBSIUN, AGE, AGEMN, AGEMX 
    from lab
		WHERE VSNO > 2 and
		LBTSCD in ('BASOPHIL', 'BILITOT', 'CHOLEST', 'CREATIN', 
		'HB', 'PGLUCF', 'PLATELET', 'RBC', 'WBC')
		;

	CREATE TABLE trbase as
		select coalesce(t.subjid, lbase.subjid) as subjid, * 
		from t, lbase
		where t.subjid = lbase.subjid
		order by subjid, LBTSCD, VSNO, LBSMDT
	;

	create table trend as
		select coalesce(t.subjid, lend.subjid) as subjid, *
		from t, lend
		where t.subjid = lend.subjid
		order by subjid, LBTSCD, VSNO, LBSMDT
	;
	
quit;

%nprint(trbase, 10)

%nprint(trend, 30)

%fcontent(trend)

* subset datasets - keep last visit only;

%sortit(trbase, subjid LBTSCD VSNO LBSMDT);

DATA subtrbase;
	SET trbase;
	WHERE LBSIVA ^= . ;
	BY subjid LBTSCD VSNO LBSMDT;	
	IF last.LBTSCD;
run;


%sortit(trend, subjid LBTSCD VSNO LBSMDT);

DATA subtrend;
	SET trend;
	WHERE LBSIVA ^= . ;
	BY subjid LBTSCD VSNO LBSMDT;	
	IF last.LBTSCD;
run;


* create formats ;


PROC FORMAT;
	VALUE $ paramf

	BASOPHIL  = 'Basophils'
	BILITOT = 'Total Bilirubin'
	CHOLEST = 'Cholesterol'
	CREATIN = 'Creatinine'
	HB = 'Haemoglobin'
	PGLUCF = 'Fasting Plasma Glucose'
	PLATELET = 'Platelets'
	RBC = 'Red Blood cell count'
	WBC = 'White blood cells'
	;

	VALUE treatf
	36 = "Old"
	901 = "New" ;

	VALUE levelf
	1 = "Low"
	2 = "Normal"
	3 = "High" ;

RUN;

* create indicators for hi med low;

%nprint(subtrend, 10)

DATA endpoint;
	SET subtrend;
	endlev = 2;
	IF LBSIVA > UPBDSI THEN endlev = 3 ; 
	IF LBSIVA < LOBDSI THEN endlev = 1 ;
	FORMAT endlev levelf. ;
RUN;


DATA baseline;
	SET subtrbase;
	baselev = 2;
	IF LBSIVA > UPBDSI THEN baselev = 3 ; 
	IF LBSIVA < LOBDSI THEN baselev = 1 ;
	FORMAT baselev levelf. ;
RUN;

%freqtest(endpoint, endlev * T_ASTRGP)

%freqtest(baseline, baselev * T_ASTRGP)

%fcontent(endpoint)

%fcontnt(baseline)


* Each  lab  test  should have an original result category and baseline result category ;

* Merge baseline and endpoint ;
* keep only with vsno > 2 so use inner join;

PROC SQL;
CREATE TABLE endbase as
		select coalesce(e.subjid, b.subjid) as subjid, 
			coalesce(e.LBTSCD, b.LBTSCD) as LBTSCD ,  
			coalesce(e.LBSIUN, b.LBSIUN) as	LBSIUN,
			coalesce(e.T_ASTRGP, b.T_ASTRGP) as T_ASTRGP , 

			b.LBSIVA as baseline, e.LBSIVA as endpoint , 
			baselev, endlev, 
			b.VSNO as basevsno, e.vsno as endvsno
			 
		from endpoint e, baseline b
		where e.subjid = b.subjid and e.LBTSCD = b.LBTSCD 
		order by subjid, LBTSCD
	;
QUIT;

%nprint(endbase, 40)

DATA A2.endbase;
	SET endbase;
RUN;


* check lab names and lab units for PROC Report;
* each variable has only 1 unit - obviously;
* but same unit may be used for > 1 variables;

%freqtest(endbase, LBTSCD * LBSIUN)

* check freq of treatment by levels;
* output is satisfactory;
*  ;
noprint ;
%nprint(endbase, 20)

%fcontent(endbase)

* Run macro created (in the next section) *************************;

%reportit(BASOPHIL)

%reportit(BILITOT)

%reportit(CHOLEST)

%reportit(CREATIN)

%reportit(HB)

%reportit(PGLUCF)

%reportit(PLATELET)

%reportit(RBC)

%reportit(WBC)



* need a macro to run it on one variable at a time and output its report;

%macro reportit(varn);

proc summary data= endbase (where = ( LBTSCD = "&varn")) nway completetypes;
   class LBTSCD LBSIUN T_ASTRGP endlev baselev / preloadfmt order=data missing;
   output out=counts;
   run;

   
PROC SQL;
	CREATE TABLE f36 AS
	SELECT *, 
	sum(_FREQ_) as N,
	100 * _FREQ_ / calculated N as pct,

	put(put(_FREQ_, 5.) || compress("(" || put(calculated pct , 4.1 )|| "%)"), $14.-R)
 	as props 

	FROM counts
	WHERE T_ASTRGP = 36;

	CREATE TABLE f901 AS
	SELECT *, 
	sum(_FREQ_) as N,
	100 * _FREQ_ / calculated N as pct,

	put(put(_FREQ_, 5.) || compress("("||put(calculated pct , 4.1 )|| "%)"), $14.-R )
 	as props

	FROM counts
	WHERE T_ASTRGP = 901;
	;
quit;

DATA FINAL;
	SET f36 f901;
RUN;

%sortit(final, endlev descending T_ASTRGP baselev)

DATA final;
	SET final;
	col = put(put(T_ASTRGP, treatf.) || " " || put(baselev, levelf.), $32.) ;
RUN;

* check data;
%allprint(final);

PROC TRANSPOSE data = final (drop = _TYPE_ _FREQ_ N pct ) 
		out = report (drop = _name_)
	 	;
	 ID col;
	BY endlev ;
	VAR props;
RUN;

DATA REPORT;
	FORMAT Col1 $15. endlev levelf.
		New_Low $15.-r New_Normal $15.-r New_High $15.-r
		Old_Low $15.-r Old_Normal $15.-r Old_High $15.-r ;
	SET REPORT;
	IF _n_ = 1 THEN Col1 = 'End of Study';
	IF _n_ > 1 THEN Col1 = '';
	RUN;

%allprint(report )

DATA _NULL_;
	SET final;
	IF _n_ = 1 THEN DO;
		call symput('labname', compress(put(LBTSCD, $16.))) ;
		call symput( 'labunit', put( "(Lab Units:"||compress(put(LBSIUN, 4.)||")") , $16.));
	END;
	RUN;

* Format the page for better printing;
GOPTIONS reset=all;
TITLE1; TITLE2;
FOOTNOTE1; FOOTNOTE2;

 OPTIONS  PAPERSIZE = A4 ORIENTATION = LANDSCAPE 
		 LEFTMARGIN= 3cm; 

 ODS PDF File= "&path\A2\&labname.pdf" style = journal
		;

DATA _NULL_ ;
	SET report end = eof;
	FILE PRINT ;
	
	IF _n_ = 1 THEN DO;

		PUT @45  'Table 5.6'
			;
		PUT @33  'Laboratory Normal Range Shift Table'
			;

		PUT @1 14* '_' @16 11* '_' 
			@28  11* '_' @40  11* '_' @52  11* '_'
			@64 11* '_' @76 11* '_' @88 11* '_' 
			;
		PUT @55 'Baseline Value';

		PUT @1 "&labname" @16 11* '_' 
			@28  11* '_' @40  11* '_' @52  11* '_'
			@64 11* '_' @76 11* '_' @88 11* '_' 
			;

		PUT @1 "&labunit" @32 'New Treatment' @60 'Standard Treatment'   ;

		PUT @28  11* '_' @40  11* '_' @52  11* '_'
			@64 11* '_' @76 11* '_' @88 11* '_' 
			;

		PUT @28  ' Low ' @40  ' Normal ' @52  ' High '
			@64 ' Low '  @76 ' Normal '  @88 ' High '
			;

		PUT @1 14* '_' @16 11* '_' 
			@28  11* '_' @40  11* '_' @52  11* '_'
			@64 11* '_' @76 11* '_' @88 11* '_' 
			;
	END;

		IF endlev ne lag1(endlev) then put @1 Col1 @16 endlev 
			@28  New_Low @40  New_Normal @52  New_High
			@64 Old_Low @76 Old_Normal @88 Old_High ;
		IF eof THEN DO;
			PUT @1 100 * '_' ;
			PUT @1 "Created by &path\A2_sgup072.sas on %sysfunc(today(),worddate12.) using &sysver";
			PUT @1 100 * '_' ;
		END;
RUN;
ODS PDF CLOSE;

%mend reportit;


* macro ends here ***************;



* create and test macro ****************************;


proc summary data= endbase (where = ( LBTSCD = 'BASOPHIL')) nway completetypes;
   class LBTSCD LBSIUN T_ASTRGP endlev baselev / preloadfmt order=data missing;
   output out=counts;
   run;
proc print;
   run;

   %fcontent(counts)

PROC SQL;
	CREATE TABLE f36 AS
	SELECT *, 
	sum(_FREQ_) as N,
	100 * _FREQ_ / calculated N as pct,

	put(put(_FREQ_, 5.) || compress("(" || put(calculated pct , 4.1 )|| "%)"), $14.-R)
 	as props 

	FROM counts
	WHERE T_ASTRGP = 36;

	CREATE TABLE f901 AS
	SELECT *, 
	sum(_FREQ_) as N,
	100 * _FREQ_ / calculated N as pct,

	put(put(_FREQ_, 5.) || compress("("||put(calculated pct , 4.1 )|| "%)"), $14.-R )
 	as props

	FROM counts
	WHERE T_ASTRGP = 901;
	;
quit;

DATA FINAL;
	SET f36 f901;
RUN;


%sortit(final, endlev descending T_ASTRGP baselev)

DATA final;
	SET final;
	col = put(put(T_ASTRGP, treatf.) || " " || put(baselev, levelf.), $32.) ;
RUN;

%allprint(final);

%fcontent(final);


PROC TRANSPOSE data = final (drop = _TYPE_ _FREQ_ N pct ) out = report (drop = _name_)
	 ;
	 ID col;
	BY endlev ;
	VAR props;
RUN;

DATA REPORT;
	FORMAT Col1 $15. endlev levelf.
		New_Low $15.-r New_Normal $15.-r New_High $15.-r
		Old_Low $15.-r Old_Normal $15.-r Old_High $15.-r ;
	SET REPORT;
	IF _n_ = 1 THEN Col1 = 'End of Study';
	IF _n_ > 1 THEN Col1 = '';
	RUN;

	%allprint(report )

DATA _NULL_;
	SET final;
	IF _n_ = 1 THEN DO;
		call symput('labname', compress(put(LBTSCD, $16.))) ;
		call symput( 'labunit', put( "(Lab Units:"||compress(put(LBSIUN, 4.)||")") , $16.));
	END;
	RUN;

%put _user_ ;

* Format the page for better printing;
ORIENTATION = Landscape;

GOPTIONS reset=all;
TITLE1;
TITLE2;

 OPTIONS  PAPERSIZE = A4 ORIENTATION = LANDSCAPE 
		 LEFTMARGIN= 3cm; 

 ODS PDF File= "&labname.pdf" style = journal
		;

DATA _NULL_ ;
	SET report end = eof;
	FILE PRINT ;
	
	IF _n_ = 1 THEN DO;

		PUT @45  'Table 5.6'
			;
		PUT @33  'Laboratory Normal Range Shift Table'
			;

		PUT @1 14* '_' @16 11* '_' 
			@28  11* '_' @40  11* '_' @52  11* '_'
			@64 11* '_' @76 11* '_' @88 11* '_' 
			;
		PUT @55 'Baseline Value';

		PUT @1 "&labname" @16 11* '_' 
			@28  11* '_' @40  11* '_' @52  11* '_'
			@64 11* '_' @76 11* '_' @88 11* '_' 
			;

		PUT @1 "&labunit" @32 'New Treatment' @60 'Standard Treatment'   ;

		PUT @28  11* '_' @40  11* '_' @52  11* '_'
			@64 11* '_' @76 11* '_' @88 11* '_' 
			;

		PUT @28  ' Low ' @40  ' Normal ' @52  ' High '
			@64 ' Low '  @76 ' Normal '  @88 ' High '
			;

		PUT @1 14* '_' @16 11* '_' 
			@28  11* '_' @40  11* '_' @52  11* '_'
			@64 11* '_' @76 11* '_' @88 11* '_' 
			;
	END;

		IF endlev ne lag1(endlev) then put @1 Col1 @16 endlev 
			@28  New_Low @40  New_Normal @52  New_High
			@64 Old_Low @76 Old_Normal @88 Old_High ;
		IF eof THEN DO;
			PUT @1 100 * '_' ;
			PUT @1 "Created by &path\A2_sgup072.sas on %sysfunc(today(),worddate12.) using &sysver";
			PUT @1 100 * '_' ;
		END;
RUN;
ODS PDF CLOSE;

******** end of macro test *******************;

************* End of shift tables *******************;


 
/*SAS code - shift plots*/


/******************************************************/
* shift plots ;
/******************************************************/

* create formats;

PROC FORMAT ;
VALUE treatf
	36 = "Old"
	901 = "New" ;
RUN;

* read dataset created for question 2;

DATA endbase ;
	SET A2.endbase;
	FORMAT T_ASTRGP treatf. endpoint 5.1 baseline 5.1 ;
	LABEL T_ASTRGP = 'Treatment Group';
RUN;

* check dataset;
%fcontent(endbase)

%nprint(endbase, 10)

* Run macro to create frequency plots;
* code for macro given in the next section;

%plotfreq(BASOPHIL)


%plotfreq(BILITOT)


%plotfreq(CHOLEST)

%plotfreq(CREATIN)

%plotfreq(HB)

%plotfreq(PGLUCF)

%plotfreq(PLATELET)

%plotfreq(RBC)

%plotfreq(WBC)



* Create macro for plots;

%macro plotfreq(varn);

* get a summary so that all levels are included;
proc summary data= endbase (where = ( LBTSCD = "&varn")) nway completetypes;
   class LBTSCD T_ASTRGP endlev baselev / preloadfmt order=data missing;
   output out=counts;
   run;

 * calculate table percent for each treatment ;
PROC SQL;
	CREATE TABLE p36 AS
	SELECT *, 
	sum(_FREQ_) as N,
	100 * _FREQ_ / calculated N as pct

	FROM counts
	WHERE T_ASTRGP = 36;

	CREATE TABLE p901 AS
	SELECT *, 
	sum(_FREQ_) as N,
	100 * _FREQ_ / calculated N as pct

	FROM counts
	WHERE T_ASTRGP = 901;
	;
quit;

* and concatenate the two datasets ;

DATA plottab;
	SET p36 p901;
RUN;

* sort for us in sgpanel;

%sortit(plottab, T_ASTRGP baselev endlev )

* only create plots where baseline is not same as endpoint;
PROC SGPANEL data= plottab (where = ( endlev ne baselev ));
	* treatment group side by side ;
	PANELBY T_ASTRGP / sort = DESCENDING;
	* labels for the variables to be shown on the panel ;
	LABEL pct= 'Percent of Subjects';
	LABEL T_ASTRGP = 'Treatment';
	LABEL endlev = 'Level of Lab Values at Endpoint';
	LABEL baselev = 'Level of Lab Values at Baseline';
	
	* plot end levels by category of baseline level;
	vbarparm CATEGORY= baselev response = pct /  group = endlev 
				GROUPDISPLAY=CLUSTER GROUPORDER = ASCENDING ;

	* Title and subtitle for the panel;
	TITLE2 "Shifts in Lab Values of &varn";
	TITLE3 'By Treatment Group';
	* add foot notes for easy reference;
	footnote justify=left  "Plot includes only levels where baseline differs from endpoint."; * leave a line space ;
	footnote2 justify=left "Report by Saurabh Gupta on %sysfunc(today(),worddate12.)";
	footnote3 justify=left "Program: &topdir\A2_sgup072_20May.sas run on SAS Version: &sysver ";
run;
quit;


%mend plotfreq;



**********************************************************;

* Test Macro ;

* proc summary ;

proc summary data= endbase (where = ( LBTSCD = "BASOPHIL")) nway completetypes;
   class LBTSCD LBSIUN T_ASTRGP endlev baselev / preloadfmt order=data missing;
   output out=counts;
   run;

   %allprint(counts)
   

PROC SQL;
	CREATE TABLE p36 AS
	SELECT *, 
	sum(_FREQ_) as N,
	100 * _FREQ_ / calculated N as pct

	FROM counts
	WHERE T_ASTRGP = 36;

	CREATE TABLE p901 AS
	SELECT *, 
	sum(_FREQ_) as N,
	100 * _FREQ_ / calculated N as pct

	FROM counts
	WHERE T_ASTRGP = 901;
	;
quit;

DATA plottab;
	SET p36 p901;
RUN;

%sortit(plottab, T_ASTRGP baselev endlev )

%allprint(plottab)

PROC SGPANEL data= plottab (where = ( endlev ne baselev ));
	* class variables to be used for the plots ;
	* 3 columns and 2 rows ;
	PANELBY T_ASTRGP / sort = descending ;
	* labels for the variables to be shown on the panel ;
	LABEL pct= 'Percent of Subjects';
	LABEL T_ASTRGP = 'Treatment';
	LABEL endlev = 'Level of Lab Values at Endpoint';
	LABEL baselev = 'Level of Lab Values at Baseline';
	
	vbarparm CATEGORY= baselev response = pct /  group = endlev 
				GROUPDISPLAY=CLUSTER GROUPORDER = ASCENDING ;

	* Title and subtitle for the panel;
	TITLE2 'Shifts in Abnormality of Lab Values';
	TITLE3 'By Treatment Group';
	* add foot notes for easy reference;
	footnote justify=left  "Plot includes only levels where baseline differs from endpoint."; * leave a line space ;
	footnote2 justify=left "Report by Saurabh Gupta on %sysfunc(today(),worddate12.)";
	footnote3 justify=left "Program: &topdir\A2_sgup072_20May.sas run on SAS Version: &sysver ";
run;
quit;


%allprint(plottab)



**********************************************************;



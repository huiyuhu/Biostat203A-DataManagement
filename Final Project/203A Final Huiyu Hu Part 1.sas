%let dir = "/folders/myfolders/203A Final Project";

%MACRO stats(dataset,var);
	*read data;
	proc format;
	value age 4 - 10 = '4 - 10'
			  11 - 17 = '11 - 17';	
	run;
	libname final &dir;
	*proc import datafile="/folders/myfolders/203A Final Project/Master_Child_SDQ.csv";
	proc import datafile=&dataset
	out=final.masterdata
	dbms=csv
	replace;
	getnames=yes;
	run;
	
	data final.mydata (drop=RanNum);
	set final.masterdata;
	SDQ_ESS1 = input(SDQ_ESS,10.);
	SDQ_CPS1 = input(SDQ_CPS,10.);
	SDQ_IHA1 = input(SDQ_IHA,10.);
	SDQ_PPS1 = input(SDQ_PPS,10.);
	SDQ_PSB1 = input(SDQ_PSB,10.);
	drop SDQ_ESS SDQ_CPS SDQ_IHA SDQ_PPS SDQ_PSB;
	rename SDQ_ESS1 = SDQ_ESS
			SDQ_CPS1 =SDQ_CPS
			SDQ_IHA1 = SDQ_IHA
			SDQ_PPS1 = SDQ_PPS
			SDQ_PSB1 = SDQ_PSB;
	format subjage age.;
	*where  RanNum = 17;
	run;
	proc sort data=final.mydata;
	 by SubjNum;
	run;
	
	
	******part1-(0)*******;
	*Prepare data for each of 4 assessment time points (BASELINE, EXIT, FOLLOWUP1, and FOLLOWUP2)?;
	data BASELINE;
	set final.mydata;
	where visit = "BASELINE";
	nmiss = nmiss(SDQ_ESS,SDQ_CPS,SDQ_IHA,SDQ_PPS,SDQ_PSB);
	bl_ess=SDQ_ESS;
	bl_cps=SDQ_CPS;
	bl_iha=SDQ_IHA;
	bl_pps=SDQ_PPS;
	bl_psb=SDQ_PSB;
	if (nmiss = 0) then bl_n = 1; else bl_n = 0;
	keep SubjNum SubjGender SubjAge Visit bl_ess bl_cps bl_iha bl_pps bl_psb bl_n;
	run;
	
	data EXIT;
	set final.mydata;
	where visit = "EXIT";
	nmiss = nmiss(SDQ_ESS,SDQ_CPS,SDQ_IHA,SDQ_PPS,SDQ_PSB);
	et_ess=SDQ_ESS;
	et_cps=SDQ_CPS;
	et_iha=SDQ_IHA;
	et_pps=SDQ_PPS;
	et_psb=SDQ_PSB;
	if (nmiss = 0) then et_n = 1; else et_n = 0;
	keep SubjNum SubjGender SubjAge Visit et_ess et_cps et_iha et_pps et_psb et_n;
	run;
	
	data FU1;
	set final.mydata;
	where visit = "FOLLOWUP1";
	nmiss = nmiss(SDQ_ESS,SDQ_CPS,SDQ_IHA,SDQ_PPS,SDQ_PSB);
	FU1_ess=SDQ_ESS;
	FU1_cps=SDQ_CPS;
	FU1_iha=SDQ_IHA;
	FU1_pps=SDQ_PPS;
	FU1_psb=SDQ_PSB;
	if (nmiss = 0) then FU1_n = 1; else FU1_n = 0;
	keep SubjNum SubjGender SubjAge Visit FU1_ess FU1_cps FU1_iha FU1_pps FU1_psb FU1_n;
	run;
	
	data FU2;
	set final.mydata;
	where visit = "FOLLOWUP2";
	nmiss = nmiss(SDQ_ESS,SDQ_CPS,SDQ_IHA,SDQ_PPS,SDQ_PSB);
	FU2_ess=SDQ_ESS;
	FU2_cps=SDQ_CPS;
	FU2_iha=SDQ_IHA;
	FU2_pps=SDQ_PPS;
	FU2_psb=SDQ_PSB;
	if (nmiss = 0) then FU2_n = 1; else FU2_n = 0;
	keep SubjNum SubjGender SubjAge Visit FU2_ess FU2_cps FU2_iha FU2_pps FU2_psb FU2_n;
	run;
	
	DATA final.mydata_wide;
	merge baseline exit fu1 fu2;
	by SubjNum;
	sumvisit = bl_n + et_n + fu1_n + fu2_n;
	run;
	******part1-(1)*******;
	*What number and percentage of children completed assessments at each of
	 the 4 assessment time points (BASELINE, EXIT, FOLLOWUP1, and FOLLOWUP2)?;
	title "Part 1";
	proc freq data=final.mydata_wide;
	table bl_n et_n fu1_n  fu2_n;
	run;
	******part1-(2)*******;
	*How many children completed only one assessment? 
	How many children completed 2 assessments? 3? 4?;
	title "Part 2";
	proc freq data=final.mydata_wide;
	table sumvisit;
	run;
	******part1-(3)*******;
	*What were the frequencies and percentages for each ‘combination’ 
	of completed assessments present in the data?;
	title "Part 3";
	data part3;
	set final.mydata_wide;
	if (bl_n=1)&(et_n=1) then bl_et=1; else bl_et=0;
	if (bl_n=1)&(fu1_n=1) then bl_fu1=1; else bl_fu1=0;
	if (bl_n=1)&(fu2_n=1) then bl_fu2=1; else bl_fu2=0;
	if (et_n=1)&(fu1_n=1) then et_fu1=1; else et_fu1=0;
	if (et_n=1)&(fu2_n=1) then et_fu2=1; else et_fu2=0;
	if (fu1_n=1)&(fu2_n=1) then fu1_fu2=1; else fu1_fu2=0;
	if (bl_n=1)&(et_n=1)&(fu1_n=1) then bl_et_fu1=1; else bl_et_fu1=0;
	if (bl_n=1)&(et_n=1)&(fu2_n=1) then bl_et_fu2=1; else bl_et_fu2=0;
	if (bl_n=1)&(fu1_n=1)&(fu2_n=1) then bl_fu1_fu2=1; else bl_fu1_fu2=0;
	if (et_n=1)&(fu1_n=1)&(fu2_n=1) then et_fu1_fu2=1; else et_fu1_fu2=0;
	keep bl_et bl_fu1 bl_fu2 et_fu1 et_fu2 fu1_fu2 bl_et_fu1 bl_et_fu2 bl_fu1_fu2 et_fu1_fu2;
	run;
	
	proc freq data=part3;
	table bl_et bl_fu1 bl_fu2 et_fu1 et_fu2 fu1_fu2 bl_et_fu1 bl_et_fu2 bl_fu1_fu2 et_fu1_fu2;
	run;
	
	******part1-(4)*******;
	*Calculate the mean and standard deviation for each SDQ subscale 
	separately for each assessment time point.;
	title "Part 4";
	data mydata_F;
	set final.mydata_wide;
	where SubjGender = "FEMALE";
	run;
	data mydata_M;
	set final.mydata_wide;
	where SubjGender = "MALE";
	RUN;
	proc means data=final.mydata_wide MAXDEC = 4  n mean std;
	var bl_ess bl_cps bl_iha bl_pps bl_psb
		et_ess et_cps et_iha et_pps et_psb
		FU1_ess FU1_cps FU1_iha FU1_pps FU1_psb
		FU2_ess FU2_cps FU2_iha FU2_pps FU2_psb;
	run;
	proc means data=final.mydata_wide MAXDEC = 4  n mean std;
	class &var;
	var bl_ess bl_cps bl_iha bl_pps bl_psb
		et_ess et_cps et_iha et_pps et_psb
		FU1_ess FU1_cps FU1_iha FU1_pps FU1_psb
		FU2_ess FU2_cps FU2_iha FU2_pps FU2_psb;
	run;
	
	******part1-(5)*******;
    Title "Part 5";
	* Prepare data;
	data last_ass;
	set final.mydata_wide;
	IF (FU2_N = 1) THEN 
	do; 
	LAST = "FU2";
	last_ess = fu2_ess; last_cps = fu2_cps; 
	last_iha = fu2_iha; last_pps = fu2_pps;
	last_psb = FU2_psb;
	end;
	ELSE IF (FU1_N = 1) THEN
	do; 
	LAST = "FU1";
	last_ess = fu1_ess; last_cps = fu1_cps;
	last_iha = fu1_iha; last_pps = fu1_pps;
	last_psb = fu1_psb;
	end;
	ELSE IF (ET_N =1) THEN
	do; 
	LAST = "EXIT"; last_ess = et_ess; 
	last_cps = et_cps; last_iha = et_iha; 
	last_pps = et_pps; last_psb = et_psb;
	end;
	ELSE if (bl_n =1) then do; 
	LAST = "BASELINE";
	last_ess = bl_ess; last_cps = bl_cps;
	last_iha = bl_iha; last_pps = bl_pps;
	last_psb = bl_psb;
	end;
	keep SubjNum bl_ess bl_cps bl_iha bl_pps bl_psb
	 last last_ess last_cps last_iha last_pps last_psb;
	RUN;
	
	* Before t-test, remove the observations whose last visit is BASELINE;
	data last_ass_no_baseline;
		set last_ass;
		where last ^= "BAS";
	run;
	
	******part1-(5)*******;
	*alculate a t-test to determine if statistically significant improvement 
	was observed from baseline to last available assessment time point;
	proc ttest data=last_ass_no_baseline sides=2 alpha=0.05 h0=0;
	title "Paired sample t-test ESS";
	paired  bl_ess* last_ess bl_cps* last_cps bl_iha* last_iha bl_pps*last_pps bl_psb* last_psb;
	run;
%MEND stats;

%stats("/folders/myfolders/203A Final Project/Master_Child_SDQ.csv",subjage);
%stats("/folders/myfolders/203A Final Project/Master_Child_SDQ.csv",subjgender);
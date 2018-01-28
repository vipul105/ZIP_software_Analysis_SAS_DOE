/* Constructing the Data-Set for ANOVA and GLM Construction: 
   Response Variable (y): Time taken to compress files in Seconds*/
data Time;
     do Bl=-1 to 1 by 2;
        do So=-1 to 1 by 1;
            do Si=-1 to 1 by 1;
                do Nu=-1 to 1 by 1;
                    input y @@;
                    output;
                end;
            end;
        end;
     end; 
datalines;
110 113 58 231 301 199 878 694 676 59 100 78 127 235 157 468 479 387 69 92 38 180 192 124 496 340 327 83 74 32 261 206 147 500 436 401 80 104 42 293 190 188 570 338 374 32 48 29 100 71 72 302 128 130
107 112 60 226 306 190 858 688 671 58 103 80 125 237 159 473 476 381 71 90 39 186 188 127 493 344 322 85 73 31 266 202 146 506 439 406 80 108 44 296 193 190 574 330 379 34 50 31 105 72 72 306 130 131
;
/* Mapping Variable classifiactions to factor levels:
   Nu: [-1,0,1] =[1,250,500]; "Nu" - Number of files;
   Si: [-1,0,1] =[0.5,1.5,3.0]; "Si" - Size of the files in GB 
   So: [-1,0,1] =[7Zip,WinRAR,PeaZIP]; "So" - Compression Sofwate used
   Bl: [-1, 1 ] =[UBUNTU, Windows]; "Bl" - Operating System tested on
*/ 

/* PRINTING OUT ORIGINAL Data-Set */
proc print data =Time;
run;

/* TRANSFORMING THE RESPONSE VARIABLE TO LOG10(Y),TO SATISFY ANOVA ASSUMPTIONS 
   AND REMOVE TREND IN MODEL RESIDUAL PLOT */
data two;
set Time;
trans_y = y**0.42;
run;

proc print data=two;
run;

/*Constructing the block model for the CRBD model with 4 treatment effects   */

data inter;
    set two;
    NuSi=Nu*Si;
    NuSo=Nu*So;
    NuBl=Nu*Bl;
    SiSo=Si*So;
    SiBl=Si*Bl;
    SoBl=So*Bl;
    NuSiSo=NuSi*So;
    NuSiBl=NuSi*Bl;
    NuSoBl=NuSo*Bl;
    SiSoBl=SiSo*Bl;
    NuSiSoBl=NuSiSo*Bl;
run;

proc print data=inter;
run;

/* 
 Obtaining the ANOVA table and checking for significant terms: *
 ONLY SIGNIFICANT TERMS INCLUDED:
 */
proc anova data=inter;
class Nu Si So Bl NuSi NuSo NuBl SiSo SiBl SoBl NuSiSo NuSiBl NuSoBl SiSoBl NuSiSoBl; 
model trans_y = Nu|Si|So Bl; /*Nu Si So Bl NuSi NuSo NuBl SiSo SiBl SoBl SiBl; */
/*model log_y = So|Si Si|Nu Bl|So; NuSi NuSo NuBl SiSo SiBl SoBl NuSiSo NuSiBl NuSoBl SiSoBl NuSiSoBl;*/ 
/*means So|Si Si|Nu Bl|So/lines tukey alpha=0.05; */
run;
 
/* Constructing a GLM model to identify and test for significant predictor variables in the model. 
   ONLY SIGNIFICANT TERMS INCLUDED [FINAL MODEL]:
*/
proc glm data=inter; 
    class Nu Si So Bl NuSi NuSo NuBl SiSo SiBl SoBl NuSiSo NuSiBl NuSoBl SiSoBl NuSiSoBl; 
    model trans_y = So|Si Si|Nu Bl; 
    means So|Si Si|Nu Bl/lines tukey alpha=0.05;
    output out=diag p=pred r=res;
run; 
/* The reduced glm model for estimate and analysis of the predictor variables in the model. 
   Although the term NuSo is found to be insignificant, it is included in the model since a higher order
   interaction term NuSiSo is found to be significant in the model. */ 

/* Residuals vs. Predicted values */
proc sort; 
by pred;
symbol1 v=circle i=sm50; 
title1 'Residual Plot';

proc gplot; 
plot res*pred/frame; 
run;

/* QQ-Plot and Histogram */
proc univariate data=diag noprint;
var res; 
qqplot res / normal;
histogram res / normal; 
run;

/*Constructing the diagnostics plots and checking for any violations of ANOVA assumptions
  [Normality of model residuals, constant error variance, etc.] */
proc reg outest=effects data=inter;
    model trans_y = So Si Nu Bl SiSo NuSi SoBl SiBl; /*So*Si Si*Nu Bl*So Bl*Si;/*A B C D AB AC AD BC BD CD ABC ABD ACD BCD ABCD; */
run;

/*DIAGNOSTICS PLOTS OF TEH ORIGINAL (UN-tRANSFORMED) MODEL: */
proc reg outest=effects data=inter;
    model y = So Si Nu Bl SiSo NuSi NuSo; /* NuSiSoSo*Si Si*Nu Bl*So Bl*Si;/*A B C D AB AC AD BC BD CD ABC ABD ACD BCD ABCD; */
run;


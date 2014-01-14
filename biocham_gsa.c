/* BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
 Copyright 2004-2009, INRIA, Projet Contraintes

 This program is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

 GNU prolog file ppl_biocham.pl
 by Aurelien Rizk
*/
#include <stdio.h>
#include <string.h> /* strncmp */
#include <stdlib.h>
#include <gprolog.h>
#include <math.h>
#include <unistd.h>

#define PARTITION 36 // PARITION must be > than nbclients tasks
#define ASK_FIRST_JOB 0 //ask for a job and deliver previous result
#define ASK_JOB 1 //ask for the first job
#define SEND_JOB 2
#define QUIT 3

#define SLEEP 11
#define SEARCH_JOB 12
#define FIRST_SEARCH_JOB 13
#define ASK_CMAES_JOB 14



#define CONDPAR 0 //if 1 parallelize on multiple conditions

int condpar=0;
int serial=1;

int dimg,nbpopg,nbspawng,nbcondg;
int nbevalsg, nbparamsg,levelsg,pathsg;
double normg;


#define TAG 0



void get_nbcond(){
  int result;
  PlTerm args[1];
  Pl_Query_Begin(TRUE);
  args[0]=Mk_Variable();
  result=Pl_Query_Call(Find_Atom("taille_cond"),1,args);

  nbcondg=Rd_Integer_Check(args[0]);
  Pl_Query_End(PL_RECOVER);

}


void store_params(int nbevals, int nbparams){

  nbevalsg=nbevals;
  nbparamsg=nbparams;

}

void store_params_morris( int nbparams, int levels, int paths, double norm){

  nbparamsg=nbparams;
  levelsg=levels;
  pathsg=paths;
  normg=norm;
}



/////////MORRIS method


//initialize r runs  of k+1 points

typedef struct{
  int l;//number of levels
  int k;//number of factors
  int r;//number of paths
  float ***data;//array of matrices
}sample;



typedef struct{
  int k;//number of factors
  int r;//number of paths
  float ** data;//k arrays of e_effects

  float * mu;//array of mu for all factors
  float * sigma;
  float * mu_s;
}e_effects;

typedef struct{
  int l;//number of levels
  int k;//number of factors
  int r;//number of paths
  float **data;//array of array of evaluations
}evals;


//compute elementary effects (possibly with mpi)

float ** compute_run(int k, int l, float *x0);
void affiche(float ** m,int l,int c);
float *generate_random_x0(int k, int l);
sample generate_sample(int k, int l,int r);
void disp_sample(sample s);
void detect_effect(int * sign, int * factor, float **r_mat, int nl);
void morris(int k, int l, int r, double norm, int nbtasks);
double f_morris_biocham( double *x, int dim);

//
Bool testm(){


  //float *x0;//=malloc(sizeof(float)*4);
  //x0[0]=0;x0[1]=1.0/3;x0[2]=1.0/3;x0[3]=0;
  //x0=generate_random_x0(4,6);
  //compute_run(4,6,x0);

  // sample s=generate_sample(4,6,2);
  //disp_sample(s);
  morris(4,6,2,1,0);

  return TRUE;
}


void affiche(float ** m,int l,int c)
// printf("affichage du tableau ligne par ligne :"); 
 {
  int i,j;
  for(i=0;i<c;i++)
   {
    printf("ligne %d : ",i);
    for(j=0;j<l;j++) printf("%3.2f ",m[j][i]);
    printf("\n");
   }
 }

void disp_evals(int k, int l , int r, evals e){

  int i,j;

  for(i=0;i<r;i++){

    printf("\nevals run %d \n",i);
    for(j=0;j<k+1;j++){
      printf("%f  ",(e.data)[i][j]);

    }

  }

}


void send_effects(int k, int l, int r, e_effects e){

  PlTerm plist[k];
  PlTerm args[1];
  int i,result;
    Pl_Query_Begin(TRUE);
  for(i=0;i<k;i++){

      plist[i] = Mk_Float(e.mu_s[i]);

  }

    args[0]=Mk_Proper_List(k,plist);
    result=Pl_Query_Call(Find_Atom("morris_effects"),1,args);
    Pl_Query_End(PL_RECOVER);


}


void disp_effects(int k, int l , int r, e_effects e){

  int i;

  for(i=0;i<k;i++){

    printf("\ne_effects for factor %d \n",i);
    //for(j=0;j<r;j++){
    //  printf("%f  ",(e.data)[i][j]);
      
    //}
    printf("mu %f sigma %f mu_s %f \n",e.mu[i],e.sigma[i],e.mu_s[i]);

  }



  FILE *dataf, *plotf ;
dataf = fopen("morris.csv", "w" );
 plotf =fopen("morris.plot","w");

 fprintf(plotf,"set logscale y \n");
 fprintf(plotf,"set logscale x \n");
    fprintf(plotf,"set xlabel \"|mean|\" \n");
    fprintf(plotf,"set xlabel \"variance\" \n");
    fprintf(plotf,"set xrange [*:*] \n");
    fprintf(plotf,"set yrange [*:*] \n");
    fprintf(plotf,"set key outside Left reverse \n");


  for(i=0;i<k;i++){

    fprintf(dataf,"%f\t%f  \n",fabs(e.mu[i]),e.sigma[i]);
    fprintf(plotf,"set label\"k%d\" at %f,%f \n", i, fabs(e.mu[i]), e.sigma[i]);
  }
    fprintf(plotf,"plot \"morris.csv\" title \"Parameters\" using 1:2 \n");

  fclose(dataf);//fclose(plotf);

}

float vabs(float v){
  if(v>0){return v;}else{return(-v);}
}

int rand2(int min, int max){
  //return int between min and max (all inclusive)
  float rr=(rand()-1)/( ( (float) RAND_MAX) +1);
  int rr2 =(int) (rr*(1+max-min));

  //printf("r : %d \n",rr2+min);
 return(rr2+min);

}

float **compute_run(int k, int l, float *x0){
  int i,j;
  float **bmat=malloc(k*sizeof(float *)); 

  float delta=l/(2*(l-1.0));
  // printf("delta  %f \n",delta);
  for(i=0; i<k; i++){
    bmat[i]=malloc((k+1)*sizeof(float));
  }


 for(i=0; i<k; i++){
   for(j=0; j<k+1; j++){
    bmat[i][j]=0;
   }
 }


 for(i=0; i<k; i++){
   for(j=k; j>i; j--){
    bmat[i][j]=1;
   }
 }
 //printf("matrice B :\n");
 //affiche(bmat,k,k+1);


 float rr;
 for(i=0; i<k; i++){
   rr= rand()/( ( (float) RAND_MAX) +1);
   // printf("rr : %f\n",rr);
   if(rr>0.5){
     for(j=0; j<k+1; j++){
       bmat[i][j]= (bmat[i][j] ==1) ? 0 : 1;
     }
   }
 }


 //printf("matrice B modifiee:\n");
 //affiche(bmat,k,k+1);

 //permutation
 float * temp;
 for(i=k-1; i>0; i--){
   j=rand2(0,i);
   //swap columns j and i
   temp=bmat[i];
   bmat[i]=bmat[j];
   bmat[j]=temp;

 }

 //printf("matrice permutee:\n");
 //affiche(bmat,k,k+1);

 //multiplication by delta
 for(i=0; i<k; i++){
   for(j=0; j<k+1; j++){
     bmat[i][j]=bmat[i][j]*delta;;
   }
 }

 //ajout à l'état initial
 for(i=0; i<k; i++){
   for(j=0; j<k+1; j++){
     bmat[i][j]=bmat[i][j] +x0[i];
   }
 }
 //printf("matrice finale:\n");
 //affiche(bmat,k,k+1);

 return bmat;
  }


float *generate_random_x0(int k, int l){
  int i,r;

  float *x0= malloc(sizeof(float)*k);
  //printf("x0:");
 for(i=0; i<k; i++){
   r= rand2(0,l/2 -1);
   x0[i]=r/(l-1.0);
   //printf("  %f ",x0[i]);
 }
 //printf("\n");
 return x0;
}



sample generate_sample(int k, int l,int r){
  sample s;
  s.l=l;s.k=k;s.r=r;
  s.data=malloc(r*sizeof(float **));
  int i;

  for(i=0;i<r;i++){
      (s.data)[i]=compute_run(k,l,generate_random_x0(k,l));
    }

  return(s);
}

void disp_sample(sample s){
  int i;
 for(i=0; i<s.r; i++){
   printf("run %d \n",i);
   affiche((s.data)[i],s.k,s.k+1);
 }

}


evals generate_empty_evals(int k, int l, int r){
  evals e;
  e.l=l;e.k=k;e.r=r;
  e.data=malloc(r*sizeof(float **));
  int i;

  for(i=0;i<r;i++){
    (e.data)[i]=malloc((k+1)*sizeof(float));
    }

  return(e);




}

e_effects generate_empty_effects(int k,  int r){
  e_effects e;
  e.k=k;e.r=r;
  e.data=malloc(k*sizeof(float *));
  e.mu=malloc(k*sizeof(float));
  e.sigma=malloc(k*sizeof(float));
  e.mu_s=malloc(k*sizeof(float));
  int i;

  for(i=0;i<k;i++){
    (e.data)[i]=malloc((r)*sizeof(float));
    }

  return(e);

}


void morris(int k, int l, int r, double norm, int nbtasks){
  sample s;
  float delta=l/(2*(l-1.0));
  s=generate_sample(k,l,r);
  evals e=generate_empty_evals(k,l,r);
  e_effects ef=generate_empty_effects(k,r);
  int i,j;
  int ii;
  double temp[k+1];






  // disp_sample(s);
 
if(nbtasks==0)
   {

  for(i=0;i<r;i++){
    for(j=0;j<k+1;j++){

      for(ii=0;ii<k;ii++){
	temp[ii]=(double) (s.data)[i][ii][j];
	//printf("r %d  j %d tempii %f \n", i,j ,temp[ii]);
      }

      //(e.data)[i][j]=norm/(norm + (f_morris_biocham(temp,k)));//data=jeme ligne de la matrice s.data[i]
        (e.data)[i][j]= f_morris_biocham(temp,k);//data=jeme ligne de la matrice s.data[i]

       
 
	//  (e.data)[i][j]=compute_vdegree((s.data)[i],j);//data=jeme ligne de la matrice s.data[i]
      ///     (e.data)[i][j]=rand2(0,5);
     }
  }


   }






//disp_evals(k,l,r,e);
  //compute e_effects
  int factor,sign;
 for(i=0;i<r;i++){
   //printf("\neffects for run %d\n",i);
    for(j=0;j<k;j++){

      detect_effect(&sign, &factor, (s.data)[i], j);
      //printf("effect on %d with sign %d :",factor,sign);

      (ef.data)[factor][i]=((e.data)[i][j+1] - (e.data)[i][j] )/(sign*delta);
      //      diff=(e.data)[i][j]- (e.data)[i][j];
      //printf(" %g \n",(ef.data)[factor][i]);
     }
  }




  float sum,sum2,sum3;
  for(i=0;i<k;i++){
    sum=0;sum3=0;
    for(j=0;j<r;j++){
      sum+= (ef.data)[i][j];
      sum3+=vabs((ef.data)[i][j]);
    }
    (ef.mu)[i]=sum/r;
    (ef.mu_s)[i]=sum3/r;
  }

  float vv;
  for(i=0;i<k;i++){
    sum2=0;
    for(j=0;j<r;j++){
      vv=((ef.data)[i][j]  -(ef.mu)[i]);
      //printf("vv : %g \n",vv);
      sum2+=vv*vv;
    }
    (ef.sigma)[i]=sqrt(sum2/(r-1));
    //printf("sigma : %g \n",(ef.sigma)[i]);
  }

  send_effects(k,l,r,ef);
  //disp_effects(k,l,r,ef);
  //compute mu indices
  //mu indices mu_star indices, 

}

// nl : num ligne dans la matrice r_mat
void detect_effect(int * sign, int * factor, float **r_mat, int nl){

  int i=-1;;
  float diff=0;
  //for(i=0;i<k;i++){
  while(!diff){
    i++;
    diff=r_mat[i][nl+1] - r_mat[i][nl];
  }

  *factor=i;
  if(diff>0){*sign=1;}else{*sign=-1;}

}



double f_morris_biocham( double *x, int dim)
{
  int i;
  double dist = 0.;

  PlTerm plist[dim];
  PlTerm cost;
  PlTerm args[2];
  int result;


  Pl_Query_Begin(TRUE);
  /*make vars */

   
  cost=Mk_Variable();
  for (i = 0; i < dim; ++i){
    plist[i] = Mk_Float((double) x[i]);
  }
  args[0]=Mk_Proper_List(dim,plist);
  args[1]=cost;
  // printf("f_morris\n");
  result=Pl_Query_Call(Find_Atom("distance_morris"),2,args);

  dist=Rd_Float_Check(args[1]);

   Pl_Query_End(PL_RECOVER);

   
  return dist;
}


Bool call_morris(int dim, int lev, int pa, double norm){
  int levels=lev;
  int paths=pa;

  //  srand(2);
  morris(dim,levels,paths,norm,0);

  return TRUE;
}




/* BIOCHAM system http://contraintes.inria.fr/BIOCHAM/
 Copyright 2004-2010, INRIA, Projet Contraintes

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
#include <ppl_c.h>
#include <gmp.h>
#include <string.h> /* strncmp */
#include <stdlib.h>
#include <gprolog.h>
/*___________________________________________________________________________
 *
 * Function Declarations
 *___________________________________________________________________________
*/

#define CLOSED 1
#define OPEN 2
#define NC 0

#define DEBUGLEVEL 0
#define Fprintf if(DEBUGLEVEL) fprintf
#define Fflush if(DEBUGLEVEL) fflush
#define MAIN 0


typedef struct _orth
{
  int dim;
  int is_empty;
  int is_universe;
  double  *h_corner;
  int  *h_corner_f;
  double  *l_corner;
  int  *l_corner_f;
  struct _orth *next;
} orth;

typedef struct _sdomain
{
  int nb_orth;
  int is_empty;
  int is_universe;
  orth *first;
  orth *last;
} sdomain;


typedef struct _domain
{
  int made_of_orths;
  int ppl;
  ppl_Pointset_Powerset_NNC_Polyhedron_t  ppl_domain; 
  sdomain *orth_domain;
} domain;





void init_tab_dom();

static const char* variable_output_function(ppl_dimension_type var);
void domain_simpl(ppl_Pointset_Powerset_NNC_Polyhedron_t *dom);
void free_dom(ppl_Pointset_Powerset_NNC_Polyhedron_t *dom);
ppl_Pointset_Powerset_NNC_Polyhedron_t * copy_dom(ppl_Pointset_Powerset_NNC_Polyhedron_t *dom1);
void dom_disp_ppl(ppl_Pointset_Powerset_NNC_Polyhedron_t *dom);
void dist_var_pol(int key_var, double val, ppl_const_Polyhedron_t *p, double *distf);
Bool var_coeff_store_init();
double f_min(double x ,double y);
double f_max(double x ,double y);
int is_empty_orth(orth * o);
int orth_is_incl(orth *o1, orth *o2);

sdomain *d_build_sd_empty();
sdomain *d_build_sd_universe();
sdomain *sd_copy(sdomain *sd);
sdomain * oneconstraint_sd(int coeffvar, int varkey, double coeff_const,int ctype, int dim);
sdomain *sd_union(sdomain *sd1, sdomain *sd2);
sdomain *sd_inter(sdomain *sd1, sdomain *sd2);

void sd_simpl(sdomain *sd);
int dim_incl_l(int lf1, double lv1, int lf2, double lv2);
int dim_incl_h(int lf1, double lv1, int lf2, double lv2);

ppl_Polyhedron_t oneconstraint_pol(int nbvars, double coeff_const,int ctype);
ppl_Pointset_Powerset_NNC_Polyhedron_t d_union_ppl(ppl_Pointset_Powerset_NNC_Polyhedron_t d1, ppl_Pointset_Powerset_NNC_Polyhedron_t d2);
ppl_Pointset_Powerset_NNC_Polyhedron_t d_inter_ppl(ppl_Pointset_Powerset_NNC_Polyhedron_t d1, ppl_Pointset_Powerset_NNC_Polyhedron_t d2);
ppl_Pointset_Powerset_NNC_Polyhedron_t d_build_ppl(int nbvars,double coeff_const,int ctype);
ppl_Pointset_Powerset_NNC_Polyhedron_t d_build_ppl_universe(); 
ppl_Pointset_Powerset_NNC_Polyhedron_t d_build_ppl_empty();

Bool  var_norm_init(int a);
Bool d_build_dom_empty(int *res);
Bool d_build_dom_universe(int *res);
Bool d_build_dom(int nbvars,double coeff_const,int ctype, int *res);
Bool d_inter_dom(int k1, int k2, int *k4);
Bool d_union_dom(int k1, int k2, int *k4);
Bool d_disp_ppl(int k, char **output);
Bool dist_var_domain(int domkey, int varkey, double varval, double *dist);
double dist_var_orth(int varkey, double val, orth *o);

ppl_Pointset_Powerset_NNC_Polyhedron_t sdomain_to_ppl_dom(sdomain* sd);
void orth_disp(orth * o);

void delete_orth(orth * o);
void delete_sdomain(sdomain * sd);
orth *orth_copy(orth *o);

//global vars
domain **tab_dom;
int empty_key=1;
int size_tab_dom=1000000;
char* tab_vars[1000];

int tab_formula_coeffs[1000];
double tab_formula_varnorm[1000];
int tab_constraint_coeffs[1000];
int tab_constraint_keys[1000];

int tab_obj_keys[1000];
double tab_obj_vals[1000];

char buffer[5000];
int b_i=0;//buffer index

int cpt_ppl=0;
int cpt_sd=0;

int empty_key_vars=0;
int empty_key_coeffs=1;
int empty_key_obj=1;

int simplify=0;

FILE* filedebug;

Bool open_file(int a){
  filedebug=fopen("filedebug","wt");

  return TRUE;
}

Bool close_file(int a){
  fclose(filedebug);

  return TRUE;
}


Bool init_ppl(int  d)
{
  Fprintf(stderr,"init_ppl(%d);",d);
  Fflush(stderr);
  ppl_initialize();
  ppl_io_set_variable_output_function(variable_output_function); 
  var_coeff_store_init();
  var_norm_init(1); 
  tab_dom=malloc(size_tab_dom*sizeof(domain *));
  ppl_restore_pre_PPL_rounding();
  return TRUE;
}

void version(int i){
  int i0,i1;
  i0=ppl_version_major();  
  i1=ppl_version_minor();
  printf("ppl version: %d %d !\n",i0,i1);

}

Bool end_ppl(int d)
{
  ppl_set_rounding_for_PPL();
  Fprintf(stderr,"end_ppl(%d);",d);
  Fflush(stderr);
  ppl_finalize();
  ppl_restore_pre_PPL_rounding();
  return TRUE;
}

int disp_const(ppl_const_Constraint_t pconst)
{
  int i,i2,coeffvar,test,sign;
  size_t k;
  int nbprint=0;
  mpz_t coeffinit_z,fact_z,coeffconst_z;
  mpf_t res,fact_f,coeff_f;
  ppl_Coefficient_t coeff_ppl;
  char * var;
  int type,coeffprint;
  mpz_init(coeffinit_z);  mpz_init(fact_z); mpz_init(coeffconst_z);
  ppl_Constraint_space_dimension(pconst,&k);
  i2=k;
   ppl_new_Coefficient(&coeff_ppl);
      mpf_init(coeff_f);  mpf_init(fact_f);  mpf_init(res);
    
    test=0;i=-1;

    while(test==0){
 
      i++;
      ppl_Constraint_coefficient(pconst, i, coeff_ppl);
      ppl_Coefficient_to_mpz_t(coeff_ppl,coeffconst_z);
      test=mpz_sgn(coeffconst_z);
    }
   
   coeffvar=tab_formula_coeffs[i];
      mpz_set_si(coeffinit_z,coeffvar);
     
      mpz_cdiv_q(fact_z, coeffconst_z, coeffinit_z);   
   for(i=0;i<i2;i++){
     ppl_Constraint_coefficient(pconst, i, coeff_ppl);
    ppl_Coefficient_to_mpz_t(coeff_ppl,coeffconst_z);
 
    mpf_set_z(coeff_f, coeffconst_z);
    mpf_set_z(fact_f,fact_z);

 
    mpf_div(res, coeff_f, fact_f);

 
    coeffprint=mpf_get_si(res);

    if(coeffprint!=0){
      if(nbprint!=0){
	if(coeffprint>0)
	  b_i +=sprintf(buffer + b_i," + ");
	else
	  b_i += sprintf(buffer + b_i, " - ");
	coeffprint=abs(coeffprint);
      }

    if(coeffprint!=1)b_i += sprintf(buffer + b_i, "%d*",coeffprint);
    nbprint++;
    var=tab_vars[i];
    b_i += sprintf(buffer + b_i, "%s",var);
    }

  }
  type=ppl_Constraint_type(pconst);

  sign=mpz_sgn(fact_z);

  if(sign>0){
  switch(type)
    {
    case PPL_CONSTRAINT_TYPE_LESS_THAN : b_i += sprintf(buffer + b_i, " < ");
      break;
    case PPL_CONSTRAINT_TYPE_LESS_OR_EQUAL: b_i += sprintf(buffer + b_i, " =< ");
      break;
    case PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL: b_i += sprintf(buffer + b_i, " >= ");
      break;
    case PPL_CONSTRAINT_TYPE_GREATER_THAN:b_i += sprintf(buffer + b_i, " > ");
      break;
    case PPL_CONSTRAINT_TYPE_EQUAL:b_i += sprintf(buffer + b_i, " = ");
      break;
    };
  } else
  switch(type)
    {
    case PPL_CONSTRAINT_TYPE_LESS_THAN : b_i += sprintf(buffer + b_i, " > ");
      break;
    case PPL_CONSTRAINT_TYPE_LESS_OR_EQUAL: b_i += sprintf(buffer + b_i, " >= ");
      break;
    case PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL: b_i += sprintf(buffer + b_i, " =< ");
      break;
    case PPL_CONSTRAINT_TYPE_GREATER_THAN:b_i += sprintf(buffer + b_i, " < ");
      break;
    case PPL_CONSTRAINT_TYPE_EQUAL:b_i += sprintf(buffer + b_i, " = ");
      break;
    };


  ppl_Constraint_inhomogeneous_term(pconst,coeff_ppl);
  ppl_Coefficient_to_mpz_t(coeff_ppl,coeffconst_z);
  mpf_set_z(coeff_f, coeffconst_z);

  mpf_ui_sub(coeff_f,0,coeff_f);
  //mpf_neg(coeff_f, coeff_f); //don't know why but better compatibility between gmp libraries versions when not using mpf_neg
  mpf_set_z(fact_f,fact_z);
  mpf_div(res, coeff_f, fact_f);
  b_i += gmp_sprintf(buffer +b_i,"%Fg",res);


  mpz_clear(coeffinit_z);mpz_clear(fact_z); mpz_clear(coeffconst_z);
  mpf_clear(res); mpf_clear(fact_f); mpf_clear(coeff_f);
  ppl_delete_Coefficient(coeff_ppl);


  return(0);

}



void disp_pol(ppl_const_Polyhedron_t * pol)
{
  int test,is_universe=0,is_empty=0,k=0;
  ppl_const_Constraint_t c;
  ppl_Constraint_System_const_iterator_t cit;
  ppl_Constraint_System_const_iterator_t cit0;
  ppl_const_Constraint_System_t cs;


  is_universe=ppl_Polyhedron_is_universe(*pol);
  is_empty=ppl_Polyhedron_is_empty(*pol);


  ppl_Polyhedron_get_constraints( *pol,  &cs);


  ppl_new_Constraint_System_const_iterator(&cit);
  ppl_new_Constraint_System_const_iterator(&cit0);

  ppl_Constraint_System_begin(cs, cit);
  ppl_Constraint_System_end(cs, cit0);


  test=ppl_Constraint_System_const_iterator_equal_test(cit,cit0);

  if(is_empty)b_i += sprintf(buffer + b_i, "The formula is false.\n");
      else if(is_universe)b_i += sprintf(buffer + b_i, "The formula is true.\n");
  else
  {

      while(test==0)
	{
	  if(k!=0)b_i += sprintf(buffer + b_i, ", ");
	  k++;
	  ppl_Constraint_System_const_iterator_dereference(cit, &c);
  	  disp_const(c);

	  ppl_Constraint_System_const_iterator_increment(cit);
	  
	  test=ppl_Constraint_System_const_iterator_equal_test(cit,cit0);
	}
      }
    ppl_delete_Constraint_System_const_iterator(cit);
    ppl_delete_Constraint_System_const_iterator(cit0);
}


static const char*
variable_output_function(ppl_dimension_type var) {
  char* name;
  
  name=tab_vars[var];
  return name;
}



      
//table of polyedra

///get new key
int get_key()
{
  int res;
  // printf("res %d\n ",empty_key);
  res=empty_key;
  if (res >= size_tab_dom-1){
    //  printf("double tab_dom\n");
    size_tab_dom= size_tab_dom*2;
    //printf("realloc ..\n");
    tab_dom=realloc(tab_dom,size_tab_dom);
    //printf("realloc done\n");
  }
  empty_key++;
  return(res);
}

domain* key_to_dom(int key)
{domain * dom;
  //printf("key read %d\n", key);
  dom = tab_dom[key];
  return(dom);
}

void store(int key, domain * dom)
{
  //  printf("key store %d\n", key);
  tab_dom[key]=dom;

}

Bool var_get_key(char *s, int * res)
{
  ppl_set_rounding_for_PPL();
  Fprintf(stderr,"var_get_key(\"%s\",z);\n",s);
  Fflush(stderr);
  *res=empty_key_vars;
  empty_key_vars++;
     tab_vars[(*res)]=s;
  ppl_restore_pre_PPL_rounding();
  return TRUE;
}


Bool send_objective(int varkey, double varval)
{
  Fprintf(stderr,"send_objective(\"%d\",%g);\n",varkey,varval);
  Fflush(stderr);
 
  tab_obj_keys[empty_key_obj]=varkey;
  tab_obj_vals[empty_key_obj]=varval;
  empty_key_obj++;

  return TRUE; 
}



Bool empty_tab_constraint(int a){
  Fprintf(stderr,"empty_tab_constraint(%d);",a);
  Fflush(stderr);
  empty_key_coeffs=1;
  return TRUE;
}

Bool empty_tab_dom(int a){
  Fprintf(stderr,"empty_tab_dom(%d);\n",a);
  Fflush(stderr);
  empty_key=1;
  return TRUE;
}

Bool empty_tab_vars(int a){
  Fprintf(stderr,"empty_tab_vars(%d);\n",a);
  Fflush(stderr);
  empty_key_vars=0;
  return TRUE;
}

Bool var_coeff_assign(int key,int coeff)
{
  ppl_set_rounding_for_PPL();
  Fprintf(stderr,"var_coeff_assign(%d, %d);\n",key,coeff);  
  Fflush(stderr);
  tab_constraint_coeffs[empty_key_coeffs]=coeff;
  tab_constraint_keys[empty_key_coeffs]=key;
  empty_key_coeffs++;
  ppl_restore_pre_PPL_rounding();
  return TRUE;
}

Bool  var_coeff_store_init(){
   int i;
   for(i=0;i<200;i++){
    
     tab_formula_coeffs[i]=0;
   }
   return TRUE;
}



Bool  var_norm_init(int a){
  ppl_set_rounding_for_PPL();
  Fprintf(stderr,"var_norm_init(%d);\n",a);
  Fflush(stderr);

   int i;
   for(i=0;i<1000;i++){
    
     tab_formula_varnorm[i]=1;
   }
   ppl_restore_pre_PPL_rounding();
   return TRUE;
}

Bool var_norm_store(int key,double norm)
{
  ppl_set_rounding_for_PPL();
  Fprintf(stderr,"var_norm_store(%d, %g);\n",key,norm);
  Fflush(stderr);
    tab_formula_varnorm[key]=norm;
  ppl_restore_pre_PPL_rounding();
  return TRUE;
}



Bool var_coeff_store(int key,int coeff)
{
  ppl_set_rounding_for_PPL();
  Fprintf(stderr,"var_coeff_store(%d, %d);\n",key,coeff);
  Fflush(stderr);
  int coeff0,coeff1;
  coeff0=tab_formula_coeffs[key];
  coeff1=abs(coeff);
  if (coeff0<coeff1){
    
    tab_formula_coeffs[key]=coeff1;
}
  ppl_restore_pre_PPL_rounding();
  return TRUE;
}


void exit2(int res)
{
  exit(res);
}
int var_coeff_read(int key)
{
    
  return(tab_constraint_coeffs[key]);
}

int var_key_read(int key)
{
    
  return(tab_constraint_keys[key]);
}




//nbvars, coeff constant et type
ppl_Polyhedron_t oneconstraint_pol(int nbvars, double coeff_const,int ctype){
  //  printf("onepol \n");
  ppl_Polyhedron_t pol1;
  ppl_Linear_Expression_t le;
  ppl_Constraint_t pconst;
  ppl_Constraint_System_t constsys;
  ppl_Coefficient_t cp1,inp;
  int c_i,key;
  mpz_t c1m,inm,num,den;
  mpq_t coeff_const_q;
  mpz_init(c1m);mpz_init(inm);mpq_init(coeff_const_q);
  mpz_init(num);mpz_init(den);  
  int k=1;
 
  mpq_set_d(coeff_const_q,coeff_const);
  mpq_get_num(num,coeff_const_q);
  mpq_get_den(den,coeff_const_q);

  //gmp_printf("num : %Zd \n",num);
  //gmp_printf("den : %Zd \n",den);

  ppl_new_Linear_Expression_with_dimension(&le, empty_key_vars);


  ppl_new_Coefficient(&cp1);
  //in case there is no vars
  //cp1 must be initialized
  //if(nbvars==0){
  //  mpz_set_si(c1m,0);
  // ppl_new_Coefficient_from_mpz_t(&cp1,c1m);
  //}
  
  while(k!=nbvars+1){
    c_i=var_coeff_read(k);
    mpz_set_si(c1m,c_i);
    key=var_key_read(k);

    mpz_mul(c1m,c1m,den);

    ppl_assign_Coefficient_from_mpz_t(cp1,c1m);
    //gmp_printf("coeff var k %d : %Zd \n",k,den);
    ppl_Linear_Expression_add_to_coefficient(le, key, cp1);
    k++;
  }

  mpz_set(inm,num);
  ppl_new_Coefficient_from_mpz_t(&inp,inm);
  ppl_Linear_Expression_add_to_inhomogeneous(le, inp);


  switch(ctype)
    {
    case 1: ppl_new_Constraint(&pconst, le,PPL_CONSTRAINT_TYPE_LESS_THAN);
      break;
    case 2: ppl_new_Constraint(&pconst, le,PPL_CONSTRAINT_TYPE_LESS_OR_EQUAL);
      break;
    case 3: ppl_new_Constraint(&pconst, le,PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);
      break;
    case 4: ppl_new_Constraint(&pconst, le,PPL_CONSTRAINT_TYPE_GREATER_THAN);
      break;
    case 5: ppl_new_Constraint(&pconst, le,PPL_CONSTRAINT_TYPE_EQUAL);
      break;
    }
  

  ppl_new_Constraint_System_from_Constraint(&constsys, pconst);//builds a copy of pconst
  ppl_new_NNC_Polyhedron_from_Constraint_System(&pol1, constsys);//buils a copy


   ppl_delete_Coefficient(inp);
   ppl_delete_Coefficient(cp1);

    ppl_delete_Linear_Expression(le); 
    ppl_delete_Constraint(pconst);
    ppl_delete_Constraint_System(constsys);
  
  

     mpz_clear(c1m);mpz_clear(inm);
     mpq_clear(coeff_const_q);
     mpz_clear(num);mpz_clear(den);  
  
  return(pol1);
}

Bool d_build_dom_empty(int *res){
  ppl_set_rounding_for_PPL();
  Fprintf(stderr,"d_build_dom_empty(z);\n");
  Fflush(stderr);
  domain *d=malloc(sizeof(domain));
  int k;
  sdomain *sd=d_build_sd_empty();

  d->made_of_orths=1;
  d->orth_domain=sd;
  d->ppl=0;
  d->ppl_domain=NULL;

  k=get_key();
  store(k,d);
  
  *res=k;
  ppl_restore_pre_PPL_rounding();
  return TRUE;
}


Bool d_build_dom_universe(int *res){
  ppl_set_rounding_for_PPL();
  Fprintf(stderr,"d_build_dom_universe(z);");
  Fflush(stderr);
  domain *d=malloc(sizeof(domain));
  sdomain *sd=d_build_sd_universe();
  int k;

  d->made_of_orths=1;
  d->orth_domain=sd;
  d->ppl=0;
  d->ppl_domain=NULL;

  k=get_key();
  store(k,d);
  
  *res=k;
  ppl_restore_pre_PPL_rounding();
  return TRUE;
}

 
Bool d_build_dom(int nbvars,double coeff_const,int ctype, int *res){
  ppl_set_rounding_for_PPL();
  Fprintf(stderr,"d_build_dom(%d,%g,%d,z);\n",nbvars,coeff_const,ctype);
  Fflush(stderr);
  int k;
  ppl_Polyhedron_t p;
  ppl_Pointset_Powerset_NNC_Polyhedron_t d;
  sdomain *sd;
  domain *dom=malloc(sizeof(domain));

  if(nbvars>1)
    {
      //printf("ppl domains\n");
      //d=malloc(sizeof(ppl_Pointset_Powerset_NNC_Polyhedron_t));
      p=oneconstraint_pol(nbvars,coeff_const,ctype);

      ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron(&d,p);
      
      ppl_delete_Polyhedron(p);
      dom->made_of_orths=0;
      dom->ppl_domain=d;
      dom->ppl=1;
      dom->orth_domain=NULL;
    }
  if(nbvars==1)
    {
      sd=oneconstraint_sd(var_coeff_read(1),var_key_read(1),coeff_const,ctype,empty_key_vars);

      dom->made_of_orths=1;
      dom->orth_domain=sd;
      dom->ppl=0;    
      dom->ppl_domain=NULL;
    }

  if(nbvars==0)
    {
      dom->ppl=0;
      dom->ppl_domain=NULL;

      switch(ctype)
	{
	case 1:
	  if(coeff_const<0)
	    {
	      sd=d_build_sd_universe();
	      dom->made_of_orths=1;
	      dom->orth_domain=sd;  
	    }
	  else
	    {
	      sd=d_build_sd_empty();
	      dom->made_of_orths=1;
	      dom->orth_domain=sd;
	    }
	  break;
	case 2:
	  if(coeff_const<=0)
	    {
	      sd=d_build_sd_universe();
	      dom->made_of_orths=1;
	      dom->orth_domain=sd;  
	    }
	  else
	    {
	      sd=d_build_sd_empty();
	      dom->made_of_orths=1;
	      dom->orth_domain=sd;
	    }
	  break;
	case 3:
	  if(coeff_const>=0)
	    {
	      sd=d_build_sd_universe();
	      dom->made_of_orths=1;
	      dom->orth_domain=sd;  
	    }
	  else
	    {
	      sd=d_build_sd_empty();
	      dom->made_of_orths=1;
	      dom->orth_domain=sd;
	    }
	  break;
	case 4:
	  if(coeff_const>0)
	    {
	      sd=d_build_sd_universe();
	      dom->made_of_orths=1;
	      dom->orth_domain=sd;  
	    }
	  else
	    {
	      sd=d_build_sd_empty();
	      dom->made_of_orths=1;
	      dom->orth_domain=sd;
	    }
	  break;
	case 5:
	  if(coeff_const==0)
	    {
	      sd=d_build_sd_universe();
	      dom->made_of_orths=1;
	      dom->orth_domain=sd;  
	    }
	  else
	    {
	      sd=d_build_sd_empty();
	      dom->made_of_orths=1;
	      dom->orth_domain=sd;
	    }
	  break;
	}
    }

  k=get_key();

  store(k,dom);
  *res=k;
  ppl_restore_pre_PPL_rounding();
  return TRUE;
}



Bool d_inter_dom(int k1, int k2, int *k4){
  ppl_set_rounding_for_PPL();
  Fprintf(stderr,"d_inter_dom(%d,%d,z);\n",k1,k2);
  Fflush(stderr);
  int k3;
  domain *d1, *d2, *d3;
  d3=malloc(sizeof(domain));
 
  d1=key_to_dom(k1);
  d2=key_to_dom(k2);

  if( d1->made_of_orths && d2-> made_of_orths )
    {
      d3->made_of_orths=1;
      // printf("orths \n");
      d3->orth_domain=sd_inter(d1->orth_domain,d2->orth_domain);
      d3->ppl=0;
      d3->ppl_domain=NULL;

      k3=get_key();
      store(k3,d3);
      *k4=k3;
      
    return TRUE;
      
    }
  if(!(d1->ppl))
    {
      //d1->made_of_orths=0;
      d1->ppl_domain=sdomain_to_ppl_dom(d1->orth_domain);
      d1->ppl=1;
    }

  if(!(d2->ppl))
    {
      //d2->made_of_orths=0;
      d2->ppl_domain=sdomain_to_ppl_dom(d2->orth_domain);
      d2->ppl=1;
    }

  d3->ppl=1;
  d3->made_of_orths=0;
  d3->ppl_domain=d_inter_ppl(d1->ppl_domain,d2->ppl_domain);
  d3->orth_domain=NULL;

  k3=get_key();
  store(k3,d3);
  *k4=k3;
  ppl_restore_pre_PPL_rounding();
  return TRUE;
}


Bool d_union_dom(int k1, int k2, int *k4){
  ppl_set_rounding_for_PPL();
  Fprintf(stderr,"d_union_dom(%d,%d,z);\n",k1,k2);
  Fflush(stderr);
  int k3;
  domain *d1, *d2, *d3;
  d3=malloc(sizeof(domain));
 
  d1=key_to_dom(k1);
  d2=key_to_dom(k2);

  if( d1->made_of_orths && d2-> made_of_orths )
    {
      d3->made_of_orths=1;
      d3->orth_domain=sd_union(d1->orth_domain,d2->orth_domain);
      d3->ppl=0;
      d3->ppl_domain=NULL;

      k3=get_key();
      store(k3,d3);
      *k4=k3;
      return TRUE;
      
    }
  if(!(d1->ppl))
    {
      //printf("uconvertd1\n");
      //d1->made_of_orths=0;
      d1->ppl_domain=sdomain_to_ppl_dom(d1->orth_domain);
      d1->ppl=1;
    }

  if(!(d2->ppl))
    {
      //printf("uconvertd2\n");
      //d2->made_of_orths=0;
      d2->ppl_domain=sdomain_to_ppl_dom(d2->orth_domain);
      d2->ppl=1;
    }

  d3->ppl=1;
  d3->made_of_orths=0;
  d3->ppl_domain=d_union_ppl(d1->ppl_domain,d2->ppl_domain);
  d3->orth_domain=NULL;

  k3=get_key();
  store(k3,d3);
  *k4=k3;

  ppl_restore_pre_PPL_rounding();
  return TRUE;
}

Bool test_e(int k){
  int is_u,is_e;
  is_u=0;
  is_e=0;
  sdomain *sd;
  domain *dom0=malloc(sizeof(domain));
  ppl_Pointset_Powerset_NNC_Polyhedron_t dom;
  if(k==1){
    sd=d_build_sd_empty();
    dom0->made_of_orths=1;
    dom0->orth_domain=sd; 
    dom=sdomain_to_ppl_dom(dom0->orth_domain);

    //    dom=d_build_ppl_empty();


  }
  else{
    //printf("should be true \n");
    sd=d_build_sd_universe();
    dom0->made_of_orths=1;
    dom0->orth_domain=sd; 
    dom=sdomain_to_ppl_dom(dom0->orth_domain);


    //dom=d_build_ppl_universe();
  }

/*   is_u=ppl_Pointset_Powerset_NNC_Polyhedron_is_universe(dom); */
/*   is_e=ppl_Pointset_Powerset_NNC_Polyhedron_is_empty(dom); */

/*   if(is_e){printf("false !\n"); return TRUE;} */
/*  if(is_u){printf("true ! \n"); return TRUE;} */

 return TRUE;

}


Bool d_disp_ppl(int k, char **output){
  ppl_set_rounding_for_PPL();
  Fprintf(stderr,"d_disp_ppl(%d,output);",k);
  Fflush(stderr);
  domain *dom0;
  ppl_Pointset_Powerset_NNC_Polyhedron_t *dom;
  ppl_const_Polyhedron_t p;
  int test,is_u,is_e;
  int i=0;
  ppl_Pointset_Powerset_NNC_Polyhedron_iterator_t dit0;
  ppl_Pointset_Powerset_NNC_Polyhedron_iterator_t dit1;
  is_u=0;
  is_e=0;
  dom0=key_to_dom(k);
  if(dom0->made_of_orths && (dom0->ppl)==0)
    {
      //dom0->made_of_orths=0;
      dom0->ppl_domain=sdomain_to_ppl_dom(dom0->orth_domain);
      dom0->ppl=1;
    }


  dom=&(dom0->ppl_domain);
  
  FILE *fp;
  fp = fopen("/dev/null", "w");
  if (fp == NULL)
    {fp = fopen ("nul", "w");
      if (fp == NULL)
        {
	  printf("could not open 'nul' nor /dev/null\n");
        }
    }
  

  ppl_io_fprint_Pointset_Powerset_NNC_Polyhedron(fp,*dom);

   is_u=ppl_Pointset_Powerset_NNC_Polyhedron_is_universe(*dom); 
   is_e=ppl_Pointset_Powerset_NNC_Polyhedron_is_empty(*dom); 

   if(is_e){
     b_i +=sprintf(buffer + b_i,"The formula is false.\n");
     *output= buffer;
     b_i=0;
     return TRUE;
   } 
  if(is_u){
    b_i +=sprintf(buffer + b_i,"The formula is true.\n");
    *output= buffer;
    b_i=0;
    return TRUE;
  } 

  ppl_new_Pointset_Powerset_NNC_Polyhedron_iterator(&dit0);
  ppl_new_Pointset_Powerset_NNC_Polyhedron_iterator(&dit1);

  ppl_Pointset_Powerset_NNC_Polyhedron_iterator_begin(*dom,dit1);
  ppl_Pointset_Powerset_NNC_Polyhedron_iterator_end(*dom,dit0);

  test=ppl_Pointset_Powerset_NNC_Polyhedron_iterator_equal_test(dit1,dit0);


  while(test==0){
    if(i!=0) b_i +=sprintf(buffer + b_i,"\n| \n");
    i++;

    ppl_Pointset_Powerset_NNC_Polyhedron_iterator_dereference(dit1,&p);

     disp_pol(&p);    
 

    //   ppl_Polyhedron_remove_higher_space_dimensions( p, 1);
 	

    //ppl_io_print_Polyhedron(p);

    ppl_Pointset_Powerset_NNC_Polyhedron_iterator_increment(dit1);
    test=ppl_Pointset_Powerset_NNC_Polyhedron_iterator_equal_test(dit1,dit0);

  }
  b_i +=sprintf(buffer + b_i,"\n");

  ppl_delete_Pointset_Powerset_NNC_Polyhedron_iterator(dit1);
  ppl_delete_Pointset_Powerset_NNC_Polyhedron_iterator(dit0);
    *output= buffer;
  b_i=0;
  ppl_restore_pre_PPL_rounding();
  return TRUE;
}



Bool dist_var_domain(int domkey, int varkey, double varval, double *dist){
  ppl_set_rounding_for_PPL();
  Fprintf(stderr,"dist_var_domain(%d,%d,%g,ff);\n",domkey,varkey,varval);


  ppl_Pointset_Powerset_NNC_Polyhedron_t *dom;
  ppl_const_Polyhedron_t p;
  int test;
  domain *dom0;
  ppl_Pointset_Powerset_NNC_Polyhedron_iterator_t dit0;
  ppl_Pointset_Powerset_NNC_Polyhedron_iterator_t dit1;

  int raw =0x7f800000;
  float inf = *( float* )&raw;

  *dist=(double) inf;
   
  dom0=key_to_dom(domkey);
  if(dom0->made_of_orths && (dom0->ppl)==0)
    {
      //dom0->made_of_orths=0;
      //printf("convert domain %d\n",domkey);
      dom0->ppl_domain=sdomain_to_ppl_dom(dom0->orth_domain);
      dom0->ppl=1;
    }

  dom=&(dom0->ppl_domain);

  double disttemp;
 

  ppl_new_Pointset_Powerset_NNC_Polyhedron_iterator(&dit0);
  ppl_new_Pointset_Powerset_NNC_Polyhedron_iterator(&dit1);

  ppl_Pointset_Powerset_NNC_Polyhedron_iterator_begin(*dom,dit1);
  ppl_Pointset_Powerset_NNC_Polyhedron_iterator_end(*dom,dit0);

  test=ppl_Pointset_Powerset_NNC_Polyhedron_iterator_equal_test(dit1,dit0);


  while(test==0){
   
    ppl_Pointset_Powerset_NNC_Polyhedron_iterator_dereference(dit1,&p);

    

    dist_var_pol(varkey,varval,&p,&disttemp);
  
  if(*dist > disttemp){*dist=disttemp;}

    ppl_Pointset_Powerset_NNC_Polyhedron_iterator_increment(dit1);
    test=ppl_Pointset_Powerset_NNC_Polyhedron_iterator_equal_test(dit1,dit0);

  }
  
  ppl_delete_Pointset_Powerset_NNC_Polyhedron_iterator(dit1);
  ppl_delete_Pointset_Powerset_NNC_Polyhedron_iterator(dit0);
  
  ppl_restore_pre_PPL_rounding();
    return TRUE;


}



Bool dist_var_domain2(int domkey, int varkey, double varval, double *dist){
  Fprintf(stderr,"dist_var_domain2(%d,%d,%g,ff);\n",domkey,varkey,varval);

  int nb_orth,i;
  domain *dom0;
  sdomain *sd;
  int raw =0x7f800000;
  float inf = *( float* )&raw;
  orth *o;
  double norm=tab_formula_varnorm[varkey];

  *dist=(double) inf;
   
  dom0=key_to_dom(domkey);
  sd=dom0->orth_domain;
  nb_orth=sd->nb_orth;

  double disttemp;
  o=sd->first;
  for(i=0;i<nb_orth;i++){
    //printf("norm : %g \n",norm);
    disttemp=(dist_var_orth(varkey,varval,o))*norm;
      o=o->next;
  if(*dist > disttemp){*dist=disttemp;}


  }
  
    return TRUE;

}





double dist_var_orth(int varkey, double val, orth *o){
  int lf=o->l_corner_f[varkey];
  double lv=o->l_corner[varkey];


  int hf=o->h_corner_f[varkey];
  double hv=o->h_corner[varkey];



  double res;

  switch(lf)
    {
    case NC:
      switch(hf)
	{
	case NC:
	  res=0;
	  break;
	case OPEN:
	  if(val>hv)
	    {res=val-hv;}
	  else
	    {res=0;}
	  break;
	case CLOSED:
	  if(val>hv)
	    {res=val-hv;}
	  else
	    {res=0;}
	  break;
	}
      break;
    case OPEN:
      switch(hf)
	{
	case NC:
	  if(val<lv)
	    {res=lv-val;}
	  else
	    {res=0;}
	  break;
	case OPEN:
	  if(val<lv)
	    {res=lv-val;}
	  else
	    if(val>hv)
	      {res=val-hv;}
	    else
	      {res=0;}
	  break;
	case CLOSED:
	  if(val<lv)
	    {res=lv-val;}
	  else
	    if(val>hv)
	      {res=val-hv;}
	    else
	      {res=0;}
	  break;
	}  
      break;
    case CLOSED:
      switch(hf)
	{
	case NC:
	  if(val<lv)
	    {res=lv-val;}
	  else
	    {res=0;}
	  break;
	case OPEN:
	  if(val<lv)
	    {res=lv-val;}
	  else
	    if(val>hv)
	      {res=val-hv;}
	    else
	      {res=0;}
	  break;
	case CLOSED:
	  if(val<lv)
	    {res=lv-val;}
	  else
	    if(val>hv)
	      {res=val-hv;}
	    else
	      {res=0;}
	  break;
	}

      break;
    }
  return res;

}



void dist_var_pol(int key_var, double val, ppl_const_Polyhedron_t *p, double *distf){
  mpz_t c1m,r1_nz,r1_dz,r2_nz,r2_dz,coeff1z,num,den;
  mpf_t r1_nf,r1_df,r2_nf,r2_df,res1f,res2f;
  mpq_t val_q;
  double dist,res1d,res2d;
  ppl_Coefficient_t coeff1,cp1,r1_n,r1_d,r2_n,r2_d;
  ppl_Linear_Expression_t le1,le2;
  ppl_Constraint_t constr1, constr2;
  ppl_Polyhedron_t p1,p2;
  ppl_Generator_t point;
  int t1,t2,tsup;

  int raw =0x7f800000;
  float inf = *( float* )&raw;

  //build linear expression x-xval
  ppl_new_Linear_Expression_with_dimension(&le1, empty_key_vars);
  mpz_init(c1m);
  mpz_init(num);
  mpz_init(den);
  mpq_init(val_q);
  mpz_init(coeff1z);
  mpf_init(r1_nf);
  mpf_init(r1_df);
  mpf_init(r2_nf);
  mpf_init(r2_df);
  mpf_init(res1f);
  mpf_init(res2f);


  mpq_set_d(val_q,val);
  mpq_get_num(num,val_q);
  mpq_get_den(den,val_q);

  ppl_new_Coefficient(&r1_n);
  ppl_new_Coefficient(&r1_d);
  ppl_new_Coefficient(&r2_n);
  ppl_new_Coefficient(&r2_d);

  ppl_new_Generator_zero_dim_point(&point);

  mpz_set_si(coeff1z,1);
 mpz_ui_sub(num,0,num);
 //mpz_neg(num,num);
  ppl_new_Coefficient_from_mpz_t(&cp1,num);
  ppl_new_Coefficient_from_mpz_t(&coeff1,den);
  ppl_Linear_Expression_add_to_coefficient(le1, key_var,coeff1);//coeff  variable =1
  ppl_Linear_Expression_add_to_inhomogeneous(le1, cp1);//coeff  constant



  ppl_new_Linear_Expression_from_Linear_Expression(&le2, le1);
  //build constraints
  ppl_new_Constraint(&constr2, le2,PPL_CONSTRAINT_TYPE_LESS_OR_EQUAL);
  ppl_new_Constraint(&constr1, le1,PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);



  ppl_new_NNC_Polyhedron_from_NNC_Polyhedron(&p1, *p);
  ppl_new_NNC_Polyhedron_from_NNC_Polyhedron(&p2, *p);


  ppl_Polyhedron_add_constraint(p1,  constr1);

  ppl_Polyhedron_add_constraint(p2,  constr2);

  t1=ppl_Polyhedron_minimize_with_point(p1, le1, r1_n, r1_d, &tsup, point);

  t2=ppl_Polyhedron_maximize_with_point(p2, le2, r2_n, r2_d, &tsup, point);

  mpz_init(r1_nz);  mpz_init(r1_dz);  mpz_init(r2_nz);  mpz_init(r2_dz);
  ppl_Coefficient_to_mpz_t(r1_n,r1_nz);
  ppl_Coefficient_to_mpz_t(r1_d,r1_dz);
  ppl_Coefficient_to_mpz_t(r2_n,r2_nz);
  ppl_Coefficient_to_mpz_t(r2_d,r2_dz);


  mpz_mul(r1_dz,r1_dz,den);
  mpz_mul(r2_dz,r2_dz,den);

  mpf_set_z(r1_nf, r1_nz);
  mpf_set_z(r1_df, r1_dz);
  mpf_set_z(r2_nf, r2_nz);
  mpf_set_z(r2_df, r2_dz);

  mpf_ui_sub(r2_nf,0,r2_nf);
  //mpf_neg(r2_nf,r2_nf);

  if(t1>0){
    if(t2>0){
      mpf_div(res1f,r1_nf,r1_df);
      mpf_div(res2f,r2_nf,r2_df);
      res1d=mpf_get_d(res1f);
      res2d=mpf_get_d(res2f);
      dist=f_min(res1d,res2d);
    }
    else{
      mpf_div(res1f,r1_nf,r1_df);
      dist=mpf_get_d(res1f);
      
    }
  }
  else{
    if(t2>0){
      mpf_div(res2f,r2_nf,r2_df);
      dist=mpf_get_d(res2f);
    }
    else{
      dist=(double) inf;
    }
  }
  *distf=dist;


   ppl_delete_Polyhedron(p1);
   ppl_delete_Polyhedron(p2);

   ppl_delete_Coefficient(coeff1);
   ppl_delete_Coefficient(cp1);
   ppl_delete_Coefficient(r1_n);
   ppl_delete_Coefficient(r1_d);
   ppl_delete_Coefficient(r2_n);
   ppl_delete_Coefficient(r2_d);

   ppl_delete_Linear_Expression(le1);
   ppl_delete_Linear_Expression(le2);

   ppl_delete_Constraint(constr1);
   ppl_delete_Constraint(constr2);

   ppl_delete_Generator(point);


   mpz_clear(c1m);
   mpz_clear(r1_nz);
   mpz_clear(r1_dz);
   mpz_clear(r2_nz);
   mpz_clear(r2_dz);
   mpz_clear(coeff1z);
   mpz_clear(num);
   mpz_clear(den);

   mpf_clear(r1_nf);
   mpf_clear(r1_df);
   mpf_clear(r2_nf);
   mpf_clear(r2_df);
   mpf_clear(res1f);
   mpf_clear(res2f);

   mpq_clear(val_q);

}


double f_min(double x ,double y){

  if(x<y) return x; else
    return y;
}

double f_max(double x ,double y){

  if(x>y)
    return x; else
    return y;
}

Bool freedomkey(int key){
  ppl_set_rounding_for_PPL();
  //Fprintf(stderr,"freedomkey(%d);",key);
  domain * dom0;
  dom0=key_to_dom(key);
  ppl_Pointset_Powerset_NNC_Polyhedron_t  dom= dom0->ppl_domain;
  sdomain *sd=dom0->orth_domain;
  // printf("key : %d \n",key);
  if(dom0->ppl)
    {//printf("delete ppl.. ");
      ppl_delete_Pointset_Powerset_NNC_Polyhedron(dom);
    }

  if(dom0->made_of_orths)
    {  //printf("delete sd.. ");  
      delete_sdomain(sd);
    }
  //printf("free dom ..");
  free(dom0);
  //printf("done\n");
  ppl_restore_pre_PPL_rounding();
   return TRUE;
}

void delete_orth(orth * o){

  double *hc, *lc;
  int * hcf, *lcf;
  hc=o->h_corner;
  hcf=o->h_corner_f;

  lc=o->l_corner;
  lcf=o->l_corner_f;

  free(hc);free(lc);
  free(hcf);free(lcf);

  free(o);
}


void delete_sdomain(sdomain * sd){
  int nb = sd->nb_orth;
  int i;
  orth *o1,*o2;

  if(nb==0)
    {
      free(sd);
    }
  else
    {
      o1=sd->first;
      for(i=0;i< nb-1;i++){
	o2=o1->next;
	delete_orth(o1);
	o1=o2;
	
      }
      delete_orth(o1);
      free(sd);
    }
}

Bool free_tab_dom(int k){
  ppl_set_rounding_for_PPL();
  Fprintf(stderr,"free_tab_dom(%d);\n",k);
  Fflush(stderr);
  int i = 0;

  for (i=1;i<empty_key;i++){
    freedomkey(i);
   }

  //init_tab_dom();
  ppl_restore_pre_PPL_rounding();
  return TRUE;
}

void init_tab_dom(){
  int i = 0;
  //printf("init tab_dom\n");
  for (i=0;i<size_tab_dom-1;i++){
    tab_dom[i]=NULL;
   }

}





//API simple domain

sdomain* d_build_sd_empty(){
  sdomain *d=malloc(sizeof(sdomain));
  d->nb_orth=0;
  d->is_empty=1;
  d->is_universe=0;
  d->first=NULL;  
  d->last=NULL;
  return d;
}


sdomain* d_build_sd_universe(){
  sdomain *d=malloc(sizeof(sdomain));
  d->nb_orth=0;
  d->is_universe=1;
  d->is_empty=0;
  d->first=NULL;  
  d->last=NULL;
  return d;
}


 orth* new_orth(int dim){
   orth *o=malloc(sizeof(orth));
  double *hc=malloc(dim*sizeof(double));
  int *hcf=malloc(dim*sizeof(int));
  
  double *lc=malloc(dim*sizeof(double));
  int *lcf=malloc(dim*sizeof(int));
  int i;
  for(i=0;i<dim;i++){
    hcf[i]=NC;
    lcf[i]=NC;
  }

  o->h_corner=hc;
  o->h_corner_f=hcf;
  o->l_corner=lc;
  o->l_corner_f=lcf;
  o->dim=dim;

  o->next=NULL;
  o->is_empty=1;
  o->is_universe=0;

  return o;
}

 orth* oneconstraint_orth(int coeffvar, int varkey, double coeff_const,int ctype, int dim){
   orth *o=new_orth(dim);
   o->is_empty=0;  //non empty because orth with one constraint and one variable
   o->is_universe=0;

   //   printf("coeffvar : %d varkey : %d \n",coeffvar,varkey);
   if( coeffvar > 0)
     {

  switch(ctype)
    {
    case 1:
      o->h_corner[varkey]=-coeff_const/coeffvar;
      o->h_corner_f[varkey]=OPEN;
      break;
    case 2:
      o->h_corner[varkey]=-coeff_const/coeffvar;
      o->h_corner_f[varkey]=CLOSED;
      break;
    case 3:
      o->l_corner[varkey]=-coeff_const/coeffvar;
      o->l_corner_f[varkey]=CLOSED;
      break;
    case 4:
      o->l_corner[varkey]=-coeff_const/coeffvar;
      //printf("val : %f\n",-coeff_const/coeffvar);
      o->l_corner_f[varkey]=OPEN;
      break;
    case 5:
      o->l_corner[varkey]=-coeff_const/coeffvar;
      o->l_corner_f[varkey]=CLOSED;
      o->h_corner[varkey]=-coeff_const/coeffvar;
      o->h_corner_f[varkey]=CLOSED;
      break;
    }

     }

   else 

     {
  switch(ctype)
    {
    case 4:
      o->h_corner[varkey]=-coeff_const/coeffvar;
      o->h_corner_f[varkey]=OPEN;
      break;
    case 3:
      o->h_corner[varkey]=-coeff_const/coeffvar;
      o->h_corner_f[varkey]=CLOSED;
      break;
    case 2:
      o->l_corner[varkey]=-coeff_const/coeffvar;
      o->l_corner_f[varkey]=CLOSED;
      break;
    case 1:
      o->l_corner[varkey]=-coeff_const/coeffvar;
      //printf("val : %f\n",-coeff_const/coeffvar);
      o->l_corner_f[varkey]=OPEN;
      break;
    case 5:
      o->l_corner[varkey]=-coeff_const/coeffvar;
      o->l_corner_f[varkey]=CLOSED;
      o->h_corner[varkey]=-coeff_const/coeffvar;
      o->h_corner_f[varkey]=CLOSED;
      break;
    }





     }
  return o;


}


sdomain * oneconstraint_sd(int coeffvar, int varkey, double coeff_const,int ctype, int dim){
  //printf("coeffvr %d \n varkey %d \n coeffconst %g \n ctype %d \n",coeffvar,varkey,coeff_const,ctype);
  //printf("onesd dim : %d \n",dim);  
 orth *o=oneconstraint_orth( coeffvar, varkey, coeff_const, ctype, dim);
  sdomain *d=malloc(sizeof(sdomain));
  d->first=o;
  d->last=o;
  d->is_empty=0;
  d->is_universe=0;
  d->nb_orth=1;
  return d;
}

void add_orth(sdomain *sd, orth *o){
  orth * tmp;
  if(sd->nb_orth>0)
    {
      tmp = sd->last;
      tmp->next=o;
      sd->last=o;
      sd->nb_orth=sd->nb_orth+1;
      sd->is_empty=0;
    }
  else
    {
      sd->first=o;
      sd->last=o;
      sd->nb_orth=1;
      sd->is_empty=0;
    }
}


void sd_disp(sdomain * sd){
  int i;
  orth *o;
  printf("nborth : %d \nempty: %d\nuniverse: %d\n",sd->nb_orth, sd->is_empty, sd->is_universe);
  o=sd->first;
 orth_disp(o); 
 for(i=0;i< (sd->nb_orth)-1;i++){
   o=o->next;
   printf("orth %d : \n",i+1);
   orth_disp(o); 
 }

}


void orth_disp(orth * o){
  int i;
  int d1,d2,d3,d4;
  for(i=0;i< (o->dim);i++){

    d1=o->h_corner_f[i];
    d2=o->h_corner[i];
    d3=o->l_corner_f[i];
    d4=o->l_corner[i];

    //printf("hf: %d\nhv: %d\nlf:%d\nlv: %d \n",d1,d2,d3,d4);

 }

}
//TODO simpl de domain

orth *orth_inter(orth *o1, orth *o2){
  int i;
  orth *o=new_orth(o1->dim);

  for(i=0;i< (o1->dim);i++){

    switch(o1->h_corner_f[i])
      {
      case NC:
	o->h_corner_f[i]=o2->h_corner_f[i];
	o->h_corner[i]=o2->h_corner[i];
	break;
     
      case OPEN:
	switch(o2->h_corner_f[i])
	  {
	  case NC:
	    o->h_corner_f[i]=o1->h_corner_f[i];
	    o->h_corner[i]=o1->h_corner[i];
	    break;
	  case OPEN:
	    o->h_corner_f[i]=OPEN;
	    o->h_corner[i]=f_min(o1->h_corner[i],o2->h_corner[i]);
	    break;
	  case CLOSED:
	    if (o1->h_corner[i] <= o2->h_corner[i])
	      {	
		o->h_corner_f[i]=OPEN;
		o->h_corner[i]=o1->h_corner[i];
	      }
	    else
	      {
		o->h_corner_f[i]=CLOSED;
		o->h_corner[i]=o2->h_corner[i];
	      }
	    break;
	  }
      break;

    case CLOSED:
      switch(o2->h_corner_f[i])
	{
	case NC:
	  o->h_corner_f[i]=o1->h_corner_f[i];
	  o->h_corner[i]=o1->h_corner[i];
	  break;
	case OPEN:
	    if (o1->h_corner[i] < o2->h_corner[i])
	      {	
		o->h_corner_f[i]=CLOSED;
		o->h_corner[i]=o1->h_corner[i];
	      }
	    else
	      {
		o->h_corner_f[i]=OPEN;
		o->h_corner[i]=o2->h_corner[i];
	      }
	  break;
	case CLOSED:
	  o->h_corner_f[i]=CLOSED;
	  o->h_corner[i]=f_min(o1->h_corner[i],o2->h_corner[i]);
	  break;
	}      
      break;
    }


   switch(o1->l_corner_f[i])
      {
      case NC:
	o->l_corner_f[i]=o2->l_corner_f[i];
	o->l_corner[i]=o2->l_corner[i];
	break;
     
      case OPEN:
	switch(o2->l_corner_f[i])
	  {
	  case NC:
	    o->l_corner_f[i]=o1->l_corner_f[i];
	    o->l_corner[i]=o1->l_corner[i];
	    break;
	  case OPEN:
	    o->l_corner_f[i]=OPEN;
	    o->l_corner[i]=f_max(o1->l_corner[i],o2->l_corner[i]);
	    break;
	  case CLOSED:
	    if (o1->l_corner[i] >= o2->l_corner[i])
	      {	
		o->l_corner_f[i]=OPEN;
		o->l_corner[i]=o1->l_corner[i];
	      }
	    else
	      {
		o->l_corner_f[i]=CLOSED;
		o->l_corner[i]=o2->l_corner[i];
	      }
	    break;
	  }
      break;

    case CLOSED:
      switch(o2->l_corner_f[i])
	{
	case NC:
	  o->l_corner_f[i]=o1->l_corner_f[i];
	  o->l_corner[i]=o1->l_corner[i];
	  break;
	case OPEN:
	    if (o1->l_corner[i] > o2->l_corner[i])
	      {	
		o->l_corner_f[i]=CLOSED;
		o->l_corner[i]=o1->l_corner[i];
	      }
	    else
	      {
		o->l_corner_f[i]=OPEN;
		o->l_corner[i]=o2->l_corner[i];
	      }
	  break;
	case CLOSED:
	  o->l_corner_f[i]=OPEN;
	  o->l_corner[i]=f_max(o1->l_corner[i],o2->l_corner[i]);
	  break;
	}
      break;
    }

  }
  o->is_empty=0;
  o->is_universe=0;

  return o;

}


sdomain *sd_inter(sdomain *sd1, sdomain *sd2){
  sdomain *sd3;
  orth *o,*oi,*oj;
  int s1,s2,i,j;

  // printf("inter %d and %d orths \n",sd1->nb_orth,sd2->nb_orth);

  if(sd1->is_universe==1)
    {
      sd3=sd_copy(sd2);
      //delete_sdomain(sd1);
      //printf("0\n");
      return sd3;
    }
  if(sd2->is_universe==1)
    {
      sd3=sd_copy(sd1);
      //delete_sdomain(sd2);
      //printf("1\n");
      return sd3;
    }

  sd3=d_build_sd_empty();

  s1=sd1->nb_orth;
  s2=sd2->nb_orth;
  oi=sd1->first;
  oj=sd2->first;

  for(i=0;i<s1;i++){
    for(j=0;j<s2;j++){
      o=orth_inter(oi,oj);
      if(! is_empty_orth(o))
	{	
	  //printf("add\n");
	  //orth_disp(o);
	  add_orth(sd3,o);
	}
      else
	{
	  delete_orth(o);
	}
      oj=oj->next;
    }
    oi=oi->next;
    oj=sd2->first;
  }

  //delete_sdomain(sd1);
  //delete_sdomain(sd2);

   sd_simpl(sd3);
  //printf("done\n");
  //sd_disp(sd3);
  return sd3;
}

sdomain *sd_copy(sdomain *sd){
  sdomain *sd2=malloc(sizeof(sdomain));
  int nb=sd->nb_orth;
  int i;

  orth *o, *o2;
  if(sd->is_empty==1)
    {
      sd2->nb_orth=0;
      sd2->is_empty=1;
      sd2->is_universe=0;  
      sd2->first=NULL;  
      sd2->last=NULL;
      return sd2;
    }

 if(sd->is_universe==1)
    {
      sd2->nb_orth=0;
      sd2->is_empty=0;
      sd2->is_universe=1;  
      sd2->first=NULL;  
      sd2->last=NULL;
      return sd2;
    }


  sd2->nb_orth=sd->nb_orth;

  if(nb>0)
    {
      o=sd->first;
      o2=orth_copy(o);
      sd2->first=o2;
    }
  else
    {
      sd2->nb_orth=0;
      sd2->is_empty=1;
      sd2->is_universe=0;
      sd2->first=NULL;  
      sd2->last=NULL;
      return sd2;
    }

  if(nb==1)
    {
      sd2->last=sd2->first;
      sd2->is_universe=sd->is_universe;
      sd2->is_empty=sd->is_empty;  
      return sd2;
    }

  for(i=0;i< nb-1;i++)
    {
      o=o->next;
      o2->next=orth_copy(o);
      o2=o2->next;
    }
  sd2->last=o2;
  sd2->is_universe=sd->is_universe;
  sd2->is_empty=sd->is_empty;  
 return sd2;
 
}


orth *orth_copy(orth *o){
  int i;
  orth *o2=new_orth(o->dim);

  for(i=0;i< (o->dim);i++){

    o2->h_corner[i]=o->h_corner[i];
    o2->h_corner_f[i]=o->h_corner_f[i];
    o2->l_corner[i]=o->l_corner[i];
    o2->l_corner_f[i]=o->l_corner_f[i];
  }

  o2->is_empty=o->is_empty;
  o2->is_universe=o->is_universe;

  return o2;
}

sdomain *sd_union(sdomain *sd1, sdomain *sd2){
  sdomain *sd3;
  //  printf("1 : \n");
  //sd_disp(sd1);

  //  printf("2 : \n");
  //sd_disp(sd2);
  if(sd1->is_empty==1)
    {
      //delete_sdomain(sd1);
      sd3=sd_copy(sd2);
      return sd3;
    }
  if(sd2->is_empty==1)
    {
      sd3=sd_copy(sd1);
      //delete_sdomain(sd2);
      return sd3;
    }

  if(sd1->is_universe==1)
    {
      //delete_sdomain(sd1);
      //delete_sdomain(sd2);
      sd3=sd_copy(sd1);;
      return sd3;
    }
  if(sd2->is_universe==1)
    {
      //delete_sdomain(sd1);
      //delete_sdomain(sd2);
      sd3=sd_copy(sd2);

      return sd3;
    }    
  
  sd1=sd_copy(sd1);
  sd2=sd_copy(sd2);

  sd3=malloc(sizeof(sdomain));
  sd3->first=sd1->first;
  sd3->last=sd2->last;
  orth* o=sd1->last;
  o->next=sd2->first;
  sd3->nb_orth=sd2->nb_orth + sd1->nb_orth;
  sd3->is_empty=0;
  sd3->is_universe=0;

  free(sd1);
  free(sd2);

  //printf("3  : \n");
  //sd_disp(sd3);

  sd_simpl(sd3);
  //if(simplify==1){sd_simpl(sd3);simplify=0;}
  //else simplify++;

   //printf("3 simpl : \n");
   //sd_disp(sd3);
  return sd3;
}

int is_empty_orth(orth * o){
  int dim = o->dim;
  int is_empty=0;
  int lf, hf;

  int i=0;
  while(is_empty!=1  && i<dim){

    lf=o->l_corner_f[i];
    hf=o->h_corner_f[i];


    if(lf==CLOSED && hf==CLOSED)
      {
	if(o->h_corner[i] < o->l_corner[i])
	  is_empty=1;
      }
    else if(lf!=NC && hf!=NC)
      {
	if(o->h_corner[i] <= o->l_corner[i])
	  is_empty=1;	
      }
      
    i++;
  }

  return is_empty;

}

void sd_simpl(sdomain *sd){

  int nb=sd->nb_orth;
  orth *o1;
  orth *o2;
  orth *o3;
  //  orth *on;
  o1=sd->first;
  // on=sd->last;

  int i=0;
  int j=0;
  int incl=0;

  while(i<nb && sd->nb_orth>1){
   
    o2=o1->next;
	
    incl=orth_is_incl(o1,o2);
    //printf("incl0 : %d \n",incl);
    while(incl==0 && j<(sd->nb_orth - 2) )
      {
	
	o2=o2->next;
	if(orth_is_incl(o1,o2)) incl=1;
	//printf(" i:%d j: %d incl : %d \n",i,j,incl);
	j++;
      }
    if(incl){
      o3=o1;
      sd->first=o1->next;
      sd->nb_orth=sd->nb_orth-1;
    }
    else
      {

	(sd->last)->next=o1;
	sd->last=o1;
	sd->first=o1->next;
      }

    o1=o1->next;
    if(incl){delete_orth(o3);}
    j=0;
    i++;
  }
  //  printf("done\n");

}

int orth_is_incl(orth *o1, orth *o2){
  int incl=1;
  int dim = o1->dim;
  int i=0;
  while(incl==1  && i<dim){
    if(
       dim_incl_l(o1->l_corner_f[i],o1->l_corner[i],o2->l_corner_f[i],o2->l_corner[i])
       &&
       dim_incl_h(o1->h_corner_f[i],o1->h_corner[i],o2->h_corner_f[i],o2->h_corner[i])
       )
      incl=1;
    else
      incl=0;

    i++;
  }

  return incl;

}




//is 1 included in 2 ?
int dim_incl_l(int lf1, double lv1, int lf2, double lv2){
  int is_incl=0;
  switch(lf2)
    {
    case NC:
      is_incl=1;
      break;
    case OPEN:
      if(lf1==OPEN && lv1>=lv2) is_incl=1;
      else if(lf1==CLOSED && lv1>lv2) is_incl=1;
      break;
    case CLOSED:
      if(lf1==OPEN && lv1>=lv2) is_incl=1;
      else if(lf1==CLOSED && lv1>=lv2) is_incl=1;
      break;
    }
  return is_incl;

}



int dim_incl_h(int hf1, double hv1, int hf2, double hv2){
  int is_incl=0;
  switch(hf2)
    {
    case NC:
      is_incl=1;
      break;
    case OPEN:
      if(hf1==OPEN && hv1<=hv2) is_incl=1;
      else if(hf1==CLOSED && hv1<hv2) is_incl=1;
      break;
    case CLOSED:
      if(hf1==OPEN && hv1<=hv2) is_incl=1;
      else if(hf1==CLOSED && hv1<=hv2) is_incl=1;
      break;
    }
  return is_incl;

}

ppl_Polyhedron_t orth_to_pol(orth* o){

  int  dim;
  int i,newstruct=0;

  ppl_Linear_Expression_t le;
  ppl_Constraint_t pconst;
  ppl_Polyhedron_t p;
  ppl_Coefficient_t cp1,inp;
  mpz_t num,den;
  mpq_t coeff_const_q;

  if(o->is_empty)
    {
      ppl_new_NNC_Polyhedron_from_space_dimension( &p,  empty_key_vars, 1);
      return p;
    }


 if(o->is_universe)
    {
      ppl_new_NNC_Polyhedron_from_space_dimension( &p,  empty_key_vars, 0);
      return p;
    }


  dim=o->dim;
  i=0;
  mpq_init(coeff_const_q);
  mpz_init(num);mpz_init(den);  

  ppl_new_NNC_Polyhedron_from_space_dimension( &p,  dim, 0);

  for(i=0;i<dim;i++){
    switch(o->h_corner_f[i])
      {
      case NC:
	break;
      case OPEN:
	mpq_set_d(coeff_const_q,-o->h_corner[i]);
	mpq_get_num(num,coeff_const_q);
	mpq_get_den(den,coeff_const_q);

	ppl_new_Linear_Expression_with_dimension(&le, dim);
	ppl_new_Coefficient_from_mpz_t(&cp1,den);
	ppl_new_Coefficient_from_mpz_t(&inp,num);

	ppl_Linear_Expression_add_to_coefficient(le, i, cp1);
	ppl_Linear_Expression_add_to_inhomogeneous(le, inp);
	ppl_new_Constraint(&pconst, le,PPL_CONSTRAINT_TYPE_LESS_THAN);
	ppl_Polyhedron_add_constraint( p, pconst);
	newstruct=1;
	break;
      case CLOSED:
	
	mpq_set_d(coeff_const_q,-o->h_corner[i]);
	mpq_get_num(num,coeff_const_q);
	mpq_get_den(den,coeff_const_q);

	ppl_new_Linear_Expression_with_dimension(&le, dim);
	ppl_new_Coefficient_from_mpz_t(&cp1,den);
	ppl_new_Coefficient_from_mpz_t(&inp,num);
	ppl_Linear_Expression_add_to_coefficient(le, i, cp1);
	ppl_Linear_Expression_add_to_inhomogeneous(le, inp);
	ppl_new_Constraint(&pconst, le,PPL_CONSTRAINT_TYPE_LESS_OR_EQUAL);
	ppl_Polyhedron_add_constraint( p, pconst);
	newstruct=1;
	break;
      }

    if(newstruct){    
      ppl_delete_Linear_Expression(le); 
      ppl_delete_Constraint(pconst);
      ppl_delete_Coefficient(inp);
      ppl_delete_Coefficient(cp1);
    }

    newstruct=0;

    switch(o->l_corner_f[i])
      {
      case NC:
	break;
      case OPEN:
	mpq_set_d(coeff_const_q,-o->l_corner[i]);
	mpq_get_num(num,coeff_const_q);
	mpq_get_den(den,coeff_const_q);

	ppl_new_Linear_Expression_with_dimension(&le, dim);
	ppl_new_Coefficient_from_mpz_t(&cp1,den);
	ppl_new_Coefficient_from_mpz_t(&inp,num);
	ppl_Linear_Expression_add_to_coefficient(le, i, cp1);
	ppl_Linear_Expression_add_to_inhomogeneous(le, inp);
	ppl_new_Constraint(&pconst, le,PPL_CONSTRAINT_TYPE_GREATER_THAN);
	ppl_Polyhedron_add_constraint( p, pconst);
	newstruct=1;	
	break;
      case CLOSED:
	
	mpq_set_d(coeff_const_q,-o->l_corner[i]);
	mpq_get_num(num,coeff_const_q);
	mpq_get_den(den,coeff_const_q);

	ppl_new_Linear_Expression_with_dimension(&le, dim);
	ppl_new_Coefficient_from_mpz_t(&cp1,den);
	ppl_new_Coefficient_from_mpz_t(&inp,num);
	ppl_Linear_Expression_add_to_coefficient(le, i, cp1);
	ppl_Linear_Expression_add_to_inhomogeneous(le, inp);
	ppl_new_Constraint(&pconst, le,PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);
	ppl_Polyhedron_add_constraint( p, pconst);
	newstruct=1;
	break;
      }
    if(newstruct){    
      ppl_delete_Linear_Expression(le); 
      ppl_delete_Constraint(pconst);
      ppl_delete_Coefficient(inp);
      ppl_delete_Coefficient(cp1);
    }
    newstruct=0;

  }

     mpq_clear(coeff_const_q);
     mpz_clear(num);mpz_clear(den);  

  return p;

}


ppl_Pointset_Powerset_NNC_Polyhedron_t sdomain_to_ppl_dom(sdomain* sd){
  //printf("sd_to_ppl\n");

  ppl_Pointset_Powerset_NNC_Polyhedron_t d;
  ppl_Polyhedron_t p;
  int i;  
  orth *o;
  

  //sd_disp(sd);

  //d=malloc(sizeof(ppl_Pointset_Powerset_NNC_Polyhedron_t));
  

  
  if(sd->is_empty)
    {
      ppl_new_NNC_Polyhedron_from_space_dimension( &p,  empty_key_vars, 1);
      ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron(&d,p);
      
      ppl_delete_Polyhedron(p);
      
      return d;
    }
  

  if(sd->is_universe)
    {
      //printf("test univers\n");
      ppl_new_NNC_Polyhedron_from_space_dimension( &p,  empty_key_vars, 0);
      ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron(&d,p);
      
      ppl_delete_Polyhedron(p);
      
      return d;
    }


  ppl_new_NNC_Polyhedron_from_space_dimension( &p,  empty_key_vars, 1);
  ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron(&d,p);
  

  ppl_delete_Polyhedron(p);



  o=sd->first;

  for(i=0;i<sd->nb_orth;i++){

   p=orth_to_pol(o);

    ppl_Pointset_Powerset_NNC_Polyhedron_add_disjunct(d, p);

    ppl_delete_Polyhedron(p);

o=o->next;
  }
  

  return d;
}



ppl_Pointset_Powerset_NNC_Polyhedron_t d_build_ppl_empty(){
  ppl_Pointset_Powerset_NNC_Polyhedron_t d;
  ppl_Polyhedron_t p;
  
  ppl_new_NNC_Polyhedron_from_space_dimension( &p,  empty_key_vars, 1);
  ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron(&d,p);

  ppl_delete_Polyhedron(p);

  return d;
}


ppl_Pointset_Powerset_NNC_Polyhedron_t d_build_ppl_universe(){
  ppl_Pointset_Powerset_NNC_Polyhedron_t d;
  ppl_Polyhedron_t p;
  
  ppl_new_NNC_Polyhedron_from_space_dimension( &p,  empty_key_vars, 0);
  ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron(&d,p);
  ppl_delete_Polyhedron(p);

  return d;
}

ppl_Pointset_Powerset_NNC_Polyhedron_t d_build_ppl(int nbvars,double coeff_const,int ctype){
  ppl_Polyhedron_t p;
  ppl_Pointset_Powerset_NNC_Polyhedron_t d;
  p=oneconstraint_pol(nbvars,coeff_const,ctype);

  ppl_new_Pointset_Powerset_NNC_Polyhedron_from_NNC_Polyhedron(&d,p);

  ppl_delete_Polyhedron(p);

  return d;
}



ppl_Pointset_Powerset_NNC_Polyhedron_t d_inter_ppl(  ppl_Pointset_Powerset_NNC_Polyhedron_t d1,   ppl_Pointset_Powerset_NNC_Polyhedron_t d2){
  ppl_Pointset_Powerset_NNC_Polyhedron_t d3;
  //  d3=malloc(sizeof(ppl_Pointset_Powerset_NNC_Polyhedron_t));
  //printf("d_inter_ppl\n");
  //printf("d1:  \n");
  //ppl_io_print_Pointset_Powerset_NNC_Polyhedron(d1);
  //printf("\n");
  //printf("d2:  \n");  
  //ppl_io_print_Pointset_Powerset_NNC_Polyhedron(d2);

  ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron(&d3,d1);
  ppl_Pointset_Powerset_NNC_Polyhedron_intersection_assign(d3,d2);


  //printf("d3:  \n"); 
  //ppl_io_print_Pointset_Powerset_NNC_Polyhedron(d3); 
  //printf("\n");
  ppl_Pointset_Powerset_NNC_Polyhedron_omega_reduce(d3);

  //printf("d3 reduced:  \n");  
  //ppl_io_print_Pointset_Powerset_NNC_Polyhedron(d3);
  //printf("\n");
  //ppl_Pointset_Powerset_NNC_Polyhedron_pairwise_reduce(d3);//not efficient  

  return d3;
}


ppl_Pointset_Powerset_NNC_Polyhedron_t d_union_ppl(ppl_Pointset_Powerset_NNC_Polyhedron_t d1, ppl_Pointset_Powerset_NNC_Polyhedron_t d2){
  ppl_Pointset_Powerset_NNC_Polyhedron_t  d3;

   
     
  ppl_new_Pointset_Powerset_NNC_Polyhedron_from_Pointset_Powerset_NNC_Polyhedron(&d3,d1);
  ppl_Pointset_Powerset_NNC_Polyhedron_upper_bound_assign(d3,d2);
 
  ppl_Pointset_Powerset_NNC_Polyhedron_omega_reduce(d3);
  //ppl_Pointset_Powerset_NNC_Polyhedron_pairwise_reduce(*d3);//not efficient
 

  return d3; 
}



/* void dist_var_const(int key_var, double val, ppl_const_Constraint_t *p, double *distf){ */
/*   mpz_t c1m,r1_nz,r1_dz,r2_nz,r2_dz,coeff1z,num,den; */
/*   mpf_t r1_nf,r1_df,r2_nf,r2_df,res1f,res2f; */
/*   mpq_t val_q; */
/*   double dist,res1d,res2d; */
/*   ppl_Coefficient_t coeff1,cp1,r1_n,r1_d,r2_n,r2_d; */
/*   ppl_Linear_Expression_t le1,le2; */
/*   ppl_Constraint_t constr1, constr2; */
/*   ppl_Polyhedron_t p1,p2; */
/*   ppl_Generator_t point; */
/*   int t1,t2,tsup; */

/*   int raw =0x7f800000; */
/*   float inf = *( float* )&raw; */

/*   //build linear expression x-xval */
/*   ppl_new_Linear_Expression_with_dimension(&le1, empty_key_vars); */
/*   mpz_init(c1m); */
/*   mpz_init(num); */
/*   mpz_init(den); */
/*   mpq_init(val_q); */
/*   mpz_init(coeff1z); */
/*   mpf_init(r1_nf); */
/*   mpf_init(r1_df); */
/*   mpf_init(r2_nf); */
/*   mpf_init(r2_df); */
/*   mpf_init(res1f); */
/*   mpf_init(res2f); */


/*   mpq_set_d(val_q,val); */
/*   mpq_get_num(num,val_q); */
/*   mpq_get_den(den,val_q); */

/*   ppl_new_Coefficient(&r1_n); */
/*   ppl_new_Coefficient(&r1_d); */
/*   ppl_new_Coefficient(&r2_n); */
/*   ppl_new_Coefficient(&r2_d); */

/*   ppl_new_Generator_zero_dim_point(&point); */

/*   mpz_set_si(coeff1z,1); */
/*   mpz_neg(num,num); */
/*   ppl_new_Coefficient_from_mpz_t(&cp1,num); */
/*   ppl_new_Coefficient_from_mpz_t(&coeff1,den); */
/*   ppl_Linear_Expression_add_to_coefficient(le1, key_var,coeff1);//coeff  variable =1 */
/*   ppl_Linear_Expression_add_to_inhomogeneous(le1, cp1);//coeff  constant */



/*   ppl_new_Linear_Expression_from_Linear_Expression(&le2, le1); */
/*   //build constraints */
/*   ppl_new_Constraint(&constr2, le2,PPL_CONSTRAINT_TYPE_LESS_OR_EQUAL); */
/*   ppl_new_Constraint(&constr1, le1,PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL); */



/*   ppl_new_NNC_Polyhedron_from_NNC_Polyhedron(&p1, *p); */
/*   ppl_new_NNC_Polyhedron_from_NNC_Polyhedron(&p2, *p); */


/*   ppl_Polyhedron_add_constraint(p1,  constr1); */

/*   ppl_Polyhedron_add_constraint(p2,  constr2); */

/*   t1=ppl_Polyhedron_minimize_with_point(p1, le1, r1_n, r1_d, &tsup, point); */

/*   t2=ppl_Polyhedron_maximize_with_point(p2, le2, r2_n, r2_d, &tsup, point); */

/*   mpz_init(r1_nz);  mpz_init(r1_dz);  mpz_init(r2_nz);  mpz_init(r2_dz); */
/*   ppl_Coefficient_to_mpz_t(r1_n,r1_nz); */
/*   ppl_Coefficient_to_mpz_t(r1_d,r1_dz); */
/*   ppl_Coefficient_to_mpz_t(r2_n,r2_nz); */
/*   ppl_Coefficient_to_mpz_t(r2_d,r2_dz); */


/*   mpz_mul(r1_dz,r1_dz,den); */
/*   mpz_mul(r2_dz,r2_dz,den); */

/*   mpf_set_z(r1_nf, r1_nz); */
/*   mpf_set_z(r1_df, r1_dz); */
/*   mpf_set_z(r2_nf, r2_nz); */
/*   mpf_set_z(r2_df, r2_dz); */

/*   mpf_neg(r2_nf,r2_nf); */

/*   if(t1>0){ */
/*     if(t2>0){ */
/*       mpf_div(res1f,r1_nf,r1_df); */
/*       mpf_div(res2f,r2_nf,r2_df); */
/*       res1d=mpf_get_d(res1f); */
/*       res2d=mpf_get_d(res2f); */
/*       dist=f_min(res1d,res2d); */
/*     } */
/*     else{ */
/*       mpf_div(res1f,r1_nf,r1_df); */
/*       dist=mpf_get_d(res1f); */
      
/*     } */
/*   } */
/*   else{ */
/*     if(t2>0){ */
/*       mpf_div(res2f,r2_nf,r2_df); */
/*       dist=mpf_get_d(res2f); */
/*     } */
/*     else{ */
/*       dist=(double) inf; */
/*     } */
/*   } */
/*   *distf=dist; */


/*    ppl_delete_Polyhedron(p1); */
/*    ppl_delete_Polyhedron(p2); */

/*    ppl_delete_Coefficient(coeff1); */
/*    ppl_delete_Coefficient(cp1); */
/*    ppl_delete_Coefficient(r1_n); */
/*    ppl_delete_Coefficient(r1_d); */
/*    ppl_delete_Coefficient(r2_n); */
/*    ppl_delete_Coefficient(r2_d); */

/*    ppl_delete_Linear_Expression(le1); */
/*    ppl_delete_Linear_Expression(le2); */

/*    ppl_delete_Constraint(constr1); */
/*    ppl_delete_Constraint(constr2); */

/*    ppl_delete_Generator(point); */


/*    mpz_clear(c1m); */
/*    mpz_clear(r1_nz); */
/*    mpz_clear(r1_dz); */
/*    mpz_clear(r2_nz); */
/*    mpz_clear(r2_dz); */
/*    mpz_clear(coeff1z); */
/*    mpz_clear(num); */
/*    mpz_clear(den); */

/*    mpf_clear(r1_nf); */
/*    mpf_clear(r1_df); */
/*    mpf_clear(r2_nf); */
/*    mpf_clear(r2_df); */
/*    mpf_clear(res1f); */
/*    mpf_clear(res2f); */

/*    mpq_clear(val_q); */

/* } */

/* double dist_to_hplane(ppl_const_Constraint_t pconst){ */

/*   size_t dim; */
/*   ppl_Linear_Expression_t obj, vectnorm; */
/*   int i; */
/*   int index; */

/* ppl_Coefficient_t pointval; */
/* mpz_t  */
/*   ppl_Constraint_space_dimension(pconst,&dim); */

/*   //compute point in hplane */


/*  index=dim-1;    //index of non null composant of point in hplane */
/*     //value of this composant */

/*  for(i=0;i<dim:i++){ */
/*   } */
 
/*   //substract point to obj */

/*   //computye scalar product of obj and vect norm */


/* } */


Bool disp_obj(int a){
  int i;
  for (i=1;i<empty_key_obj;i++){
    //printf("varkey: %d varval: %g\n",tab_obj_keys[i],tab_obj_vals[i]);;
   }
  return TRUE;
}

Bool clean_obj(int a){

  empty_key_obj=1;

  return TRUE;
}




#if MAIN
#include "main.c"
#endif

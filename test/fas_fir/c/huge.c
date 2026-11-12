#include <stdio.h>
#include <string.h>
#include <stdlib.h>
static unsigned long long st=88172645463325252ULL;
static unsigned long long rnd(void){st^=st<<13;st^=st>>7;st^=st<<17;return st;}
int fails=0;

void f_add_1500(void*,void*,void*);
void f_sub_1500(void*,void*,void*);
void f_mul_1500(void*,void*,void*);
void f_udiv_1500(void*,void*,void*);
void f_urem_1500(void*,void*,void*);
void f_sdiv_1500(void*,void*,void*);
void f_srem_1500(void*,void*,void*);
void f_shl_1500(void*,void*,void*);
void f_lshr_1500(void*,void*,void*);
void f_ashr_1500(void*,void*,void*);
void f_umin_1500(void*,void*,void*);
void f_smax_1500(void*,void*,void*);
void f_ctz_1500(void*,void*);
void f_clz_1500(void*,void*);
void f_popcnt_1500(void*,void*);
double f_sitofp_1500(void*);
static void fill_1500(unsigned _BitInt(1500)*x,int k){ unsigned char*p=(unsigned char*)x; for(size_t i=0;i<sizeof *x;i++)p[i]=rnd(); *x=*x;
  if(k==0)*x=0; else if(k==1)*x=1; else if(k==2)*x=(unsigned _BitInt(1500))-1; else if(k==3)*x=((unsigned _BitInt(1500))-1)>>1; else if(k==4)*x=~(((unsigned _BitInt(1500))-1)>>1); else if(k==5)*x>>= (rnd()%1500); else if(k==6)*x=rnd(); }
static void t_1500(void){ enum{N=1500};
 for(int it=0;it<400;it++){ unsigned _BitInt(1500) a,b,r,e; fill_1500(&a,it%9); fill_1500(&b,(it/9)%9); _BitInt(1500) sa=(_BitInt(1500))a, sb=(_BitInt(1500))b; (void)sa;(void)sb;
  if(1){ memset(&r,0xa5,sizeof r); f_add_1500(&r,&a,&b); e=(unsigned _BitInt(1500))(a+b); if(memcmp(&r,&e,(1500+7)/8)){ if(fails++<100000)printf("FAIL add 1500 it=%d\n",it);} }
  if(1){ memset(&r,0xa5,sizeof r); f_sub_1500(&r,&a,&b); e=(unsigned _BitInt(1500))(a-b); if(memcmp(&r,&e,(1500+7)/8)){ if(fails++<100000)printf("FAIL sub 1500 it=%d\n",it);} }
  if(1){ memset(&r,0xa5,sizeof r); f_mul_1500(&r,&a,&b); e=(unsigned _BitInt(1500))(a*b); if(memcmp(&r,&e,(1500+7)/8)){ if(fails++<100000)printf("FAIL mul 1500 it=%d\n",it);} }
  if(b!=0){ memset(&r,0xa5,sizeof r); f_udiv_1500(&r,&a,&b); e=(unsigned _BitInt(1500))(a/b); if(memcmp(&r,&e,(1500+7)/8)){ if(fails++<100000)printf("FAIL udiv 1500 it=%d\n",it);} }
  if(b!=0){ memset(&r,0xa5,sizeof r); f_urem_1500(&r,&a,&b); e=(unsigned _BitInt(1500))(a%b); if(memcmp(&r,&e,(1500+7)/8)){ if(fails++<100000)printf("FAIL urem 1500 it=%d\n",it);} }
  if(sb!=0 && !(sa==(_BitInt(1500))~(((unsigned _BitInt(1500))-1)>>1) && sb==-1)){ memset(&r,0xa5,sizeof r); f_sdiv_1500(&r,&a,&b); e=(unsigned _BitInt(1500))(sa/sb); if(memcmp(&r,&e,(1500+7)/8)){ if(fails++<100000)printf("FAIL sdiv 1500 it=%d\n",it);} }
  if(sb!=0 && !(sa==(_BitInt(1500))~(((unsigned _BitInt(1500))-1)>>1) && sb==-1)){ memset(&r,0xa5,sizeof r); f_srem_1500(&r,&a,&b); e=(unsigned _BitInt(1500))(sa%sb); if(memcmp(&r,&e,(1500+7)/8)){ if(fails++<100000)printf("FAIL srem 1500 it=%d\n",it);} }
  if(1){ memset(&r,0xa5,sizeof r); f_shl_1500(&r,&a,&b); e=(unsigned _BitInt(1500))(a<<(b%N)); if(memcmp(&r,&e,(1500+7)/8)){ if(fails++<100000)printf("FAIL shl 1500 it=%d\n",it);} }
  if(1){ memset(&r,0xa5,sizeof r); f_lshr_1500(&r,&a,&b); e=(unsigned _BitInt(1500))(a>>(b%N)); if(memcmp(&r,&e,(1500+7)/8)){ if(fails++<100000)printf("FAIL lshr 1500 it=%d\n",it);} }
  if(1){ memset(&r,0xa5,sizeof r); f_ashr_1500(&r,&a,&b); e=(unsigned _BitInt(1500))(sa>>(b%N)); if(memcmp(&r,&e,(1500+7)/8)){ if(fails++<100000)printf("FAIL ashr 1500 it=%d\n",it);} }
  if(1){ memset(&r,0xa5,sizeof r); f_umin_1500(&r,&a,&b); e=(unsigned _BitInt(1500))(a<b?a:b); if(memcmp(&r,&e,(1500+7)/8)){ if(fails++<100000)printf("FAIL umin 1500 it=%d\n",it);} }
  if(1){ memset(&r,0xa5,sizeof r); f_smax_1500(&r,&a,&b); e=(unsigned _BitInt(1500))(sa>sb?sa:sb); if(memcmp(&r,&e,(1500+7)/8)){ if(fails++<100000)printf("FAIL smax 1500 it=%d\n",it);} }
  { f_ctz_1500(&r,&a); e=(unsigned _BitInt(1500))(a?__builtin_ctzg(a):1500); if(memcmp(&r,&e,(1500+7)/8)){ if(fails++<100000)printf("FAIL ctz 1500 it=%d\n",it);} }
  { f_clz_1500(&r,&a); e=(unsigned _BitInt(1500))(a?__builtin_clzg(a):1500); if(memcmp(&r,&e,(1500+7)/8)){ if(fails++<100000)printf("FAIL clz 1500 it=%d\n",it);} }
  { f_popcnt_1500(&r,&a); e=(unsigned _BitInt(1500))(__builtin_popcountg(a)); if(memcmp(&r,&e,(1500+7)/8)){ if(fails++<100000)printf("FAIL popcnt 1500 it=%d\n",it);} }
  { unsigned _BitInt(1500) m = sa<0 ? (unsigned _BitInt(1500))-a : a; int bl = m ? 1500-__builtin_clzg(m) : 0; double d=f_sitofp_1500(&a), x=(double)sa; if(bl<=1023 && d!=x){ if(fails++<100000)printf("FAIL sitofp 1500 it=%d %g %g\n",it,d,x);} }
}}
void f_add_4096(void*,void*,void*);
void f_sub_4096(void*,void*,void*);
void f_mul_4096(void*,void*,void*);
void f_udiv_4096(void*,void*,void*);
void f_urem_4096(void*,void*,void*);
void f_sdiv_4096(void*,void*,void*);
void f_srem_4096(void*,void*,void*);
void f_shl_4096(void*,void*,void*);
void f_lshr_4096(void*,void*,void*);
void f_ashr_4096(void*,void*,void*);
void f_umin_4096(void*,void*,void*);
void f_smax_4096(void*,void*,void*);
void f_ctz_4096(void*,void*);
void f_clz_4096(void*,void*);
void f_popcnt_4096(void*,void*);
double f_sitofp_4096(void*);
static void fill_4096(unsigned _BitInt(4096)*x,int k){ unsigned char*p=(unsigned char*)x; for(size_t i=0;i<sizeof *x;i++)p[i]=rnd(); *x=*x;
  if(k==0)*x=0; else if(k==1)*x=1; else if(k==2)*x=(unsigned _BitInt(4096))-1; else if(k==3)*x=((unsigned _BitInt(4096))-1)>>1; else if(k==4)*x=~(((unsigned _BitInt(4096))-1)>>1); else if(k==5)*x>>= (rnd()%4096); else if(k==6)*x=rnd(); }
static void t_4096(void){ enum{N=4096};
 for(int it=0;it<400;it++){ unsigned _BitInt(4096) a,b,r,e; fill_4096(&a,it%9); fill_4096(&b,(it/9)%9); _BitInt(4096) sa=(_BitInt(4096))a, sb=(_BitInt(4096))b; (void)sa;(void)sb;
  if(1){ memset(&r,0xa5,sizeof r); f_add_4096(&r,&a,&b); e=(unsigned _BitInt(4096))(a+b); if(memcmp(&r,&e,(4096+7)/8)){ if(fails++<100000)printf("FAIL add 4096 it=%d\n",it);} }
  if(1){ memset(&r,0xa5,sizeof r); f_sub_4096(&r,&a,&b); e=(unsigned _BitInt(4096))(a-b); if(memcmp(&r,&e,(4096+7)/8)){ if(fails++<100000)printf("FAIL sub 4096 it=%d\n",it);} }
  if(1){ memset(&r,0xa5,sizeof r); f_mul_4096(&r,&a,&b); e=(unsigned _BitInt(4096))(a*b); if(memcmp(&r,&e,(4096+7)/8)){ if(fails++<100000)printf("FAIL mul 4096 it=%d\n",it);} }
  if(b!=0){ memset(&r,0xa5,sizeof r); f_udiv_4096(&r,&a,&b); e=(unsigned _BitInt(4096))(a/b); if(memcmp(&r,&e,(4096+7)/8)){ if(fails++<100000)printf("FAIL udiv 4096 it=%d\n",it);} }
  if(b!=0){ memset(&r,0xa5,sizeof r); f_urem_4096(&r,&a,&b); e=(unsigned _BitInt(4096))(a%b); if(memcmp(&r,&e,(4096+7)/8)){ if(fails++<100000)printf("FAIL urem 4096 it=%d\n",it);} }
  if(sb!=0 && !(sa==(_BitInt(4096))~(((unsigned _BitInt(4096))-1)>>1) && sb==-1)){ memset(&r,0xa5,sizeof r); f_sdiv_4096(&r,&a,&b); e=(unsigned _BitInt(4096))(sa/sb); if(memcmp(&r,&e,(4096+7)/8)){ if(fails++<100000)printf("FAIL sdiv 4096 it=%d\n",it);} }
  if(sb!=0 && !(sa==(_BitInt(4096))~(((unsigned _BitInt(4096))-1)>>1) && sb==-1)){ memset(&r,0xa5,sizeof r); f_srem_4096(&r,&a,&b); e=(unsigned _BitInt(4096))(sa%sb); if(memcmp(&r,&e,(4096+7)/8)){ if(fails++<100000)printf("FAIL srem 4096 it=%d\n",it);} }
  if(1){ memset(&r,0xa5,sizeof r); f_shl_4096(&r,&a,&b); e=(unsigned _BitInt(4096))(a<<(b%N)); if(memcmp(&r,&e,(4096+7)/8)){ if(fails++<100000)printf("FAIL shl 4096 it=%d\n",it);} }
  if(1){ memset(&r,0xa5,sizeof r); f_lshr_4096(&r,&a,&b); e=(unsigned _BitInt(4096))(a>>(b%N)); if(memcmp(&r,&e,(4096+7)/8)){ if(fails++<100000)printf("FAIL lshr 4096 it=%d\n",it);} }
  if(1){ memset(&r,0xa5,sizeof r); f_ashr_4096(&r,&a,&b); e=(unsigned _BitInt(4096))(sa>>(b%N)); if(memcmp(&r,&e,(4096+7)/8)){ if(fails++<100000)printf("FAIL ashr 4096 it=%d\n",it);} }
  if(1){ memset(&r,0xa5,sizeof r); f_umin_4096(&r,&a,&b); e=(unsigned _BitInt(4096))(a<b?a:b); if(memcmp(&r,&e,(4096+7)/8)){ if(fails++<100000)printf("FAIL umin 4096 it=%d\n",it);} }
  if(1){ memset(&r,0xa5,sizeof r); f_smax_4096(&r,&a,&b); e=(unsigned _BitInt(4096))(sa>sb?sa:sb); if(memcmp(&r,&e,(4096+7)/8)){ if(fails++<100000)printf("FAIL smax 4096 it=%d\n",it);} }
  { f_ctz_4096(&r,&a); e=(unsigned _BitInt(4096))(a?__builtin_ctzg(a):4096); if(memcmp(&r,&e,(4096+7)/8)){ if(fails++<100000)printf("FAIL ctz 4096 it=%d\n",it);} }
  { f_clz_4096(&r,&a); e=(unsigned _BitInt(4096))(a?__builtin_clzg(a):4096); if(memcmp(&r,&e,(4096+7)/8)){ if(fails++<100000)printf("FAIL clz 4096 it=%d\n",it);} }
  { f_popcnt_4096(&r,&a); e=(unsigned _BitInt(4096))(__builtin_popcountg(a)); if(memcmp(&r,&e,(4096+7)/8)){ if(fails++<100000)printf("FAIL popcnt 4096 it=%d\n",it);} }
  { unsigned _BitInt(4096) m = sa<0 ? (unsigned _BitInt(4096))-a : a; int bl = m ? 4096-__builtin_clzg(m) : 0; double d=f_sitofp_4096(&a), x=(double)sa; if(bl<=1023 && d!=x){ if(fails++<100000)printf("FAIL sitofp 4096 it=%d %g %g\n",it,d,x);} }
}}
int main(void){t_1500();t_4096();printf("fails=%d\n",fails);return fails!=0;}

# 32 "force.c"
static char *SRCID_force_c = "$Id: force.c,v 1.72 2002/08/01 21:56:52 lindahl Exp $";
# 1 "/usr/include/math.h" 1 3
# 27 "/usr/include/math.h" 3
# 1 "/usr/include/features.h" 1 3
# 283 "/usr/include/features.h" 3
# 1 "/usr/include/sys/cdefs.h" 1 3
# 284 "/usr/include/features.h" 2 3
# 312 "/usr/include/features.h" 3
# 1 "/usr/include/gnu/stubs.h" 1 3
# 313 "/usr/include/features.h" 2 3
# 28 "/usr/include/math.h" 2 3





# 1 "/usr/include/bits/huge_val.h" 1 3
# 25 "/usr/include/bits/huge_val.h" 3
# 1 "/usr/include/features.h" 1 3
# 26 "/usr/include/bits/huge_val.h" 2 3
# 34 "/usr/include/math.h" 2 3






# 1 "/usr/include/bits/mathdef.h" 1 3
# 41 "/usr/include/math.h" 2 3
# 63 "/usr/include/math.h" 3
# 1 "/usr/include/bits/mathcalls.h" 1 3
# 54 "/usr/include/bits/mathcalls.h" 3
extern double acos (double __x) ; extern double __acos (double __x) ;

extern double asin (double __x) ; extern double __asin (double __x) ;

extern double atan (double __x) ; extern double __atan (double __x) ;

extern double atan2 (double __y, double __x) ; extern double __atan2 (double __y, double __x) ;


extern double cos (double __x) ; extern double __cos (double __x) ;

extern double sin (double __x) ; extern double __sin (double __x) ;

extern double tan (double __x) ; extern double __tan (double __x) ;
# 78 "/usr/include/bits/mathcalls.h" 3
extern double cosh (double __x) ; extern double __cosh (double __x) ;

extern double sinh (double __x) ; extern double __sinh (double __x) ;

extern double tanh (double __x) ; extern double __tanh (double __x) ;



extern double acosh (double __x) ; extern double __acosh (double __x) ;

extern double asinh (double __x) ; extern double __asinh (double __x) ;

extern double atanh (double __x) ; extern double __atanh (double __x) ;





extern double exp (double __x) ; extern double __exp (double __x) ;
# 106 "/usr/include/bits/mathcalls.h" 3
extern double frexp (double __x, int *__exponent) ; extern double __frexp (double __x, int *__exponent) ;


extern double ldexp (double __x, int __exponent) ; extern double __ldexp (double __x, int __exponent) ;


extern double log (double __x) ; extern double __log (double __x) ;


extern double log10 (double __x) ; extern double __log10 (double __x) ;


extern double modf (double __x, double *__iptr) ; extern double __modf (double __x, double *__iptr) ;



extern double expm1 (double __x) ; extern double __expm1 (double __x) ;


extern double log1p (double __x) ; extern double __log1p (double __x) ;


extern double logb (double __x) ; extern double __logb (double __x) ;
# 143 "/usr/include/bits/mathcalls.h" 3
extern double pow (double __x, double __y) ; extern double __pow (double __x, double __y) ;


extern double sqrt (double __x) ; extern double __sqrt (double __x) ;



extern double hypot (double __x, double __y) ; extern double __hypot (double __x, double __y) ;




extern double cbrt (double __x) ; extern double __cbrt (double __x) ;






extern double ceil (double __x) ; extern double __ceil (double __x) ;


extern double fabs (double __x) __attribute__ ((__const__)); extern double __fabs (double __x) __attribute__ ((__const__));


extern double floor (double __x) ; extern double __floor (double __x) ;


extern double fmod (double __x, double __y) ; extern double __fmod (double __x, double __y) ;




extern int __isinf (double __value) __attribute__ ((__const__));


extern int __finite (double __value) __attribute__ ((__const__));




extern int isinf (double __value) __attribute__ ((__const__));


extern int finite (double __value) __attribute__ ((__const__));


extern double drem (double __x, double __y) ; extern double __drem (double __x, double __y) ;



extern double significand (double __x) ; extern double __significand (double __x) ;




extern double copysign (double __x, double __y) __attribute__ ((__const__)); extern double __copysign (double __x, double __y) __attribute__ ((__const__));
# 209 "/usr/include/bits/mathcalls.h" 3
extern int __isnan (double __value) __attribute__ ((__const__));



extern int isnan (double __value) __attribute__ ((__const__));


extern double j0 (double) ; extern double __j0 (double) ;
extern double j1 (double) ; extern double __j1 (double) ;
extern double jn (int, double) ; extern double __jn (int, double) ;
extern double y0 (double) ; extern double __y0 (double) ;
extern double y1 (double) ; extern double __y1 (double) ;
extern double yn (int, double) ; extern double __yn (int, double) ;





extern double erf (double) ; extern double __erf (double) ;
extern double erfc (double) ; extern double __erfc (double) ;
extern double lgamma (double) ; extern double __lgamma (double) ;
# 238 "/usr/include/bits/mathcalls.h" 3
extern double gamma (double) ; extern double __gamma (double) ;






extern double lgamma_r (double, int *__signgamp) ; extern double __lgamma_r (double, int *__signgamp) ;






extern double rint (double __x) ; extern double __rint (double __x) ;


extern double nextafter (double __x, double __y) __attribute__ ((__const__)); extern double __nextafter (double __x, double __y) __attribute__ ((__const__));





extern double remainder (double __x, double __y) ; extern double __remainder (double __x, double __y) ;



extern double scalb (double __x, double __n) ; extern double __scalb (double __x, double __n) ;




extern double scalbn (double __x, int __n) ; extern double __scalbn (double __x, int __n) ;



extern int ilogb (double __x) ; extern int __ilogb (double __x) ;
# 64 "/usr/include/math.h" 2 3
# 82 "/usr/include/math.h" 3
# 1 "/usr/include/bits/mathcalls.h" 1 3
# 54 "/usr/include/bits/mathcalls.h" 3
extern float acosf (float __x) ; extern float __acosf (float __x) ;

extern float asinf (float __x) ; extern float __asinf (float __x) ;

extern float atanf (float __x) ; extern float __atanf (float __x) ;

extern float atan2f (float __y, float __x) ; extern float __atan2f (float __y, float __x) ;


extern float cosf (float __x) ; extern float __cosf (float __x) ;

extern float sinf (float __x) ; extern float __sinf (float __x) ;

extern float tanf (float __x) ; extern float __tanf (float __x) ;
# 78 "/usr/include/bits/mathcalls.h" 3
extern float coshf (float __x) ; extern float __coshf (float __x) ;

extern float sinhf (float __x) ; extern float __sinhf (float __x) ;

extern float tanhf (float __x) ; extern float __tanhf (float __x) ;



extern float acoshf (float __x) ; extern float __acoshf (float __x) ;

extern float asinhf (float __x) ; extern float __asinhf (float __x) ;

extern float atanhf (float __x) ; extern float __atanhf (float __x) ;





extern float expf (float __x) ; extern float __expf (float __x) ;
# 106 "/usr/include/bits/mathcalls.h" 3
extern float frexpf (float __x, int *__exponent) ; extern float __frexpf (float __x, int *__exponent) ;


extern float ldexpf (float __x, int __exponent) ; extern float __ldexpf (float __x, int __exponent) ;


extern float logf (float __x) ; extern float __logf (float __x) ;


extern float log10f (float __x) ; extern float __log10f (float __x) ;


extern float modff (float __x, float *__iptr) ; extern float __modff (float __x, float *__iptr) ;



extern float expm1f (float __x) ; extern float __expm1f (float __x) ;


extern float log1pf (float __x) ; extern float __log1pf (float __x) ;


extern float logbf (float __x) ; extern float __logbf (float __x) ;
# 143 "/usr/include/bits/mathcalls.h" 3
extern float powf (float __x, float __y) ; extern float __powf (float __x, float __y) ;


extern float sqrtf (float __x) ; extern float __sqrtf (float __x) ;



extern float hypotf (float __x, float __y) ; extern float __hypotf (float __x, float __y) ;




extern float cbrtf (float __x) ; extern float __cbrtf (float __x) ;






extern float ceilf (float __x) ; extern float __ceilf (float __x) ;


extern float fabsf (float __x) __attribute__ ((__const__)); extern float __fabsf (float __x) __attribute__ ((__const__));


extern float floorf (float __x) ; extern float __floorf (float __x) ;


extern float fmodf (float __x, float __y) ; extern float __fmodf (float __x, float __y) ;




extern int __isinff (float __value) __attribute__ ((__const__));


extern int __finitef (float __value) __attribute__ ((__const__));




extern int isinff (float __value) __attribute__ ((__const__));


extern int finitef (float __value) __attribute__ ((__const__));


extern float dremf (float __x, float __y) ; extern float __dremf (float __x, float __y) ;



extern float significandf (float __x) ; extern float __significandf (float __x) ;




extern float copysignf (float __x, float __y) __attribute__ ((__const__)); extern float __copysignf (float __x, float __y) __attribute__ ((__const__));
# 209 "/usr/include/bits/mathcalls.h" 3
extern int __isnanf (float __value) __attribute__ ((__const__));



extern int isnanf (float __value) __attribute__ ((__const__));


extern float j0f (float) ; extern float __j0f (float) ;
extern float j1f (float) ; extern float __j1f (float) ;
extern float jnf (int, float) ; extern float __jnf (int, float) ;
extern float y0f (float) ; extern float __y0f (float) ;
extern float y1f (float) ; extern float __y1f (float) ;
extern float ynf (int, float) ; extern float __ynf (int, float) ;





extern float erff (float) ; extern float __erff (float) ;
extern float erfcf (float) ; extern float __erfcf (float) ;
extern float lgammaf (float) ; extern float __lgammaf (float) ;
# 238 "/usr/include/bits/mathcalls.h" 3
extern float gammaf (float) ; extern float __gammaf (float) ;






extern float lgammaf_r (float, int *__signgamp) ; extern float __lgammaf_r (float, int *__signgamp) ;






extern float rintf (float __x) ; extern float __rintf (float __x) ;


extern float nextafterf (float __x, float __y) __attribute__ ((__const__)); extern float __nextafterf (float __x, float __y) __attribute__ ((__const__));





extern float remainderf (float __x, float __y) ; extern float __remainderf (float __x, float __y) ;



extern float scalbf (float __x, float __n) ; extern float __scalbf (float __x, float __n) ;




extern float scalbnf (float __x, int __n) ; extern float __scalbnf (float __x, int __n) ;



extern int ilogbf (float __x) ; extern int __ilogbf (float __x) ;
# 83 "/usr/include/math.h" 2 3
# 99 "/usr/include/math.h" 3
# 1 "/usr/include/bits/mathcalls.h" 1 3
# 54 "/usr/include/bits/mathcalls.h" 3
extern long double acosl (long double __x) ; extern long double __acosl (long double __x) ;

extern long double asinl (long double __x) ; extern long double __asinl (long double __x) ;

extern long double atanl (long double __x) ; extern long double __atanl (long double __x) ;

extern long double atan2l (long double __y, long double __x) ; extern long double __atan2l (long double __y, long double __x) ;


extern long double cosl (long double __x) ; extern long double __cosl (long double __x) ;

extern long double sinl (long double __x) ; extern long double __sinl (long double __x) ;

extern long double tanl (long double __x) ; extern long double __tanl (long double __x) ;
# 78 "/usr/include/bits/mathcalls.h" 3
extern long double coshl (long double __x) ; extern long double __coshl (long double __x) ;

extern long double sinhl (long double __x) ; extern long double __sinhl (long double __x) ;

extern long double tanhl (long double __x) ; extern long double __tanhl (long double __x) ;



extern long double acoshl (long double __x) ; extern long double __acoshl (long double __x) ;

extern long double asinhl (long double __x) ; extern long double __asinhl (long double __x) ;

extern long double atanhl (long double __x) ; extern long double __atanhl (long double __x) ;





extern long double expl (long double __x) ; extern long double __expl (long double __x) ;
# 106 "/usr/include/bits/mathcalls.h" 3
extern long double frexpl (long double __x, int *__exponent) ; extern long double __frexpl (long double __x, int *__exponent) ;


extern long double ldexpl (long double __x, int __exponent) ; extern long double __ldexpl (long double __x, int __exponent) ;


extern long double logl (long double __x) ; extern long double __logl (long double __x) ;


extern long double log10l (long double __x) ; extern long double __log10l (long double __x) ;


extern long double modfl (long double __x, long double *__iptr) ; extern long double __modfl (long double __x, long double *__iptr) ;



extern long double expm1l (long double __x) ; extern long double __expm1l (long double __x) ;


extern long double log1pl (long double __x) ; extern long double __log1pl (long double __x) ;


extern long double logbl (long double __x) ; extern long double __logbl (long double __x) ;
# 143 "/usr/include/bits/mathcalls.h" 3
extern long double powl (long double __x, long double __y) ; extern long double __powl (long double __x, long double __y) ;


extern long double sqrtl (long double __x) ; extern long double __sqrtl (long double __x) ;



extern long double hypotl (long double __x, long double __y) ; extern long double __hypotl (long double __x, long double __y) ;




extern long double cbrtl (long double __x) ; extern long double __cbrtl (long double __x) ;






extern long double ceill (long double __x) ; extern long double __ceill (long double __x) ;


extern long double fabsl (long double __x) __attribute__ ((__const__)); extern long double __fabsl (long double __x) __attribute__ ((__const__));


extern long double floorl (long double __x) ; extern long double __floorl (long double __x) ;


extern long double fmodl (long double __x, long double __y) ; extern long double __fmodl (long double __x, long double __y) ;




extern int __isinfl (long double __value) __attribute__ ((__const__));


extern int __finitel (long double __value) __attribute__ ((__const__));




extern int isinfl (long double __value) __attribute__ ((__const__));


extern int finitel (long double __value) __attribute__ ((__const__));


extern long double dreml (long double __x, long double __y) ; extern long double __dreml (long double __x, long double __y) ;



extern long double significandl (long double __x) ; extern long double __significandl (long double __x) ;




extern long double copysignl (long double __x, long double __y) __attribute__ ((__const__)); extern long double __copysignl (long double __x, long double __y) __attribute__ ((__const__));
# 209 "/usr/include/bits/mathcalls.h" 3
extern int __isnanl (long double __value) __attribute__ ((__const__));



extern int isnanl (long double __value) __attribute__ ((__const__));


extern long double j0l (long double) ; extern long double __j0l (long double) ;
extern long double j1l (long double) ; extern long double __j1l (long double) ;
extern long double jnl (int, long double) ; extern long double __jnl (int, long double) ;
extern long double y0l (long double) ; extern long double __y0l (long double) ;
extern long double y1l (long double) ; extern long double __y1l (long double) ;
extern long double ynl (int, long double) ; extern long double __ynl (int, long double) ;





extern long double erfl (long double) ; extern long double __erfl (long double) ;
extern long double erfcl (long double) ; extern long double __erfcl (long double) ;
extern long double lgammal (long double) ; extern long double __lgammal (long double) ;
# 238 "/usr/include/bits/mathcalls.h" 3
extern long double gammal (long double) ; extern long double __gammal (long double) ;






extern long double lgammal_r (long double, int *__signgamp) ; extern long double __lgammal_r (long double, int *__signgamp) ;






extern long double rintl (long double __x) ; extern long double __rintl (long double __x) ;


extern long double nextafterl (long double __x, long double __y) __attribute__ ((__const__)); extern long double __nextafterl (long double __x, long double __y) __attribute__ ((__const__));





extern long double remainderl (long double __x, long double __y) ; extern long double __remainderl (long double __x, long double __y) ;



extern long double scalbl (long double __x, long double __n) ; extern long double __scalbl (long double __x, long double __n) ;




extern long double scalbnl (long double __x, int __n) ; extern long double __scalbnl (long double __x, int __n) ;



extern int ilogbl (long double __x) ; extern int __ilogbl (long double __x) ;
# 100 "/usr/include/math.h" 2 3
# 113 "/usr/include/math.h" 3
extern int signgam;
# 240 "/usr/include/math.h" 3
typedef enum
{
  _IEEE_ = -1,
  _SVID_,
  _XOPEN_,
  _POSIX_,
  _ISOC_
} _LIB_VERSION_TYPE;




extern _LIB_VERSION_TYPE _LIB_VERSION;
# 265 "/usr/include/math.h" 3
struct exception

  {
    int type;
    char *name;
    double arg1;
    double arg2;
    double retval;
  };




extern int matherr (struct exception *__exc);
# 34 "force.c" 2




# 1 "/usr/include/assert.h" 1 3
# 36 "/usr/include/assert.h" 3
# 1 "/usr/include/features.h" 1 3
# 37 "/usr/include/assert.h" 2 3
# 68 "/usr/include/assert.h" 3
extern void __assert_fail (__const char *__assertion, __const char *__file,
                           unsigned int __line, __const char *__function)
             __attribute__ ((__noreturn__));


extern void __assert_perror_fail (int __errnum, __const char *__file,
                                  unsigned int __line,
                                  __const char *__function)
             __attribute__ ((__noreturn__));




extern void __assert (const char *__assertion, const char *__file, int __line)
             __attribute__ ((__noreturn__));
# 39 "force.c" 2
# 48 "force.c"
# 1 "wnblist.h" 1
# 36 "wnblist.h"
static char *SRCID_wnblist_h = "$Id: wnblist.h,v 1.4 2002/02/28 10:32:06 spoel Exp $";



# 1 "/usr/include/stdio.h" 1 3
# 28 "/usr/include/stdio.h" 3
# 1 "/usr/include/features.h" 1 3
# 29 "/usr/include/stdio.h" 2 3





# 1 "/usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stddef.h" 1 3
# 199 "/usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stddef.h" 3
typedef unsigned int size_t;
# 35 "/usr/include/stdio.h" 2 3

# 1 "/usr/include/bits/types.h" 1 3
# 26 "/usr/include/bits/types.h" 3
# 1 "/usr/include/features.h" 1 3
# 27 "/usr/include/bits/types.h" 2 3


# 1 "/usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stddef.h" 1 3
# 30 "/usr/include/bits/types.h" 2 3


typedef unsigned char __u_char;
typedef unsigned short __u_short;
typedef unsigned int __u_int;
typedef unsigned long __u_long;

__extension__ typedef unsigned long long int __u_quad_t;
__extension__ typedef long long int __quad_t;
# 49 "/usr/include/bits/types.h" 3
typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;

__extension__ typedef signed long long int __int64_t;
__extension__ typedef unsigned long long int __uint64_t;

typedef __quad_t *__qaddr_t;

typedef __u_quad_t __dev_t;
typedef __u_int __uid_t;
typedef __u_int __gid_t;
typedef __u_long __ino_t;
typedef __u_int __mode_t;
typedef __u_int __nlink_t;
typedef long int __off_t;
typedef __quad_t __loff_t;
typedef int __pid_t;
typedef int __ssize_t;
typedef __u_long __rlim_t;
typedef __u_quad_t __rlim64_t;
typedef __u_int __id_t;

typedef struct
  {
    int __val[2];
  } __fsid_t;


typedef int __daddr_t;
typedef char *__caddr_t;
typedef long int __time_t;
typedef unsigned int __useconds_t;
typedef long int __suseconds_t;
typedef long int __swblk_t;

typedef long int __clock_t;


typedef int __clockid_t;


typedef int __timer_t;






typedef int __key_t;


typedef unsigned short int __ipc_pid_t;



typedef long int __blksize_t;




typedef long int __blkcnt_t;
typedef __quad_t __blkcnt64_t;


typedef __u_long __fsblkcnt_t;
typedef __u_quad_t __fsblkcnt64_t;


typedef __u_long __fsfilcnt_t;
typedef __u_quad_t __fsfilcnt64_t;


typedef __u_quad_t __ino64_t;


typedef __loff_t __off64_t;


typedef long int __t_scalar_t;
typedef unsigned long int __t_uscalar_t;


typedef int __intptr_t;


typedef unsigned int __socklen_t;




# 1 "/usr/include/bits/pthreadtypes.h" 1 3
# 23 "/usr/include/bits/pthreadtypes.h" 3
# 1 "/usr/include/bits/sched.h" 1 3
# 68 "/usr/include/bits/sched.h" 3
struct __sched_param
  {
    int __sched_priority;
  };
# 24 "/usr/include/bits/pthreadtypes.h" 2 3


struct _pthread_fastlock
{
  long int __status;
  int __spinlock;

};



typedef struct _pthread_descr_struct *_pthread_descr;





typedef struct __pthread_attr_s
{
  int __detachstate;
  int __schedpolicy;
  struct __sched_param __schedparam;
  int __inheritsched;
  int __scope;
  size_t __guardsize;
  int __stackaddr_set;
  void *__stackaddr;
  size_t __stacksize;
} pthread_attr_t;



typedef struct
{
  struct _pthread_fastlock __c_lock;
  _pthread_descr __c_waiting;
} pthread_cond_t;



typedef struct
{
  int __dummy;
} pthread_condattr_t;


typedef unsigned int pthread_key_t;





typedef struct
{
  int __m_reserved;
  int __m_count;
  _pthread_descr __m_owner;
  int __m_kind;
  struct _pthread_fastlock __m_lock;
} pthread_mutex_t;



typedef struct
{
  int __mutexkind;
} pthread_mutexattr_t;



typedef int pthread_once_t;
# 140 "/usr/include/bits/pthreadtypes.h" 3
typedef unsigned long int pthread_t;
# 144 "/usr/include/bits/types.h" 2 3
# 37 "/usr/include/stdio.h" 2 3
# 45 "/usr/include/stdio.h" 3
typedef struct _IO_FILE FILE;
# 55 "/usr/include/stdio.h" 3
typedef struct _IO_FILE __FILE;
# 65 "/usr/include/stdio.h" 3
# 1 "/usr/include/libio.h" 1 3
# 32 "/usr/include/libio.h" 3
# 1 "/usr/include/_G_config.h" 1 3
# 9 "/usr/include/_G_config.h" 3
# 1 "/usr/include/bits/types.h" 1 3
# 10 "/usr/include/_G_config.h" 2 3




# 1 "/usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stddef.h" 1 3
# 287 "/usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stddef.h" 3
typedef long int wchar_t;
# 312 "/usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stddef.h" 3
typedef unsigned int wint_t;
# 15 "/usr/include/_G_config.h" 2 3
# 24 "/usr/include/_G_config.h" 3
# 1 "/usr/include/wchar.h" 1 3
# 48 "/usr/include/wchar.h" 3
# 1 "/usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stddef.h" 1 3
# 49 "/usr/include/wchar.h" 2 3

# 1 "/usr/include/bits/wchar.h" 1 3
# 51 "/usr/include/wchar.h" 2 3
# 67 "/usr/include/wchar.h" 3
typedef struct
{
  int __count;
  union
  {
    wint_t __wch;
    char __wchb[4];
  } __value;
} __mbstate_t;
# 25 "/usr/include/_G_config.h" 2 3

typedef struct
{
  __off_t __pos;
  __mbstate_t __state;
} _G_fpos_t;
typedef struct
{
  __off64_t __pos;
  __mbstate_t __state;
} _G_fpos64_t;
# 44 "/usr/include/_G_config.h" 3
# 1 "/usr/include/gconv.h" 1 3
# 26 "/usr/include/gconv.h" 3
# 1 "/usr/include/features.h" 1 3
# 27 "/usr/include/gconv.h" 2 3

# 1 "/usr/include/wchar.h" 1 3
# 48 "/usr/include/wchar.h" 3
# 1 "/usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stddef.h" 1 3
# 49 "/usr/include/wchar.h" 2 3

# 1 "/usr/include/bits/wchar.h" 1 3
# 51 "/usr/include/wchar.h" 2 3
# 29 "/usr/include/gconv.h" 2 3


# 1 "/usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stddef.h" 1 3
# 32 "/usr/include/gconv.h" 2 3





enum
{
  __GCONV_OK = 0,
  __GCONV_NOCONV,
  __GCONV_NODB,
  __GCONV_NOMEM,

  __GCONV_EMPTY_INPUT,
  __GCONV_FULL_OUTPUT,
  __GCONV_ILLEGAL_INPUT,
  __GCONV_INCOMPLETE_INPUT,

  __GCONV_ILLEGAL_DESCRIPTOR,
  __GCONV_INTERNAL_ERROR
};



enum
{
  __GCONV_IS_LAST = 0x0001,
  __GCONV_IGNORE_ERRORS = 0x0002
};



struct __gconv_step;
struct __gconv_step_data;
struct __gconv_loaded_object;
struct __gconv_trans_data;



typedef int (*__gconv_fct) (struct __gconv_step *, struct __gconv_step_data *,
                            __const unsigned char **, __const unsigned char *,
                            unsigned char **, size_t *, int, int);


typedef int (*__gconv_init_fct) (struct __gconv_step *);
typedef void (*__gconv_end_fct) (struct __gconv_step *);



typedef int (*__gconv_trans_fct) (struct __gconv_step *,
                                  struct __gconv_step_data *, void *,
                                  __const unsigned char *,
                                  __const unsigned char **,
                                  __const unsigned char *, unsigned char **,
                                  size_t *);


typedef int (*__gconv_trans_context_fct) (void *, __const unsigned char *,
                                          __const unsigned char *,
                                          unsigned char *, unsigned char *);


typedef int (*__gconv_trans_query_fct) (__const char *, __const char ***,
                                        size_t *);


typedef int (*__gconv_trans_init_fct) (void **, const char *);
typedef void (*__gconv_trans_end_fct) (void *);

struct __gconv_trans_data
{

  __gconv_trans_fct __trans_fct;
  __gconv_trans_context_fct __trans_context_fct;
  __gconv_trans_end_fct __trans_end_fct;
  void *__data;
  struct __gconv_trans_data *__next;
};



struct __gconv_step
{
  struct __gconv_loaded_object *__shlib_handle;
  __const char *__modname;

  int __counter;

  char *__from_name;
  char *__to_name;

  __gconv_fct __fct;
  __gconv_init_fct __init_fct;
  __gconv_end_fct __end_fct;



  int __min_needed_from;
  int __max_needed_from;
  int __min_needed_to;
  int __max_needed_to;


  int __stateful;

  void *__data;
};



struct __gconv_step_data
{
  unsigned char *__outbuf;
  unsigned char *__outbufend;



  int __flags;



  int __invocation_counter;



  int __internal_use;

  __mbstate_t *__statep;
  __mbstate_t __state;



  struct __gconv_trans_data *__trans;
};



typedef struct __gconv_info
{
  size_t __nsteps;
  struct __gconv_step *__steps;
  __extension__ struct __gconv_step_data __data [0];
} *__gconv_t;
# 45 "/usr/include/_G_config.h" 2 3
typedef union
{
  struct __gconv_info __cd;
  struct
  {
    struct __gconv_info __cd;
    struct __gconv_step_data __data;
  } __combined;
} _G_iconv_t;

typedef int _G_int16_t __attribute__ ((__mode__ (__HI__)));
typedef int _G_int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int _G_uint16_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int _G_uint32_t __attribute__ ((__mode__ (__SI__)));
# 33 "/usr/include/libio.h" 2 3
# 53 "/usr/include/libio.h" 3
# 1 "/usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stdarg.h" 1 3
# 43 "/usr/lib/gcc-lib/i386-redhat-linux/2.96/include/stdarg.h" 3
typedef __builtin_va_list __gnuc_va_list;
# 54 "/usr/include/libio.h" 2 3
# 160 "/usr/include/libio.h" 3
struct _IO_jump_t; struct _IO_FILE;
# 170 "/usr/include/libio.h" 3
typedef void _IO_lock_t;





struct _IO_marker {
  struct _IO_marker *_next;
  struct _IO_FILE *_sbuf;



  int _pos;
# 193 "/usr/include/libio.h" 3
};


enum __codecvt_result
{
  __codecvt_ok,
  __codecvt_partial,
  __codecvt_error,
  __codecvt_noconv
};
# 261 "/usr/include/libio.h" 3
struct _IO_FILE {
  int _flags;




  char* _IO_read_ptr;
  char* _IO_read_end;
  char* _IO_read_base;
  char* _IO_write_base;
  char* _IO_write_ptr;
  char* _IO_write_end;
  char* _IO_buf_base;
  char* _IO_buf_end;

  char *_IO_save_base;
  char *_IO_backup_base;
  char *_IO_save_end;

  struct _IO_marker *_markers;

  struct _IO_FILE *_chain;

  int _fileno;
  int _blksize;
  __off_t _old_offset;



  unsigned short _cur_column;
  signed char _vtable_offset;
  char _shortbuf[1];



  _IO_lock_t *_lock;
# 305 "/usr/include/libio.h" 3
  __off64_t _offset;





  void *__pad1;
  void *__pad2;

  int _mode;

  char _unused2[15 * sizeof (int) - 2 * sizeof (void *)];

};


typedef struct _IO_FILE _IO_FILE;


struct _IO_FILE_plus;

extern struct _IO_FILE_plus _IO_2_1_stdin_;
extern struct _IO_FILE_plus _IO_2_1_stdout_;
extern struct _IO_FILE_plus _IO_2_1_stderr_;
# 344 "/usr/include/libio.h" 3
typedef __ssize_t __io_read_fn (void *__cookie, char *__buf, size_t __nbytes);







typedef __ssize_t __io_write_fn (void *__cookie, __const char *__buf,
                                 size_t __n);







typedef int __io_seek_fn (void *__cookie, __off64_t *__pos, int __w);


typedef int __io_close_fn (void *__cookie);
# 396 "/usr/include/libio.h" 3
extern int __underflow (_IO_FILE *) ;
extern int __uflow (_IO_FILE *) ;
extern int __overflow (_IO_FILE *, int) ;
extern wint_t __wunderflow (_IO_FILE *) ;
extern wint_t __wuflow (_IO_FILE *) ;
extern wint_t __woverflow (_IO_FILE *, wint_t) ;
# 426 "/usr/include/libio.h" 3
extern int _IO_getc (_IO_FILE *__fp) ;
extern int _IO_putc (int __c, _IO_FILE *__fp) ;
extern int _IO_feof (_IO_FILE *__fp) ;
extern int _IO_ferror (_IO_FILE *__fp) ;

extern int _IO_peekc_locked (_IO_FILE *__fp) ;





extern void _IO_flockfile (_IO_FILE *) ;
extern void _IO_funlockfile (_IO_FILE *) ;
extern int _IO_ftrylockfile (_IO_FILE *) ;
# 456 "/usr/include/libio.h" 3
extern int _IO_vfscanf (_IO_FILE * __restrict, const char * __restrict,
                        __gnuc_va_list, int *__restrict) ;
extern int _IO_vfprintf (_IO_FILE *__restrict, const char *__restrict,
                         __gnuc_va_list) ;
extern __ssize_t _IO_padn (_IO_FILE *, int, __ssize_t) ;
extern size_t _IO_sgetn (_IO_FILE *, void *, size_t) ;

extern __off64_t _IO_seekoff (_IO_FILE *, __off64_t, int, int) ;
extern __off64_t _IO_seekpos (_IO_FILE *, __off64_t, int) ;

extern void _IO_free_backup_area (_IO_FILE *) ;
# 66 "/usr/include/stdio.h" 2 3
# 80 "/usr/include/stdio.h" 3
typedef _G_fpos_t fpos_t;
# 129 "/usr/include/stdio.h" 3
# 1 "/usr/include/bits/stdio_lim.h" 1 3
# 130 "/usr/include/stdio.h" 2 3



extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;
# 144 "/usr/include/stdio.h" 3
extern int remove (__const char *__filename) ;

extern int rename (__const char *__old, __const char *__new) ;




extern FILE *tmpfile (void) ;
# 163 "/usr/include/stdio.h" 3
extern char *tmpnam (char *__s) ;




extern char *tmpnam_r (char *__s) ;
# 180 "/usr/include/stdio.h" 3
extern char *tempnam (__const char *__dir, __const char *__pfx)
             __attribute__ ((__malloc__));




extern int fclose (FILE *__stream) ;

extern int fflush (FILE *__stream) ;



extern int fflush_unlocked (FILE *__stream) ;
# 203 "/usr/include/stdio.h" 3
extern FILE *fopen (__const char *__restrict __filename,
                    __const char *__restrict __modes) ;

extern FILE *freopen (__const char *__restrict __filename,
                      __const char *__restrict __modes,
                      FILE *__restrict __stream) ;
# 233 "/usr/include/stdio.h" 3
extern FILE *fdopen (int __fd, __const char *__modes) ;
# 256 "/usr/include/stdio.h" 3
extern void setbuf (FILE *__restrict __stream, char *__restrict __buf) ;



extern int setvbuf (FILE *__restrict __stream, char *__restrict __buf,
                    int __modes, size_t __n) ;




extern void setbuffer (FILE *__restrict __stream, char *__restrict __buf,
                       size_t __size) ;


extern void setlinebuf (FILE *__stream) ;




extern int fprintf (FILE *__restrict __stream,
                    __const char *__restrict __format, ...) ;

extern int printf (__const char *__restrict __format, ...) ;

extern int sprintf (char *__restrict __s,
                    __const char *__restrict __format, ...) ;


extern int vfprintf (FILE *__restrict __s, __const char *__restrict __format,
                     __gnuc_va_list __arg) ;

extern int vprintf (__const char *__restrict __format, __gnuc_va_list __arg)
            ;

extern int vsprintf (char *__restrict __s, __const char *__restrict __format,
                     __gnuc_va_list __arg) ;



extern int snprintf (char *__restrict __s, size_t __maxlen,
                     __const char *__restrict __format, ...)
             __attribute__ ((__format__ (__printf__, 3, 4)));

extern int vsnprintf (char *__restrict __s, size_t __maxlen,
                      __const char *__restrict __format, __gnuc_va_list __arg)
             __attribute__ ((__format__ (__printf__, 3, 0)));
# 327 "/usr/include/stdio.h" 3
extern int fscanf (FILE *__restrict __stream,
                   __const char *__restrict __format, ...) ;

extern int scanf (__const char *__restrict __format, ...) ;

extern int sscanf (__const char *__restrict __s,
                   __const char *__restrict __format, ...) ;
# 353 "/usr/include/stdio.h" 3
extern int fgetc (FILE *__stream) ;
extern int getc (FILE *__stream) ;


extern int getchar (void) ;







extern int getc_unlocked (FILE *__stream) ;
extern int getchar_unlocked (void) ;




extern int fgetc_unlocked (FILE *__stream) ;




extern int fputc (int __c, FILE *__stream) ;
extern int putc (int __c, FILE *__stream) ;


extern int putchar (int __c) ;







extern int fputc_unlocked (int __c, FILE *__stream) ;




extern int putc_unlocked (int __c, FILE *__stream) ;
extern int putchar_unlocked (int __c) ;





extern int getw (FILE *__stream) ;


extern int putw (int __w, FILE *__stream) ;




extern char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream)
            ;
# 419 "/usr/include/stdio.h" 3
extern char *gets (char *__s) ;
# 443 "/usr/include/stdio.h" 3
extern int fputs (__const char *__restrict __s, FILE *__restrict __stream)
            ;
# 453 "/usr/include/stdio.h" 3
extern int puts (__const char *__s) ;



extern int ungetc (int __c, FILE *__stream) ;



extern size_t fread (void *__restrict __ptr, size_t __size,
                     size_t __n, FILE *__restrict __stream) ;

extern size_t fwrite (__const void *__restrict __ptr, size_t __size,
                      size_t __n, FILE *__restrict __s) ;



extern size_t fread_unlocked (void *__restrict __ptr, size_t __size,
                              size_t __n, FILE *__restrict __stream) ;
extern size_t fwrite_unlocked (__const void *__restrict __ptr, size_t __size,
                               size_t __n, FILE *__restrict __stream) ;




extern int fseek (FILE *__stream, long int __off, int __whence) ;

extern long int ftell (FILE *__stream) ;

extern void rewind (FILE *__stream) ;
# 497 "/usr/include/stdio.h" 3
extern int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos)
            ;

extern int fsetpos (FILE *__stream, __const fpos_t *__pos) ;
# 533 "/usr/include/stdio.h" 3
extern void clearerr (FILE *__stream) ;

extern int feof (FILE *__stream) ;

extern int ferror (FILE *__stream) ;



extern void clearerr_unlocked (FILE *__stream) ;
extern int feof_unlocked (FILE *__stream) ;
extern int ferror_unlocked (FILE *__stream) ;




extern void perror (__const char *__s) ;




extern int sys_nerr;
extern __const char *__const sys_errlist[];
# 564 "/usr/include/stdio.h" 3
extern int fileno (FILE *__stream) ;




extern int fileno_unlocked (FILE *__stream) ;






extern FILE *popen (__const char *__command, __const char *__modes) ;


extern int pclose (FILE *__stream) ;





extern char *ctermid (char *__s) ;
# 611 "/usr/include/stdio.h" 3
extern void flockfile (FILE *__stream) ;



extern int ftrylockfile (FILE *__stream) ;


extern void funlockfile (FILE *__stream) ;
# 41 "wnblist.h" 2


extern void dump_nblist(FILE *out,t_forcerec *fr,int nDNL);

extern void read_nblist(FILE *in,FILE *out,int **mat,int natoms);
# 49 "force.c" 2
# 58 "force.c"
# 1 "poisson.h" 1
# 36 "poisson.h"
static char *SRCID_poisson_h = "$Id: poisson.h,v 1.4 2002/02/28 10:32:05 spoel Exp $";






typedef struct {
  int nx,ny,nz;
  real ***ptr;
} t_PSgrid;

extern void unpack_PSgrid(t_PSgrid *grid,int *nx,int *ny,int *nz,real ****ptr);

extern void symmetrize_PSgrid(FILE *fp,t_PSgrid *grid,real sum);

extern void calc_nxyz(int nx,int ny,int nz,
                      int **nnx,int **nny,int **nnz);


extern real ps_gather_f(FILE *log,bool bVerbose,
                        int natoms,rvec x[],rvec f[],real charge[],rvec box,
                        real pot[],t_PSgrid *grid,rvec beta,t_nrnb *nrnb);

extern void spread_q_poisson(FILE *log,bool bVerbose,bool bCoulomb,
                             int natoms,rvec x[],real prop[],rvec box,
                             real rc,t_PSgrid *grid,t_nrnb *nrnb,
                             bool bOld,real r1);






extern int solve_poisson(FILE *log,t_PSgrid *pot,t_PSgrid *rho,
                         bool bVerbose,t_nrnb *nrnb,int maxnit,real tol,
                         rvec box);




static void calc_invh_h(rvec box,int nx,int ny,int nz,rvec invh,rvec h)
{
  invh[XX] = nx/box[XX];
  invh[YY] = ny/box[YY];
  invh[ZZ] = nz/box[ZZ];
  h[XX] = 1.0/invh[XX];
  h[YY] = 1.0/invh[YY];
  h[ZZ] = 1.0/invh[ZZ];
}

extern real do_poisson(FILE *log, bool bVerbose,
                       t_inputrec *ir, int natoms,
                       rvec x[], rvec f[],
                       real charge[], rvec box,
                       real phi[], t_commrec *cr,
                       t_nrnb *nrnb, int *nit,
                       bool bOld);


extern real do_optimize_poisson(FILE *log, bool bVerbose,
                                t_inputrec *ir, int natoms,
                                rvec x[], rvec f[],
                                real charge[], rvec box,
                                real phi[], t_commrec *cr,
                                t_nrnb *nrnb, rvec f_ref[],
                                real phi_ref[], rvec beta,
                                bool bOld);
# 59 "force.c" 2






t_forcerec *mk_forcerec(void)
{
  t_forcerec *fr;

  snew(fr,1);

  return fr;
}
# 93 "force.c"
static real *mk_nbfp(t_idef *idef,bool bBHAM)
{
  real *nbfp;
  int i,j,k,atnr;

  atnr=idef->atnr;
  if (bBHAM) {
    snew(nbfp,3*atnr*atnr);
    for(i=k=0; (i<atnr); i++) {
      for(j=0; (j<atnr); j++,k++) {
        BHAMA(nbfp,atnr,i,j) = idef->iparams[k].bham.a;
        BHAMB(nbfp,atnr,i,j) = idef->iparams[k].bham.b;
        BHAMC(nbfp,atnr,i,j) = idef->iparams[k].bham.c;
      }
    }
  }
  else {
    snew(nbfp,2*atnr*atnr);
    for(i=k=0; (i<atnr); i++) {
      for(j=0; (j<atnr); j++,k++) {
        C6(nbfp,atnr,i,j) = idef->iparams[k].lj.c6;
        C12(nbfp,atnr,i,j) = idef->iparams[k].lj.c12;
      }
    }
  }
  return nbfp;
}

static void check_solvent(FILE *fp,t_topology *top,t_forcerec *fr,
                          t_mdatoms *md,t_nsborder *nsb)
{




  t_block *cgs,*excl,*mols;
  atom_id *cgid;
  int i,j,m,j0,j1,nj,k,aj,ak,tjA,tjB,nl_m,nl_n,nl_o;
  int warncount;
  bool bOneCG;
  bool *bAllExcl,bAE,bOrder;
  bool *bHaveLJ,*bHaveCoul;

  cgs = &(top->blocks[ebCGS]);
  excl = &(top->atoms.excl);
  mols = &(top->blocks[ebMOLS]);

  if (fp)
    fprintf(fp,"Going to determine what solvent types we have.\n");
  snew(fr->solvent_type,cgs->nr+1);
  snew(fr->mno_index,(cgs->nr+1)*3);


  cgid = make_invblock(cgs,cgs->nra);

  warncount=0;


  if (fp)
    fprintf(fp,"There are %d molecules, %d charge groups and %d atoms\n",
            mols->nr,cgs->nr,cgs->nra);
  for(i=0; (i<mols->nr); i++) {

    bOneCG = TRUE;

    j0 = mols->index[i];
    j1 = mols->index[i+1];
    nj = j1-j0;
    for(j=j0+1; (j<j1); j++) {
      bOneCG = bOneCG && (cgid[mols->a[j]] == cgid[mols->a[j-1]]);
    }
    if (fr->bSolvOpt && bOneCG && nj>1) {

      snew(bAllExcl,nj);
      bAE = TRUE;

      for(j=j0; (j<j1) && bAE; j++) {



        for(k=0; (k<nj); k++)
          bAllExcl[k] = FALSE;

        for(k=excl->index[j]; (k<excl->index[j+1]); k++) {
          ak = excl->a[k];

          if ((ak < j0) || (ak >= j1))
            fatal_error(0,"Exclusion outside molecule? ak = %d, j0 = %d, j1 = %d, mol is %d",ak,j0,j1,i);
          bAllExcl[ak-j0] = TRUE;
        }

        for(k=0; (k<nj); k++)
          bAE = bAE && bAllExcl[k];
      }
      if (bAE) {
        snew(bHaveCoul,nj);
        snew(bHaveLJ,nj);
        for(j=j0; (j<j1); j++) {

          aj = mols->a[j];
          bHaveCoul[j-j0] = ((top->atoms.atom[aj].q != 0.0) ||
                             (top->atoms.atom[aj].qB != 0.0));

          tjA = top->atoms.atom[aj].type;
          tjB = top->atoms.atom[aj].typeB;
          bHaveLJ[j-j0] = FALSE;
          for(k=0; (k<fr->ntype); k++) {
            if (fr->bBHAM)
              bHaveLJ[j-j0] = (bHaveLJ[j-j0] ||
                               (BHAMA(fr->nbfp,fr->ntype,tjA,k) != 0.0) ||
                               (BHAMB(fr->nbfp,fr->ntype,tjA,k) != 0.0) ||
                               (BHAMC(fr->nbfp,fr->ntype,tjA,k) != 0.0) ||
                               (BHAMA(fr->nbfp,fr->ntype,tjB,k) != 0.0) ||
                               (BHAMB(fr->nbfp,fr->ntype,tjB,k) != 0.0) ||
                               (BHAMC(fr->nbfp,fr->ntype,tjB,k) != 0.0));
            else
              bHaveLJ[j-j0] = (bHaveLJ[j-j0] ||
                               (C6(fr->nbfp,fr->ntype,tjA,k) != 0.0) ||
                               (C12(fr->nbfp,fr->ntype,tjA,k) != 0.0) ||
                               (C6(fr->nbfp,fr->ntype,tjB,k) != 0.0) ||
                               (C12(fr->nbfp,fr->ntype,tjB,k) != 0.0));
          }
        }
# 229 "force.c"
        aj=mols->a[j0];
        if((nj==3) && bHaveCoul[0] && bHaveLJ[0] &&
           !bHaveLJ[1] && !bHaveLJ[2] &&
           (top->atoms.atom[aj+1].q == top->atoms.atom[aj+2].q))
          fr->solvent_type[cgid[aj]] = esolWATER;
        else {




          for(k=0; (k<nj) && (bHaveLJ[k] && bHaveCoul[k]); k++)
            ;
          nl_n = k;
          for(; (k<nj) && (!bHaveLJ[k] && bHaveCoul[k]); k++)
            ;
          nl_o = k;
          for(; (k<nj) && (bHaveLJ[k] && !bHaveCoul[k]); k++)
            ;
          nl_m = k;

          bOrder = FALSE;
          for(; (k<nj); k++)
            bOrder = bOrder || (bHaveLJ[k] || bHaveCoul[k]);
          if (bOrder) {




            if (nl_n != nj) {
              warncount++;
              if(warncount<11)
                fprintf(fp,"The order in molecule %d could be optimized"
                        " for better performance\n",i);
              if(warncount==10)
                fprintf(fp,"(More than 10 molecules where the order can be optimized)\n");
            }
            nl_m = nl_n = nl_o = nj;
          }
          fr->mno_index[cgid[aj]*3] = nl_m;
          fr->mno_index[cgid[aj]*3+1] = nl_n;
          fr->mno_index[cgid[aj]*3+2] = nl_o;
          fr->solvent_type[cgid[aj]] = esolMNO;

        }


        for(j=j0; (j<j1); j++)
          if (md->bPerturbed[mols->a[j]])
            fr->solvent_type[cgid[mols->a[j0]]] = esolNO;

        sfree(bHaveLJ);
        sfree(bHaveCoul);
      }
      else {



        fr->solvent_type[cgid[mols->a[j0]]] = esolNO;
      }
      sfree(bAllExcl);
    }
    else {

      for(j=mols->index[i]; (j<mols->index[i+1]); j++) {
        fr->solvent_type[cgid[mols->a[j]]] = esolNO;
      }
    }
  }
  if (debug) {
    for(i=0; (i<cgs->nr); i++)
      fprintf(debug,"MNO: cg = %5d, m = %2d, n = %2d, o = %2d\n",
              i,fr->mno_index[3*i],fr->mno_index[3*i+1],fr->mno_index[3*i+2]);
  }


  fr->nMNOMol = 0;
  fr->nWatMol = 0;
  for(m=0; m<3; m++)
    fr->nMNOav[m] = 0;
  for(i=0; i<mols->nr; i++) {
    j = mols->a[mols->index[i]];
    if (j>=START(nsb) && j<START(nsb)+HOMENR(nsb)) {
        if (fr->solvent_type[cgid[j]] == esolMNO) {
          fr->nMNOMol++;
          for(m=0; m<3; m++)
            fr->nMNOav[m] += fr->mno_index[3*cgid[j]+m];
        }
        else if (fr->solvent_type[cgid[j]] == esolWATER)
          fr->nWatMol++;
    }
  }
  if (fr->nMNOMol > 0)
    for(m=0; (m<3); m++)
      fr->nMNOav[m] /= fr->nMNOMol;

  sfree(cgid);

  if (fp) {
    fprintf(fp,"There are %d optimized solvent molecules on node %d\n",
            fr->nMNOMol,nsb->nodeid);
    if (fr->nMNOMol > 0)
      fprintf(fp,"  aver. nr. of atoms per molecule: vdwc %.1f coul %.1f vdw %.1f\n",
              fr->nMNOav[1],fr->nMNOav[2]-fr->nMNOav[1],fr->nMNOav[0]-fr->nMNOav[2]);
    fprintf(fp,"There are %d optimized water molecules on node %d\n",
            fr->nWatMol,nsb->nodeid);
  }
}

static void calc_rffac(FILE *log,int eel,real eps,real Rc,real Temp,
                       real zsq,matrix box,
                       real *kappa,real *epsfac,real *krf,real *crf)
{

  static bool bFirst=TRUE;
  real k1,k2,I,vol,rmin;

  if ((eel == eelRF) || (eel == eelGRF)) {
    vol = det(box);
    I = zsq/vol;
    if (eel == eelGRF) {

      if (Temp <= 0.0)
        fatal_error(0,"Temperature is %f while using"
                    " Generalized Reaction Field\n",Temp);

      *kappa = sqrt(2*I/(EPSILON0*eps*BOLTZ*Temp));
    }
    else
      *kappa = 0;


    if (eps == 0) {
      *krf = 1/(2*Rc*Rc*Rc);
      *crf = 0;
    }
    else {
      k1 = (1+*kappa*Rc);
      k2 = eps*sqr((real)(*kappa*Rc));

      *krf = (((eps-1)*k1+k2)/((2*eps+1)*k1+2*k2)/(Rc*Rc*Rc));
      *crf = 1/Rc + *krf*Rc*Rc;
    }
    *epsfac = ONE_4PI_EPS0;
    rmin = pow(*krf*2.0,-1.0/3.0);

    if (bFirst) {
      if (eel == eelGRF)
        please_cite(log,"Tironi95a");
      fprintf(log,"%s:\n"
              "epsRF = %10g, I   = %10g, volume = %10g, kappa  = %10g\n"
              "rc    = %10g, krf = %10g, crf    = %10g, epsfac = %10g\n",
              eel_names[eel],eps,I,vol,*kappa,Rc,*krf,*crf,*epsfac);
      fprintf(log,
              "The electrostatics potential has its minimum at rc = %g\n",
              rmin);

      bFirst=FALSE;
    }
  }
  else {



    *kappa = 0.0;
    *krf = 0.0;
    *crf = 0.0;
    if (eps == 0)
      eps = 1;
    *epsfac = ONE_4PI_EPS0/eps;
  }
}

void update_forcerec(FILE *log,t_forcerec *fr,matrix box)
{
  calc_rffac(log,fr->eeltype,
             fr->epsilon_r,fr->rcoulomb,fr->temp,fr->zsquare,box,
             &fr->kappa,&fr->epsfac,&fr->k_rf,&fr->c_rf);
}

static double calc_avcsix(FILE *log,real *nbfp,int atnr,
                          int natoms,int type[],bool bBHAM)
{
  int i,j,tpi,tpj;
  double csix;


  csix = 0;
  for(i=0; (i<natoms); i++) {
    tpi = type[i];




    for(j=0; (j<natoms); j++) {
      tpj = type[j];




      if (bBHAM)
        csix += BHAMC(nbfp,atnr,tpi,tpj);
      else
        csix += C6(nbfp,atnr,tpi,tpj);
    }
  }
  csix /= (natoms*natoms);
  if (debug)
    fprintf(debug,"Average C6 parameter is: %10g\n",csix);

  return csix;
}

void set_avcsix(FILE *log,t_forcerec *fr,t_mdatoms *mdatoms)
{
  fr->avcsix=calc_avcsix(log,fr->nbfp,fr->ntype,mdatoms->nr,
                         mdatoms->typeA,fr->bBHAM);
}

static void set_bham_b_max(FILE *log,t_forcerec *fr,t_mdatoms *mdatoms)
{
  int i,j,tpi,tpj,ntypes,natoms,*type;
  real b,bmin;
  real *nbfp;

  fprintf(log,"Determining largest Buckingham b parameter for table\n");
  nbfp = fr->nbfp;
  ntypes = fr->ntype;
  type = mdatoms->typeA;
  natoms = mdatoms->nr;

  bmin = -1;
  fr->bham_b_max = 0;
  for(i=0; (i<natoms); i++) {
    tpi = type[i];
    if (tpi >= ntypes)
      fatal_error(0,"Atomtype[%d] = %d, maximum = %d",i,tpi,ntypes);

    for(j=0; (j<natoms); j++) {
      tpj = type[j];
      if (tpj >= ntypes)
        fatal_error(0,"Atomtype[%d] = %d, maximum = %d",j,tpj,ntypes);
      b = BHAMB(nbfp,ntypes,tpi,tpj);
      if (b > fr->bham_b_max)
        fr->bham_b_max = b;
      if ((b < bmin) || (bmin==-1))
        bmin = b;
    }
  }
  fprintf(log,"Buckingham b parameters, min: %g, max: %g\n",
          bmin,fr->bham_b_max);
}

void init_forcerec(FILE *fp,
                   t_forcerec *fr,
                   t_inputrec *ir,
                   t_topology *top,
                   t_commrec *cr,
                   t_mdatoms *mdatoms,
                   t_nsborder *nsb,
                   matrix box,
                   bool bMolEpot,
                   char *tabfn,
                   bool bNoSolvOpt)
{
  int i,j,m,natoms,ngrp,tabelemsize;
  real q,zsq,nrdf,T;
  rvec box_size;
  double rtab;
  t_block *mols,*cgs;
  t_idef *idef;

  if (check_box(box))
    fatal_error(0,check_box(box));

  cgs = &(top->blocks[ebCGS]);
  mols = &(top->blocks[ebMOLS]);
  idef = &(top->idef);

  natoms = mdatoms->nr;


  fr->fc_stepsize = ir->fc_stepsize;


  fr->efep = ir->efep;
  fr->sc_alpha = ir->sc_alpha;
  fr->sc_sigma6 = pow(ir->sc_sigma,6);


  fr->bGrid = (ir->ns_type == ensGRID);
  fr->ndelta = ir->ndelta;
  fr->ePBC = ir->ePBC;
  fr->rlist = ir->rlist;
  fr->rlistlong = max(ir->rlist,max(ir->rcoulomb,ir->rvdw));
  fr->eeltype = ir->coulombtype;
  fr->vdwtype = ir->vdwtype;

  fr->bTwinRange = fr->rlistlong > fr->rlist;
  fr->bEwald = fr->eeltype==eelPME || fr->eeltype==eelEWALD;
  fr->bvdwtab = fr->vdwtype != evdwCUT;
  fr->bRF = (fr->eeltype==eelRF || fr->eeltype==eelGRF) &&
                    fr->vdwtype==evdwCUT;
  fr->bcoultab = (fr->eeltype!=eelCUT && !fr->bRF) || fr->bEwald;

  if (getenv("GMX_FORCE_TABLES")) {
    fr->bvdwtab = TRUE;
    fr->bcoultab = TRUE;
  }

  if (fp) {
    fprintf(fp,"Table routines are used for coulomb: %s\n",bool_names[fr->bcoultab]);
    fprintf(fp,"Table routines are used for vdw:     %s\n",bool_names[fr->bvdwtab ]);
  }


  if(fr->bEwald) {
    fr->ewaldcoeff=calc_ewaldcoeff(ir->rcoulomb, ir->ewald_rtol);
    if (fp)
      fprintf(fp,"Using a Gaussian width (1/beta) of %g nm for Ewald\n",
              1/fr->ewaldcoeff);
  }


  fr->bDomDecomp = ir->bDomDecomp;
  fr->Dimension = ir->decomp_dir;


  fr->epsilon_r = ir->epsilon_r;
  fr->fudgeQQ = ir->fudgeQQ;
  fr->rcoulomb_switch = ir->rcoulomb_switch;
  fr->rcoulomb = ir->rcoulomb;

  if (bNoSolvOpt || getenv("GMX_NO_SOLV_OPT"))
    fr->bSolvOpt = FALSE;
  else
    fr->bSolvOpt = TRUE;


  fr->zsquare = 0.0;
  fr->temp = 0.0;

  if (fr->eeltype == eelGRF) {
    zsq = 0.0;
    for (i=0; (i<cgs->nr); i++) {
      q = 0;
      for(j=cgs->index[i]; (j<cgs->index[i+1]); j++)
        q+=mdatoms->chargeT[cgs->a[j]];
      if (q != 0.0)




        zsq += fabs(q);
    }
    fr->zsquare = zsq;

    T = 0.0;
    nrdf = 0.0;
    for(i=0; (i<ir->opts.ngtc); i++) {
      nrdf += ir->opts.nrdf[i];
      T += (ir->opts.nrdf[i] * ir->opts.ref_t[i]);
    }
    if (nrdf == 0)
      fatal_error(0,"No degrees of freedom!");
    fr->temp = T/nrdf;
  }
  else if (EEL_LR(fr->eeltype) || (fr->eeltype == eelSHIFT) ||
           (fr->eeltype == eelUSER) || (fr->eeltype == eelSWITCH)) {
# 607 "force.c"
    for(m=0; (m<DIM); m++)
      box_size[m]=box[m][m];

    if (fr->phi == ((void *)0))
      snew(fr->phi,mdatoms->nr);

    if ((fr->eeltype==eelPPPM) || (fr->eeltype==eelPOISSON) ||
        (fr->eeltype == eelSHIFT && fr->rcoulomb > fr->rcoulomb_switch))
        set_shift_consts(fp,fr->rcoulomb_switch,fr->rcoulomb,box_size,fr);
  }


  if (fr->bTwinRange) {
    snew(fr->f_twin,natoms);
    snew(fr->fshift_twin,SHIFTS);
  }

  if (EEL_LR(fr->eeltype)) {
    snew(fr->f_pme,natoms);
  }
# 637 "force.c"
  if (fr->cg_cm == ((void *)0))
    snew(fr->cg_cm,cgs->nr);
  if (fr->shift_vec == ((void *)0))
    snew(fr->shift_vec,SHIFTS);

  if (fr->fshift == ((void *)0))
    snew(fr->fshift,SHIFTS);

  if (bMolEpot && (fr->nmol==0)) {
    fr->nmol=mols->nr;
    fr->mol_nr=make_invblock(mols,natoms);
    snew(fr->mol_epot,fr->nmol);
    fr->nstcalc=ir->nstenergy;
  }

  if (fr->nbfp == ((void *)0)) {
    fr->ntype = idef->atnr;
    fr->bBHAM = (idef->functype[0] == F_BHAM);
    fr->nbfp = mk_nbfp(idef,fr->bBHAM);
  }

  fr->eg_excl = ir->opts.eg_excl;


  fr->rvdw = ir->rvdw;
  fr->rvdw_switch = ir->rvdw_switch;
  if ((fr->vdwtype != evdwCUT) && (fr->vdwtype != evdwUSER) && !fr->bBHAM) {
    if (fr->rvdw_switch >= fr->rvdw)
      fatal_error(0,"rvdw_switch (%g) must be < rvdw (%g)",
                  fr->rvdw_switch,fr->rvdw);
    if (fp)
      fprintf(fp,"Using %s Lennard-Jones, switch between %g and %g nm\n",
              (fr->eeltype==eelSWITCH) ? "switched":"shifted",
              fr->rvdw_switch,fr->rvdw);
  }

  if (fp)
    fprintf(fp,"Cut-off's:   NS: %g   Coulomb: %g   %s: %g\n",
            fr->rlist,fr->rcoulomb,fr->bBHAM ? "BHAM":"LJ",fr->rvdw);

  if (ir->eDispCorr != edispcNO)
    set_avcsix(fp,fr,mdatoms);
  if (fr->bBHAM)
    set_bham_b_max(fp,fr,mdatoms);


  update_forcerec(fp,fr,box);
# 705 "force.c"
  if (fr->bcoultab || fr->bvdwtab) {
    if (EEL_LR(fr->eeltype)) {
      bool bcoulsave,bvdwsave;




      bcoulsave=fr->bcoultab;
      bvdwsave=fr->bvdwtab;
      fr->bcoultab=FALSE;
      fr->bvdwtab=FALSE;
      fr->rtab=1.0;
      make_tables(fp,fr,MASTER(cr),tabfn);
      fr->bcoultab=bcoulsave;
      fr->bvdwtab=bvdwsave;
      fr->coulvdw14tab=fr->coulvdwtab;
      fr->coulvdwtab=((void *)0);
    }
    fr->rtab = max(fr->rlistlong+1.2,1.0);
  }
  else if (fr->efep != efepNO) {
    if (fr->rlistlong == 0) {
      char *ptr,*envvar="FEP_TABLE_LENGTH";
      fr->rtab = 5;
      ptr = getenv(envvar);
      if (ptr) {
        sscanf(ptr,"%lf",&rtab);
        fr->rtab = rtab;
      }
      if (fp)
        fprintf(fp,"\nNote: Setting the free energy table length to %g nm\n"
                "      You can set this value with the environment variable %s"
                "\n\n",fr->rtab,envvar);
    }
    else
      fr->rtab = max(fr->rlistlong+1.2,1.0);
  }
  else
    fr->rtab = 1.0;


  make_tables(fp,fr,MASTER(cr),tabfn);
  if(!(EEL_LR(fr->eeltype) && (fr->bcoultab || fr->bvdwtab)))
    fr->coulvdw14tab=fr->coulvdwtab;




  tabelemsize=fr->bBHAM ? 16 : 12;
  snew(fr->coultab,4*(fr->ntab+1));
  snew(fr->vdwtab,(tabelemsize-4)*(fr->ntab+1));
  for(i=0; i<=fr->ntab; i++) {
    for(j=0; j<4; j++)
      fr->coultab[4*i+j]=fr->coulvdwtab[tabelemsize*i+j];
    for(j=0; j<tabelemsize-4; j++)
      fr->vdwtab[(tabelemsize-4)*i+j]=fr->coulvdwtab[tabelemsize*i+4+j];
  }
  if (!fr->mno_index)
    check_solvent(fp,top,fr,mdatoms,nsb);


  if(ir->bQMMM)
    fprintf(stderr,"QMMM requested..... this is still under development\n");
  else
    fprintf(stderr,"no QMMM requested\n");
  fr->bQMMM = ir->bQMMM;
  fr->qr = mk_QMMMrec();

  if(fr->bQMMM)
    init_QMMMrec(cr,mdatoms,box,top,ir,fr->qr);

}





void pr_forcerec(FILE *fp,t_forcerec *fr,t_commrec *cr)
{
  fprintf(fp,"%s: %e\n","fr->rlist",fr->rlist);
  fprintf(fp,"%s: %e\n","fr->rcoulomb",fr->rcoulomb);
  fprintf(fp,"%s: %e\n","fr->fudgeQQ",fr->fudgeQQ);
  fprintf((fp),"%s: %d\n","fr->ndelta",fr->ndelta);
  fprintf((fp),"%s: %s\n","fr->bGrid",bool_names[fr->bGrid]);
  fprintf((fp),"%s: %s\n","fr->bTwinRange",bool_names[fr->bTwinRange]);


  fprintf((fp),"%s: %d\n","fr->ntab",fr->ntab);
  if (fr->ntab > 0) {
    fprintf(fp,"%s: %e\n","fr->rcoulomb_switch",fr->rcoulomb_switch);
    fprintf(fp,"%s: %e\n","fr->rcoulomb",fr->rcoulomb);
  }

  fprintf((fp),"%s: %d\n","fr->nmol",fr->nmol);
  fprintf((fp),"%s: %d\n","fr->nstcalc",fr->nstcalc);

  fflush(fp);
}

void ns(FILE *fp,
        t_forcerec *fr,
        rvec x[],
        rvec f[],
        matrix box,
        t_groups *grps,
        t_grpopts *opts,
        t_topology *top,
        t_mdatoms *md,
        t_commrec *cr,
        t_nrnb *nrnb,
        t_nsborder *nsb,
        int step,
        real lambda,
        real *dvdlambda)
{
  static bool bFirst=TRUE;
  static int nDNL;
  char *ptr;
  int nsearch;

  if (bFirst) {
    ptr=getenv("DUMPNL");
    if (ptr) {
      nDNL=atoi(ptr);
      fprintf(fp,"nDNL = %d\n",nDNL);
    } else
      nDNL=0;

    init_neighbor_list(fp,fr,HOMENR(nsb));

    bFirst=FALSE;
  }

  if (fr->bTwinRange)
    fr->nlr=0;





  if (cr->nodeid == 0)
    fr->cg0=0;
  else
    fr->cg0=nsb->workload[cr->nodeid-1];
  fr->hcg=nsb->workload[cr->nodeid];

  nsearch = search_neighbours(fp,fr,x,box,top,grps,cr,nsb,nrnb,md,
                              lambda,dvdlambda);
  if (debug)
    fprintf(debug,"nsearch = %d\n",nsearch);






  if (nDNL > 0)
    dump_nblist(fp,fr,nDNL);
}

void force(FILE *fp, int step,
           t_forcerec *fr, t_inputrec *ir,
           t_idef *idef, t_nsborder *nsb,
           t_commrec *cr, t_commrec *mcr,
           t_nrnb *nrnb,
           t_groups *grps, t_mdatoms *md,
           int ngener, t_grpopts *opts,
           rvec x[], rvec f[],
           real epot[], t_fcdata *fcd,
           bool bVerbose, matrix box,
           real lambda, t_graph *graph,
           t_block *excl, bool bNBFonly,
           matrix lr_vir, rvec mu_tot,
           real qsum, bool bGatherOnly)
{
  int i,nit;
  bool bDoEpot;
  rvec box_size;
  real Vlr,Vcorr=0;


  for(i=0; (i<DIM); i++)
    box_size[i]=box[i][i];

  bDoEpot=((fr->nmol > 0) && (fr->nstcalc > 0) && (mod(step,fr->nstcalc)==0));

  if (bDoEpot)
    for(i=0; (i<fr->nmol); i++)
      fr->mol_epot[i]=0.0;
  debug_gmx();




  if(fr->bQMMM)
    epot[F_EQM]=calculate_QMMM(cr,f,fr,md);



  do_fnbf(fp,cr,fr,x,f,md,
          fr->bBHAM ? grps->estat.ee[egBHAM] : grps->estat.ee[egLJ],
          grps->estat.ee[egCOUL],box_size,nrnb,
          lambda,&epot[F_DVDL],FALSE,-1);
  debug_gmx();

  if (debug)
    pr_rvecs(debug,0,"fshift after SR",fr->fshift,SHIFTS);





  if (debug && 0)
    p_graph(debug,"DeBUGGGG",graph);


  if (!bNBFonly) {
    shift_self(graph,box,x);
    if (debug && 0) {
      fprintf(debug,"BBBBBBBBBBBBBBBB\n");
      fprintf(debug,"%5d\n",graph->nnodes);
      for(i=graph->start; (i<=graph->end); i++)
        fprintf(debug,"%5d%5s%5s%5d%8.3f%8.3f%8.3f\n",
                i,"A","B",i,x[i][XX],x[i][YY],x[i][ZZ]);
      fprintf(debug,"%10.5f%10.5f%10.5f\n",
              box[XX][XX],box[YY][YY],box[ZZ][ZZ]);
    }
    if (TRICLINIC(box))
        inc_nrnb(nrnb,eNR_SHIFTX,2*graph->nnodes);
    else
        inc_nrnb(nrnb,eNR_SHIFTX,graph->nnodes);
    debug_gmx();
  }

  if (EEL_LR(fr->eeltype)) {
    switch (fr->eeltype) {
    case eelPPPM:
      Vlr = do_pppm(fp,FALSE,x,fr->f_pme,md->chargeT,
                    box_size,fr->phi,cr,nsb,nrnb);
      break;
    case eelPOISSON:
      Vlr = do_poisson(fp,FALSE,ir,md->nr,x,fr->f_pme,md->chargeT,
                       box_size,fr->phi,cr,nrnb,&nit,TRUE);
      break;
    case eelPME:
      Vlr = do_pme(fp,FALSE,ir,x,fr->f_pme,md->chargeT,
                   box,cr,nsb,nrnb,lr_vir,fr->ewaldcoeff,bGatherOnly);
      break;
    case eelEWALD:
      Vlr = do_ewald(fp,FALSE,ir,x,fr->f_pme,md->chargeT,
                     box_size,cr,nsb,lr_vir,fr->ewaldcoeff);
      break;
    default:
      Vlr = 0;
      fatal_error(0,"No such electrostatics method implemented %s",
                  eel_names[fr->eeltype]);
    }
    if(fr->bEwald)
      Vcorr =
        ewald_LRcorrection(fp,nsb,cr,fr,md->chargeT,excl,x,box,mu_tot,qsum,
                           ir->ewald_geometry,ir->epsilon_surface,lr_vir);
    else
      Vcorr = shift_LRcorrection(fp,nsb,cr,fr,md->chargeT,excl,x,TRUE,box,lr_vir);
    epot[F_LR] = Vlr + Vcorr;
    if (debug)
      fprintf(debug,"Vlr = %g, Vcorr = %g, Vlr_corr = %g\n",
              Vlr,Vcorr,epot[F_LR]);
    if (debug) {
      pr_rvecs(debug,0,"lr_vir after corr",lr_vir,DIM);
      pr_rvecs(debug,0,"fshift after LR Corrections",fr->fshift,SHIFTS);
    }
  }
  debug_gmx();

  if (debug)
    print_nrnb(debug,nrnb);
  debug_gmx();




  if (!bNBFonly) {
    calc_bonds(fp,cr,mcr,
               idef,x,f,fr,graph,epot,nrnb,box,lambda,md,
               opts->ngener,grps->estat.ee[egLJ14],grps->estat.ee[egCOUL14],
               fcd,step,fr->bSepDVDL && do_per_step(step,ir->nstlog));
    debug_gmx();
  }
  if (debug)
    pr_rvecs(debug,0,"fshift after bondeds",fr->fshift,SHIFTS);

  for(i=0; (i<F_EPOT); i++)
    if (i != F_DISRES)
      epot[F_EPOT]+=epot[i];
}

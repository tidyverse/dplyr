# BMSC

Version: 0.1.1

## Newly broken

*   checking whether package ‘BMSC’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/RtmpQIz1bP/file1f4f49aaf621/BMSC.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BMSC’ ...
** package ‘BMSC’ successfully unpacked and MD5 sums checked
** libs
g++ -std=gnu++14 -I"/usr/share/R/include" -DNDEBUG -I"../inst/include" -I"`"/usr/lib/R/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include" -I"/tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/BH/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include" -I"/tmp/RtmpQIz1bP/checklib1f4f7077586a/RcppEigen/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c init.cpp -o init.o
"/usr/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linRegUnConstr.stan
Compiling with: 
STANHEADERS_SRC = `"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`
PKG_CPPFLAGS = -I"../inst/include" -I"$(STANHEADERS_SRC)" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error

CXX_STD = CXX14
SOURCES = stan_files/linReg.stan stan_files/linRegHorseHoe.stan stan_files/linRegHorseHoeUnConstr.stan stan_files/linRegUnConstr.stan
OBJECTS = $(SOURCES:.stan=.o) init.o

all: $(SHLIB)
		@if test -e "/usr/bin/install_name_tool" && test -e "/usr/local/clang4/lib/libc++.1.dylib" && test -e "/usr/lib/libc++.1.dylib"; then /usr/bin/install_name_tool -change /usr/local/clang4/lib/libc++.1.dylib /usr/lib/libc++.1.dylib $(SHLIB); fi

clean:
		rm -rf stan_files/*.o
		rm -rf *.so *.o
		rm -rf stan_files/*.cc
		rm -rf stan_files/*.hpp

%.cc: %.stan
		"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" $<

.phony: all clean
Wrote C++ file "stan_files/linRegUnConstr.cc"
"/usr/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linRegHorseHoe.stan
Compiling with: 
STANHEADERS_SRC = `"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`
PKG_CPPFLAGS = -I"../inst/include" -I"$(STANHEADERS_SRC)" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error

CXX_STD = CXX14
SOURCES = stan_files/linReg.stan stan_files/linRegHorseHoe.stan stan_files/linRegHorseHoeUnConstr.stan stan_files/linRegUnConstr.stan
OBJECTS = $(SOURCES:.stan=.o) init.o

all: $(SHLIB)
		@if test -e "/usr/bin/install_name_tool" && test -e "/usr/local/clang4/lib/libc++.1.dylib" && test -e "/usr/lib/libc++.1.dylib"; then /usr/bin/install_name_tool -change /usr/local/clang4/lib/libc++.1.dylib /usr/lib/libc++.1.dylib $(SHLIB); fi

clean:
		rm -rf stan_files/*.o
		rm -rf *.so *.o
		rm -rf stan_files/*.cc
		rm -rf stan_files/*.hpp

%.cc: %.stan
		"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" $<

.phony: all clean
Wrote C++ file "stan_files/linRegHorseHoe.cc"
"/usr/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linReg.stan
Compiling with: 
STANHEADERS_SRC = `"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`
PKG_CPPFLAGS = -I"../inst/include" -I"$(STANHEADERS_SRC)" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error

CXX_STD = CXX14
SOURCES = stan_files/linReg.stan stan_files/linRegHorseHoe.stan stan_files/linRegHorseHoeUnConstr.stan stan_files/linRegUnConstr.stan
OBJECTS = $(SOURCES:.stan=.o) init.o

all: $(SHLIB)
		@if test -e "/usr/bin/install_name_tool" && test -e "/usr/local/clang4/lib/libc++.1.dylib" && test -e "/usr/lib/libc++.1.dylib"; then /usr/bin/install_name_tool -change /usr/local/clang4/lib/libc++.1.dylib /usr/lib/libc++.1.dylib $(SHLIB); fi

clean:
		rm -rf stan_files/*.o
		rm -rf *.so *.o
		rm -rf stan_files/*.cc
		rm -rf stan_files/*.hpp

%.cc: %.stan
		"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" $<

.phony: all clean
Wrote C++ file "stan_files/linReg.cc"
"/usr/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linRegHorseHoeUnConstr.stan
Compiling with: 
STANHEADERS_SRC = `"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`
PKG_CPPFLAGS = -I"../inst/include" -I"$(STANHEADERS_SRC)" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error

CXX_STD = CXX14
SOURCES = stan_files/linReg.stan stan_files/linRegHorseHoe.stan stan_files/linRegHorseHoeUnConstr.stan stan_files/linRegUnConstr.stan
OBJECTS = $(SOURCES:.stan=.o) init.o

all: $(SHLIB)
		@if test -e "/usr/bin/install_name_tool" && test -e "/usr/local/clang4/lib/libc++.1.dylib" && test -e "/usr/lib/libc++.1.dylib"; then /usr/bin/install_name_tool -change /usr/local/clang4/lib/libc++.1.dylib /usr/lib/libc++.1.dylib $(SHLIB); fi

clean:
		rm -rf stan_files/*.o
		rm -rf *.so *.o
		rm -rf stan_files/*.cc
		rm -rf stan_files/*.hpp

%.cc: %.stan
		"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" $<

.phony: all clean
Wrote C++ file "stan_files/linRegHorseHoeUnConstr.cc"
Error in readRDS("/tmp/RtmpQIz1bP/file1f4f64f67c67") : 
  error reading from connection
Calls: .Last -> readRDS
3: traceback(1)
2: readRDS("/tmp/RtmpQIz1bP/file1f4f64f67c67")
1: .Last()
Makevars:18: recipe for target 'stan_files/linRegHorseHoeUnConstr.cc' failed
make: *** [stan_files/linRegHorseHoeUnConstr.cc] Error 1
make: *** Waiting for unfinished jobs....
g++ -std=gnu++14 -I"/usr/share/R/include" -DNDEBUG -I"../inst/include" -I"`"/usr/lib/R/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include" -I"/tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/BH/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include" -I"/tmp/RtmpQIz1bP/checklib1f4f7077586a/RcppEigen/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c stan_files/linRegUnConstr.cc -o stan_files/linRegUnConstr.o
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:28:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegUnConstr.hpp:18,
                 from stan_files/linRegUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/detail/shared_count.hpp:355:33: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     explicit shared_count( std::auto_ptr<Y> & r ): pi_( new sp_counted_impl_p<Y>( r.get() ) )
                                 ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegUnConstr.hpp:18,
                 from stan_files/linRegUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:256:65: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
 template< class T, class R > struct sp_enable_if_auto_ptr< std::auto_ptr< T >, R >
                                                                 ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegUnConstr.hpp:18,
                 from stan_files/linRegUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:471:31: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     explicit shared_ptr( std::auto_ptr<Y> & r ): px(r.get()), pn()
                               ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegUnConstr.hpp:18,
                 from stan_files/linRegUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:484:22: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr( std::auto_ptr<Y> && r ): px(r.get()), pn()
                      ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegUnConstr.hpp:18,
                 from stan_files/linRegUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:567:34: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr & operator=( std::auto_ptr<Y> & r )
                                  ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegUnConstr.hpp:18,
                 from stan_files/linRegUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:576:34: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr & operator=( std::auto_ptr<Y> && r )
                                  ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegUnConstr.hpp:18,
                 from stan_files/linRegUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp: In member function ‘boost::shared_ptr<T>& boost::shared_ptr<T>::operator=(std::auto_ptr<_Up>&&)’:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:578:38: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
         this_type( static_cast< std::auto_ptr<Y> && >( r ) ).swap( *this );
                                      ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/invocation/detail/that_ptr.hpp:13:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/invocation/invoke.hpp:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/adapter/fused.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/generation/make_fused.hpp:13,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/include/make_fused.hpp:11,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/resize.hpp:28,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/state_wrapper.hpp:26,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/ublas_wrapper.hpp:33,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint.hpp:25,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/stan/math/prim/arr/functor/integrate_ode_rk45.hpp:17,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/stan/math/prim/arr.hpp:44,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/stan/math/prim/mat.hpp:325,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/stan/math/rev/mat.hpp:12,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/src/stan/model/test_gradients.hpp:7,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:34,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegUnConstr.hpp:18,
                 from stan_files/linRegUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/get_pointer.hpp: At global scope:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/get_pointer.hpp:48:40: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
 template<class T> T * get_pointer(std::auto_ptr<T> const& p)
                                        ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
g++ -std=gnu++14 -I"/usr/share/R/include" -DNDEBUG -I"../inst/include" -I"`"/usr/lib/R/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include" -I"/tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/BH/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include" -I"/tmp/RtmpQIz1bP/checklib1f4f7077586a/RcppEigen/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c stan_files/linReg.cc -o stan_files/linReg.o
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:28:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linReg.hpp:18,
                 from stan_files/linReg.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/detail/shared_count.hpp:355:33: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     explicit shared_count( std::auto_ptr<Y> & r ): pi_( new sp_counted_impl_p<Y>( r.get() ) )
                                 ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linReg.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linReg.hpp:18,
                 from stan_files/linReg.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:256:65: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
 template< class T, class R > struct sp_enable_if_auto_ptr< std::auto_ptr< T >, R >
                                                                 ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linReg.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linReg.hpp:18,
                 from stan_files/linReg.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:471:31: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     explicit shared_ptr( std::auto_ptr<Y> & r ): px(r.get()), pn()
                               ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linReg.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linReg.hpp:18,
                 from stan_files/linReg.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:484:22: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr( std::auto_ptr<Y> && r ): px(r.get()), pn()
                      ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linReg.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linReg.hpp:18,
                 from stan_files/linReg.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:567:34: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr & operator=( std::auto_ptr<Y> & r )
                                  ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linReg.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linReg.hpp:18,
                 from stan_files/linReg.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:576:34: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr & operator=( std::auto_ptr<Y> && r )
                                  ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linReg.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linReg.hpp:18,
                 from stan_files/linReg.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp: In member function ‘boost::shared_ptr<T>& boost::shared_ptr<T>::operator=(std::auto_ptr<_Up>&&)’:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:578:38: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
         this_type( static_cast< std::auto_ptr<Y> && >( r ) ).swap( *this );
                                      ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linReg.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/invocation/detail/that_ptr.hpp:13:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/invocation/invoke.hpp:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/adapter/fused.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/generation/make_fused.hpp:13,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/include/make_fused.hpp:11,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/resize.hpp:28,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/state_wrapper.hpp:26,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/ublas_wrapper.hpp:33,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint.hpp:25,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/stan/math/prim/arr/functor/integrate_ode_rk45.hpp:17,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/stan/math/prim/arr.hpp:44,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/stan/math/prim/mat.hpp:325,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/stan/math/rev/mat.hpp:12,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/src/stan/model/test_gradients.hpp:7,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:34,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linReg.hpp:18,
                 from stan_files/linReg.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/get_pointer.hpp: At global scope:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/get_pointer.hpp:48:40: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
 template<class T> T * get_pointer(std::auto_ptr<T> const& p)
                                        ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linReg.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
g++ -std=gnu++14 -I"/usr/share/R/include" -DNDEBUG -I"../inst/include" -I"`"/usr/lib/R/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include" -I"/tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/BH/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include" -I"/tmp/RtmpQIz1bP/checklib1f4f7077586a/RcppEigen/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c stan_files/linRegHorseHoe.cc -o stan_files/linRegHorseHoe.o
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:28:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoe.hpp:18,
                 from stan_files/linRegHorseHoe.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/detail/shared_count.hpp:355:33: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     explicit shared_count( std::auto_ptr<Y> & r ): pi_( new sp_counted_impl_p<Y>( r.get() ) )
                                 ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoe.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoe.hpp:18,
                 from stan_files/linRegHorseHoe.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:256:65: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
 template< class T, class R > struct sp_enable_if_auto_ptr< std::auto_ptr< T >, R >
                                                                 ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoe.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoe.hpp:18,
                 from stan_files/linRegHorseHoe.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:471:31: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     explicit shared_ptr( std::auto_ptr<Y> & r ): px(r.get()), pn()
                               ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoe.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoe.hpp:18,
                 from stan_files/linRegHorseHoe.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:484:22: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr( std::auto_ptr<Y> && r ): px(r.get()), pn()
                      ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoe.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoe.hpp:18,
                 from stan_files/linRegHorseHoe.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:567:34: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr & operator=( std::auto_ptr<Y> & r )
                                  ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoe.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoe.hpp:18,
                 from stan_files/linRegHorseHoe.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:576:34: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr & operator=( std::auto_ptr<Y> && r )
                                  ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoe.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoe.hpp:18,
                 from stan_files/linRegHorseHoe.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp: In member function ‘boost::shared_ptr<T>& boost::shared_ptr<T>::operator=(std::auto_ptr<_Up>&&)’:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:578:38: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
         this_type( static_cast< std::auto_ptr<Y> && >( r ) ).swap( *this );
                                      ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoe.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/invocation/detail/that_ptr.hpp:13:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/invocation/invoke.hpp:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/adapter/fused.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/generation/make_fused.hpp:13,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/include/make_fused.hpp:11,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/resize.hpp:28,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/state_wrapper.hpp:26,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/ublas_wrapper.hpp:33,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint.hpp:25,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/stan/math/prim/arr/functor/integrate_ode_rk45.hpp:17,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/stan/math/prim/arr.hpp:44,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/stan/math/prim/mat.hpp:325,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/stan/math/rev/mat.hpp:12,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/src/stan/model/test_gradients.hpp:7,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/stan_fit.hpp:34,
                 from /tmp/RtmpQIz1bP/checklib1f4f7077586a/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoe.hpp:18,
                 from stan_files/linRegHorseHoe.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/get_pointer.hpp: At global scope:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/get_pointer.hpp:48:40: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
 template<class T> T * get_pointer(std::auto_ptr<T> const& p)
                                        ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoe.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
rm stan_files/linRegUnConstr.cc stan_files/linRegHorseHoe.cc stan_files/linRegHorseHoeUnConstr.cc stan_files/linReg.cc
ERROR: compilation failed for package ‘BMSC’
* removing ‘/tmp/RtmpQIz1bP/file1f4f49aaf621/BMSC.Rcheck/BMSC’

```
### CRAN

```
* installing *source* package ‘BMSC’ ...
** package ‘BMSC’ successfully unpacked and MD5 sums checked
** libs
g++ -std=gnu++14 -I"/usr/share/R/include" -DNDEBUG -I"../inst/include" -I"`"/usr/lib/R/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include" -I"/tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/BH/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include" -I"/tmp/RtmpHZE3OQ/checklib2ea054405e86/RcppEigen/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c init.cpp -o init.o
"/usr/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linRegHorseHoe.stan
Compiling with: 
STANHEADERS_SRC = `"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`
PKG_CPPFLAGS = -I"../inst/include" -I"$(STANHEADERS_SRC)" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error

CXX_STD = CXX14
SOURCES = stan_files/linReg.stan stan_files/linRegHorseHoe.stan stan_files/linRegHorseHoeUnConstr.stan stan_files/linRegUnConstr.stan
OBJECTS = $(SOURCES:.stan=.o) init.o

all: $(SHLIB)
		@if test -e "/usr/bin/install_name_tool" && test -e "/usr/local/clang4/lib/libc++.1.dylib" && test -e "/usr/lib/libc++.1.dylib"; then /usr/bin/install_name_tool -change /usr/local/clang4/lib/libc++.1.dylib /usr/lib/libc++.1.dylib $(SHLIB); fi

clean:
		rm -rf stan_files/*.o
		rm -rf *.so *.o
		rm -rf stan_files/*.cc
		rm -rf stan_files/*.hpp

%.cc: %.stan
		"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" $<

.phony: all clean
Wrote C++ file "stan_files/linRegHorseHoe.cc"
"/usr/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linReg.stan
Compiling with: 
STANHEADERS_SRC = `"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`
PKG_CPPFLAGS = -I"../inst/include" -I"$(STANHEADERS_SRC)" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error

CXX_STD = CXX14
SOURCES = stan_files/linReg.stan stan_files/linRegHorseHoe.stan stan_files/linRegHorseHoeUnConstr.stan stan_files/linRegUnConstr.stan
OBJECTS = $(SOURCES:.stan=.o) init.o

all: $(SHLIB)
		@if test -e "/usr/bin/install_name_tool" && test -e "/usr/local/clang4/lib/libc++.1.dylib" && test -e "/usr/lib/libc++.1.dylib"; then /usr/bin/install_name_tool -change /usr/local/clang4/lib/libc++.1.dylib /usr/lib/libc++.1.dylib $(SHLIB); fi

clean:
		rm -rf stan_files/*.o
		rm -rf *.so *.o
		rm -rf stan_files/*.cc
		rm -rf stan_files/*.hpp

%.cc: %.stan
		"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" $<

.phony: all clean
Wrote C++ file "stan_files/linReg.cc"
"/usr/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linRegHorseHoeUnConstr.stan
Compiling with: 
STANHEADERS_SRC = `"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`
PKG_CPPFLAGS = -I"../inst/include" -I"$(STANHEADERS_SRC)" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error

CXX_STD = CXX14
SOURCES = stan_files/linReg.stan stan_files/linRegHorseHoe.stan stan_files/linRegHorseHoeUnConstr.stan stan_files/linRegUnConstr.stan
OBJECTS = $(SOURCES:.stan=.o) init.o

all: $(SHLIB)
		@if test -e "/usr/bin/install_name_tool" && test -e "/usr/local/clang4/lib/libc++.1.dylib" && test -e "/usr/lib/libc++.1.dylib"; then /usr/bin/install_name_tool -change /usr/local/clang4/lib/libc++.1.dylib /usr/lib/libc++.1.dylib $(SHLIB); fi

clean:
		rm -rf stan_files/*.o
		rm -rf *.so *.o
		rm -rf stan_files/*.cc
		rm -rf stan_files/*.hpp

%.cc: %.stan
		"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" $<

.phony: all clean
Wrote C++ file "stan_files/linRegHorseHoeUnConstr.cc"
"/usr/lib/R/bin/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" stan_files/linRegUnConstr.stan
Compiling with: 
STANHEADERS_SRC = `"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`
PKG_CPPFLAGS = -I"../inst/include" -I"$(STANHEADERS_SRC)" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error

CXX_STD = CXX14
SOURCES = stan_files/linReg.stan stan_files/linRegHorseHoe.stan stan_files/linRegHorseHoeUnConstr.stan stan_files/linRegUnConstr.stan
OBJECTS = $(SOURCES:.stan=.o) init.o

all: $(SHLIB)
		@if test -e "/usr/bin/install_name_tool" && test -e "/usr/local/clang4/lib/libc++.1.dylib" && test -e "/usr/lib/libc++.1.dylib"; then /usr/bin/install_name_tool -change /usr/local/clang4/lib/libc++.1.dylib /usr/lib/libc++.1.dylib $(SHLIB); fi

clean:
		rm -rf stan_files/*.o
		rm -rf *.so *.o
		rm -rf stan_files/*.cc
		rm -rf stan_files/*.hpp

%.cc: %.stan
		"$(R_HOME)/bin$(R_ARCH_BIN)/Rscript" -e "source(file.path('..', 'tools', 'make_cc.R')); make_cc(commandArgs(TRUE))" $<

.phony: all clean
Wrote C++ file "stan_files/linRegUnConstr.cc"
g++ -std=gnu++14 -I"/usr/share/R/include" -DNDEBUG -I"../inst/include" -I"`"/usr/lib/R/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include" -I"/tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/BH/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include" -I"/tmp/RtmpHZE3OQ/checklib2ea054405e86/RcppEigen/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c stan_files/linReg.cc -o stan_files/linReg.o
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:28:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linReg.hpp:18,
                 from stan_files/linReg.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/detail/shared_count.hpp:355:33: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     explicit shared_count( std::auto_ptr<Y> & r ): pi_( new sp_counted_impl_p<Y>( r.get() ) )
                                 ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linReg.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linReg.hpp:18,
                 from stan_files/linReg.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:256:65: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
 template< class T, class R > struct sp_enable_if_auto_ptr< std::auto_ptr< T >, R >
                                                                 ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linReg.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linReg.hpp:18,
                 from stan_files/linReg.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:471:31: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     explicit shared_ptr( std::auto_ptr<Y> & r ): px(r.get()), pn()
                               ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linReg.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linReg.hpp:18,
                 from stan_files/linReg.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:484:22: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr( std::auto_ptr<Y> && r ): px(r.get()), pn()
                      ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linReg.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linReg.hpp:18,
                 from stan_files/linReg.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:567:34: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr & operator=( std::auto_ptr<Y> & r )
                                  ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linReg.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linReg.hpp:18,
                 from stan_files/linReg.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:576:34: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr & operator=( std::auto_ptr<Y> && r )
                                  ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linReg.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linReg.hpp:18,
                 from stan_files/linReg.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp: In member function ‘boost::shared_ptr<T>& boost::shared_ptr<T>::operator=(std::auto_ptr<_Up>&&)’:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:578:38: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
         this_type( static_cast< std::auto_ptr<Y> && >( r ) ).swap( *this );
                                      ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linReg.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/invocation/detail/that_ptr.hpp:13:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/invocation/invoke.hpp:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/adapter/fused.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/generation/make_fused.hpp:13,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/include/make_fused.hpp:11,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/resize.hpp:28,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/state_wrapper.hpp:26,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/ublas_wrapper.hpp:33,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint.hpp:25,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/stan/math/prim/arr/functor/integrate_ode_rk45.hpp:17,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/stan/math/prim/arr.hpp:44,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/stan/math/prim/mat.hpp:325,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/stan/math/rev/mat.hpp:12,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/src/stan/model/test_gradients.hpp:7,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:34,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linReg.hpp:18,
                 from stan_files/linReg.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/get_pointer.hpp: At global scope:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/get_pointer.hpp:48:40: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
 template<class T> T * get_pointer(std::auto_ptr<T> const& p)
                                        ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linReg.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
g++ -std=gnu++14 -I"/usr/share/R/include" -DNDEBUG -I"../inst/include" -I"`"/usr/lib/R/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include" -I"/tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/BH/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include" -I"/tmp/RtmpHZE3OQ/checklib2ea054405e86/RcppEigen/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c stan_files/linRegUnConstr.cc -o stan_files/linRegUnConstr.o
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:28:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegUnConstr.hpp:18,
                 from stan_files/linRegUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/detail/shared_count.hpp:355:33: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     explicit shared_count( std::auto_ptr<Y> & r ): pi_( new sp_counted_impl_p<Y>( r.get() ) )
                                 ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegUnConstr.hpp:18,
                 from stan_files/linRegUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:256:65: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
 template< class T, class R > struct sp_enable_if_auto_ptr< std::auto_ptr< T >, R >
                                                                 ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegUnConstr.hpp:18,
                 from stan_files/linRegUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:471:31: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     explicit shared_ptr( std::auto_ptr<Y> & r ): px(r.get()), pn()
                               ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegUnConstr.hpp:18,
                 from stan_files/linRegUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:484:22: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr( std::auto_ptr<Y> && r ): px(r.get()), pn()
                      ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegUnConstr.hpp:18,
                 from stan_files/linRegUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:567:34: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr & operator=( std::auto_ptr<Y> & r )
                                  ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegUnConstr.hpp:18,
                 from stan_files/linRegUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:576:34: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr & operator=( std::auto_ptr<Y> && r )
                                  ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegUnConstr.hpp:18,
                 from stan_files/linRegUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp: In member function ‘boost::shared_ptr<T>& boost::shared_ptr<T>::operator=(std::auto_ptr<_Up>&&)’:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:578:38: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
         this_type( static_cast< std::auto_ptr<Y> && >( r ) ).swap( *this );
                                      ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/invocation/detail/that_ptr.hpp:13:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/invocation/invoke.hpp:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/adapter/fused.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/generation/make_fused.hpp:13,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/include/make_fused.hpp:11,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/resize.hpp:28,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/state_wrapper.hpp:26,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/ublas_wrapper.hpp:33,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint.hpp:25,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/stan/math/prim/arr/functor/integrate_ode_rk45.hpp:17,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/stan/math/prim/arr.hpp:44,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/stan/math/prim/mat.hpp:325,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/stan/math/rev/mat.hpp:12,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/src/stan/model/test_gradients.hpp:7,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:34,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegUnConstr.hpp:18,
                 from stan_files/linRegUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/get_pointer.hpp: At global scope:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/get_pointer.hpp:48:40: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
 template<class T> T * get_pointer(std::auto_ptr<T> const& p)
                                        ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
g++ -std=gnu++14 -I"/usr/share/R/include" -DNDEBUG -I"../inst/include" -I"`"/usr/lib/R/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include" -I"/tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/BH/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include" -I"/tmp/RtmpHZE3OQ/checklib2ea054405e86/RcppEigen/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c stan_files/linRegHorseHoe.cc -o stan_files/linRegHorseHoe.o
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:28:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoe.hpp:18,
                 from stan_files/linRegHorseHoe.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/detail/shared_count.hpp:355:33: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     explicit shared_count( std::auto_ptr<Y> & r ): pi_( new sp_counted_impl_p<Y>( r.get() ) )
                                 ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoe.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoe.hpp:18,
                 from stan_files/linRegHorseHoe.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:256:65: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
 template< class T, class R > struct sp_enable_if_auto_ptr< std::auto_ptr< T >, R >
                                                                 ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoe.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoe.hpp:18,
                 from stan_files/linRegHorseHoe.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:471:31: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     explicit shared_ptr( std::auto_ptr<Y> & r ): px(r.get()), pn()
                               ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoe.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoe.hpp:18,
                 from stan_files/linRegHorseHoe.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:484:22: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr( std::auto_ptr<Y> && r ): px(r.get()), pn()
                      ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoe.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoe.hpp:18,
                 from stan_files/linRegHorseHoe.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:567:34: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr & operator=( std::auto_ptr<Y> & r )
                                  ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoe.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoe.hpp:18,
                 from stan_files/linRegHorseHoe.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:576:34: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr & operator=( std::auto_ptr<Y> && r )
                                  ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoe.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoe.hpp:18,
                 from stan_files/linRegHorseHoe.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp: In member function ‘boost::shared_ptr<T>& boost::shared_ptr<T>::operator=(std::auto_ptr<_Up>&&)’:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:578:38: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
         this_type( static_cast< std::auto_ptr<Y> && >( r ) ).swap( *this );
                                      ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoe.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/invocation/detail/that_ptr.hpp:13:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/invocation/invoke.hpp:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/adapter/fused.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/generation/make_fused.hpp:13,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/include/make_fused.hpp:11,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/resize.hpp:28,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/state_wrapper.hpp:26,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/ublas_wrapper.hpp:33,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint.hpp:25,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/stan/math/prim/arr/functor/integrate_ode_rk45.hpp:17,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/stan/math/prim/arr.hpp:44,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/stan/math/prim/mat.hpp:325,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/stan/math/rev/mat.hpp:12,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/src/stan/model/test_gradients.hpp:7,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:34,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoe.hpp:18,
                 from stan_files/linRegHorseHoe.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/get_pointer.hpp: At global scope:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/get_pointer.hpp:48:40: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
 template<class T> T * get_pointer(std::auto_ptr<T> const& p)
                                        ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoe.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
g++ -std=gnu++14 -I"/usr/share/R/include" -DNDEBUG -I"../inst/include" -I"`"/usr/lib/R/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include" -I"/tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/BH/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include" -I"/tmp/RtmpHZE3OQ/checklib2ea054405e86/RcppEigen/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c stan_files/linRegHorseHoeUnConstr.cc -o stan_files/linRegHorseHoeUnConstr.o
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:28:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoeUnConstr.hpp:18,
                 from stan_files/linRegHorseHoeUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/detail/shared_count.hpp:355:33: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     explicit shared_count( std::auto_ptr<Y> & r ): pi_( new sp_counted_impl_p<Y>( r.get() ) )
                                 ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoeUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoeUnConstr.hpp:18,
                 from stan_files/linRegHorseHoeUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:256:65: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
 template< class T, class R > struct sp_enable_if_auto_ptr< std::auto_ptr< T >, R >
                                                                 ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoeUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoeUnConstr.hpp:18,
                 from stan_files/linRegHorseHoeUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:471:31: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     explicit shared_ptr( std::auto_ptr<Y> & r ): px(r.get()), pn()
                               ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoeUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoeUnConstr.hpp:18,
                 from stan_files/linRegHorseHoeUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:484:22: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr( std::auto_ptr<Y> && r ): px(r.get()), pn()
                      ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoeUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoeUnConstr.hpp:18,
                 from stan_files/linRegHorseHoeUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:567:34: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr & operator=( std::auto_ptr<Y> & r )
                                  ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoeUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoeUnConstr.hpp:18,
                 from stan_files/linRegHorseHoeUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:576:34: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
     shared_ptr & operator=( std::auto_ptr<Y> && r )
                                  ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoeUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoeUnConstr.hpp:18,
                 from stan_files/linRegHorseHoeUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp: In member function ‘boost::shared_ptr<T>& boost::shared_ptr<T>::operator=(std::auto_ptr<_Up>&&)’:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:578:38: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
         this_type( static_cast< std::auto_ptr<Y> && >( r ) ).swap( *this );
                                      ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoeUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/invocation/detail/that_ptr.hpp:13:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/invocation/invoke.hpp:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/adapter/fused.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/functional/generation/make_fused.hpp:13,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/fusion/include/make_fused.hpp:11,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/resize.hpp:28,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/state_wrapper.hpp:26,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint/util/ublas_wrapper.hpp:33,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/numeric/odeint.hpp:25,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/stan/math/prim/arr/functor/integrate_ode_rk45.hpp:17,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/stan/math/prim/arr.hpp:44,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/stan/math/prim/mat.hpp:325,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/stan/math/rev/mat.hpp:12,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/src/stan/model/test_gradients.hpp:7,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/stan_fit.hpp:34,
                 from /tmp/RtmpHZE3OQ/checklib2ea054405e86/rstan/include/rstan/rstaninc.hpp:3,
                 from stan_files/linRegHorseHoeUnConstr.hpp:18,
                 from stan_files/linRegHorseHoeUnConstr.cc:3:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/get_pointer.hpp: At global scope:
/home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/get_pointer.hpp:48:40: warning: ‘template<class> class std::auto_ptr’ is deprecated [-Wdeprecated-declarations]
 template<class T> T * get_pointer(std::auto_ptr<T> const& p)
                                        ^
In file included from /usr/include/c++/5/bits/locale_conv.h:41:0,
                 from /usr/include/c++/5/locale:43,
                 from /usr/include/c++/5/iomanip:43,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/RcppCommon.h:52,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include/Rcpp.h:27,
                 from stan_files/linRegHorseHoeUnConstr.cc:1:
/usr/include/c++/5/bits/unique_ptr.h:49:28: note: declared here
   template<typename> class auto_ptr;
                            ^
g++ -std=gnu++14 -shared -L/usr/lib/R/lib -Wl,-Bsymbolic-functions -Wl,-z,relro -o BMSC.so stan_files/linReg.o stan_files/linRegHorseHoe.o stan_files/linRegHorseHoeUnConstr.o stan_files/linRegUnConstr.o init.o -L/usr/lib/R/lib -lR
rm stan_files/linRegUnConstr.cc stan_files/linRegHorseHoe.cc stan_files/linRegHorseHoeUnConstr.cc stan_files/linReg.cc
installing to /tmp/RtmpHZE3OQ/file2ea062865566/BMSC.Rcheck/BMSC/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded
* DONE (BMSC)

```
# classyfireR

Version: 0.1.1

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      3: eval_bare(get_expr(quo), get_env(quo)) at /tmp/RtmpKGtzsH/R.INSTALL560218033eb5/testthat/R/expectation.R:90
      4: dplyr::is.tbl(retrieve_classification(inchi_sub$query_id)) at /tmp/Rtmpf6Jrqt/R.INSTALL301c2f4e2bdc/rlang/R/eval.R:99
      5: retrieve_classification(inchi_sub$query_id) at /tmp/Rtmpg9w4lZ/R.INSTALL350bf9b78b/dplyr/R/tbl.r:34
      6: jsonlite::fromJSON(text_content) at /tmp/RtmpQIz1bP/file1f4e4ac142fc/classyfireR.Rcheck/00_pkg_src/classyfireR/R/retrieve_classification.R:27
      7: fromJSON_string(txt = txt, simplifyVector = simplifyVector, simplifyDataFrame = simplifyDataFrame, 
             simplifyMatrix = simplifyMatrix, flatten = flatten, ...) at /tmp/Rtmp5AcGsl/R.INSTALL7b1114f82d78/jsonlite/R/fromJSON.R:99
      8: parseJSON(txt, bigint_as_char) at /tmp/Rtmp5AcGsl/R.INSTALL7b1114f82d78/jsonlite/R/fromJSON.R:115
      9: parse_string(txt, bigint_as_char) at /tmp/Rtmp5AcGsl/R.INSTALL7b1114f82d78/jsonlite/R/parseJSON.R:5
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 10 SKIPPED: 0 FAILED: 1
      1. Error: submit-classification (@test-submission.R#16) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# fingertipsR

Version: 0.1.9

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      16: function_list[[k]](value) at /tmp/RtmpYknE8m/R.INSTALL2fc5633bb177/magrittr/R/freduce.R:20
      17: mutate_at(., .vars = character_fields, as.character)
      18: mutate(.tbl, !!!funs) at /tmp/Rtmpg9w4lZ/R.INSTALL350bf9b78b/dplyr/R/colwise-mutate.R:116
      19: mutate.data.frame(.tbl, !!!funs) at /tmp/Rtmpg9w4lZ/R.INSTALL350bf9b78b/dplyr/R/manip.r:282
      20: as.data.frame(mutate(tbl_df(.data), ...)) at /tmp/Rtmpg9w4lZ/R.INSTALL350bf9b78b/dplyr/R/dataframe.R:92
      21: mutate(tbl_df(.data), ...) at /tmp/Rtmpg9w4lZ/R.INSTALL350bf9b78b/dplyr/R/dataframe.R:92
      22: mutate.tbl_df(tbl_df(.data), ...) at /tmp/Rtmpg9w4lZ/R.INSTALL350bf9b78b/dplyr/R/manip.r:282
      23: mutate_impl(.data, dots) at /tmp/Rtmpg9w4lZ/R.INSTALL350bf9b78b/dplyr/R/tbl-df.r:78
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 70 SKIPPED: 0 FAILED: 1
      1. Error: warning messages work (@test-extract.R#44) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# Nmisc

Version: 0.3.3

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      1/1 mismatches
      x[1]: NA
      y[1]: "CRAN"
      
      # A tibble: 1 x 7
        package_name requested_by is_base source source_path version is_installed
        <chr>        <chr>        <lgl>   <chr>  <chr>       <chr>   <lgl>       
      1 dplyr        description  FALSE   <NA>   <NA>        0.7.8   TRUE        
      Successfully created file '/tmp/RtmpE9GCtD/file36fd1318e912'. 
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 130 SKIPPED: 0 FAILED: 1
      1. Failure: add_package_info works (@test_project_packages.R#47) 
      
      Error: testthat unit tests failed
      Execution halted
    ```


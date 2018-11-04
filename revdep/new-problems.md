# banR

Version: 0.2.0

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library("testthat")
      > library("banR")
      > test_check("banR")
      ── 1. Error: Input and output DFs have a similar number of rows (@test_geocodetb
      The API sent back an error 503
      1: banR::geocode_tbl(tbl = table_test, adresse = adresses, code_insee = code_insee) at testthat/test_geocodetbl.R:31
      2: stop("The API sent back an error ", httr::status_code(query_results)) at /tmp/RtmpHZE3OQ/file2edb5ad42b3c/banR.Rcheck/00_pkg_src/banR/R/geocode_tbl.R:100
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 6 SKIPPED: 0 FAILED: 1
      1. Error: Input and output DFs have a similar number of rows (@test_geocodetbl.R#31) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# BMSC

Version: 0.1.1

## Newly broken

*   checking whether package ‘BMSC’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/RtmpHZE3OQ/file2ea6523397d4/BMSC.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘BMSC’ ...
** package ‘BMSC’ successfully unpacked and MD5 sums checked
** libs
g++ -std=gnu++14 -I"/usr/share/R/include" -DNDEBUG -I"../inst/include" -I"`"/usr/lib/R/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/tmp/RtmpHZE3OQ/checklib2ea660e7e940/StanHeaders/include" -I"/tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/BH/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include" -I"/tmp/RtmpHZE3OQ/checklib2ea660e7e940/RcppEigen/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c init.cpp -o init.o
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
Error in readRDS("/tmp/RtmpHZE3OQ/file2ea67d90dc6b") : 
  error reading from connection
Calls: .Last -> readRDS
3: traceback(1)
2: readRDS("/tmp/RtmpHZE3OQ/file2ea67d90dc6b")
1: .Last()
Makevars:18: recipe for target 'stan_files/linRegHorseHoe.cc' failed
make: *** [stan_files/linRegHorseHoe.cc] Error 1
make: *** Waiting for unfinished jobs....
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
Error in readRDS("/tmp/RtmpHZE3OQ/file2ea67d90dc6b") : 
  error reading from connection
Calls: .Last -> readRDS
3: traceback(1)
2: readRDS("/tmp/RtmpHZE3OQ/file2ea67d90dc6b")
1: .Last()
Makevars:18: recipe for target 'stan_files/linReg.cc' failed
make: *** [stan_files/linReg.cc] Error 1
g++ -std=gnu++14 -I"/usr/share/R/include" -DNDEBUG -I"../inst/include" -I"`"/usr/lib/R/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/tmp/RtmpHZE3OQ/checklib2ea660e7e940/StanHeaders/include" -I"/tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/BH/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include" -I"/tmp/RtmpHZE3OQ/checklib2ea660e7e940/RcppEigen/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c stan_files/linRegUnConstr.cc -o stan_files/linRegUnConstr.o
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:28:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/rstaninc.hpp:3,
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
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/rstaninc.hpp:3,
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
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/rstaninc.hpp:3,
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
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/rstaninc.hpp:3,
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
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/rstaninc.hpp:3,
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
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/rstaninc.hpp:3,
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
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/rstaninc.hpp:3,
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
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/StanHeaders/include/stan/math/prim/arr/functor/integrate_ode_rk45.hpp:17,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/StanHeaders/include/stan/math/prim/arr.hpp:44,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/StanHeaders/include/stan/math/prim/mat.hpp:325,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/StanHeaders/include/stan/math/rev/mat.hpp:12,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/StanHeaders/include/src/stan/model/test_gradients.hpp:7,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/stan_fit.hpp:34,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/rstaninc.hpp:3,
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
g++ -std=gnu++14 -I"/usr/share/R/include" -DNDEBUG -I"../inst/include" -I"`"/usr/lib/R/bin/Rscript" --vanilla -e "cat(system.file('include', 'src', package = 'StanHeaders'))"`" -DBOOST_RESULT_OF_USE_TR1 -DBOOST_NO_DECLTYPE -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -I"/tmp/RtmpHZE3OQ/checklib2ea660e7e940/StanHeaders/include" -I"/tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/BH/include" -I"/home/kirill/git/R/dplyr/revdep/libs/old/Rcpp/include" -I"/tmp/RtmpHZE3OQ/checklib2ea660e7e940/RcppEigen/include"    -fpic  -g -O2 -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g -c stan_files/linRegHorseHoeUnConstr.cc -o stan_files/linRegHorseHoeUnConstr.o
In file included from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/smart_ptr/shared_ptr.hpp:28:0,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/shared_ptr.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/time_clock.hpp:17,
                 from /home/kirill/git/R/dplyr/revdep/libs/old/BH/include/boost/date_time/posix_time/posix_time_types.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/rstaninc.hpp:3,
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
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/rstaninc.hpp:3,
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
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/rstaninc.hpp:3,
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
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/rstaninc.hpp:3,
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
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/rstaninc.hpp:3,
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
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/rstaninc.hpp:3,
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
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/stan_fit.hpp:13,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/rstaninc.hpp:3,
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
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/StanHeaders/include/stan/math/prim/arr/functor/integrate_ode_rk45.hpp:17,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/StanHeaders/include/stan/math/prim/arr.hpp:44,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/StanHeaders/include/stan/math/prim/mat.hpp:325,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/StanHeaders/include/stan/math/rev/mat.hpp:12,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/StanHeaders/include/src/stan/model/log_prob_grad.hpp:4,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/StanHeaders/include/src/stan/model/test_gradients.hpp:7,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/StanHeaders/include/src/stan/services/diagnose/diagnose.hpp:10,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/stan_fit.hpp:34,
                 from /tmp/RtmpHZE3OQ/checklib2ea660e7e940/rstan/include/rstan/rstaninc.hpp:3,
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
rm stan_files/linRegUnConstr.cc stan_files/linRegHorseHoe.cc stan_files/linRegHorseHoeUnConstr.cc stan_files/linReg.cc
ERROR: compilation failed for package ‘BMSC’
* removing ‘/tmp/RtmpHZE3OQ/file2ea6523397d4/BMSC.Rcheck/BMSC’

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
# codemetar

Version: 0.1.6

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      4: create_codemeta("codemetar", codemeta) at /tmp/Rtmpf6Jrqt/R.INSTALL301c2f4e2bdc/rlang/R/eval.R:99
      5: add_github_topics(cm) at /tmp/RtmpHZE3OQ/file2ed63f004a62/codemetar.Rcheck/00_pkg_src/codemetar/R/create_codemeta.R:85
      6: gh::gh("GET /repos/:owner/:repo/topics", repo = repo, owner = owner, .send_headers = c(Accept = "application/vnd.github.mercy-preview+json")) at /tmp/RtmpHZE3OQ/file2ed63f004a62/codemetar.Rcheck/00_pkg_src/codemetar/R/guess_metadata.R:251
      7: gh_process_response(raw) at /tmp/RtmprRFXge/R.INSTALLe863bad2ff9/gh/R/package.R:121
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 73 SKIPPED: 10 FAILED: 5
      1. Error: add_github_topics (@test-guess_metadata.R#103) 
      2. Error: we can write a codemeta document given a package name (@test-write_codemeta.R#4) 
      3. Error: We can read an existing codemeta.json file (@test-write_codemeta.R#29) 
      4. Error: We can use either a path or pkg name in writing (@test-write_codemeta.R#38) 
      5. Error: we can write codemeta given a codemeta object (@test-write_codemeta.R#60) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    Warning in engine$weave(file, quiet = quiet, encoding = enc) :
      Pandoc (>= 1.12.3) and/or pandoc-citeproc not available. Falling back to R Markdown v1.
    Quitting from lines 79-80 (codemeta-intro.Rmd) 
    Error: processing vignette 'codemeta-intro.Rmd' failed with diagnostics:
    GitHub API error (403): 403 Forbidden
      API rate limit exceeded for 13.71.27.76. (But here's the good news: Authenticated requests get a higher rate limit. Check out the documentation for more details.)
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
        package_name requested_by is_base source source_path version    is_installed
        <chr>        <chr>        <lgl>   <chr>  <chr>       <chr>      <lgl>       
      1 dplyr        description  FALSE   <NA>   <NA>        0.7.7.9000 TRUE        
      Successfully created file '/tmp/Rtmp6xlAQx/file768f10edf2c2'. 
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 130 SKIPPED: 0 FAILED: 1
      1. Failure: add_package_info works (@test_project_packages.R#47) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rdefra

Version: 0.3.5

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      6: httr::http_error(myURL) at /tmp/RtmpKGtzsH/R.INSTALL560218033eb5/testthat/R/expect-that.R:22
      7: http_error.character(myURL) at /tmp/RtmpkILZkN/R.INSTALL7c374a163b8b/httr/R/response-status.r:162
      8: http_error(HEAD(x, ...)) at /tmp/RtmpkILZkN/R.INSTALL7c374a163b8b/httr/R/response-status.r:166
      9: HEAD(x, ...) at /tmp/RtmpkILZkN/R.INSTALL7c374a163b8b/httr/R/response-status.r:166
      10: request_perform(req, hu$handle$handle) at /tmp/RtmpkILZkN/R.INSTALL7c374a163b8b/httr/R/http-head.r:28
      11: request_fetch(req$output, req$url, handle) at /tmp/RtmpkILZkN/R.INSTALL7c374a163b8b/httr/R/request.R:137
      12: request_fetch.write_memory(req$output, req$url, handle) at /tmp/RtmpkILZkN/R.INSTALL7c374a163b8b/httr/R/write-function.R:74
      13: curl::curl_fetch_memory(url, handle = handle) at /tmp/RtmpkILZkN/R.INSTALL7c374a163b8b/httr/R/write-function.R:76
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 20 SKIPPED: 1 FAILED: 1
      1. Error: Hourly data for station ABD/2014 should be available (@test-data.R#11) 
      
      Error: testthat unit tests failed
      Execution halted
    ```


#ifndef DPLYR_STOP_H
#define DPLYR_STOP_H

namespace Rcpp {

    template <typename T1>
    inline void stop(const char* fmt, const T1& arg1) {
        throw Rcpp::exception( tfm::format(fmt, arg1 ).c_str() );
    }
    
    template <typename T1, typename T2>
    inline void stop(const char* fmt, const T1& arg1, const T2& arg2) {
        throw Rcpp::exception( tfm::format(fmt, arg1, arg2 ).c_str() );
    }
    
    template <typename T1, typename T2, typename T3>
    inline void stop(const char* fmt, const T1& arg1, const T2& arg2, const T3& arg3) {
        throw Rcpp::exception( tfm::format(fmt, arg1, arg2, arg3).c_str() );
    }
    
    template <typename T1, typename T2, typename T3, typename T4>
    inline void stop(const char* fmt, const T1& arg1, const T2& arg2, const T3& arg3, const T4& arg4) {
        throw Rcpp::exception( tfm::format(fmt, arg1, arg2, arg3, arg4).c_str() );
    }
    
    template <typename T1, typename T2, typename T3, typename T4, typename T5>
    inline void stop(const char* fmt, const T1& arg1, const T2& arg2, const T3& arg3, const T4& arg4, const T5& arg5) {
        throw Rcpp::exception( tfm::format(fmt, arg1, arg2, arg3, arg4, arg5).c_str() );
    }
    
    template <typename T1, typename T2, typename T3, typename T4, typename T5, typename T6>
    inline void stop(const char* fmt, const T1& arg1, const T2& arg2, const T3& arg3, const T4& arg4, const T5& arg5, const T6& arg6) {
        throw Rcpp::exception( tfm::format(fmt, arg1, arg2, arg3, arg4, arg5, arg6).c_str() );
    }
    
    template <typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7>
    inline void stop(const char* fmt, const T1& arg1, const T2& arg2, const T3& arg3, const T4& arg4, const T5& arg5, const T6& arg6, const T7& arg7) {
        throw Rcpp::exception( tfm::format(fmt, arg1, arg2, arg3, arg4, arg5, arg6, arg7).c_str() );
    }
    
    template <typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename T8>
    inline void stop(const char* fmt, const T1& arg1, const T2& arg2, const T3& arg3, const T4& arg4, const T5& arg5, const T6& arg6, const T7& arg7, const T8& arg8) {
        throw Rcpp::exception( tfm::format(fmt, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8).c_str() );
    }
    
    template <typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename T8, typename T9>
    inline void stop(const char* fmt, const T1& arg1, const T2& arg2, const T3& arg3, const T4& arg4, const T5& arg5, const T6& arg6, const T7& arg7, const T8& arg8, const T9& arg9) {
        throw Rcpp::exception( tfm::format(fmt, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9).c_str() );
    }
    
    template <typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7, typename T8, typename T9, typename T10>
    inline void stop(const char* fmt, const T1& arg1, const T2& arg2, const T3& arg3, const T4& arg4, const T5& arg5, const T6& arg6, const T7& arg7, const T8& arg8, const T9& arg9, const T10& arg10) {
        throw Rcpp::exception( tfm::format(fmt, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10).c_str() );
    }
}

#endif

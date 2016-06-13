#ifndef dplyr_Result_ResultSet_H
#define dplyr_Result_ResultSet_H

namespace dplyr {

    class ResultSet {
    public:
        ResultSet( ) : results(), names(), n(0) {}

        void add_result( const std::string& name, Result* result ){
            results.push_back( result ) ;
            names.push_back( name ) ;
            n++ ;
        }

        Result* get(int k){ return results[k] ; }
        inline int size() const { return n ; }
        Rcpp::String name(int k) const { return names[k] ; }

    private:
        pointer_vector<Result> results ;
        std::vector<std::string> names ;
        int n ;
    } ;

}

#endif

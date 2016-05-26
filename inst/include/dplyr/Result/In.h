#ifndef dplyr_Result_In_H
#define dplyr_Result_In_H

namespace dplyr {

    template <int RTYPE>
    class In : public Mutater<LGLSXP,In<RTYPE> > {
    public:
        typedef typename Rcpp::Vector<RTYPE> Vec ;
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;

        In( Vec data_, Vec table_ ) :
            data(data_),
            table(table_),
            set(table.begin(), table.end())
        {}

        void process_slice( LogicalVector& out, const SlicingIndex& index, const SlicingIndex& out_index){
            int n = index.size() ;
            for( int i=0; i<n; i++){
                STORAGE value = data[index[i]] ;
                if(Vec::is_na(value)){
                    out[ out_index[i] ] = false ;
                } else {
                    out[ out_index[i] ] = set.count(value) ;
                }
            }
        }

    private:
        Vec data, table ;
        dplyr_hash_set<STORAGE> set ;

    } ;

}

#endif

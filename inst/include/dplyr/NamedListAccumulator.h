#ifndef dplyr_NamedListAccumulator_H
#define dplyr_NamedListAccumulator_H

namespace dplyr {

    struct Index {
        int index ;
        enum Origin { HASH, RMATCH, NEW } origin ;
        
        Index( int index_, Origin origin_ ) : index(index_), origin(origin_){}
    } ;
    
    template <typename Data>
    class NamedListAccumulator {
    public:
        
        NamedListAccumulator() : 
            data(), names(), r_match( "match" ), hash()
        {}
        
        inline void set(SEXP name, SEXP x){
            if( ! Rcpp::traits::same_type<Data, RowwiseDataFrame>::value )
                check_supported_type(x, name);
            
            Index index = get_index(name) ; 
            int idx = index.index ;
            switch( index.origin ){
                case Index::HASH :
                    data[idx] = x ;
                    break ;
                case Index::RMATCH :
                    data[idx] = x ;
                    hash.insert( std::make_pair( name, idx ) ) ;
                    break ;
                case Index::NEW :
                    data.push_back(x) ;
                    names.push_back(name) ;
                    hash.insert( std::make_pair( name, idx ) ) ;
                    break ; 
            } ;
        }
        
        inline void rm(SEXP name){   
            Index index = get_index(name) ;
            if( index.origin == Index::NEW ) return ;
            
            int idx = index.index ;
            // update data and names
            names.erase(names.begin() + idx) ;
            data.erase(data.begin() + idx ) ;
            
            // update the hash table as well
            for( dplyr_hash_map<SEXP,int>::iterator it=hash.begin(); it != hash.end(); ++it){
                int k = it->second ;
                // nothing to do in that case
                if( k < idx ) continue ;
                
                if( k == idx ){
                    // need to remove the data from the hash table
                    it = hash.erase(it) ;
                    continue ;
                } else {
                    // decrement the index
                    it->second-- ;    
                }
                
            }
        }
        
        inline operator List() const {
            List out = wrap(data) ;
            out.names() = names ;
            return out ;
        }
        
        inline size_t size() const {
            return data.size() ;    
        }
        
    private:
        
        std::vector<SEXP> data ;
        CharacterVector names ;
        Function r_match ;
        
        dplyr_hash_map<SEXP,int> hash ;
        
        Index get_index( SEXP name ){
            
            // first, look in the hash table for exact match
            dplyr_hash_map<SEXP,int>::iterator it = hash.find(name) ;
            if( it != hash.end() ){
                return Index( it->second, Index::HASH );                 
            }
            
            // otherwise, don't give up just yet, name might still be 
            // in there but with a different encoding
            // 
            // this is very expensive, but I don't know how else to do this stuff
            // since C/RAPI does not give us the proper tools
            CharacterVector y = CharacterVector::create(name) ;
            int idx = as<int>( r_match(y, names ) ) ;
            if( idx != NA_INTEGER ){
                // great, we have a match. Let's update the data
                return Index( idx-1, Index::RMATCH) ; 
            }
            
            // no match 
            return Index( data.size(), Index::NEW ) ;
            
        }
        
        
    } ;

}
#endif

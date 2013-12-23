#ifndef dplyr_Result_Lead_H
#define dplyr_Result_Lead_H

namespace dplyr {
    
    template <int RTYPE>
    class Lead : public Result {
    public:
        Lead( SEXP data_, int n_ ) : data(data_), n(n_){}
        
        virtual SEXP process(const GroupedDataFrame& gdf ){
            int nrows = gdf.nrows() ;
            int ng = gdf.ngroups() ; 
            
            Vector<RTYPE> out = no_init(nrows) ;
            GroupedDataFrame::group_iterator git = gdf.group_begin(); 
            for( int i=0; i<ng; i++, ++git){
                process_slice(out, *git, *git) ;
            }
            return out ;
        }
        
        virtual SEXP process(const FullDataFrame& df){
            int nrows = df.nrows() ;
            Vector<RTYPE> out = no_init(nrows) ;
            SlicingIndex index = df.get_index() ;
            process_slice( out, index, index );
            return out ;
        }
        
        virtual SEXP process(const SlicingIndex& index){
            int nrows = index.size() ;
            Vector<RTYPE> out = no_init(nrows) ;
            SlicingIndex fake(0, nrows) ;
            process_slice( out, index, fake );
            return out ;
        }
        
    private:
        
        void process_slice( Vector<RTYPE>& out, const SlicingIndex& index, const SlicingIndex& out_index){
            int chunk_size = index.size() ;
            int i=0 ;
            for( ; i<chunk_size-n; i++ ){
                out[out_index[i]] = data[index[i]+n] ;    
            }
            for(; i<chunk_size; i++){
                out[out_index[i]] = Vector<RTYPE>::get_na() ;    
            }
        }
        
        Vector<RTYPE> data ;
        int n ;
    } ;
    
    template <int RTYPE>
    class TypedLead : public Result {
    public:
        TypedLead(SEXP data_, int n_, CharacterVector classes_) : lead(data_, n_), classes(classes_){}
        
        virtual SEXP process(const GroupedDataFrame& gdf ){
            return promote( lead.process(gdf) ) ;
        }
        virtual SEXP process(const FullDataFrame& df){
            return promote( lead.process(df) ) ;    
        }
        virtual SEXP process(const SlicingIndex& index){
            return promote( lead.process(index) ) ;    
        }
        
    private:
        
        SEXP promote( Vector<RTYPE> res ){
            res.attr( "class" ) = classes ;
            return res ;
        }
        
        Lead<RTYPE> lead ;
        CharacterVector classes ;
    } ;
    
    
}

#endif

#ifndef dplyr_Order_H
#define dplyr_Order_H

namespace dplyr {
      
class OrderVisitors ;  
class OrderVisitors_Compare {
public:
    OrderVisitors_Compare( const OrderVisitors& obj_ ) ; 
    
    bool operator()(int i, int j) const ;
    
private:
    const OrderVisitors& obj ;
    int n ;
    
} ;
    
class OrderVisitors {
public:
    
    OrderVisitors( Rcpp::List args, Rcpp::LogicalVector ascending_, int n_ ) ;
    OrderVisitors( Rcpp::DataFrame data ) ;
    OrderVisitors( Rcpp::DataFrame data, Rcpp::CharacterVector names ) ;
    ~OrderVisitors() ;
    
    Rcpp::IntegerVector apply() const  ;
    
    std::vector<OrderVisitor*> visitors ;
    int n;
    int nrows ;    
} ;    
    
} // namespace dplyr


#endif

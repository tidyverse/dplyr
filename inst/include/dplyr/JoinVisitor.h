#ifndef dplyr_JoinVisitor_H
#define dplyr_JoinVisitor_H

namespace dplyr{

    class JoinVisitor{
    public:
        virtual ~JoinVisitor(){}

        virtual size_t hash(int i) = 0 ;
        virtual bool equal(int i, int j) = 0 ;

        virtual SEXP subset( const std::vector<int>& indices ) = 0;
        virtual SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ) = 0;

    } ;

}

#endif

order_ <- function(..., data){
    parent_frame <- parent.frame()
    if( missing(data) ) {
        env <- parent_frame
    } else {
        env <- as.environment(data)
        parent.env(env) <- parent_frame
    }
    order_impl( dots(...) , env )
}  

.data_dots <- function(fun, DOTS = dots){
    f <- function(data, ...){}
    body(f) <- substitute({
        parent_frame <- parent.frame()
        env <- as.environment(data)
        parent.env(env) <- parent_frame
        FUN( data, DOTS(...) , env )   
    } , list( FUN = substitute(fun), DOTS = substitute(DOTS) ) )
    f
}

arrange_ <- .data_dots(arrange_impl)
filter_  <- .data_dots(filter_impl)
filter_grouped_df <- .data_dots(filter_grouped_df_impl)

mutate_  <- .data_dots(mutate_impl, named_dots)

equal_ <- function(x, y){
    equal_data_frame(x, y) 
}

sort_ <- function( data ){
    sort_impl(data)    
}



.active_binding_fun <- function(index, subsets){
  function() {
    materialize_binding(index, subsets)
  }
}

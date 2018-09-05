
.active_binding_fun <- function(index, mask){
  function() {
    materialize_binding(index, mask)
  }
}

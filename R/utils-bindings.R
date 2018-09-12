
.active_binding_fun <- function(index, mask_proxy_xp){
  function() {
    materialize_binding(index, mask_proxy_xp)
  }
}

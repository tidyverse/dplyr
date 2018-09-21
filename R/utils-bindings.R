.make_active_binding_fun <- function(index, mask_proxy_xp_wrapped) {
  force(mask_proxy_xp_wrapped)
  function() {
    materialize_binding(index, mask_proxy_xp_wrapped)
  }
}

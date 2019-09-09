#include <Rcpp.h>
#include <tools/utils.h>

namespace dplyr {
namespace internal {

const rlang_api_ptrs_t& rlang_api() {
  static rlang_api_ptrs_t ptrs;
  return ptrs;
}

} // namespace internal
} // namespace dplyr

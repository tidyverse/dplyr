#include <dplyr.h>


namespace dplyr {
namespace internal {

rlang_api_ptrs_t rlang_api() {
  static rlang_api_ptrs_t ptrs;
  return ptrs;
}

} // namespace internal
} // namespace dplyr

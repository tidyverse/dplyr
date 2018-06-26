#ifndef dplyr_dplyr_HybridHandlerMap_H
#define dplyr_dplyr_HybridHandlerMap_H

#include <tools/hash.h>
#include <dplyr/HybridHandler.h>

typedef dplyr_hash_map<SEXP, dplyr::HybridHandler> HybridHandlerMap;

void install_in_handlers(HybridHandlerMap& handlers);
void install_debug_handlers(HybridHandlerMap& handlers);

#endif // dplyr_dplyr_HybridHandlerMap_H

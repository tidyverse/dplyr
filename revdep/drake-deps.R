source("revdep/drake-base.R")

drake::make(plan_deps) %>% vis_drake_graph()

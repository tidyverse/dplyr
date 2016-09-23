cmake_minimum_required(VERSION 3.6)
project(RSQLite)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")

include_directories(/home/muelleki/R/x86_64-pc-linux-gnu-library/current/BH/include)
include_directories(/home/muelleki/R/x86_64-pc-linux-gnu-library/current/Rcpp/include)
include_directories(/home/muelleki/git/R/r-svn/R-3-3-1/include)
include_directories(inst/include)

file(GLOB SOURCE_FILES src/*.cpp src/*.c)
add_library(dplyr ${SOURCE_FILES})

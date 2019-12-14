cmake_minimum_required(VERSION 3.12)
project(pmcsn C)

set(CMAKE_C_STANDARD 99)

add_executable(pmcsn sca2.c rngs.c rvgs.c list.c rvms.c )
cmake_minimum_required(VERSION 3.6 FATAL_ERROR)

project(MutableArrayNif CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include_directories($ENV{OTP_ROOT}/erts-7.1/include)

add_library(MutableArrayNif SHARED dllmain.cpp MutableArrayNif.cpp)

set_directory_properties(PROPERTIES VS_STARTUP_PROJECT MutableArrayNif)
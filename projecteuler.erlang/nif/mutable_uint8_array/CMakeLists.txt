cmake_minimum_required(VERSION 3.6 FATAL_ERROR)

project(MutableUInt8ArrayNif CXX)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include_directories($ENV{OTP_ROOT}/erts-10.0.1/include)

add_library(MutableUInt8ArrayNif SHARED dllmain.cpp MutableUInt8ArrayNif.cpp)

set_directory_properties(PROPERTIES VS_STARTUP_PROJECT MutableUInt8ArrayNif)
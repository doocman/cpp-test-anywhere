
cmake_minimum_required(VERSION 3.13...3.27)

project(CppTestAnywhere)

if(PROJECT_IS_TOP_LEVEL)
    set(CTA_DEFAULT_ENABLE_TESTS ON)
    set(CMAKE_CXX_STANDARD 23)
else()
    set(CTA_DEFAULT_ENABLE_TESTS OFF)
endif()
if(NOT CMAKE_CXX_STANDARD)
    set(CMAKE_CXX_STANDARD 23)
endif()

set(CTA_ENABLE_TESTS ${CTA_DEFAULT_ENABLE_TESTS} CACHE BOOL "Use to build the tests for CTA")

add_subdirectory(inc)

function(cta_add_test TARGET)
    add_test(NAME ${TARGET} COMMAND ${TARGET})
endfunction()

if(CTA_ENABLE_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()


set(LIB_NAME cpp-test-anywhere)
add_library(${LIB_NAME} INTERFACE)
add_library(cta::cta ALIAS ${LIB_NAME})

target_include_directories(${LIB_NAME} INTERFACE "")

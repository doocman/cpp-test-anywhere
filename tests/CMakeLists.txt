
add_executable(cta_tests tests.cpp)

cta_add_test(cta_tests)

target_link_libraries(cta_tests PRIVATE cta::cta cmpr::add_warnings)

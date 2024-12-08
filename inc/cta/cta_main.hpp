
#include <cstdlib>

#include <cta/cta.hpp>

int main() {
  auto test_result = cta::just_run_tests();
  if (test_result.total_tests == 0) {
    std::cout << "WARNING: No tests run\n";
  }
  if (failed(test_result)) {
    std::cerr << "Failed tests found\n";
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

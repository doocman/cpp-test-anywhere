
#include <cta/cta.hpp>

#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>

namespace cta_tests {
using namespace ::cta;
static std::vector<std::string> failed_test_out{};
CTA_BEGIN_TESTS_INTERNAL(failing_tests, 1)
CTA_TEST(nonequal, ctx) {
  ctx.expect_that(1, eq(2),
                  std::back_insert_iterator(failed_test_out.emplace_back()));
}
CTA_TEST(nonequal_str, ctx) {
  ctx.expect_that("Hi", str_eq("Not Hi."),
                  std::back_insert_iterator(failed_test_out.emplace_back()));
}
CTA_TEST(nonequal_str, ctx) {
  ctx.expect_that("Hi there", str_contains("Hi."),
                  std::back_insert_iterator(failed_test_out.emplace_back()));
}
CTA_END_TESTS()

CTA_BEGIN_TESTS(expects)
CTA_TEST(equality, ctx) { ctx.expect_that(1, eq(1)); }
CTA_TEST(str_equality, ctx) { ctx.expect_that("Hello", str_eq("Hello")); }
CTA_TEST(str_contains, ctx) { ctx.expect_that("Hello", str_contains("He")); }
CTA_TEST(run_failing_tests, ctx) {
  auto results = internal::run_tests<1>();
  ctx.expect_that(results.total_tests, eq(3));
  ctx.expect_that(results.failed, eq(results.total_tests));
  if (size(failed_test_out) == 3) {
    ctx.expect_that(failed_test_out[0], str_contains("1"));
    ctx.expect_that(failed_test_out[0], str_contains("2"));
    ctx.expect_that(failed_test_out[1], str_contains("Hi"));
    ctx.expect_that(failed_test_out[1], str_contains("Not Hi."));
    ctx.expect_that(failed_test_out[2], str_contains("Hi there"));
    ctx.expect_that(failed_test_out[2], str_contains("Hi."));
  } else {
    ctx.expect_that(size(failed_test_out), eq(3));
  }
}
CTA_END_TESTS()
} // namespace cta_tests

int main(int, char **) {
  auto *test_reg_first = cta::internal::tests_register_t<0>::first();
  if (test_reg_first == nullptr) {
    std::cout << "No tests registered\n";
    return EXIT_FAILURE;
  }
  auto test_result = cta::just_run_tests();
  if (test_result.total_tests == 0) {
    std::cout << "No tests run\n";
    return EXIT_FAILURE;
  }
  if (failed(test_result)) {
    std::cout << "Failed tests found\n";
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

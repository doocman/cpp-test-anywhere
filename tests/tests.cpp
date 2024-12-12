
#include <cta/cta.hpp>

#include <array>
#include <bitset>
#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>

namespace cta_tests {
using namespace ::cta;
static std::vector<std::string> failed_test_out{};
static auto failed_test_print_dest() {
  return std::back_insert_iterator(failed_test_out.emplace_back());
}
CTA_BEGIN_TESTS_INTERNAL(failing_tests, 1)
CTA_TEST(nonequal, ctx) { ctx.expect_that(1, eq(2), failed_test_print_dest()); }
CTA_TEST(nonequal_str, ctx) {
  ctx.expect_that("Hi", str_eq("Not Hi."), failed_test_print_dest());
}
CTA_TEST(nonequal_str, ctx) {
  ctx.expect_that("Hi there", str_contains("Hi."), failed_test_print_dest());
}
CTA_TEST(no_elements_are, ctx) {
  ctx.expect_that(std::array{1, 2, 3}, elements_are(1, 2, 4),
                  failed_test_print_dest());
}
CTA_TEST(no_elements_are_bitset, ctx) {
  ctx.expect_that(std::bitset<4>("0101"), std::bitset<4>("0100"),
                  failed_test_print_dest());
}
CTA_END_TESTS()

CTA_BEGIN_TESTS(expects)
CTA_TEST(equality, ctx) { ctx.expect_that(1, eq(1)); }
CTA_TEST(str_equality, ctx) { ctx.expect_that("Hello", str_eq("Hello")); }
CTA_TEST(str_contains, ctx) { ctx.expect_that("Hello", str_contains("He")); }
CTA_TEST(elements_are, ctx) {
  ctx.expect_that(std::array{1, 2, 3}, elements_are(1, 2, eq(3)));
}
CTA_TEST(run_failing_tests, ctx) {
  constexpr auto test_count = 5;
  auto results = internal::run_tests<1>();
  ctx.expect_that(results.total_tests, eq(test_count));
  ctx.expect_that(results.failed, eq(results.total_tests));
  if (size(failed_test_out) == test_count) {
    ctx.expect_that(failed_test_out[0], str_contains("1"));
    ctx.expect_that(failed_test_out[0], str_contains("2"));
    ctx.expect_that(failed_test_out[0], str_contains("equal"));
    ctx.expect_that(failed_test_out[1], str_contains("Hi"));
    ctx.expect_that(failed_test_out[1], str_contains("Not Hi."));
    ctx.expect_that(failed_test_out[1], str_contains("equal"));
    ctx.expect_that(failed_test_out[2], str_contains("Hi there"));
    ctx.expect_that(failed_test_out[2], str_contains("Hi."));
    ctx.expect_that(failed_test_out[2], str_contains("contain"));
    ctx.expect_that(failed_test_out[3], str_contains("'1', '2', '3'"));
    ctx.expect_that(failed_test_out[3], str_contains("4"));
    ctx.expect_that(failed_test_out[3], str_contains("elements are"));
    ctx.expect_that(failed_test_out[4], str_contains("0101"));
    ctx.expect_that(failed_test_out[4], str_contains("0100"));
  } else {
    ctx.expect_that(size(failed_test_out), eq(test_count));
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

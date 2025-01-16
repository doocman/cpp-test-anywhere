
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
CTA_TEST(nonequal) { expect_that(1, eq(2), failed_test_print_dest()); }
CTA_TEST(nonequal_str) {
  expect_that("Hi", str_eq("Not Hi."), failed_test_print_dest());
}
CTA_TEST(noncontains_str) {
  expect_that("Hi there", str_contains("Hi."), failed_test_print_dest());
}
CTA_TEST(no_elements_are) {
  expect_that(std::array{1, 2, 3}, elements_are(1, 2, 4),
              failed_test_print_dest());
}
CTA_TEST(no_elements_are_bitset) {
  expect_that(std::bitset<4>("0101"), std::bitset<4>("0100"),
              failed_test_print_dest());
}
CTA_END_TESTS()
struct failing_fixture {
  int v = 1;
};
CTA_BEGIN_TESTS_F_INTERNAL(failing_fixture, failing_fixture, 1)
CTA_TEST(v_is_2) { expect_that(v, eq(2), failed_test_print_dest()); }
CTA_END_TESTS_F()

CTA_BEGIN_TESTS(expects)
CTA_TEST(equality) { expect_that(1, eq(1)); }
CTA_TEST(str_equality) { expect_that("Hello", str_eq("Hello")); }
CTA_TEST(str_contains) { expect_that("Hello", str_contains("He")); }
CTA_TEST(elements_are) {
  expect_that(std::array{1, 2, 3}, elements_are(1, 2, eq(3)));
}
CTA_TEST(run_failing_tests) {
  constexpr auto test_count = 6;
  auto results = internal::run_tests<1>();
  expect_that(results.total_tests, eq(test_count));
  expect_that(results.failed, eq(results.total_tests));
  if (size(failed_test_out) == test_count) {
    expect_that(failed_test_out[0], str_contains("1"));
    expect_that(failed_test_out[0], str_contains("2"));
    expect_that(failed_test_out[0], str_contains("equal"));
    expect_that(failed_test_out[1], str_contains("Hi"));
    expect_that(failed_test_out[1], str_contains("Not Hi."));
    expect_that(failed_test_out[1], str_contains("equal"));
    expect_that(failed_test_out[2], str_contains("Hi there"));
    expect_that(failed_test_out[2], str_contains("Hi."));
    expect_that(failed_test_out[2], str_contains("contain"));
    expect_that(failed_test_out[3], str_contains("1, 2, 3"));
    expect_that(failed_test_out[3], str_contains("4"));
    expect_that(failed_test_out[3], str_contains("elements are"));
    expect_that(failed_test_out[4], str_contains("0101"));
    expect_that(failed_test_out[4], str_contains("0100"));
    expect_that(failed_test_out[5], str_contains("equal to 2"));
  } else {
    expect_that(size(failed_test_out), eq(test_count));
  }
}
CTA_END_TESTS()
struct passing_fixture {
  int v = 1;
};
CTA_BEGIN_TESTS_F(passing_fixture)
CTA_TEST(check_value) { expect_that(v, eq(1)); }
CTA_END_TESTS_F()
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

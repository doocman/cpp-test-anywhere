
#ifndef CTA_CTA_CTA_HPP
#define CTA_CTA_CTA_HPP

#include <algorithm>
#include <concepts>
#include <cstdlib>
#include <format>
#include <functional>
#include <iostream>
#include <optional>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>

#if __has_include(<source_location>)
#include <source_location>
namespace cta::etd {
using source_location = std::source_location;
}
#elif __has_include(<experimental/source_location>)
#include <experimental/source_location>
namespace cta::etd {
using source_location = std::experimental::source_location;
}
#else
#error "No source location"
#endif

// clang-format off
#define CTA_BEGIN_TESTS_INTERNAL(NAME, TAG)\
struct cta_test_case_##NAME {\
static constexpr char name[] = #NAME;\
static ::cta::internal::is_regged_t _reg;\
 };\
::cta::internal::is_regged_t cta_test_case_##NAME::_reg = ::cta::internal::register_tests<cta_test_case_##NAME, TAG>(cta_test_case_##NAME::name
/// @brief Starts a new test case, serving as a container for related tests.
/// This macro sets up the necessary infrastructure for a test suite.
/// @param NAME The name of the test case, used for identifying it in logs or reports.
/// @note Must be followed by one or more `CTA_TEST()` calls and concluded with `CTA_END_TESTS()`.
/// @warning Use this macro only once per test case; it cannot be nested.
#define CTA_BEGIN_TESTS(NAME) CTA_BEGIN_TESTS_INTERNAL(NAME, 0)


/// @brief Defines a new test within the current test case.
///
/// Use this macro after `CTA_BEGIN_TESTS()` or another `CTA_TEST()` and before `CTA_END_TESTS()`.
/// The test logic must be written inside `{}` immediately after this macro.
/// All expectations/assertions are performed using the provided context object.
/// @param NAME The name of the test, used for identification in logs or reports.
/// @param CONTEXT_ARG The name of the context object accessible within the test body.
///        This object provides shared state or utilities specific to the test.
/// @note See tests/tests.cpp for usage examples.
/// @warning If not followed by `{}`, a compile-time error will occur.
#define CTA_TEST(NAME, CONTEXT_ARG) ,::cta::internal::name_of_test(#NAME)<<[] (test_context&& CONTEXT_ARG)

/// @brief Concludes the current test case.
///
/// This macro finalizes the test infrastructure set up by `CTA_BEGIN_TESTS()`.
/// It must be used after all `CTA_TEST()` macros in the test case.
/// @note Omitting this macro will result in incomplete test case definition and compile-time errors.
#define CTA_END_TESTS() );
// clang-format on

namespace cta {
namespace ranges {
#if __cpp_lib_ranges_contains >= 202207L
using std::ranges::contains_subrange;
#else
struct _contains_subrange {
  template <std::forward_iterator I1, std::sentinel_for<I1> S1,
            std::forward_iterator I2,
            std::sentinel_for<I2> S2 class Pred = ranges::equal_to,
            class Proj1 = std::identity, class Proj2 = std::identity>
  constexpr bool operator()(I1 first1, S1 last1, I2 first2, S2 last2,
                            Pred pred = {}, Proj1 proj1 = {},
                            Proj2 proj2 = {}) const {
    return (first2 == last2) ||
           !ranges::search(first1, last1, first2, last2, pred, proj1, proj2)
                .empty();
  }
};
inline constexpr _contains_subrange contains_subrange{};
#endif
} // namespace ranges
// I have to guess here, cppreference does not state the exact feature-test
// macro for this.
#if __cpp_lib_format >= 202207L
template <typename T, typename Char>
concept formattable = std::formattable<T, Char>;
#else
template <typename T, typename Char>
concept formattable = requires(T &v, std::format_context ctx) {
  std::formatter<std::remove_cvref_t<T>>().format(v, ctx);
};
#endif

template <typename T, typename... Ts>
concept direct_invocable =
    requires(T &&t, Ts &&...ts) { t(std::forward<Ts>(ts)...); };
template <typename T, typename U>
concept weakly_inequality_comparable = requires(T const &t, U const &u) {
  { t != u } -> std::convertible_to<bool>;
};
template <typename T, typename U>
concept weakly_equality_comparable = requires(T const &t, U const &u) {
  { t == u } -> std::convertible_to<bool>;
};
struct test_result {
  int total_tests{};
  int failed{};
};

constexpr void print_reality_vs_expect(auto &&value, auto &&expectation,
                                       auto out) {
  if constexpr (formattable<decltype(value), char>) {
    out = std::format_to(out, " - Value was '{}'", value);
    if constexpr (formattable<decltype(expectation), char>) {
      out = std::format_to(out, ", expected to be '{}'", expectation);
    }
    *out = '\n';
    ++out;
  }
}

constexpr void print_failed_expect(auto &&value, auto &&expectation,
                                   etd::source_location const &sl, auto out) {
  out = std::format_to(out, "Expectation failed at {}:{}\n", sl.file_name(),
                       sl.line());
  print_reality_vs_expect(value, expectation, out);
}
constexpr void print_passed_expect(auto &&value, auto &&expectation,
                                   etd::source_location const &sl, auto out) {
  out = std::format_to(out, "Expectation passed at {}:{}\n", sl.file_name(),
                       sl.line());
  print_reality_vs_expect(value, expectation, out);
}

class test_context {
  test_result &r_;
  bool failed_{};
  bool print_failure_ = true;
  bool print_pass_ = false;

public:
  constexpr explicit test_context(test_result &r) : r_(r) {}
  template <typename Val, typename Expected, typename Out>
    requires(weakly_inequality_comparable<Expected, Val> ||
             std::predicate<Expected, Val>)
  constexpr void expect_that(
      Val &&v, Expected &&e, Out &&o,
      etd::source_location const &sl = etd::source_location::current()) {
    bool this_failed = false;
    if constexpr (weakly_inequality_comparable<Expected, Val>) {
      this_failed = (e != v);
    } else if constexpr (std::predicate<Expected, Val>) {
      this_failed =
          !std::invoke(std::forward<Expected>(e), std::forward<Val>(v));
    }
    if (this_failed) {
      if (print_failure_) {
        print_failed_expect(v, e, sl, o);
      }
      failed_ = true;
    } else if (print_pass_) {
      print_passed_expect(v, e, sl, o);
    }
  }
  template <typename Val, typename Expected>
    requires(weakly_inequality_comparable<Expected, Val> ||
             std::predicate<Expected, Val>)
  constexpr void expect_that(
      Val &&v, Expected &&e,
      etd::source_location const &sl = etd::source_location::current()) {
    expect_that(v, e, std::ostreambuf_iterator<char>(std::cout), sl);
  }

  ~test_context() {
    ++r_.total_tests;
    if (failed_) {
      ++r_.failed;
    }
  }
  constexpr void print_failures(bool print) noexcept { print_failure_ = print; }
  constexpr void print_passes(bool print) noexcept { print_pass_ = print; }
};
namespace internal {

// Tagged so that it can be used to test the framework itself.
template <int = 0> class tests_register_t {
  using run_test_fun = std::add_pointer_t<void(test_result &)>;
  tests_register_t *next_{};
  run_test_fun test_fun_{};

public:
  static std::add_lvalue_reference_t<tests_register_t *> first() {
    static tests_register_t *v{};
    return v;
  }

private:
  static bool &any_destroyed() {
    static bool v{};
    return v;
  }
  static void maybe_panic() {
    if (any_destroyed()) {
      std::abort();
    }
  }
  static std::add_lvalue_reference_t<tests_register_t *> last() {
    static tests_register_t *v{};
    return v;
  }
  static void add_register(tests_register_t &new_r) {
    maybe_panic();
    auto &l = last();
    if (l == nullptr) {
      first() = &new_r;
    } else {
      l->next_ = &new_r;
    }
    l = &new_r;
  }

public:
  constexpr tests_register_t *next() const noexcept { return next_; }
  explicit tests_register_t(run_test_fun f) : test_fun_(f) {
    add_register(*this);
  }
  tests_register_t(tests_register_t &&) = delete;
  tests_register_t operator=(tests_register_t &&) = delete;
  ~tests_register_t() { any_destroyed() = true; }
  constexpr void run(test_result &dest) const { test_fun_(dest); }
};
struct is_regged_t {};
struct name_of_test {
  std::string_view name;
  constexpr explicit name_of_test(std::string_view n) : name(n) {}
};
template <typename F> struct named_test {
  std::string_view name;
  F f;

  constexpr char operator()(test_result &r) const {
    f(test_context(r));
    return {};
  }
};
template <direct_invocable<test_context &&> F>
constexpr named_test<F> operator<<(name_of_test const &n, F func) {
  return named_test<F>(n.name, std::move(func));
}

template <typename, int, typename... Ts>
inline std::optional<std::tuple<named_test<Ts>...>> &stored_test() {
  static std::optional<std::tuple<named_test<Ts>...>> v{};
  return v;
}
template <typename Case, int tag, typename... Ts>
inline is_regged_t register_tests(std::string_view, named_test<Ts>... tests) {
  stored_test<Case, tag, Ts...>() = {std::move(tests)...};
  static tests_register_t<tag> r([](test_result &tr) {
    std::apply(
        [&tr](auto &&...ts) {
          using expander = char[sizeof...(Ts)];
          (void)expander{ts(tr)...};
        },
        *stored_test<Case, tag, Ts...>());
  });
  return {};
}
template <int tag> inline test_result run_tests() {
  test_result r{};
  auto *test_reg = tests_register_t<tag>::first();
  while (test_reg != nullptr) {
    test_reg->run(r);
    test_reg = test_reg->next();
  }
  return r;
}
} // namespace internal
template <typename T> class eq_t {
  T v_;

public:
  template <typename... Ts>
    requires(std::constructible_from<T, Ts...>)
  constexpr explicit eq_t(Ts &&...args) : v_(std::forward<Ts>(args)...) {}

  template <typename U>
    requires(weakly_equality_comparable<T, U>)
  constexpr bool operator()(U &&rhs) const {
    return v_ == rhs;
  }
  constexpr T const &_value() const { return v_; }
};
template <typename T> constexpr eq_t<T> eq(T const &v) { return eq_t<T>(v); }
constexpr eq_t<std::string_view> str_eq(std::string_view v) {
  return eq_t<std::string_view>(v);
}
template <typename T> class contains_t {
  T v_;

public:
  template <typename... Ts>
    requires(std::constructible_from<T, Ts...>)
  constexpr explicit contains_t(Ts &&...args) : v_(std::forward<Ts>(args)...) {}

  template <typename U>
  constexpr bool operator()(U &&lhs) const
    requires(requires() {
      ranges::contains_subrange(std::ranges::begin(lhs), std::ranges::end(lhs),
                                begin(v_), end(v_));
    })
  {
    return ranges::contains_subrange(std::ranges::begin(lhs),
                                     std::ranges::end(lhs), begin(v_), end(v_));
  }
  constexpr T const &_value() const { return v_; }
};
constexpr contains_t<std::string_view> str_contains(std::string_view s) {
  return contains_t<std::string_view>(s);
}

inline test_result just_run_tests() { return internal::run_tests<0>(); }
constexpr bool failed(test_result const &r) { return r.failed != 0; }

template <typename Out, typename M>
constexpr auto format_matcher(Out o, std::string_view s, M const &m) {
  return std::format_to(o, "{} {}", s, m._value());
}
} // namespace cta

namespace std {
template <typename T> struct formatter<cta::eq_t<T>, char> {
  bool quoted = false;

  template <class ParseContext>
  constexpr ParseContext::iterator parse(ParseContext &ctx) {
    return ctx.begin();
  }

  template <class FmtContext>
  FmtContext::iterator format(cta::eq_t<T> const &s, FmtContext &ctx) const {
    return cta::format_matcher(ctx.out(), "equal", s);
  }
};
template <typename T> struct formatter<cta::contains_t<T>, char> {
  bool quoted = false;

  template <class ParseContext>
  constexpr ParseContext::iterator parse(ParseContext &ctx) {
    return ctx.begin();
  }

  template <class FmtContext>
  FmtContext::iterator format(cta::contains_t<T> const &s,
                              FmtContext &ctx) const {
    return cta::format_matcher(ctx.out(), "contains", s);
  }
};
} // namespace std

#endif

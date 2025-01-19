
#ifndef CTA_CTA_CTA_HPP
#define CTA_CTA_CTA_HPP

#include <algorithm>
#include <concepts>
#include <cstdlib>
#include <format>
#include <functional>
#include <iostream>
#include <optional>
#include <streambuf>
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
template <typename T>
concept _probably_tuple = requires() { std::tuple_size<T>::value; };
template <typename T>
concept output_streamable =
    requires(T const &t, std::basic_ostream<char> &o) { o << t; };

template <typename Val, typename Expected>
  requires(weakly_inequality_comparable<Expected, Val> ||
           std::predicate<Expected, Val>)
constexpr bool test_matcher(Val &&v, Expected &&e) {
  if constexpr (weakly_inequality_comparable<Expected, Val>) {
    return (e == v);
  } else if constexpr (std::predicate<Expected, Val>) {
    return std::invoke(std::forward<Expected>(e), std::forward<Val>(v));
  }
}

template <typename E, typename V>
concept matchable = requires(V &&v, E &&e) {
  ::cta::test_matcher(std::forward<V>(v), std::forward<E>(e));
};

template <typename T, typename Out>
concept member_format_to =
    requires(T const &t, Out &&o) { t.format_to(std::forward<Out>(o)); };

template <typename T> struct _format_matcher {
  T t_;
  constexpr explicit _format_matcher(T &&t) : t_(std::forward<T>(t)) {}
};
template <typename T> _format_matcher(T &&) -> _format_matcher<T>;

struct test_result {
  int total_tests{};
  int failed{};
};

constexpr auto print_reality_vs_expect(auto &&value, auto &&expectation,
                                       auto out) {
  return std::format_to(out, " - Value was '{}', expected '{}'\n",
                        _format_matcher(value), _format_matcher(expectation));
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
  template <typename Val, matchable<Val> Expected, typename Out>
  constexpr void expect_that(
      Val &&v, Expected &&e, Out &&o,
      etd::source_location const &sl = etd::source_location::current()) {
    bool this_failed =
        !test_matcher(std::forward<Val>(v), std::forward<Expected>(e));
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
struct empty_test_base {};
template <typename Base>
  requires(std::is_default_constructible_v<Base>)
class test_wrapper : public Base {
  test_context &ctx_;

public:
  explicit constexpr test_wrapper(test_context &ctx) : ctx_(ctx) {}
  template <typename L, typename R>
    requires(requires(test_context &ctx, L &&l, R &&r) {
      ctx.expect_that(std::forward<L>(l), std::forward<R>(r));
    })
  constexpr auto expect_that(
      L &&l, R &&r,
      etd::source_location const &sl = etd::source_location::current()) const {
    return ctx_.expect_that(std::forward<L>(l), std::forward<R>(r), sl);
  }
  template <typename L, typename R, typename O>
    requires(requires(test_context &ctx, L &&l, R &&r, O &&o) {
      ctx.expect_that(std::forward<L>(l), std::forward<R>(r),
                      std::forward<O>(o));
    })
  constexpr auto expect_that(
      L &&l, R &&r, O &&o,
      etd::source_location const &sl = etd::source_location::current()) const {
    return ctx_.expect_that(std::forward<L>(l), std::forward<R>(r),
                            std::forward<O>(o), sl);
  }
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
struct is_regged_t {
  constexpr is_regged_t() noexcept = default;
  constexpr explicit is_regged_t(auto &&...) noexcept {}
};
template <std::size_t str_count> struct name_of_test {
  std::array<std::string_view, str_count> names;
  template <std::convertible_to<std::string_view>... Ts>
    requires(sizeof...(Ts) == str_count)
  constexpr explicit(str_count == 1) name_of_test(Ts &&...args)
      : names({std::string_view(args)...}) {}
};

template <typename... Ts> name_of_test(Ts...) -> name_of_test<sizeof...(Ts)>;

template <typename F, typename NameType> struct named_test {
  NameType name;
  F f;

  constexpr char operator()(test_result &r) const {
    f(test_context(r));
    return {};
  }
};
template <direct_invocable<test_context &&> F, std::size_t str_count>
constexpr named_test<F, name_of_test<str_count>>
operator<<(name_of_test<str_count> const &n, F func) {
  return named_test<F, name_of_test<str_count>>(n, std::move(func));
}

template <typename, int, typename... NamedTests>
inline std::optional<std::tuple<NamedTests...>> &stored_test() {
  static std::optional<std::tuple<NamedTests...>> v{};
  return v;
}
template <typename Case, int tag, typename... Ts, typename... Ns>
inline is_regged_t register_tests(std::string_view,
                                  named_test<Ts, Ns>... tests) {
  stored_test<Case, tag, named_test<Ts, Ns>...>() = {std::move(tests)...};
  static tests_register_t<tag> r([](test_result &tr) {
    std::apply(
        [&tr](auto &&...ts) {
          using expander = char[sizeof...(Ts)];
          (void)expander{ts(tr)...};
        },
        *stored_test<Case, tag, named_test<Ts, Ns>...>());
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
  constexpr auto format_to(auto &&out) const {
    return std::format_to(out, "equal to {}", _format_matcher(v_));
  }
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
  constexpr auto format_to(auto &&out) const {
    return std::format_to(out, "contain {}", _format_matcher(v_));
  }
};
constexpr contains_t<std::string_view> str_contains(std::string_view s) {
  return contains_t<std::string_view>(s);
}
template <typename U, typename... Ts, std::size_t... is>
constexpr bool _match_range_to_values(U &&u, std::index_sequence<is...>,
                                      Ts &&...vals) {
  return (test_matcher(std::forward<U>(u)[is], std::forward<Ts>(vals)) && ...);
}
template <typename U, typename Tupl, std::size_t... is>
constexpr bool _match_range_to_tuple(U &&u, std::index_sequence<is...>,
                                     Tupl &&matchers) {
  return std::apply(
      [&u]<typename... Ts>(Ts &&...ms) {
        return _match_range_to_values(std::forward<U>(u),
                                      std::index_sequence<is...>{},
                                      std::forward<Ts>(ms)...);
      },
      matchers);
}
template <typename... Ts> class elements_are_t {
  std::tuple<Ts...> ms_;

public:
  constexpr explicit elements_are_t(auto &&...ms)
      : ms_(std::forward<decltype(ms)>(ms)...) {}
  template <std::ranges::input_range U>
  constexpr bool operator()(U &&lhs) const {
    if (std::ranges::size(lhs) != sizeof...(Ts)) {
      return false;
    }
    return _match_range_to_tuple(
        std::forward<U>(lhs), std::make_index_sequence<sizeof...(Ts)>{}, ms_);
  }
  constexpr auto format_to(auto &&out) const {
    return std::format_to(out, "elements are {}", _format_matcher(ms_));
  }
};
template <typename... Ts>
constexpr elements_are_t<Ts...> elements_are(Ts const &...matchers) {
  return elements_are_t<Ts...>(matchers...);
}

inline test_result just_run_tests() { return internal::run_tests<0>(); }
constexpr bool failed(test_result const &r) { return r.failed != 0; }

template <typename Out, typename M>
  requires(requires(M &m) { m._value(); })
constexpr auto format_matcher(Out o, std::string_view s, M const &m) {
  return std::format_to(o, "{} {}", s, m._value());
}

template <typename Out>
class _iterator_ostreambuf : public std::basic_streambuf<char> {
  Out o_;

public:
  explicit _iterator_ostreambuf(Out o) : basic_streambuf<char>(), o_(o) {}

  constexpr Out &out() noexcept { return o_; }

protected:
  std::streamsize showmanyc() override {
    return std::numeric_limits<std::streamsize>::max();
  }
  int_type uflow() override { return {}; }
  std::streamsize xsgetn(char_type *, std::streamsize) override { return {}; }
  std::streamsize xsputn(const char_type *s, std::streamsize count) override {
    o_ = std::copy_n(s, count, o_);
    return count;
  }
};
} // namespace cta

namespace std {
template <typename T> struct formatter<::cta::_format_matcher<T>, char> {
  template <class ParseContext>
  constexpr ParseContext::iterator parse(ParseContext &ctx) {
    return ctx.begin();
  }

  template <class FmtContext>
  FmtContext::iterator format(::cta::_format_matcher<T> const &s,
                              FmtContext &ctx) const {
    using namespace string_view_literals;
    using raw_t = std::remove_cvref_t<T>;
    if constexpr (::cta::member_format_to<T, decltype(ctx.out())>) {
      return s.t_.format_to(ctx.out());
    } else if constexpr (::cta::formattable<raw_t, char>) {
      return formatter<raw_t, char>{}.format(s.t_, ctx);
    } else if constexpr (::cta::output_streamable<raw_t>) {
      auto buf = ::cta::_iterator_ostreambuf(ctx.out());
      auto stream = std::basic_ostream<char>(&buf);
      stream << s.t_;
      return buf.out();
    } else if constexpr (std::ranges::range<raw_t>) {
      auto out = format_to(ctx.out(), "[");
      bool use_comma{};
      for (auto &&e : s.t_) {
        if (use_comma) {
          out = format_to(ctx.out(), ", '{}'", ::cta::_format_matcher(e));
        } else {
          out = format_to(ctx.out(), "'{}'", ::cta::_format_matcher(e));
          use_comma = true;
        }
      }
      return format_to(out, "]");
    } else if constexpr (requires() {
                           s.t_.size();
                           s.t_[0];
                         }) {
      auto out = format_to(ctx.out(), "[");
      auto sz = s.t_.size();
      if (sz > 0) {
        out = format_to(ctx.out(), "'{}'", ::cta::_format_matcher(s.t_[0]));
        for (auto i = static_cast<decltype(sz)>(1); i < sz; ++i) {
          out = format_to(ctx.out(), ", '{}'", ::cta::_format_matcher(s.t_[i]));
        }
      }
      return format_to(out, "]");
    } else if constexpr (::cta::_probably_tuple<raw_t>) {
      auto out = format_to(ctx.out(), "[");
      if constexpr (tuple_size_v<raw_t> > 0) {
        std::apply(
            [&out](auto const &v0, auto const &...vals) {
              constexpr auto format_single = [](auto &out, auto const &v) {
                out = format_to(out, ", '{}'", ::cta::_format_matcher(v));
                return char{};
              };
              out = format_to(out, "'{}'", ::cta::_format_matcher(v0));
              using expander = char[sizeof...(vals)];
              (void)expander{format_single(out, vals)...};
            },
            s.t_);
      }
      return format_to(out, "]");
    } else {
      return std::format_to(ctx.out(), "<cannot print>");
    }
  }
};
} // namespace std

#define CTA_INTERNAL_TEST_NS(NAME) cta_test_##NAME

#define CTA_INTERNAL_BEGIN_TESTS_TYPES(NAME, FIXT, TAG, ...)                   \
  struct cta_test_case_##NAME : public ::cta::test_wrapper<FIXT> {             \
    using _wrapper = ::cta::test_wrapper<FIXT>;                                \
    using _wrapper::_wrapper;                                                  \
    static constexpr char case_name[] = #NAME;                                 \
    __VA_ARGS__                                                                \
  };                                                                           \
  static constexpr int tag = TAG;

#define CTA_BEGIN_TESTS_F_INTERNAL(NAME, FIXT, TAG)                            \
  namespace CTA_INTERNAL_TEST_NS(NAME) {                                       \
  CTA_INTERNAL_BEGIN_TESTS_TYPES(NAME, FIXT, TAG)                              \
  using _cta_fixture_t = cta_test_case_##NAME;

#define CTA_BEGIN_TESTS_TF_INTERNAL(NAME, FIXT, TAG)                           \
  namespace CTA_INTERNAL_TEST_NS(NAME) {                                       \
  inline constexpr char case_name[] = #NAME;                                   \
  template <typename... Ts> struct _fixt_wrap {                                \
    static constexpr int tag = TAG;                                            \
    template <typename Tag>                                                    \
    static constexpr void do_run_test(::cta::test_context &, Tag);             \
    using _this_t = _fixt_wrap<Ts...>;                                         \
    template <typename T> using _fixture = FIXT<T>;                            \
    template <typename... Ts2, typename... Ns>                                 \
    static ::cta::internal::is_regged_t                                        \
    do_reg_test(std::string_view name,                                         \
                ::cta::internal::named_test<Ts2, Ns>... tests) {               \
      return ::cta::internal::register_tests<_this_t, tag>(                    \
          name, std::move(tests)...);                                          \
    }

#define CTA_BEGIN_TESTS_INTERNAL(NAME, TAG)                                    \
  CTA_BEGIN_TESTS_F_INTERNAL(NAME, ::cta::empty_test_base, TAG)

#define CTA_BEGIN_TESTS_TF(NAME) CTA_BEGIN_TESTS_TF_INTERNAL(NAME, NAME, 0)
/// Defines a test-fixture from a struct. The struct must be
/// default-constructible, but all setup (teardown) logic can be placed in the
/// constructor (destructor).
#define CTA_BEGIN_TESTS_F(NAME) CTA_BEGIN_TESTS_F_INTERNAL(NAME, NAME, 0)
/// Generates a single test function. Define the test with '{}' afterwards.
/// It must be used after a CTA_BEGIN_TESTS or CTA_BEGIN_TESTS_F and before
/// a CTA_END_TESTS() or CTA_END_TESTS_F()
#define CTA_TEST(NAME)                                                         \
  static_assert(sizeof(_cta_fixture_t) != 0,                                   \
                "This macro must be after a CTA_BEGIN_TESTS and before it's "  \
                "corresponding CTA_END_TESTS()");                              \
  struct _test_tag_##NAME : _cta_fixture_t {                                   \
    using _cta_fixture_t::_cta_fixture_t;                                      \
    void do_run_test();                                                        \
  };                                                                           \
  inline static auto reg_##NAME =                                              \
      ::cta::internal::register_tests<_cta_fixture_t, tag>(                    \
          _cta_fixture_t::case_name,                                           \
          ::cta::internal::name_of_test(#NAME) <<                              \
              [](test_context &&tc) { _test_tag_##NAME(tc).do_run_test(); });  \
  inline void _test_tag_##NAME::do_run_test()

/// Generates a single test function. Define the test with '{}' afterwards.
/// It must be used after a CTA_BEGIN_TESTS_TF and before
/// a CTA_END_TESTS_TF()
#define CTA_TEST_T(NAME, ...)                                                  \
  template <typename T> struct _tb_##NAME : ::cta::test_wrapper<_fixture<T>> { \
    static_assert(                                                             \
        !std::is_same_v<_fixture<T>, void>,                                    \
        "This macro must be after a CTA_BEGIN_TESTS_TF and before it's "       \
        "corresponding CTA_END_TESTS_TF()");                                   \
    explicit _tb_##NAME(test_context &tc)                                      \
        : ::cta::test_wrapper<_fixture<T>>(tc) {}                              \
    using ::cta::test_wrapper<_fixture<T>>::expect_that;                       \
    void run() { __VA_ARGS__ }                                                 \
  };                                                                           \
  static inline auto _reg_##NAME = ::cta::internal::is_regged_t(do_reg_test(   \
      case_name, ::cta::internal::name_of_test(#NAME, typeid(Ts).name()) <<    \
                     [](test_context &&tc) { _tb_##NAME<Ts>(tc).run(); })...);

#define CTA_END_TESTS() }
#define CTA_END_TESTS_F() CTA_END_TESTS()
#define CTA_END_TESTS_TF()                                                     \
  }                                                                            \
  ;                                                                            \
  }

/// @brief Starts a new test case, serving as a container for related tests.
/// This macro sets up the necessary infrastructure for a test suite.
/// @param NAME The name of the test case, used for identifying it in logs or
/// reports.
/// @note Must be followed by one or more `CTA_TEST()` calls and concluded with
/// `CTA_END_TESTS()`.
/// @warning Use this macro only once per test case; it cannot be nested.
#define CTA_BEGIN_TESTS(NAME) CTA_BEGIN_TESTS_INTERNAL(NAME, 0)

#define CTA_TYPED_TEST(NAME, ...)                                              \
  template struct CTA_INTERNAL_TEST_NS(NAME)::_fixt_wrap<__VA_ARGS__>;

#endif

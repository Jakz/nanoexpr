#pragma once

#include <cstdint>
#include <unordered_map>
#include <unordered_set>
#include <string>
#include <string_view>
#include <regex>
#include <vector>
#include <memory>
#include <array>
#include <functional>
#include <cctype>
#include <cassert>

namespace nanoexpr
{
  /* configuration */
  namespace config
  {
    using real_t = float;
    using integral_t = int32_t;
    static constexpr bool EnableIntegerPromotionToReal = true;
    static constexpr bool EnableTrigonometricFunctions = true;
    static constexpr bool EnableHyperbolicFunctions = true;
    static constexpr bool OptimizationConstantFolding = true;
  }

  using real_t = config::real_t;
  using integral_t = config::integral_t;

  enum class ValueType
  {
    REAL, INTEGRAL, BOOL, STRING,
    POINTER,

    FIRST_CUSTOM_TYPE = 1024,

    NONE
  };

  struct string_ref
  {
    std::string data;
  };

  template<ValueType T> struct value_traits { };
  template<> struct value_traits<ValueType::REAL> { using type = real_t; };
  template<> struct value_traits<ValueType::INTEGRAL> { using type = integral_t; };
  template<> struct value_traits<ValueType::BOOL> { using type = bool; };
  template<> struct value_traits<ValueType::STRING> { using type = const string_ref*; };
  template<> struct value_traits<ValueType::POINTER> { using type = void*; };


  union Value
  {
    real_t real;
    integral_t integral;
    const string_ref* string;
    bool boolean;
    void* ptr;

    Value(integral_t integral) : integral(integral) { }
    Value(real_t real) : real(real) { }
    Value(bool boolean) : boolean(boolean) { }
    Value(const string_ref* string) : string(string) { }

    template<typename T> Value(T* ptr) : ptr(ptr) { }

    inline integral_t i() const { return integral; }
    inline real_t r() const { return real; }
    inline bool b() const { return boolean; }
    inline const string_ref* str() const { return string; }
    template<typename T> inline T* p() const { return static_cast<T*>(ptr); }

    template<typename T, typename std::enable_if<!std::is_enum<T>::value, int>::type* = nullptr> T as() const;
    template<typename T, typename std::enable_if<std::is_enum<T>::value, int>::type* = nullptr> inline T as() const { return static_cast<T>(as<integral_t>()); }

    template<ValueType T> typename value_traits<T>::type as() { return as<typename value_traits<T>::type>(); }
  };

  template<> real_t Value::as() const { return real; }
  template<> integral_t Value::as() const { return integral; }
  template<> bool Value::as() const { return real; }
  template<> const string_ref* Value::as() const { return string; }

  struct TypedValue
  {
    Value value;
    ValueType type;

    TypedValue(real_t real) : value(real), type(ValueType::REAL) { }
    TypedValue(integral_t real) : value(real), type(ValueType::INTEGRAL) { }
    TypedValue(bool real) : value(real), type(ValueType::BOOL) { }
    TypedValue(const string_ref* string) : value(string), type(ValueType::STRING) { }
  };
}

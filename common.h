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

  union Value
  {
    real_t real;
    integral_t integral;
    bool boolean;
    const void* ptr;

    Value(integral_t integral) : integral(integral) { }
    Value(real_t real) : real(real) { }
    Value(bool boolean) : boolean(boolean) { }
    Value(const string_ref* string) : ptr(string) { }

    template<typename T> Value(T* ptr) : ptr(ptr) { }

    inline integral_t i() const { return integral; }
    inline real_t r() const { return real; }
    inline bool b() const { return boolean; }
    inline const string_ref* str() const { return as<const string_ref*>(); }
    template<typename T> inline T* p() const { return static_cast<T*>(ptr); }

    template<typename T, typename std::enable_if_t<!std::is_enum_v<T> && !std::is_pointer_v<T>>* = nullptr> T as() const;
    template<typename T, typename std::enable_if_t<std::is_enum_v<T>>* = nullptr> inline T as() const { return static_cast<T>(as<integral_t>()); }
    template<typename T, typename std::enable_if_t<std::is_pointer_v<T>>* = nullptr> inline T as() const { return static_cast<T>(ptr); }
  };

  template<> real_t Value::as() const { return real; }
  template<> integral_t Value::as() const { return integral; }
  template<> bool Value::as() const { return real; }


  struct TypedValue
  {
    Value value;
    ValueType type;

    TypedValue(real_t real) : value(real), type(ValueType::REAL) { }
    TypedValue(integral_t real) : value(real), type(ValueType::INTEGRAL) { }
    TypedValue(bool real) : value(real), type(ValueType::BOOL) { }
    TypedValue(const string_ref* string) : value(string), type(ValueType::STRING) { }

    template<typename T, std::enable_if_t<std::is_pointer_v<T>>* = nullptr> TypedValue(T ptr, ValueType type) : value(ptr), type(type) { }
  };
}

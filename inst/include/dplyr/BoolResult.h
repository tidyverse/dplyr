#ifndef dplyr_tools_BoolResult_H
#define dplyr_tools_BoolResult_H

namespace dplyr {

  class BoolResult {
  public:
    BoolResult(bool result_) : result(result_) {}
    BoolResult(bool result_, const CharacterVector& msg) : result(result_), message(msg) {}

    inline operator SEXP() const {
      LogicalVector res = LogicalVector::create(result);
      res.attr("comment") = message;
      res.attr("class")   = "BoolResult";
      return res;
    }

    inline operator bool() const {
      return result;
    }

    inline std::string why_not() const {
      R_xlen_t n = message.length();
      if (n == 0)
        return "";

      if (n == 1)
        return std::string(message[0]);

      std::stringstream ss;
      ss << "\n";
      for (int i = 0; i < n; ++i) {
        ss << "- " << std::string(message[i]);
      }

      return ss.str();
    }

  private:
    bool result;
    CharacterVector message;
  };

  inline BoolResult no_because(const CharacterVector& msg) {
    return BoolResult(false, msg);
  }

  inline BoolResult yes() {
    return true;
  }

}

#endif

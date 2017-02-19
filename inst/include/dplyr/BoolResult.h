#ifndef dplyr_tools_BoolResult_H
#define dplyr_tools_BoolResult_H

namespace dplyr {

  class BoolResult {
  public:
    BoolResult(bool result_) : result(result_) {}
    BoolResult(bool result_, const std::string& msg) : result(result_), message(msg) {}

    inline operator SEXP() const {
      LogicalVector res = LogicalVector::create(result);
      res.attr("comment") = message;
      res.attr("class")   = "BoolResult";
      return res;
    }

    inline operator bool() const {
      return result;
    }

    inline const std::string& why_not() const {
      return message;
    }

  private:
    bool result;
    std::string message;
  };

  inline BoolResult no_because(const std::string& msg) {
    return BoolResult(false, msg);
  }

  inline BoolResult yes() {
    return true;
  }

}

#endif

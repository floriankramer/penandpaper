#pragma once

#include <functional>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>
#include <memory>

class Markdown {
  enum class TokenType { WORD, ORDER_MARK, LINE_BREAK, EMPTY_LINE, WHITESPACE };

  class Token {
   public:
    std::string value;
    TokenType type;
  };

  class TokenMatcher {
  public:
    TokenMatcher(TokenType type);
    virtual void step(char c) = 0;
    bool matches();
    virtual void reset();
    TokenType type();

   protected:
    int _state;
    bool _matches;
    TokenType _type;
  };

  // Matches any string of non whitespace characters
  class WordMatcher : public TokenMatcher {
   public:
    WordMatcher();
    virtual void step(char c) override;
  };

  // Matches any string of numbers terminated with a dot
  class OrderMarkMatcher : public TokenMatcher {
   public:
    OrderMarkMatcher();
    virtual void step(char c) override;
  };

  // Matches at least two spaces and a newline
  class LineBreakMatcher : public TokenMatcher {
   public:
    LineBreakMatcher();
    virtual void step(char c) override;
  };

  // Matches a newline, any whitespace and a newline
  class EmptyLineMatcher : public TokenMatcher {
   public:
    EmptyLineMatcher();
    virtual void step(char c) override;
  };

  // Matches any sequence of whitespace. Newline are only allowed as
  // the first character
  class WhitespaceMatcher : public TokenMatcher {
   public:
    WhitespaceMatcher();
    virtual void step(char c) override;
  };


  class Lexer {
   public:
    Lexer(const std::string &input);

    Token &current();
    Token any();
    bool accept(const Token &token);
    bool accept(const TokenType &type);
    bool accept(const std::string &text);

    // returns true if the next token is of type type
    bool peek(const TokenType &type);

    void expect(const Token &token);
    void expect(const TokenType &type);
    void expect(const std::string &text);

    bool isDone() const;

   private:
    void readNext();

    std::vector<std::unique_ptr<TokenMatcher>> _token_matchers;

    const std::string &_input;
    size_t _pos;

    Token _current_token;
  };

 public:
  Markdown(const std::string &in);
  virtual ~Markdown();

  std::string process();

 private:
  std::ostringstream _out;
  Lexer _lexer;
};

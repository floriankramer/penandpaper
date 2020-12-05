/**
 * Copyright 2020 Florian Kramer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#pragma once

#include <functional>
#include <memory>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

#include "MarkdownNode.h"

class Markdown {
  enum class TokenType {
    WORD,
    ORDER_MARK,
    LIST_MARK,
    LINE_BREAK,
    FORCE_LINE_BREAK,
    EMPTY_LINE,
    WHITESPACE,
    HEADING_HASHES,
    BRACKET
  };

  static const char* TOKEN_TYPE_NAMES[];

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
    bool canMatch();
    virtual void reset();
    TokenType type();

   protected:
    int _state;
    bool _matches;
    // Is there still a sequence of inputs that could lead to a match
    bool _can_match;
    TokenType _type;
  };

  // Matches any string of non whitespace, non special characters
  class WordMatcher : public TokenMatcher {
   public:
    WordMatcher();
    virtual void step(char c) override;
  };

  // Matches any string of non whitespace characters
  class BracketMatcher : public TokenMatcher {
   public:
    BracketMatcher();
    virtual void step(char c) override;
  };

  // Matches any string of numbers terminated with a dot
  class OrderMarkMatcher : public TokenMatcher {
   public:
    OrderMarkMatcher();
    virtual void step(char c) override;
  };

  // Matches any string of numbers terminated with a dot
  class ListMarkMatcher : public TokenMatcher {
   public:
    ListMarkMatcher();
    virtual void step(char c) override;
  };

  // Matches at least two spaces and a newline
  class ForceLineBreakMatcher : public TokenMatcher {
   public:
    ForceLineBreakMatcher();
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

  // Matches \n
  class LineBreakMatcher : public TokenMatcher {
   public:
    LineBreakMatcher();
    virtual void step(char c) override;
  };

  // Matches up to six hashes
  class HeadingHashesMatcher : public TokenMatcher {
   public:
    HeadingHashesMatcher();
    virtual void step(char c) override;
  };

  // The lexer tranforms the input into a list of tokens
  class Lexer {
   public:
    Lexer(const std::string &input);

    // The current token
    Token &current();

    // Accept any token and return it
    Token any();

    // If the current token matches the given one advance the current position
    bool accept(const Token &token);
    bool accept(const TokenType &type);
    bool accept(const std::string &text);

    // returns true if the next token is of type type
    bool peek(const TokenType &type);

    /**
     * @return true if the next token is one of the paragraph ending tokens
     */
    bool peekLineEnd();
    bool acceptLineEnd();

    // If the current token does not match the given one throw an exception
    void expect(const Token &token);
    void expect(const TokenType &type);
    void expect(const std::string &text);

    // Returns true if the current position is at the end of the token list
    bool isDone() const;

    // Add the current position to the top of the position stack, storing
    // a copy of it.
    void push();

    // Pop the top of the position stack and return to the position that we
    // were at the last time push was called.
    void pop();

    size_t pos();

   private:
    void readNext();
    void tokenize();

    std::vector<std::unique_ptr<TokenMatcher>> _token_matchers;
    std::vector<Token> _tokens;
    const std::string &_input;
    std::vector<size_t> _positions;
  };

 public:
  Markdown(const std::string &in,
           std::function<std::string(const std::string &, const std::string &)>
               lookup_attribute = nullptr);
  virtual ~Markdown();

  MdNode process();

 private:
  void parseLine(MdNode &parent);
  bool parseLink(MdNode &parent);
  bool parseHashesHeading(MdNode &parent);

  /**
   * @brief Blocks are non text block elements (e.g lists)
   */
  bool parseBlock(MdNode &parent);

  bool parseUnorderedList(MdNode &parent, int indent_level=0);
  bool parseOrderedList(MdNode &parent, int indent_level=0);
  bool parseList(MdNode &parent, TokenType list_mark, bool is_ordered, int indent_level=0);

  /**
   * @return The indent level of text after s
   */
  int indentLevel(const std::string &s);

  Lexer _lexer;

  std::function<std::string(const std::string &, const std::string &)>
      _lookup_attribute;
};

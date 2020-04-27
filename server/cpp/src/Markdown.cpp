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
#include "Markdown.h"

#include "Logger.h"

// =============================================================================
// TokenMatcher
// =============================================================================

Markdown::TokenMatcher::TokenMatcher(TokenType type) : _type(type) { reset(); }

bool Markdown::TokenMatcher::matches() { return _matches; }

bool Markdown::TokenMatcher::canMatch() { return _can_match; }

void Markdown::TokenMatcher::reset() {
  _state = 0;
  _matches = false;
  _can_match = true;
}

Markdown::TokenType Markdown::TokenMatcher::type() { return _type; }

Markdown::WordMatcher::WordMatcher() : TokenMatcher(TokenType::WORD) {}
void Markdown::WordMatcher::step(char c) {
  // States:
  // -1  failure
  //  0 Accept any non whitespace
  switch (_state) {
    case 0:
      if (!std::isspace(c)) {
        _matches = true;
      } else {
        _state = -1;
        _matches = false;
        _can_match = false;
      }
      break;
    default:
      _can_match = false;
      _matches = false;
  }
}

Markdown::OrderMarkMatcher::OrderMarkMatcher()
    : TokenMatcher(TokenType::ORDER_MARK) {}
void Markdown::OrderMarkMatcher::step(char c) {
  // States:
  // -1  failure
  //  0 accepts digits
  //  1 accepts digits or a dot
  //  2 accepts nothing
  switch (_state) {
    case 0:
      if (!std::isdigit(c)) {
        _state = -1;
        _can_match = false;
      } else {
        _state = 1;
      }
      break;
    case 1:
      if (c == '.') {
        _state = 2;
        _matches = true;
      } else if (!std::isdigit(c)) {
        _state = -1;
        _matches = false;
        _can_match = false;
      }
      break;
    case 2:
      _state = -1;
      _matches = false;
      _can_match = false;
      break;
    default:
      _matches = false;
      _can_match = false;
  }
}

Markdown::ListMarkMatcher::ListMarkMatcher()
    : TokenMatcher(TokenType::LIST_MARK) {}
void Markdown::ListMarkMatcher::step(char c) {
  // States:
  // -1  failure
  //  0 accepts + * -
  //  1 accepts nothing
  switch (_state) {
    case 0:
      if (c == '-' || c == '+' || c == '*') {
        _state = 1;
        _matches = true;
      } else {
        _state = -1;
        _matches = false;
        _can_match = false;
      }
      break;
    case 1:
      _state = -1;
      _matches = false;
      _can_match = false;
      break;
    default:
      _matches = false;
      _can_match = false;
  }
}

Markdown::ForceLineBreakMatcher::ForceLineBreakMatcher()
    : TokenMatcher(TokenType::FORCE_LINE_BREAK) {}
void Markdown::ForceLineBreakMatcher::step(char c) {
  // States:
  // -1  failure
  //  0 accepts a space
  //  1 accepts a space
  //  2 accepts spaces or a newline
  //  3 accepts nothing
  switch (_state) {
    case 0:
      if (c == ' ') {
        _state = 1;
      } else {
        _state = -1;
        _can_match = false;
      }
      break;
    case 1:
      if (c == ' ') {
        _state = 2;
      } else {
        _state = -1;
        _can_match = false;
      }
      break;
    case 2:
      if (c == '\n') {
        _matches = true;
        _state = 3;
      } else if (c != ' ') {
        _state = -1;
        _can_match = false;
      }
      break;
    case 3:
      _state = -1;
      _matches = false;
      _can_match = false;
      break;
    default:
      _matches = false;
      _can_match = false;
  }
}

Markdown::EmptyLineMatcher::EmptyLineMatcher()
    : TokenMatcher(TokenType::EMPTY_LINE) {}
void Markdown::EmptyLineMatcher::step(char c) {
  // States:
  // -1  failure
  //  0 accepts a newline or any whitespace
  //  1 accepts any whitespace
  //  2 accepts nothing
  switch (_state) {
    case 0:
      if (c == '\n') {
        _state = 1;
      } else if (!std::isspace(c)) {
        _state = -1;
        _can_match = false;
      }
      break;
    case 1:
      if (c == '\n') {
        _state = 2;
        _matches = true;
      } else if (!std::isspace(c)) {
        _state = -1;
        _can_match = false;
      }
      break;
    case 2:
      _state = -1;
      _matches = false;
      _can_match = false;
      break;
    default:
      _matches = false;
      _can_match = false;
  }
}

Markdown::WhitespaceMatcher::WhitespaceMatcher()
    : TokenMatcher(TokenType::WHITESPACE) {}
void Markdown::WhitespaceMatcher::step(char c) {
  // States:
  // -1  failure
  //  1 accepts any whitespace but newlines
  switch (_state) {
    case 0:
      if (c == '\n' || !std::isspace(c)) {
        _state = -1;
        _matches = false;
        _can_match = false;
      } else {
        _matches = true;
      }
      break;
    default:
      _matches = false;
      _can_match = false;
  }
}

Markdown::LineBreakMatcher::LineBreakMatcher()
    : TokenMatcher(TokenType::LINE_BREAK) {}
void Markdown::LineBreakMatcher::step(char c) {
  // States:
  // -1  failure
  //  0 accepts a newline
  //  1 nothing

  switch (_state) {
    case 0:
      if (c == '\n') {
        _state = 1;
        _matches = true;
      } else {
        _state = -1;
        _can_match = false;
      }
      break;
    case 1:
      _state = -1;
      _matches = false;
      _can_match = false;
      break;
    default:
      _matches = false;
      _can_match = false;
  }
}

Markdown::HeadingHashesMatcher::HeadingHashesMatcher()
    : TokenMatcher(TokenType::HEADING_HASHES) {}
void Markdown::HeadingHashesMatcher::step(char c) {
  // States:
  // -1  failure
  //  0 accepts a hash
  //  1 accepts a hash
  //  2 accepts a hash
  //  3 accepts a hash
  //  4 accepts a hash
  //  5 accepts a hash
  if (_state < 6 && _state > -1) {
    if (c == '#') {
      _matches = true;
      _state++;
    } else {
      _state = -1;
      _matches = false;
      _can_match = false;
    }
  } else {
    _state = -1;
    _matches = false;
    _can_match = false;
  }
}

// =============================================================================
// Lexer
// =============================================================================

Markdown::Lexer::Lexer(const std::string &input)
    : _input(input), _positions({0}) {
  // The matchers are added in order of increasing priority
  // The whitespace tokens
  _token_matchers.emplace_back(std::make_unique<Markdown::LineBreakMatcher>());
  _token_matchers.emplace_back(std::make_unique<Markdown::WhitespaceMatcher>());
  _token_matchers.emplace_back(
      std::make_unique<Markdown::ForceLineBreakMatcher>());
  _token_matchers.emplace_back(std::make_unique<Markdown::EmptyLineMatcher>());

  // The none whitespace tokens
  _token_matchers.emplace_back(std::make_unique<Markdown::WordMatcher>());
  _token_matchers.emplace_back(
      std::make_unique<Markdown::HeadingHashesMatcher>());
  _token_matchers.emplace_back(std::make_unique<Markdown::OrderMarkMatcher>());
  _token_matchers.emplace_back(std::make_unique<Markdown::ListMarkMatcher>());
  // tokenize the input
  tokenize();
}

Markdown::Token &Markdown::Lexer::current() {
  return _tokens[_positions.back()];
}

Markdown::Token Markdown::Lexer::any() {
  Token t = current();
  readNext();
  return t;
}

bool Markdown::Lexer::accept(const Token &token) {
  if (!isDone() && current().type == token.type &&
      current().value == token.value) {
    readNext();
    return true;
  }
  return false;
}

bool Markdown::Lexer::accept(const TokenType &type) {
  if (!isDone() && current().type == type) {
    readNext();
    return true;
  }
  return false;
}

bool Markdown::Lexer::accept(const std::string &text) {
  if (!isDone() && current().value == text) {
    readNext();
    return true;
  }
  return false;
}

bool Markdown::Lexer::peek(const TokenType &type) {
  if (!isDone() && current().type == type) {
    return true;
  }
  return false;
}

bool Markdown::Lexer::peekLineEnd() {
  return peek(TokenType::EMPTY_LINE) || peek(TokenType::LINE_BREAK) ||
         peek(TokenType::FORCE_LINE_BREAK);
}

bool Markdown::Lexer::acceptLineEnd() {
  return accept(TokenType::EMPTY_LINE) || accept(TokenType::LINE_BREAK) ||
         accept(TokenType::FORCE_LINE_BREAK);
}

void Markdown::Lexer::expect(const Token &token) {
  if (isDone() || current().type != token.type ||
      current().value != token.value) {
    throw std::runtime_error("Expected '" + token.value + "' but got '" +
                             current().value + "'");
  }
  readNext();
}
void Markdown::Lexer::expect(const TokenType &type) {
  if (isDone() || current().type != type) {
    throw std::runtime_error("Expected a token of type " +
                             std::to_string(int(type)) + " but got type " +
                             std::to_string(int(current().type)));
  }
  readNext();
}
void Markdown::Lexer::expect(const std::string &text) {
  if (isDone() || current().value != text) {
    throw std::runtime_error("Expected '" + text + "' but got '" +
                             current().value + "'");
  }
  readNext();
}

bool Markdown::Lexer::isDone() const {
  return _positions.back() >= _tokens.size();
}

void Markdown::Lexer::readNext() { _positions.back()++; }

void Markdown::Lexer::push() {
  size_t p = _positions.back();
  _positions.push_back(p);
}

void Markdown::Lexer::pop() {
  if (_positions.size() > 1) {
    _positions.pop_back();
  }
}

size_t Markdown::Lexer::pos() { return _positions.back(); }

void Markdown::Lexer::tokenize() {
  size_t pos = 0;
  // Take the longest match amongst all TokenMatchers. To do so, feed
  // characters from the input into the token matchers untill we reach the
  // end of the input or all matchers report that they can no longer enter
  // an accepting state.
  while (pos < _input.size()) {
    size_t start = pos;
    char first_character = _input[pos];
    // The current candidate matcher
    TokenMatcher *match = nullptr;
    // The last character of the current candidate matcher still matched on
    size_t match_pos = pos;
    size_t num_matching = 0;
    for (std::unique_ptr<TokenMatcher> &t : _token_matchers) {
      t->reset();
      t->step(first_character);
      if (t->matches()) {
        match = t.get();
        match_pos = pos;
      }
      if (t->canMatch()) {
        num_matching++;
      }
    }
    pos++;

    // feed characters into the token matchers
    while (num_matching > 0 && pos < _input.size()) {
      num_matching = 0;
      char c = _input[pos];
      for (std::unique_ptr<TokenMatcher> &t : _token_matchers) {
        t->step(c);
        if (t->matches()) {
          match_pos = pos;
          match = t.get();
        }
        if (t->canMatch()) {
          num_matching++;
        }
      }
      pos++;
    }
    pos = match_pos + 1;
    if (pos == start) {
      throw std::runtime_error(
          "Unable to parse the markdown input. Matched a token of length 0 in "
          "input part: " +
          _input.substr(start, 32));
    }
    Token t;
    t.type = match->type();
    t.value = _input.substr(start, match_pos + 1 - start);
    _tokens.push_back(t);
  }
}

// =============================================================================
// Markdown
// =============================================================================

Markdown::Markdown(
    const std::string &in,
    std::function<std::string(const std::string &, const std::string &)>
        lookup_attribute)
    : _lexer(in), _lookup_attribute(lookup_attribute) {}
Markdown::~Markdown() {}

std::string Markdown::process() {
  if (_lexer.isDone()) {
    return _out.str();
  }

  // Parse all blocks at the beginning
  while (parseBlock(_out))
    ;
  while (!_lexer.isDone()) {
    // If this is true check if the paragraph continues or is interrupted by
    // a block (list, table, etc.)
    bool check_for_block = false;
    // Skip trying to parse a line
    bool skip_line = false;

    size_t pos = _lexer.pos();
    if (_lexer.peek(TokenType::HEADING_HASHES)) {
      std::ostringstream s;
      if (parseHashesHeading(s)) {
        if (_in_paragraph) {
          _out << "</p>";
          _in_paragraph = false;
        }
        _out << s.str();
        check_for_block = true;
        skip_line = true;
      }
    }
    if (!skip_line) {
      if (!_in_paragraph) {
        _out << "<p>";
        _in_paragraph = true;
      }
      parseLine(_out);
    }
    if (_lexer.isDone()) {
      break;
    }
    if (_lexer.accept(TokenType::EMPTY_LINE)) {
      if (_in_paragraph) {
        _out << "</p>";
        _in_paragraph = false;
      }
      check_for_block = true;
    } else if (_lexer.accept(TokenType::FORCE_LINE_BREAK)) {
      _out << "<br/>";
      check_for_block = true;
    } else if (_lexer.accept(TokenType::LINE_BREAK)) {
      _out << " ";
      check_for_block = true;
    }
    if (check_for_block) {
      std::ostringstream block;
      if (parseBlock(block)) {
        if (_in_paragraph) {
          _out << "</p>";
          _in_paragraph = false;
        }
        _out << block.str();
      }
      // parse all consecutive blocks
      while (parseBlock(_out))
        ;
    }

    // Check if we processed at least one token. If we didn't we found a
    // construct we can't parse.
    if (pos == _lexer.pos()) {
      std::ostringstream s;
      s << "The parser was unable to find any matching construct at token "
        << pos << " " << _lexer.current().value;
      throw std::runtime_error(s.str());
    }
  }
  if (_in_paragraph) {
    _out << "</p>";
  }
  return _out.str();
}

bool Markdown::parseBlock(std::ostream &out) {
  return parseUnorderedList(out) || parseOrderedList(out);
}

bool Markdown::parseUnorderedList(std::ostream &out) {
  return parseList(out, TokenType::LIST_MARK, "ul", "li");
}

bool Markdown::parseOrderedList(std::ostream &out) {
  return parseList(out, TokenType::ORDER_MARK, "ol", "li");
}

bool Markdown::parseList(std::ostream &out, TokenType list_mark,
                         const std::string &list_tag,
                         const std::string &item_tag) {
  _lexer.push();
  if (_lexer.peek(TokenType::WHITESPACE)) {
    std::string val = _lexer.current().value;
    // A list may be prefixed with at most three whitespace
    if (val != " " && val != "  " && val != "   ") {
      _lexer.pop();
      return false;
    }
    // Consume the whitespace
    _lexer.any();
  }

  if (!_lexer.accept(list_mark)) {
    // A list needs to start with a list mark
    _lexer.pop();
    return false;
  }
  out << "<" << list_tag << "><" << item_tag << ">";
  while (!_lexer.isDone()) {
    parseLine(out);
    if (_lexer.isDone()) {
      break;
    }
    if (_lexer.accept(TokenType::EMPTY_LINE)) {
      break;
    } else {
      // Insert a whitespace
      out << " ";
    }
    // Consume the token ending the line.
    _lexer.any();
    // Check if the next line is indented by no more than three spaces
    if (_lexer.peek(TokenType::WHITESPACE)) {
      std::string val = _lexer.current().value;
      // A list may be prefixed with at most three whitespace
      if (val != " " && val != "  " && val != "   ") {
        // Keep parsing more lines. This is actually not standard conform,
        // but will work for now.
        continue;
      }
      // Consume the whitespace
      _lexer.any();
    }
    // Start a new ordered point
    if (_lexer.accept(list_mark)) {
      out << "</" << item_tag << "><" << item_tag << ">";
    }
  }
  out << "</" << item_tag << "></" << list_tag << ">";
  return true;
}

void Markdown::parseLine(std::ostream &out) {
  while (!_lexer.isDone()) {
    if (_lexer.peekLineEnd()) {
      return;
    } else if (_lexer.accept(TokenType::WHITESPACE)) {
      out << " ";
    } else {
      if (parseLink(out)) {
        continue;
      }
      out << _lexer.any().value;
    }
  }
}

bool Markdown::parseHashesHeading(std::ostream &out) {
  _lexer.push();
  if (!_lexer.peek(TokenType::HEADING_HASHES)) {
    _lexer.pop();
    return false;
  }
  Token hashes = _lexer.any();
  int heading_level = hashes.value.size();
  out << "<h" << heading_level << ">";
  while (!_lexer.isDone()) {
    if (_lexer.accept(hashes) || _lexer.acceptLineEnd()) {
      // the heading is done
      out << "</h" << heading_level << ">";
      return true;
    }
    if (_lexer.accept(TokenType::WHITESPACE)) {
      out << ' ';
    } else {
      out << _lexer.any().value;
    }
  }
  out << "</h" << heading_level << ">";
  return true;
}

bool Markdown::parseLink(std::ostream &out) {
  _lexer.push();
  const std::string *current = &_lexer.current().value;
  size_t pos = 0;
  // Look for an opening brace
  bool escaped = false;
  bool is_attribute_link = false;
  while (pos < current->size() && (escaped || (*current)[pos] != '[')) {
    pos++;
  }
  if (pos >= current->size()) {
    _lexer.pop();
    return false;
  }
  // The position of the name opening brace [
  ssize_t name_start = pos;
  std::string pre_link = current->substr(0, name_start);
  std::ostringstream link_name;

  // Look for the name closing brace. That could be several tokens down the line
  size_t name_end = std::string::npos;
  while (!_lexer.isDone() && !_lexer.peekLineEnd()) {
    while (pos < current->size() && (escaped || (*current)[pos] != ']')) {
      if ((*current)[pos] == ':') {
        is_attribute_link = true;
      }
      pos++;
    }
    if (pos < current->size()) {
      // We found it
      name_end = pos;
      break;
    } else {
      // We reached the end of the token
      link_name << current->substr(name_start + 1);
      // proceed with the next token
      _lexer.any();
      current = &_lexer.current().value;
      pos = 0;
      name_start = -1;
    }
  }
  if (name_end == std::string::npos) {
    _lexer.pop();
    return false;
  }
  link_name << current->substr(name_start + 1, name_end - name_start - 1);
  if (is_attribute_link) {
    // An attribute link doesn't use the second pair of braces
    out << pre_link;
    if (_lookup_attribute == nullptr) {
      out << " [attribute " << link_name.str() << " not found] ";
    } else {
      std::string ln = link_name.str();
      size_t spos = ln.find(':');
      if (spos == std::string::npos) {
        throw std::runtime_error(
            "Bug in the markdown parser: wrongly identified a link as an "
            "attribute link.");
      }
      std::string id = ln.substr(0, spos);
      std::string predicate = ln.substr(spos + 1);
      out << _lookup_attribute(id, predicate);
    }
    out << current->substr(name_end + 1);
    // We processed the current token, advance
    _lexer.any();
    return true;
  }

  // Look for the opening brace of the link target
  size_t target_start = std::string::npos;
  pos++;
  if (pos < current->size() && (*current)[pos] == '(') {
    target_start = pos;
  } else {
    // There may only be a single block of whitespace before the opening brace.
    // We must also not be done after accepting the whitespace
    if (!_lexer.accept(TokenType::WHITESPACE) || _lexer.isDone()) {
      _lexer.pop();
      return false;
    }
    current = &_lexer.current().value;
    pos = 0;
    if ((*current)[pos] != '(') {
      _lexer.pop();
      return false;
    }
    target_start = pos;
  }

  // Look for the closing brace of the link target. It must be in the same token
  while (pos < current->size() && (*current)[pos] != ')') {
    pos++;
  }

  if (pos >= current->size()) {
    _lexer.pop();
    return false;
  }

  std::string link_target =
      current->substr(target_start + 1, pos - target_start - 1);

  out << pre_link << "<a href=\"" << link_target << "\">" << link_name.str()
      << "</a>";
  _lexer.any();

  return true;
}

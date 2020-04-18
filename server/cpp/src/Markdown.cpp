#include "Markdown.h"

#include "Logger.h"

// =============================================================================
// TokenMatcher
// =============================================================================

Markdown::TokenMatcher::TokenMatcher(TokenType type) : _type(type) { reset(); }

bool Markdown::TokenMatcher::matches() { return _matches; }

void Markdown::TokenMatcher::reset() {
  _state = 0;
  _matches = false;
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
      }
      break;
    default:
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
      }
      break;
    case 2:
      _state = -1;
      _matches = false;
      break;
    default:
      _matches = false;
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
      }
      break;
    case 1:
      _state = -1;
      _matches = false;
      break;
    default:
      _matches = false;
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
      }
      break;
    case 1:
      if (c == ' ') {
        _state = 2;
      } else {
        _state = -1;
      }
      break;
    case 2:
      if (c == '\n') {
        _matches = true;
        _state = 3;
      } else if (c != ' ') {
        _state = -1;
      }
      break;
    case 3:
      _state = -1;
      _matches = false;
      break;
    default:
      _matches = false;
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
      }
      break;
    case 1:
      if (c == '\n') {
        _state = 2;
        _matches = true;
      } else if (!std::isspace(c)) {
        _state = -1;
      }
      break;
    case 2:
      _state = -1;
      _matches = false;
      break;
    default:
      _matches = false;
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
      } else {
        _matches = true;
      }
      break;
    default:
      _matches = false;
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
      }
      break;
    case 1:
      _state = -1;
      _matches = false;
      break;
    default:
      _matches = false;
  }
}

// =============================================================================
// Lexer
// =============================================================================

Markdown::Lexer::Lexer(const std::string &input)
    : _input(input), _positions({0}) {
  _token_matchers.emplace_back(std::make_unique<Markdown::LineBreakMatcher>());
  _token_matchers.emplace_back(std::make_unique<Markdown::WhitespaceMatcher>());
  _token_matchers.emplace_back(std::make_unique<Markdown::EmptyLineMatcher>());
  _token_matchers.emplace_back(
      std::make_unique<Markdown::ForceLineBreakMatcher>());
  _token_matchers.emplace_back(std::make_unique<Markdown::WordMatcher>());
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
  if (current().type == token.type && current().value == token.value) {
    readNext();
    return true;
  }
  return false;
}

bool Markdown::Lexer::accept(const TokenType &type) {
  if (current().type == type) {
    readNext();
    return true;
  }
  return false;
}

bool Markdown::Lexer::accept(const std::string &text) {
  if (current().value == text) {
    readNext();
    return true;
  }
  return false;
}

bool Markdown::Lexer::peek(const TokenType &type) {
  if (current().type == type) {
    return true;
  }
  return false;
}

bool Markdown::Lexer::peekLineEnd() {
  return peek(TokenType::EMPTY_LINE) || peek(TokenType::LINE_BREAK) ||
         peek(TokenType::FORCE_LINE_BREAK);
}

void Markdown::Lexer::expect(const Token &token) {
  if (current().type != token.type || current().value != token.value) {
    throw std::runtime_error("Expected '" + token.value + "' but got '" +
                             current().value + "'");
  }
  readNext();
}
void Markdown::Lexer::expect(const TokenType &type) {
  if (current().type != type) {
    throw std::runtime_error("Expected a token of type " +
                             std::to_string(int(type)) + " but got type " +
                             std::to_string(int(current().type)));
  }
  readNext();
}
void Markdown::Lexer::expect(const std::string &text) {
  if (current().value != text) {
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
  while (pos < _input.size()) {
    size_t start = pos;
    char first_character = _input[pos];
    TokenMatcher *match = nullptr;
    pos++;
    size_t num_matching = 0;
    for (std::unique_ptr<TokenMatcher> &t : _token_matchers) {
      t->reset();
      t->step(first_character);
      if (t->matches()) {
        num_matching++;
        match = t.get();
      }
    }
    // LOG_DEBUG << num_matching << " matchers match the first token" <<
    // LOG_END;

    // feed characters into the token matchers
    while (num_matching > 0 && pos < _input.size()) {
      num_matching = 0;
      char c = _input[pos];
      pos++;
      for (std::unique_ptr<TokenMatcher> &t : _token_matchers) {
        t->step(c);
        if (t->matches()) {
          num_matching++;
          match = t.get();
        }
      }
    }
    if (pos < _input.size()) {
      pos--;
    }
    if (pos == start) {
      throw std::runtime_error(
          "Unable to parse the markdown input. Matched a token of length 0 in "
          "input part: " +
          _input.substr(start, 32));
    }
    Token t;
    t.type = match->type();
    t.value = _input.substr(start, pos - start);
    _tokens.push_back(t);
  }
}

// =============================================================================
// Markdown
// =============================================================================

Markdown::Markdown(const std::string &in) : _lexer(in) {}
Markdown::~Markdown() {}

std::string Markdown::process() {
  if (_lexer.isDone()) {
    return _out.str();
  }

  // Parse all blocks at the beginning
  while (parseBlock(_out))
    ;

  _out << "<p>";
  _in_paragraph = true;
  while (!_lexer.isDone()) {
    size_t pos = _lexer.pos();
    if (!_in_paragraph) {
      _out << "<p>";
      _in_paragraph = true;
    }
    parseLine(_out);
    // If this is true check if the paragraph continues or is interrupted by
    // a block (list, table, etc.)
    bool check_for_block = false;
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
      // TODO: This doesn't allow for closing the paragraph
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
  }

  if (!_lexer.accept(list_mark)) {
    // A list needs to start with a list mark
    _lexer.pop();
    return false;
  }
  out << "<" << list_tag << "><" << item_tag << ">";
  while (!_lexer.isDone()) {
    parseLine(out);
    if (_lexer.accept(TokenType::EMPTY_LINE)) {
      break;
    }
    // Consume the token ending the line.
    _lexer.any();
    // Check if the next line is indented by nor more than three spaces
    if (_lexer.peek(TokenType::WHITESPACE)) {
      std::string val = _lexer.current().value;
      // A list may be prefixed with at most three whitespace
      if (val != " " && val != "  " && val != "   ") {
        continue;
      }
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
    out << pre_link << " [attribute " << link_name.str() << " not found] "
        << current->substr(name_end + 1);
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

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

Markdown::LineBreakMatcher::LineBreakMatcher()
    : TokenMatcher(TokenType::LINE_BREAK) {}
void Markdown::LineBreakMatcher::step(char c) {
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
  //  0 accepts a newline
  //  1 accepts any whitespace

  switch (_state) {
    case 0:
      if (std::isspace(c)) {
        _state = 1;
        _matches = true;
      } else {
        _state = -1;
      }
      break;
    case 1:
      if (c == '\n' || !std::isspace(c)) {
        _state = -1;
        _matches = false;
      }
      break;
    default:
      _matches = false;
  }
}

// =============================================================================
// Lexer
// =============================================================================

Markdown::Lexer::Lexer(const std::string &input) : _input(input), _pos(0) {
  _token_matchers.emplace_back(std::make_unique<Markdown::WhitespaceMatcher>());
  _token_matchers.emplace_back(std::make_unique<Markdown::EmptyLineMatcher>());
  _token_matchers.emplace_back(std::make_unique<Markdown::LineBreakMatcher>());
  _token_matchers.emplace_back(std::make_unique<Markdown::WordMatcher>());
  _token_matchers.emplace_back(std::make_unique<Markdown::OrderMarkMatcher>());
  readNext();
}

Markdown::Token &Markdown::Lexer::current() { return _current_token; }

Markdown::Token Markdown::Lexer::any() {
  Token t = _current_token;
  readNext();
  return t;
}

bool Markdown::Lexer::accept(const Token &token) {
  if (_current_token.type == token.type &&
      _current_token.value == token.value) {
    readNext();
    return true;
  }
  return false;
}

bool Markdown::Lexer::accept(const TokenType &type) {
  if (_current_token.type == type) {
    readNext();
    return true;
  }
  return false;
}

bool Markdown::Lexer::accept(const std::string &text) {
  if (_current_token.value == text) {
    readNext();
    return true;
  }
  return false;
}

bool Markdown::Lexer::peek(const TokenType &type) {
  if (_current_token.type == type) {
    return true;
  }
  return false;
}

void Markdown::Lexer::expect(const Token &token) {
  if (_current_token.type != token.type ||
      _current_token.value != token.value) {
    throw std::runtime_error("Expected '" + token.value + "' but got '" +
                             _current_token.value + "'");
  }
  readNext();
}
void Markdown::Lexer::expect(const TokenType &type) {
  if (_current_token.type != type) {
    throw std::runtime_error("Expected a token of type " +
                             std::to_string(int(type)) + " but got type " +
                             std::to_string(int(_current_token.type)));
  }
  readNext();
}
void Markdown::Lexer::expect(const std::string &text) {
  if (_current_token.value != text) {
    throw std::runtime_error("Expected '" + text + "' but got '" +
                             _current_token.value + "'");
  }
  readNext();
}

bool Markdown::Lexer::isDone() const { return _pos >= _input.size(); }

void Markdown::Lexer::readNext() {
  // LOG_DEBUG << "Looking for the next token with " << (_input.size() - _pos)
  // << " characters left" << LOG_END;
  if (isDone()) {
    // LOG_DEBUG << "Done: " << _pos << " >= " << _input.size() << ";" <<
    // LOG_END;
    return;
  }
  size_t start = _pos;
  char first_character = _input[_pos];
  TokenMatcher *match = nullptr;
  _pos++;
  size_t num_matching = 0;
  for (std::unique_ptr<TokenMatcher> &t : _token_matchers) {
    t->reset();
    t->step(first_character);
    if (t->matches()) {
      num_matching++;
      match = t.get();
    }
  }
  // LOG_DEBUG << num_matching << " matchers match the first token" << LOG_END;

  // feed characters into the token matchers
  while (num_matching > 0 && _pos < _input.size()) {
    num_matching = 0;
    char c = _input[_pos];
    _pos++;
    for (std::unique_ptr<TokenMatcher> &t : _token_matchers) {
      t->step(c);
      if (t->matches()) {
        num_matching++;
        match = t.get();
      }
    }
  }
  if (_pos < _input.size()) {
    _pos--;
  }
  if (_pos == start) {
    throw std::runtime_error(
        "Unable to parse the markdown input. Matched a token of length 0 in "
        "input part: " +
        _input.substr(start, 32));
  }
  // LOG_DEBUG << "Done matching. Got a token of size " << (_pos - start)
  //          << LOG_END;
  _current_token.type = match->type();
  _current_token.value = _input.substr(start, _pos - start);
  // LOG_DEBUG << _current_token.value << LOG_END;
}

// =============================================================================
// Markdown
// =============================================================================

Markdown::Markdown(const std::string &in) : _lexer(in) {}
Markdown::~Markdown() {}

std::string Markdown::process() {
  _out << "<p>";
  while (!_lexer.isDone()) {
    if (_lexer.peek(TokenType::WORD)) {
      _out << _lexer.any().value;
    } else if (_lexer.peek(TokenType::ORDER_MARK)) {
      _out << _lexer.any().value;
    } else if (_lexer.accept(TokenType::LINE_BREAK)) {
      _out << "<br/>";
    } else if (_lexer.accept(TokenType::EMPTY_LINE)) {
      _out << "<p/><p>";
    } else if (_lexer.accept(TokenType::WHITESPACE)) {
      _out << " ";
    }
  }
  // write out the last token
  if (_lexer.peek(TokenType::WORD)) {
    _out << _lexer.any().value;
  } else if (_lexer.peek(TokenType::ORDER_MARK)) {
    _out << _lexer.any().value;
  } else if (_lexer.accept(TokenType::LINE_BREAK)) {
    _out << "<br/>";
  } else if (_lexer.accept(TokenType::EMPTY_LINE)) {
    _out << "<p/><p>";
  } else if (_lexer.accept(TokenType::WHITESPACE)) {
    _out << " ";
  }

  // LOG_DEBUG << "The lexer is done" << LOG_END;
  _out << "</p>";
  return _out.str();
}

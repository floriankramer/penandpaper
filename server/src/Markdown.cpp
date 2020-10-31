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

#include <iomanip>

#include "Logger.h"

const char *Markdown::TOKEN_TYPE_NAMES[] = {
    "WORD",       "ORDER_MARK",       "LIST_MARK",
    "LINE_BREAK", "FORCE_LINE_BREAK", "EMPTY_LINE",
    "WHITESPACE", "HEADING_HASHES",   "BRACKET"};

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
      if (!std::isspace(c) && !(c == '[' || c == '(' || c == ']' || c == ')')) {
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

Markdown::BracketMatcher::BracketMatcher() : TokenMatcher(TokenType::BRACKET) {}
void Markdown::BracketMatcher::step(char c) {
  // States:
  // -1  failure
  //  0 Accept any non whitespace
  //  1 accepts nothing
  switch (_state) {
    case 0:
      if (c == '[' || c == '(' || c == ']' || c == ')') {
        _matches = true;
        _state = 1;
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
  _token_matchers.emplace_back(std::make_unique<Markdown::BracketMatcher>());
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
    if (pos == start || match == nullptr) {
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

MdNode Markdown::process() {
  MdNode root;
  if (_lexer.isDone()) {
    return root;
  }

  // Parse all blocks at the beginning
  while (parseBlock(root)) {
  }
  std::unique_ptr<ParagraphMdNode> paragraph = nullptr;
  while (!_lexer.isDone()) {
    // If this is true check if the paragraph continues or is interrupted by
    // a block (list, table, etc.)
    bool check_for_block = false;
    // Skip trying to parse a line
    bool skip_line = false;

    size_t pos = _lexer.pos();
    if (paragraph == nullptr) {
      // Skip any whitespace outside of a paragraph
      while (_lexer.accept(TokenType::WHITESPACE)) {
      }
    }
    if (_lexer.peek(TokenType::HEADING_HASHES)) {
      // If we can parse a heading skip the line and check for a block following
      // the heading
      std::ostringstream s;
      if (parseHashesHeading(root)) {
        if (paragraph != nullptr) {
          paragraph.release();
          paragraph = nullptr;
        }
        check_for_block = true;
        skip_line = true;
      }
    }
    if (!skip_line) {
      if (paragraph == nullptr) {
        paragraph = std::make_unique<ParagraphMdNode>();
        root.addChild(paragraph.get());
      }
      parseLine(*paragraph);
    }
    if (_lexer.isDone()) {
      break;
    }
    if (_lexer.accept(TokenType::EMPTY_LINE)) {
      if (paragraph != nullptr) {
        paragraph.release();
        paragraph = nullptr;
      }
      check_for_block = true;
    } else if (_lexer.accept(TokenType::FORCE_LINE_BREAK)) {
      if (paragraph != nullptr) {
        paragraph->addChild(new LineBreakMdNode());
      } else {
        root.addChild(new LineBreakMdNode());
      }
      check_for_block = true;
    } else if (_lexer.accept(TokenType::LINE_BREAK)) {
      check_for_block = true;
    }
    if (check_for_block) {
      if (parseBlock(root)) {
        if (paragraph != nullptr) {
          paragraph.release();
          paragraph = nullptr;
        }
      }
      // parse all consecutive blocks
      while (parseBlock(root))
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
  if (paragraph != nullptr) {
    paragraph.release();
  }
  return root;
}

bool Markdown::parseBlock(MdNode &parent) {
  return parseUnorderedList(parent) || parseOrderedList(parent);
}

bool Markdown::parseUnorderedList(MdNode &parent, int indent_level) {
  return parseList(parent, TokenType::LIST_MARK, false, indent_level);
}

bool Markdown::parseOrderedList(MdNode &parent, int indent_level) {
  return parseList(parent, TokenType::ORDER_MARK, true, indent_level);
}

bool Markdown::parseList(MdNode &parent, TokenType list_mark, bool is_ordered,
                         int indent_level) {
  // If this is not a list we can pop the lexer state
  _lexer.push();

  // The node that stores all list items
  std::unique_ptr<ListMdNode> list = std::make_unique<ListMdNode>(is_ordered);
  // The current list item. Nullptr if the last item was finished (e.g.
  // by beginning a sublist) but no new list_mark has been read yet.
  std::unique_ptr<ListItemMdNode> item = nullptr;
  while (!_lexer.isDone()) {
    // Check the indent of the next line. It either matches our indent,
    // starts a sublist or ends this list.
    int next_line_indent = 0;
    // If the line starts with whitespace read the indent out of the whitepsace.
    // We mustn't consume the whitespace yet, as it might start a sublist,
    // which the needs to parse it.
    if (_lexer.peek(TokenType::WHITESPACE)) {
      std::string val = _lexer.current().value;
      next_line_indent = indentLevel(val);
    }
    if (item == nullptr && list->children().empty() &&
        next_line_indent != indent_level) {
      // We are at the very beginning of the list and the indent is wrong.
      // Abort parsing.
      break;
    }
    if (next_line_indent == indent_level + 1) {
      // The indent is increased by one. That might start a sublist
      // Finish the current item (if it exists).
      if (item != nullptr) {
        list->addChild(item.release());
      }
      item = nullptr;

      // Check if we have a sublist
      if (!parseOrderedList(*list.get(), indent_level + 1) &&
          !parseUnorderedList(*list.get(), indent_level + 1)) {
        // The indentation did not inidicate a sublist. Simply close this list.
        break;
      } else {
        // Start parsing the next line of this list.
        continue;
      }
    } else if (next_line_indent != indent_level) {
      // If we still have an open item, finish it
      if (item != nullptr) {
        list->addChild(item.release());
      }
      // This list ends
      break;
    }
    // Now we can consume the whitespace
    if (_lexer.peek(TokenType::WHITESPACE)) {
      // Consume the whitespace
      _lexer.any();
    }

    // Check if a new list item is started
    if (_lexer.accept(list_mark)) {
      if (item != nullptr) {
        list->addChild(item.release());
      }
      item = std::make_unique<ListItemMdNode>();

      // Remove any whitespace after the list mark and before the lists content
      while (_lexer.accept(TokenType::WHITESPACE)) {
      }
    } else if (item == nullptr) {
      // This is not simply an item continuation as there is no item to continue
      break;
    }

    // Read a line and add it to item
    parseLine(*item.get());

    if (_lexer.isDone() || _lexer.accept(TokenType::EMPTY_LINE)) {
      // The list is terminated, add any open items and finish parsing
      if (item != nullptr) {
        list->addChild(item.release());
      }
      break;
    }

    // Consume the token ending the line.
    _lexer.any();
  }
  if (list->children().size() > 0) {
    // We succesfully parsed a list with at least one item
    parent.addChild(list.release());
    return true;
  } else {
    // We weren't able to parse the list
    _lexer.pop();
    return false;
  }
}

void Markdown::parseLine(MdNode &parent) {
  std::ostringstream buf;
  // assume we'll need to create a new text node
  bool is_new_text = true;
  std::unique_ptr<TextMdNode> text_local = std::make_unique<TextMdNode>();
  TextMdNode *text = text_local.get();

  // check if we can extend an old node
  if (parent.children().size() > 0 &&
      parent.children().back()->type() == MdNodeType::TEXT) {
    // merge the texts instead of creating a new text node
    is_new_text = false;
    text = static_cast<TextMdNode *>(parent.children().back().get());
  }

  // read a line
  while (!_lexer.isDone()) {
    if (_lexer.peekLineEnd()) {
      break;
    } else if (_lexer.accept(TokenType::WHITESPACE)) {
      buf << " ";
    } else {
      if (_lexer.peek(TokenType::BRACKET)) {
        MdNode tmp;
        if (parseLink(tmp)) {
          // Add the text to the node
          if (is_new_text) {
            text->text() = buf.str();
            parent.addChild(text);
            text_local.release();
          } else {
            text->text() += " " + buf.str();
          }
          // we now need a new text node
          is_new_text = true;
          text_local = std::make_unique<TextMdNode>();
          text = text_local.get();

          buf = std::ostringstream();
          // add the link to the parent
          parent.addChild(tmp.releaseChild(0));
          continue;
        }
      }
      buf << _lexer.any().value;
    }
  }
  if (buf.tellp() > 0) {
    if (is_new_text) {
      text->text() = buf.str();
      parent.addChild(text);
      text_local.release();
    } else {
      text->text() += " " + buf.str();
    }
  }
}

bool Markdown::parseHashesHeading(MdNode &parent) {
  _lexer.push();
  if (!_lexer.peek(TokenType::HEADING_HASHES)) {
    _lexer.pop();
    return false;
  }
  std::unique_ptr<HeadingMdNode> heading = std::make_unique<HeadingMdNode>();
  std::unique_ptr<TextMdNode> text = std::make_unique<TextMdNode>();
  std::ostringstream buf;

  Token hashes = _lexer.any();
  int heading_level = hashes.value.size();
  heading->level() = heading_level;
  while (!_lexer.isDone()) {
    if (_lexer.accept(hashes) || _lexer.acceptLineEnd()) {
      // the heading is done
      break;
    }
    if (_lexer.accept(TokenType::WHITESPACE)) {
      if (buf.tellp() > 0) {
        // Don't prefix the heading with whitespace
        buf << ' ';
      }
    } else {
      buf << _lexer.any().value;
    }
  }
  text->text() = buf.str();
  heading->addChild(text.release());
  parent.addChild(heading.release());
  return true;
}

bool Markdown::parseLink(MdNode &parent) {
  _lexer.push();
  // Links have to start with a [
  if (!_lexer.accept("[")) {
    _lexer.pop();
    return false;
  }
  // read the name
  std::stringstream name;
  while (!_lexer.accept("]")) {
    if (_lexer.isDone() || _lexer.acceptLineEnd()) {
      // this is not a link
      _lexer.pop();
      return false;
    }
    // Read the name verbatim
    name << _lexer.current().value;
    _lexer.any();
  }
  std::string namestr = name.str();
  size_t attr_delim_pos = namestr.find(':');
  if (attr_delim_pos != std::string::npos) {
    // this is an attr ref
    std::unique_ptr<AttrRefMdNode> ref = std::make_unique<AttrRefMdNode>();
    ref->id() = namestr.substr(0, attr_delim_pos);
    ref->predicate() = namestr.substr(attr_delim_pos + 1);
    parent.addChild(ref.release());
    return true;
  }

  // Look for the link target
  while (!_lexer.isDone()) {
    if (!_lexer.accept(TokenType::WHITESPACE)) {
      break;
    }
  }
  if (_lexer.isDone() || !_lexer.accept("(")) {
    _lexer.pop();
    return false;
  }
  // read the target
  std::ostringstream target;
  while (!_lexer.accept(")")) {
    if (_lexer.isDone() || _lexer.acceptLineEnd()) {
      // this is not a link
      _lexer.pop();
      return false;
    }
    // Read the name verbatim
    target << _lexer.current().value;
    _lexer.any();
  }
  std::unique_ptr<LinkMdNode> link = std::make_unique<LinkMdNode>();
  link->text() = namestr;
  link->target() = target.str();
  parent.addChild(link.release());
  return true;
}

int Markdown::indentLevel(const std::string &s) {
  size_t num_spaces = 0;
  // Parse from the right, as a newline would terminate the indent level
  // increase e.g. (with _ instead of spaces for readability)
  // ____
  // ____
  // Should be indent level 1, not 2
  for (size_t i = s.size(); i > 0; i--) {
    if (!std::isspace(s[i - 1]) || s[i - 1] == '\n') {
      return num_spaces / 4;
    }
    num_spaces++;
  }
  return num_spaces / 4;
}

#include "MarkdownNode.h"

#include <iostream>
#include <string>

const char *MD_NODE_TYPE_NAMES[] = {"ROOT",      "HEADING",   "TEXT",
                                    "LINK",      "ATTR_REF",  "LIST",
                                    "LIST_ITEM", "PARAGRAPH", "LINE_BREAK"};

// =============================================================================
// MdNode
// =============================================================================

MdNodeType MdNode::type() const { return MdNodeType::ROOT; }
void MdNode::toHTML(std::ostream &out,
                    MdLookupAttribute_t lookup_attribute) const {
  childrenToHtml(out, lookup_attribute);
}

void MdNode::traverse(std::function<void(MdNode &)> callback) {
  if (callback == nullptr) {
    return;
  }
  std::vector<MdNode *> to_process;
  to_process.push_back(this);
  while (!to_process.empty()) {
    MdNode *n = to_process.back();
    to_process.pop_back();
    callback(*n);
    for (MdNodePtr &c : n->_children) {
      to_process.push_back(c.get());
    }
  }
}

void MdNode::traverse(std::function<void(const MdNode &)> callback) const {
  if (callback == nullptr) {
    return;
  }
  std::vector<const MdNode *> to_process;
  to_process.push_back(this);
  while (!to_process.empty()) {
    const MdNode *n = to_process.back();
    to_process.pop_back();
    callback(*n);
    for (const MdNodePtr &c : n->_children) {
      to_process.push_back(c.get());
    }
  }
}

void MdNode::printTree() const {
  std::vector<std::pair<int, const MdNode *>> to_process;
  to_process.push_back(std::make_pair(0, this));

  while (!to_process.empty()) {
    const std::pair<int, const MdNode *> p = to_process.back();
    to_process.pop_back();
    for (int i = 0; i < p.first; ++i) {
      std::cout << "  ";
    }
    std::cout << MD_NODE_TYPE_NAMES[int(p.second->type())] << " "
              << p.second->repr() << std::endl;

    for (size_t i = p.second->children().size(); i > 0; i--) {
      const MdNodePtr &c = p.second->_children[i - 1];
      to_process.push_back(std::make_pair(p.first + 1, c.get()));
    }
  }
}

std::string MdNode::repr() const { return ""; }

void MdNode::addChild(MdNode *node) {
  _children.emplace_back();
  _children.back().reset(node);
}

void MdNode::clearChildren() { _children.clear(); }

void MdNode::eraseChild(size_t pos) {
  _children.erase(_children.begin() + pos);
}

MdNode *MdNode::releaseChild(size_t pos) {
  MdNode *child = _children[pos].release();
  eraseChild(pos);
  return child;
}

void MdNode::insertChildren(const std::vector<MdNodePtr>::const_iterator &pos,
                            const std::vector<MdNode *>::const_iterator &begin,
                            const std::vector<MdNode *>::const_iterator &end) {
  size_t num_new = end - begin;
  size_t off = pos - _children.begin();
  // Create a set of empty unique ptrs
  _children.resize(_children.size() + num_new);
  // Fill the unique ptrs with the new data, copyin away the old data
  auto src = begin;
  for (size_t i = off; i < off + num_new; ++i) {
    _children[i + num_new] = MdNodePtr(_children[i].release());
    _children[i].reset(*src);
    src++;
  }
}

void MdNode::insertChild(const std::vector<MdNodePtr>::const_iterator &pos,
                         MdNode *node) {
  _children.insert(pos, MdNodePtr(node));
}

const std::vector<MdNodePtr> &MdNode::children() const { return _children; }

void MdNode::childrenToHtml(std::ostream &out,
                            MdLookupAttribute_t lookup_attribute) const {
  if (_children.size() > 0) {
    out << " ";
  }
  for (const MdNodePtr &n : _children) {
    n->toHTML(out, lookup_attribute);
    out << " ";
  }
}

// =============================================================================
// HeadingMdNode
// =============================================================================

HeadingMdNode::HeadingMdNode() : _level(0) {}
HeadingMdNode::HeadingMdNode(int level) : _level(level) {}

void HeadingMdNode::toHTML(std::ostream &out,
                           MdLookupAttribute_t lookup_attribute) const {
  out << "<h" << _level << '>';
  childrenToHtml(out, lookup_attribute);
  out << "</h" << _level << ">\n";
}

MdNodeType HeadingMdNode::type() const { return MdNodeType::HEADING; }

int &HeadingMdNode::level() { return _level; }

std::string HeadingMdNode::repr() const { return "h" + std::to_string(_level); }

// =============================================================================
// TextMdNode
// =============================================================================

TextMdNode::TextMdNode() : _text() {}
TextMdNode::TextMdNode(const std::string &text) : _text(text) {}

void TextMdNode::toHTML(std::ostream &out,
                        MdLookupAttribute_t lookup_attribute) const {
  out << _text;
  childrenToHtml(out, lookup_attribute);
}

MdNodeType TextMdNode::type() const { return MdNodeType::TEXT; }

std::string &TextMdNode::text() { return _text; }

std::string TextMdNode::repr() const {
  if (_text.size() > 16) {
    return _text.substr(0, 16) + "...";
  } else {
    return _text;
  }
}

// =============================================================================
// LinkMdNode
// =============================================================================

LinkMdNode::LinkMdNode() : _text(), _target() {}
LinkMdNode::LinkMdNode(const std::string &text, const std::string &target)
    : _text(text), _target(target) {}

void LinkMdNode::toHTML(std::ostream &out,
                        MdLookupAttribute_t lookup_attribute) const {
  out << "<a href=\"" << _target << "\">" << _text << "</a>";
  childrenToHtml(out, lookup_attribute);
}

MdNodeType LinkMdNode::type() const { return MdNodeType::LINK; }

std::string &LinkMdNode::text() { return _text; }
std::string &LinkMdNode::target() { return _target; }

const std::string &LinkMdNode::text() const { return _text; }
const std::string &LinkMdNode::target() const { return _target; }

std::string LinkMdNode::repr() const {
  if (_target.size() > 16) {
    return _target.substr(0, 16) + "...";
  } else {
    return _target;
  }
}
// =============================================================================
// AttrRefMdNode
// =============================================================================

AttrRefMdNode::AttrRefMdNode() : _id(), _predicate() {}
AttrRefMdNode::AttrRefMdNode(const std::string &id,
                             const std::string &predicate)
    : _id(id), _predicate(predicate) {}

void AttrRefMdNode::toHTML(std::ostream &out,
                           MdLookupAttribute_t lookup_attribute) const {
  if (lookup_attribute == nullptr) {
    out << "[no context to resolve " << _id << ":" << _predicate << "]";
  } else {
    out << lookup_attribute(_id, _predicate);
  }
  childrenToHtml(out, lookup_attribute);
}

MdNodeType AttrRefMdNode::type() const { return MdNodeType::ATTR_REF; }

std::string &AttrRefMdNode::id() { return _id; }
std::string &AttrRefMdNode::predicate() { return _predicate; }

std::string AttrRefMdNode::repr() const {
  std::string target = _id + ":" + _predicate;
  if (target.size() > 16) {
    return target.substr(0, 16) + "...";
  } else {
    return target;
  }
}

// =============================================================================
// ListMdNode
// =============================================================================

ListMdNode::ListMdNode() : _is_ordered(false) {}

ListMdNode::ListMdNode(bool is_ordered) : _is_ordered(is_ordered) {}

void ListMdNode::toHTML(std::ostream &out,
                        MdLookupAttribute_t lookup_attribute) const {
  std::string lm = _is_ordered ? "ol" : "ul";
  out << '<' << lm << ">\n";
  childrenToHtml(out, lookup_attribute);
  out << "</" << lm << ">\n";
}

MdNodeType ListMdNode::type() const { return MdNodeType::LIST; }

bool &ListMdNode::isOrdered() { return _is_ordered; }

// =============================================================================
// ListItemMdNode
// =============================================================================

ListItemMdNode::ListItemMdNode() {}

void ListItemMdNode::toHTML(std::ostream &out,
                            MdLookupAttribute_t lookup_attribute) const {
  out << "<li>";
  childrenToHtml(out, lookup_attribute);
  out << "</li>\n";
}

MdNodeType ListItemMdNode::type() const { return MdNodeType::LIST_ITEM; }

// =============================================================================
// ParagraphMdNode
// =============================================================================

ParagraphMdNode::ParagraphMdNode() {}
void ParagraphMdNode::toHTML(std::ostream &out,
                             MdLookupAttribute_t lookup_attribute) const {
  out << "<p>";
  childrenToHtml(out, lookup_attribute);
  out << "</p>\n";
}
MdNodeType ParagraphMdNode::type() const { return MdNodeType::PARAGRAPH; }

// =============================================================================
// LineBreakMdNode
// =============================================================================

LineBreakMdNode::LineBreakMdNode() {}
void LineBreakMdNode::toHTML(std::ostream &out,
                             MdLookupAttribute_t lookup_attribute) const {
  out << "<br/>\n";
  childrenToHtml(out, lookup_attribute);
}
MdNodeType LineBreakMdNode::type() const { return MdNodeType::LINE_BREAK; }

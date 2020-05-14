#pragma once

#include <functional>
#include <memory>
#include <ostream>
#include <vector>

enum class MdNodeType {
  ROOT,
  HEADING,
  TEXT,
  LINK,
  ATTR_REF,
  LIST,
  LIST_ITEM,
  PARAGRAPH,
  LINE_BREAK
};

extern const char *MD_NODE_TYPE_NAMES[];

class MdNode;

using MdLookupAttribute_t =
    std::function<std::string(const std::string &, const std::string &)>;
using MdNodePtr = std::unique_ptr<MdNode>;

class MdNode {
 public:
  virtual MdNodeType type() const;
  virtual void toHTML(std::ostream &out,
                      MdLookupAttribute_t lookup_attribute = nullptr) const;

  void traverse(std::function<void(MdNode &)> callback);
  void traverse(std::function<void(const MdNode &)> callback) const;

  void addChild(MdNode *node);
  void clearChildren();
  void eraseChild(size_t pos);
  MdNode *releaseChild(size_t pos);
  void insertChildren(const std::vector<MdNodePtr>::const_iterator &pos,
                      const std::vector<MdNode *>::const_iterator &begin,
                      const std::vector<MdNode *>::const_iterator &end);

  void insertChild(const std::vector<MdNodePtr>::const_iterator &pos,
                   MdNode *node);
  const std::vector<MdNodePtr> &children() const;

  void printTree() const;
  virtual std::string repr() const;

 protected:
  void childrenToHtml(std::ostream &out,
                      MdLookupAttribute_t lookup_attribute = nullptr) const;
  std::vector<MdNodePtr> _children;
};

class HeadingMdNode : public MdNode {
 public:
  HeadingMdNode();
  HeadingMdNode(int level);
  virtual void toHTML(std::ostream &out, MdLookupAttribute_t lookup_attribute =
                                             nullptr) const override;
  virtual MdNodeType type() const override;

  int &level();
  virtual std::string repr() const override;

 private:
  int _level;
};

class TextMdNode : public MdNode {
 public:
  TextMdNode();
  TextMdNode(const std::string &text);
  virtual void toHTML(std::ostream &out, MdLookupAttribute_t lookup_attribute =
                                             nullptr) const override;
  virtual MdNodeType type() const override;

  std::string &text();
  virtual std::string repr() const override;

 private:
  std::string _text;
};

class LinkMdNode : public MdNode {
 public:
  LinkMdNode();
  LinkMdNode(const std::string &text, const std::string &target);
  virtual void toHTML(std::ostream &out, MdLookupAttribute_t lookup_attribute =
                                             nullptr) const override;
  virtual MdNodeType type() const override;

  std::string &text();
  std::string &target();

  const std::string &text() const;
  const std::string &target() const;
  virtual std::string repr() const override;

 private:
  std::string _text;
  std::string _target;
};

class AttrRefMdNode : public MdNode {
 public:
  AttrRefMdNode();
  AttrRefMdNode(const std::string &id, const std::string &predicate);
  virtual void toHTML(std::ostream &out, MdLookupAttribute_t lookup_attribute =
                                             nullptr) const override;
  virtual MdNodeType type() const override;

  std::string &id();
  std::string &predicate();
  virtual std::string repr() const override;

 private:
  std::string _id;
  std::string _predicate;
};

class ListMdNode : public MdNode {
 public:
  ListMdNode();
  ListMdNode(bool is_ordered);
  virtual void toHTML(std::ostream &out, MdLookupAttribute_t lookup_attribute =
                                             nullptr) const override;
  virtual MdNodeType type() const override;

  bool &isOrdered();

 private:
  bool _is_ordered;
};

class ListItemMdNode : public MdNode {
 public:
  ListItemMdNode();
  virtual void toHTML(std::ostream &out, MdLookupAttribute_t lookup_attribute =
                                             nullptr) const override;
  virtual MdNodeType type() const override;
};

class ParagraphMdNode : public MdNode {
 public:
  ParagraphMdNode();
  virtual void toHTML(std::ostream &out, MdLookupAttribute_t lookup_attribute =
                                             nullptr) const override;
  virtual MdNodeType type() const override;
};

class LineBreakMdNode : public MdNode {
 public:
  LineBreakMdNode();
  virtual void toHTML(std::ostream &out, MdLookupAttribute_t lookup_attribute =
                                             nullptr) const override;
  virtual MdNodeType type() const override;
};

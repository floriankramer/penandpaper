#pragma once

#include <array>
#include <vector>

/**
 * @brief A B-Tree storing where every node has to have at least Order children
 * and may have at most 2*Order + 1 children.
 */
template <typename ValueType, int Order>
class BTree {
  struct Node {
    std::array<ValueType, 2 * Order> keys;
    std::array<Node *, 2 * Order + 1> children;
  };

 public:
  BTree();
  virtual ~BTree();

  void insert(const ValueType &v);
  void erase(const ValueType &v);

  ValueType &find(const ValueType &v);

 private:
  Node *_root;
};

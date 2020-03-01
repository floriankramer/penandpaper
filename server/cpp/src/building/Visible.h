#pragma once
#include <memory>
#include <vector>

class Visible {
 public:
  Visible();
  void setVisible(bool visible);

  void clearDependencies();
  void reveals(std::shared_ptr<Visible> other);

 private:
  bool is_visible;

  std::vector<std::shared_ptr<Visible>> reveals();
  std::vector<std::shared_ptr<Visible>> revealedBy();
};

#pragma once

#include <string>
#include <unordered_map>
#include <vector>

class QGramIndex {
  struct NumericMatch {
    uint64_t value;
    double score;
  };

 public:
  static constexpr int GRAM_SIZE = 3;
  static constexpr char PADDING_CHARACTER = 1;
  using ValueType = std::string;

  struct Match {
    ValueType value;
    double score;

    bool operator<(const Match &other) {
      return score < other.score;
    }
  };

  QGramIndex();
  virtual ~QGramIndex();

  std::vector<Match> query(const std::string &word);
  void add(const std::string &alias, const ValueType &value);
  void remove(const ValueType &value);

 private:
  std::vector<std::string> split(const std::string &word);
  std::vector<NumericMatch> merge(const std::vector<std::vector<uint64_t> *> &lists);
  std::vector<NumericMatch> zipper(const std::vector<NumericMatch> &matches,
                                   const std::vector<uint64_t> *list);

  std::vector<double> _vocab_num_qgrams;
  std::vector<ValueType> _vocabulary;
  std::unordered_map<ValueType, uint64_t> _reverse_vocab;

  std::unordered_map<std::string, std::vector<uint64_t>> _gram_map;
};

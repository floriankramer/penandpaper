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

  struct Entry {
    std::string alias;
    ValueType value;
  };

  struct Match {
    Entry value;
    double score;

    bool operator<(const Match &other) { return score < other.score; }
  };

  QGramIndex();
  virtual ~QGramIndex();

  std::vector<Match> query(const std::string &word);
  void add(const std::string &alias, const ValueType &value);
  void remove(const std::string &alias, const ValueType &value);

 private:
  std::vector<std::string> split(const std::string &word);
  std::vector<NumericMatch> merge(
      const std::vector<std::vector<uint64_t> *> &lists);
  std::vector<NumericMatch> zipper(const std::vector<NumericMatch> &matches,
                                   const std::vector<uint64_t> *list);

  std::string computeVocabKey(const std::string &alias, const ValueType &value);

  std::vector<double> _vocab_num_qgrams;
  std::vector<Entry> _vocabulary;
  std::unordered_map<std::string, uint64_t> _reverse_vocab;

  std::unordered_map<std::string, std::vector<uint64_t>> _gram_map;
};

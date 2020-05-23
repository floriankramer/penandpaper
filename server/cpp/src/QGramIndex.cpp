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
#include "QGramIndex.h"

#include <algorithm>

#include "Logger.h"

QGramIndex::QGramIndex() {}

QGramIndex::~QGramIndex() {}

std::vector<QGramIndex::Match> QGramIndex::query(const std::string &word) {
  std::vector<std::string> grams = split(word);
  std::vector<std::vector<uint64_t> *> occurences;
  for (const std::string &s : grams) {
    auto it = _gram_map.find(s);
    if (it != _gram_map.end()) {
      occurences.push_back(&it->second);
    }
  }
  std::vector<NumericMatch> num_matches = merge(occurences);

  // Normalize the scores, which currently are simply how many grams match
  // the grams of word. We divide it by the number of grams in the target, to
  // avoid long words getting higher scores and by the number of grams in grams,
  // to avoid short words being contained in grams matching with a score of 1.
  // The factor of 0.5 is just there to normalize to 1 for a perfect match.
  for (size_t i = 0; i < num_matches.size(); ++i) {
    num_matches[i].score /=
        (_vocab_num_qgrams[num_matches[i].value] + grams.size()) * 0.5;
  }

  std::sort(num_matches.begin(), num_matches.end(),
            [](const NumericMatch &n, const NumericMatch &n2) {
              return n.score > n2.score;
            });

  std::vector<Match> matches;
  matches.reserve(num_matches.size());
  for (const NumericMatch &m : num_matches) {
    matches.push_back({_vocabulary[m.value], m.score});
  }
  return matches;
}

std::vector<QGramIndex::NumericMatch> QGramIndex::merge(
    const std::vector<std::vector<uint64_t> *> &lists) {
  std::vector<NumericMatch> matches;
  if (lists.size() == 0) {
    return matches;
  }
  // initialize the matches with the first list
  for (uint64_t i : *lists[0]) {
    matches.push_back({i, 1});
  }
  // Do the n-way merge through a series of consecutive joins
  for (size_t i = 1; i < lists.size(); ++i) {
    matches = zipper(matches, lists[i]);
  }
  return matches;
}

std::vector<QGramIndex::NumericMatch> QGramIndex::zipper(
    const std::vector<NumericMatch> &matches,
    const std::vector<uint64_t> *list) {
  std::vector<NumericMatch> res;
  size_t pos_l = 0;
  size_t pos_r = 0;
  while (pos_l < matches.size() && pos_r < list->size()) {
    while (pos_l < matches.size() && matches[pos_l].value < (*list)[pos_r]) {
      res.push_back(matches[pos_l]);
      pos_l++;
    }
    while (pos_r < list->size() && pos_l < matches.size() &&
           matches[pos_l].value > (*list)[pos_r]) {
      res.push_back({(*list)[pos_r], 1});
      pos_r++;
    }
    while (pos_l < matches.size() && pos_r < list->size() &&
           matches[pos_l].value == (*list)[pos_r]) {
      // Values are unique in both lists, so simply writing one results
      // to the output suffices
      res.push_back({matches[pos_l].value, matches[pos_l].score + 1});
      pos_l++;
      pos_r++;
    }
  }
  while (pos_l < matches.size()) {
    res.push_back(matches[pos_l]);
    pos_l++;
  }
  while (pos_r < list->size()) {
    res.push_back({(*list)[pos_r], 1});
    pos_r++;
  }
  return res;
}

void QGramIndex::add(const std::string &alias, const ValueType &value) {
  std::vector<std::string> grams = split(alias);

  size_t id;
  auto vit = _reverse_vocab.find(value);
  if (vit == _reverse_vocab.end()) {
    id = _vocabulary.size();
    _vocabulary.push_back({alias, value});
    // TODO: It might make a lot more sense to not differentaite between alias
    // and value but instead do the alias mapping later on.
    _vocab_num_qgrams.push_back(grams.size());
    _reverse_vocab[computeVocabKey(alias, value)] = id;
  } else {
    id = vit->second;
  }

  for (const std::string &gram : grams) {
    std::vector<size_t> &v = _gram_map[gram];
    // Dont map a gram twice to the same value. When matching inputs
    // we will still match both grams in the input with the single occurrence
    // in this list.
    if (std::find(v.begin(), v.end(), id) == v.end()) {
      v.push_back(id);
      // keep the vectors sorted
      std::sort(v.begin(), v.end());
    }
  }
}

void QGramIndex::remove(const std::string &alias, const ValueType &value) {

  auto vocab_it = _reverse_vocab.find(computeVocabKey(alias, value));
  if (vocab_it == _reverse_vocab.end()) {
    // The value isn't known
    return;
  }
  size_t v_id = vocab_it->second;

  std::vector<std::string> grams = split(value);

  for (const std::string &gram : grams) {
    auto it = _gram_map.find(gram);
    if (it != _gram_map.end()) {
      std::vector<uint64_t> &values = it->second;
      // If the vector contains the value
      values.erase(std::remove(values.begin(), values.end(), v_id), values.end());
      if (values.empty()) {
        _gram_map.erase(gram);
      }
    }
  }
  // The vocabulary is not updated. If that ever becomes an issue we'll have
  // to start doing that.
}

std::vector<std::string> QGramIndex::split(const std::string &word) {
  std::vector<std::string> grams;
  grams.reserve(word.size() + 2 * GRAM_SIZE - 2);
  std::string buf(GRAM_SIZE, PADDING_CHARACTER);
  for (size_t i = 0; i < word.size(); ++i) {
    // shift the characters in the buffer
    for (size_t j = 0; j + 1 < GRAM_SIZE; ++j) {
      buf[j] = buf[j + 1];
    }
    // the new character in the buffer is the nect character in the word
    buf[GRAM_SIZE - 1] = std::tolower(word[i]);
    grams.push_back(buf);
  }
  for (size_t i = 1; i < GRAM_SIZE; ++i) {
    // rotate the characters in the buffer
    for (size_t j = 0; j + 1 < GRAM_SIZE; ++j) {
      buf[j] = buf[j + 1];
    }
    // the new character in the buffer is the PADDING_CHARACTER
    buf[GRAM_SIZE - 1] = PADDING_CHARACTER;
    grams.push_back(buf);
  }
  return grams;
}

std::string QGramIndex::computeVocabKey(const std::string &alias,
                                        const ValueType &value) {
  return alias + char(1) + value;
}

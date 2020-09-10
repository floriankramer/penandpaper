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
#include "Util.h"

#include <cmath>
#include <cstring>

#include "Logger.h"

namespace util {

char BASE64_ENCODER[] = {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K',
                         'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
                         'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g',
                         'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
                         's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2',
                         '3', '4', '5', '6', '7', '8', '9', '-', '_'};

uint8_t BASE64_DECODER[] = {
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  62, 0,  0,  0,  63, 52, 53, 54, 55, 56, 57, 58, 59, 60,
    61, 0,  0,  0,  0,  0,  0,  0,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10,
    11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 0,  0,  0,  0,
    0,  0,  26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42,
    43, 44, 45, 46, 47, 48, 49, 50, 51, 0,  0,  0,  0,  0};

std::vector<char> base16Encode(const void *data, size_t length) {
  std::vector<char> encoded;
  encoded.resize(length * 2);
  unsigned char *usrc = (unsigned char *)(data);
  for (size_t i = 0; i < length; ++i) {
    uint8_t upper = usrc[i] >> 4;
    if (upper > 9) {
      encoded[i * 2] = 'A' + (upper - 10);
    } else {
      encoded[i * 2] = '0' + upper;
    }
    uint8_t lower = usrc[i] & 0xF;
    if (lower > 9) {
      encoded[i * 2 + 1] = 'A' + (lower - 10);
    } else {
      encoded[i * 2 + 1] = '0' + lower;
    }
  }
  return encoded;
}

void base64Encode3(const void *src, char *dest) {
  unsigned char *usrc = (unsigned char *)(src);
  // The first 6 bits
  int codepoint1 = ((*usrc) & 0b11111100) >> 2;
  // The second 6 bits
  int codepoint2 = (((*usrc) & 0b00000011) << 4);
  usrc++;
  codepoint2 |= (((*usrc) & 0b11110000) >> 4);
  // the third 6 bits
  int codepoint3 = ((*usrc) & 0b00001111) << 2;
  usrc++;
  codepoint3 |= ((*usrc) & 0b11000000) >> 6;
  // the remaining 6 bits
  int codepoint4 = ((*usrc) & 0b00111111);
  *dest = BASE64_ENCODER[codepoint1];
  dest++;
  *dest = BASE64_ENCODER[codepoint2];
  dest++;
  *dest = BASE64_ENCODER[codepoint3];
  dest++;
  *dest = BASE64_ENCODER[codepoint4];
}

std::string base64EncodeStr(const void *data, size_t length) {
  std::vector<char> encoded = base64Encode(data, length);
  return std::string(encoded.begin(), encoded.end());
}

std::vector<char> base64Encode(const void *data, size_t length) {
  const uint8_t *in = (uint8_t *)(data);
  const uint8_t *end = (uint8_t *)(data) + length;
  // 3 bytes are encoded to 4 characters
  // The encoded length with the required padding
  size_t encoded_length = (length + ((3 - (length % 3)) % 3)) / 3 * 4;
  std::vector<char> encoded;
  encoded.resize(encoded_length, '=');

  char *out = encoded.data();
  // encode bytes in pairs of three
  for (const uint8_t *p = in; p + 2 < end; p += 3) {
    base64Encode3(p, out);
    out += 4;
  }
  // if the number of bytes does not divide evenly into threes process the
  // remaining bytes
  if (length % 3 != 0) {
    uint8_t buffer[3];
    memset(buffer, 0, 3);
    size_t remaining = length % 3;
    for (size_t i = 0; i < remaining; ++i) {
      buffer[i] = *(end - remaining + i);
    }
    base64Encode3(buffer, out);
    // set the unused characters to =
    size_t used = remaining * 4;
    used = (used + ((3 - (used % 3)) % 3)) / 3;
    for (size_t i = used; i < 4; ++i) {
      *(out + i) = '=';
    }
  }
  return encoded;
}

void base64Decode3(const char *src, void *dest) {
  uint8_t *udest = (uint8_t *)(dest);
  uint8_t cp1 = BASE64_DECODER[*src];
  uint8_t cp2 = BASE64_DECODER[*(src + 1)];
  uint8_t cp3 = BASE64_DECODER[*(src + 2)];
  uint8_t cp4 = BASE64_DECODER[*(src + 3)];

  *udest = (cp1 << 2) | ((cp2 & 0b00110000) >> 4);
  udest++;
  *udest = ((cp2 & 0b00001111) << 4) | ((cp3 & 0b00111100) >> 2);
  udest++;
  *udest = ((cp3 & 0b00000011) << 6) | cp4;
}

std::vector<char> base64Decode(const char *data, size_t length) {
  const char *in = data;
  const char *end = data + length;

  size_t decoded_length = length * 3;
  decoded_length = (decoded_length + ((4 - (decoded_length % 4)) % 4)) / 4;
  std::vector<char> decoded;
  decoded.resize(decoded_length);
  char *out = decoded.data();
  for (const char *p = in; p + 3 < end; p += 4) {
    base64Decode3(p, out);
    out += 3;
  }
  if (length % 4 != 0) {
    LOG_WARN << "base64Decode: base64 string does not have a length which is a "
                "multiple of 4"
             << LOG_END;
    return decoded;
  }
  // figure out the actual lenght of data based upon the padding
  size_t num_padding = 0;
  for (const char *p = end - 1; p >= in && p > end - 5; p--) {
    if (*p == '=') {
      num_padding++;
    }
  }
  if (decoded.size() >= num_padding) {
    decoded.resize(decoded.size() - num_padding);
  } else {
    LOG_WARN << "base64Decode: Not enough bytes in output to remove padding."
             << LOG_END;
  }
  return decoded;
}

std::vector<std::string> splitStringWs(const std::string &s) {
  std::vector<std::string> res;
  size_t pos = 0;
  // skip leading whitespace
  while (pos < s.size() && std::isspace(s[pos])) {
    pos++;
  }
  while (pos < s.size()) {
    size_t start = pos;
    // seek to the end of the non whitespace
    while (pos < s.size() && !std::isspace(s[pos])) {
      pos++;
    }
    res.push_back(s.substr(start, pos - start));
    // skip the next bit of whitespace
    while (pos < s.size() && std::isspace(s[pos])) {
      pos++;
    }
  }
  return res;
}

std::string firstWords(const std::string &s, size_t num_words,
                       bool with_trailing_ws) {
  size_t pos = 0;
  size_t word_count = 0;
  // skip leading whitespace
  while (pos < s.size() && std::isspace(s[pos])) {
    pos++;
  }
  if (num_words == 0) {
    return s.substr(0, pos);
  }
  while (pos < s.size()) {
    size_t start = pos;
    // seek to the end of the non whitespace
    while (pos < s.size() && !std::isspace(s[pos])) {
      pos++;
    }
    word_count++;
    if (!with_trailing_ws && word_count >= num_words) {
      break;
    }
    // skip the next bit of whitespace
    while (pos < s.size() && std::isspace(s[pos])) {
      pos++;
    }
    // Include the trailing whitspace
    if (with_trailing_ws && word_count >= num_words) {
      break;
    }
  }
  return s.substr(0, pos);
}

}  // namespace util

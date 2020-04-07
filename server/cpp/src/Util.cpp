#include "Util.h"

#include "Logger.h"
namespace util {

char BASE64_ENCODER[] = {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K',
                         'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
                         'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g',
                         'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
                         's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2',
                         '3', '4', '5', '6', '7', '8', '9', '+', '/'};

uint8_t BASE64_DECODER[] = {
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  62, 0,  0,  0,  63, 52, 53, 54, 55, 56, 57, 58, 59, 60,
    61, 0,  0,  0,  0,  0,  0,  0,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10,
    11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 0,  0,  0,  0,
    0,  0,  26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42,
    43, 44, 45, 46, 47, 48, 49, 50, 51, 0,  0,  0,  0,  0};

std::vector<char> base64Encode(const void *data, size_t length) {
  const uint8_t *in = (uint8_t *)(data);
  const uint8_t *end = (uint8_t *)(data) + length;
  // 3 bytes are encoded to 4 characters
  // The encoded length with the required padding
  size_t encoded_length = (length / 3 + ((3 - (length % 3)) % 3)) * 4;
  LOG_DEBUG << "Encoding " << length << " bytes to " << encoded_length
            << "characters." << LOG_END;
  std::vector<char> encoded;
  encoded.resize(encoded_length, '=');

  size_t out_pos = 0;
  size_t bit_pos = 0;
  int_fast8_t codepoint = 0;
  int_fast8_t bits = 0;
  while (in != end) {
    codepoint += int_fast8_t((*in) & (1 << bit_pos) > 0) << bits;
    bits++;
    if (bits == 6) {
      encoded[out_pos] = BASE64_ENCODER[codepoint];
      out_pos++;
      bits = 0;
      codepoint = 0;
    }
    bit_pos++;
    if (bit_pos == 8) {
      bit_pos = 0;
      in++;
    }
  }
  return encoded;
}

std::vector<char> base64Decode(const char *data, size_t length) {
  const char *in = data;
  const char *end = data + length;

  size_t decoded_length_bound = (length / 4) * 3 + 3;
  std::vector<char> decoded;
  decoded.reserve(decoded_length_bound);

  uint8_t codepoint = 0;
  char current_byte = 0;
  int_fast8_t bits = 0;
  size_t bit_pos = 0;
  while (in != end && *in != '=') {
    codepoint = BASE64_DECODER[*in];
    ++in;
    for (int i = 0; i < 6; ++i) {
      current_byte |= char((codepoint & (1 << i)) > 0) << bits;
      bits++;
      if (bits == 8) {
        decoded.push_back(current_byte);
        current_byte = 0;
        bits = 0;
      }
    }
  }
  return decoded;
}
}  // namespace util

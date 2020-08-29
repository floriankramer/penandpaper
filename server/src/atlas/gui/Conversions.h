#pragma once
#include <QImage>
#include "core/Image.h"

namespace atlas {

class Conversions {
public:
  static QImage qImageFromImage(const Image &img);
};

}

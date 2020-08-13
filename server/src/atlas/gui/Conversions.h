#pragma once
#include <QImage>
#include "rendering/Image.h"

namespace atlas {

class Conversions {
public:
  static QImage qImageFromImage(const Image &img);
};

}

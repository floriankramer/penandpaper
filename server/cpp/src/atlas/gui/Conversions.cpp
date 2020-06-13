#include "Conversions.h"

namespace atlas {

QImage Conversions::qImageFromImage(const Image &img) {
  QImage qimg(img.width(), img.height(), QImage::Format_ARGB32);
  for (int y = 0; y < qimg.height(); ++y) {
    QRgb *scanline = (QRgb *)qimg.scanLine(y);
    for (int x = 0; x < qimg.width(); ++x) {
      scanline[x] = qRgba(img(x, y).r, img(x, y).g, img(x, y).b, img(x, y).a);
    }
  }
  return qimg;
}

}  // namespace atlas

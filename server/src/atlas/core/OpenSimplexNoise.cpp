#include "OpenSimplexNoise.h"

#include <cmath>
#include <ctime>

OpenSimplexNoise::OpenSimplexNoise() : OpenSimplexNoise(time(NULL)) {}

OpenSimplexNoise::OpenSimplexNoise(long seed) {
  static const std::vector<Vec2> GRADIENTS_2D = generateGradients2D();
  uint16_t source[PSIZE];
  for (short i = 0; i < PSIZE; ++i) {
    source[i] = i;
  }
  for (int i = PSIZE - 1; i >= 0; i--) {
    seed = seed * 6364136223846793005L + 1442695040888963407L;
    int r = (int)((seed + 31) % (i + 1));
    if (r < 0) r += (i + 1);
    perm[i] = source[r];
    permGrad2[i] = GRADIENTS_2D[perm[i]];
    source[r] = source[i];
  }
}

double OpenSimplexNoise::noise2(double x, double y) const {
  const static std::vector<LatticePoint2D> LOOKUP_2D = generateLookup2D();

  double s = 0.366025403784439 * (x + y);
  double xs = x + s;
  double ys = y + s;

  double value = 0;

  // Get base points and offsets
  int xsb = std::floor(xs);
  int ysb = std::floor(ys);
  double xsi = xs - xsb;
  double ysi = ys - ysb;

  // Index to point list
  int a = xsi + ysi;
  int index = (a << 2) | int(xsi - ysi / 2 + 1 - a / 2.0) << 3 |
              int(ysi - xsi / 2 + 1 - a / 2.0) << 4;

  double ssi = (xsi + ysi) * -0.211324865405187;
  double xi = xsi + ssi;
  double yi = ysi + ssi;

  // Point contributions
  for (int i = 0; i < 4; ++i) {
    const LatticePoint2D &c = LOOKUP_2D[index + i];

    double dx = xi + c.dx;
    double dy = yi + c.dy;
    double attn = 2.0 / 3.0 - dx * dx - dy * dy;
    if (attn <= 0) continue;

    int pxm = (xsb + c.xsv) & PMASK;
    int pym = (ysb + c.ysv) & PMASK;
    Vec2 grad = permGrad2[perm[pxm] ^ pym];
    double extrapolation = grad.x * dx + grad.x * dy;
    attn *= attn;
    value += attn * attn * extrapolation;
  }
  return value;
}

OpenSimplexNoise::LatticePoint2D::LatticePoint2D(int xsv, int ysv)
    : xsv(xsv), ysv(ysv) {
  double ssv = (xsv + ysv) * -0.211324865405187;
  dx = -xsv - ssv;
  dy = -ysv - ssv;
}

std::vector<OpenSimplexNoise::LatticePoint2D>
OpenSimplexNoise::generateLookup2D() {
  std::vector<LatticePoint2D> lookup2d;
  lookup2d.reserve(8 * 4);
  for (int i = 0; i < 8; i++) {
    int i1, j1, i2, j2;
    if ((i & 1) == 0) {
      if ((i & 2) == 0) {
        i1 = -1;
        j1 = 0;
      } else {
        i1 = 1;
        j1 = 0;
      }
      if ((i & 4) == 0) {
        i2 = 0;
        j2 = -1;
      } else {
        i2 = 0;
        j2 = 1;
      }
    } else {
      if ((i & 2) != 0) {
        i1 = 2;
        j1 = 1;
      } else {
        i1 = 0;
        j1 = 1;
      }
      if ((i & 4) != 0) {
        i2 = 1;
        j2 = 2;
      } else {
        i2 = 1;
        j2 = 0;
      }
    }
    lookup2d.emplace_back(0, 0);
    lookup2d.emplace_back(1, 1);
    lookup2d.emplace_back(i1, j1);
    lookup2d.emplace_back(i2, j2);
  }
  return lookup2d;
}

std::vector<OpenSimplexNoise::Vec2> OpenSimplexNoise::generateGradients2D() {
  const static double N2 = 0.05481866495625118;
  const static Vec2 grad2[] = {
      Vec2{0.130526192220052 / N2, 0.99144486137381 / N2},
      Vec2{0.38268343236509 / N2, 0.923879532511287 / N2},
      Vec2{0.608761429008721 / N2, 0.793353340291235 / N2},
      Vec2{0.793353340291235 / N2, 0.608761429008721 / N2},
      Vec2{0.923879532511287 / N2, 0.38268343236509 / N2},
      Vec2{0.99144486137381 / N2, 0.130526192220051 / N2},
      Vec2{0.99144486137381 / N2, -0.130526192220051 / N2},
      Vec2{0.923879532511287 / N2, -0.38268343236509 / N2},
      Vec2{0.793353340291235 / N2, -0.60876142900872 / N2},
      Vec2{0.608761429008721 / N2, -0.793353340291235 / N2},
      Vec2{0.38268343236509 / N2, -0.923879532511287 / N2},
      Vec2{0.130526192220052 / N2, -0.99144486137381 / N2},
      Vec2{-0.130526192220052 / N2, -0.99144486137381 / N2},
      Vec2{-0.38268343236509 / N2, -0.923879532511287 / N2},
      Vec2{-0.608761429008721 / N2, -0.793353340291235 / N2},
      Vec2{-0.793353340291235 / N2, -0.608761429008721 / N2},
      Vec2{-0.923879532511287 / N2, -0.38268343236509 / N2},
      Vec2{-0.99144486137381 / N2, -0.130526192220052 / N2},
      Vec2{-0.99144486137381 / N2, 0.130526192220051 / N2},
      Vec2{-0.923879532511287 / N2, 0.38268343236509 / N2},
      Vec2{-0.793353340291235 / N2, 0.608761429008721 / N2},
      Vec2{-0.608761429008721 / N2, 0.793353340291235 / N2},
      Vec2{-0.38268343236509 / N2, 0.923879532511287 / N2},
      Vec2{-0.130526192220052 / N2, 0.99144486137381 / N2}};
  size_t num_grad_2 = sizeof(grad2) / sizeof(Vec2);

  std::vector<Vec2> gradients2D;
  gradients2D.reserve(PSIZE);
  for (int i = 0; i < PSIZE; i++) {
    gradients2D.push_back(grad2[i % num_grad_2]);
  }
  return gradients2D;
}

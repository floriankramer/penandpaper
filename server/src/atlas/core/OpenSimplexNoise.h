#ifndef OPEN_SIMPLEX_NOISE2S_H_
#define OPEN_SIMPLEX_NOISE2S_H_

#include <vector>
#include <cstdint>

/**
 * @brief OpenSimplexNoise2S from https://github.com/KdotJPG/OpenSimplex2/
 */
class OpenSimplexNoise {
  static constexpr int PSIZE = 2048;
  static constexpr int PMASK = 2047;

  struct Vec2 {
    double x;
    double y;
  };

  class LatticePoint2D {
  public:
    LatticePoint2D(int xsv, int ysv);

    int xsv;
    int ysv;

    double dx;
    double dy;
  };

public:
  OpenSimplexNoise();
  OpenSimplexNoise(long seed);

  double noise2(double x, double y) const;

private:
  uint16_t perm[PSIZE];
  Vec2 permGrad2[PSIZE];

  static std::vector<LatticePoint2D> generateLookup2D();
  static std::vector<Vec2> generateGradients2D();

};

#endif

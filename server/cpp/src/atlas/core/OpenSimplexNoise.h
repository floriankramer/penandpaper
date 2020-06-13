#ifndef OPEN_SIMPLEX_NOISE_H__
#define OPEN_SIMPLEX_NOISE_H__

/*
 * OpenSimplex (Simplectic) Noise in cpp.
 * Ported to C from Kurt Spencer's java implementation by Stephen M. Cameron
 * Ported to cpp from the c implementation used in the godot engine.
 *
 * v1.1 (October 6, 2014)
 * - Ported to C
 *
 * v1.1 (October 5, 2014)
 * - Added 2D and 4D implementations.
 * - Proper gradient sets for all dimensions, from a
 *   dimensionally-generalizable scheme with an actual
 *   rhyme and reason behind it.
 * - Removed default permutation array in favor of
 *   default seed.
 * - Changed seed-based constructor to be independent
 *   of any particular randomization library, so results
 *   will be the same when ported to other languages.
 */

#if ((__GNUC_STDC_INLINE__) || (__STDC_VERSION__ >= 199901L))
#include <stdint.h>
#define INLINE inline
#elif (defined(_MSC_VER) || defined(__GNUC_GNU_INLINE__))
#include <stdint.h>
#define INLINE __inline
#else
/* ANSI C doesn't have inline or stdint.h. */
#define INLINE
#endif

// Modified to work without allocating memory, also removed some unused
// function.

class OpenSimplexNoise {
 public:
  OpenSimplexNoise(int64_t seed);

  int open_simplex_noise(int64_t seed, struct osn_context *ctx);
  double noise2(double x, double y);
  double noise3(double x, double y, double z);
  double noise4(double x, double y, double z, double w);

 private:
  double extrapolate2(int xsb, int ysb, double dx, double dy);
  double extrapolate3(int xsb, int ysb, int zsb, double dx, double dy,
                      double dz);
  double extrapolate4(int xsb, int ysb, int zsb, int wsb, double dx, double dy,
                      double dz, double dw);

  static INLINE int fastFloor(double x) {
    int xi = (int)x;
    return x < xi ? xi - 1 : xi;
  }

  int16_t perm[256];
  int16_t permGradIndex3D[256];
};
#endif

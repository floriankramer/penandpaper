import TinySDF from 'tiny-sdf'

export class Glyph {
  character: string = ''
  uMin: number = 0
  uMax: number = 0
  vMin: number = 0
  vMax: number = 0
  advance: number = 0 // the distance from the origin to the origin of the next glyph
  bearingX: number = 0 // the distance from the origin to the left side of the glyph
  bearingY: number = 0 // the distance from the origin to the top of the glyph
  width: number = 0 // the width of the glyph
  height: number = 0 // the height of the glyph
}

export class Font {
  texture: WebGLTexture | null = null
  glyphs: Map<string, Glyph> = new Map()

  load (fontname: string, gl: WebGLRenderingContext) {
    if (this.texture != null) {
      // Don't load the font twice
      return
    }
    let fontsize = 24 // Font size in pixels
    let bufferSize = 3 // Whitespace buffer around a glyph in pixels
    let radius = 8 // How many pixels around the glyph shape to use for encoding distance
    let cutoff = 0.25 // How much of the radius (relative) is used for the inside part the glyph
    let minGlyph = 32 // The first glyph to render
    let maxGlyph = 126 // The last glyph to render

    let fontWeight = 'normal' // css font-weight
    let tinySDFGenerator = new TinySDF(fontsize, bufferSize, radius, cutoff, fontname, fontWeight)

    let numGlyphs = maxGlyph - minGlyph + 1
    let gridSize = Math.ceil(Math.sqrt(numGlyphs))
    let cellWidth = tinySDFGenerator.size
    let cellHeight = tinySDFGenerator.size
    let textureSize = gridSize * tinySDFGenerator.size
    textureSize = Math.round(Math.pow(2, Math.ceil(Math.log2(textureSize))))
    console.log('font cell size', cellWidth, cellHeight)
    console.log('Using a font texture of size', textureSize)

    let buffer = new Uint8Array(textureSize * textureSize * 4)
    let row = 0
    let col = 0
    // Render the glyphs into the texture and store their information
    for (let c = minGlyph; c <= maxGlyph; ++c) {
      let g = new Glyph()
      g.character = String.fromCharCode(c)

      g.uMin = col * cellWidth / textureSize
      g.uMax = (col + 1) * cellWidth / textureSize
      g.vMin = row * cellHeight / textureSize
      g.vMax = (row + 1) * cellHeight / textureSize
      // TODO read out these values from the font metrics of the font
      // Apparaently js doesn't support that. A possible solution is
      // rendering the font using freefont and writing the glyph
      // information using json
      g.width = cellWidth
      g.height = cellHeight
      g.bearingX = 0
      g.bearingY = cellHeight
      g.advance = cellWidth
      let sdf: ImageData = tinySDFGenerator.draw(g.character)
      for (let x = 0; x < cellWidth; ++x) {
        for (let y = 0; y < cellHeight; ++y) {
          // buffer[col * cellWidth + x + (y + row * cellHeight) * textureSize] = ((col + row) % 2 == 0) ? 128 : 64  // sdf[x + y * cellWidth]
          buffer[col * cellWidth + x + (y + row * cellHeight) * textureSize] = sdf.data[(x + y * cellWidth) * 4]
        }
      }
      // Advance to the next grid position
      col += 1
      if (col >= gridSize) {
        row += 1
        col = 0
      }
      this.glyphs.set(g.character, g)
    }

    this.texture = gl.createTexture()
    if (this.texture === null) {
      console.log('Error: Unable to create a texture for the fonts.')
      return
    }
    gl.bindTexture(gl.TEXTURE_2D, this.texture)
    // Upload the data into a luminance texture
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.LUMINANCE, textureSize, textureSize,
                  0, gl.LUMINANCE, gl.UNSIGNED_BYTE, buffer)
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR)
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)

    // {
    //   console.log('reading back the texture')
    //   var fb = gl.createFramebuffer()
    //   gl.bindFramebuffer(gl.FRAMEBUFFER, fb)
    //   gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, this.texture, 0)

    //   if (gl.checkFramebufferStatus(gl.FRAMEBUFFER) == gl.FRAMEBUFFER_COMPLETE) {
    //     var pixels = new Uint8Array(textureSize * textureSize * 4)
    //     gl.readPixels(0, 0, textureSize, textureSize, gl.RGBA, gl.UNSIGNED_BYTE, pixels)
    //     console.log(pixels)
    //   } else {
    //     console.log(gl.FRAMEBUFFER_COMPLETE, gl.FRAMEBUFFER_INCOMPLETE_ATTACHMENT, gl.FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT, gl.FRAMEBUFFER_INCOMPLETE_DIMENSIONS, gl.FRAMEBUFFER_UNSUPPORTED)
    //     console.log('reading back failed, unable to create the framebuffer:', gl.checkFramebufferStatus(gl.FRAMEBUFFER).toString())
    //   }
    //   gl.deleteFramebuffer(fb)
    // }
  }
}

export class FontLoader {
  static default: Font = new Font()

  static loadFont (fontname: string, gl: WebGLRenderingContext) {
    FontLoader.default.load(fontname, gl)
  }
}

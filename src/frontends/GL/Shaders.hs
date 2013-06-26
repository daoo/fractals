module GL.Shaders where

texFragShader :: String
texFragShader =
  "#version 130\n\

  \precision highp float;\n\

  \in  vec2 texCoord;\n\
  \out vec4 fragmentColor;\n\

  \uniform sampler2D framebuffer;\n\

  \void main() {\n\
  \  fragmentColor = texture2D(framebuffer, texCoord);\n\
  \}"

texVertShader :: String
texVertShader =
  "#version 130\n\

  \in  vec3 position;\n\
  \out vec2 texCoord;\n\

  \void main() {\n\
  \  texCoord    = (position.xy + vec2(1.0)) * vec2(0.5, -0.5);\n\
  \  gl_Position = vec4(position, 1);\n\
  \}"

redFragShader :: String
redFragShader =
  "#version 130\n\
  \precision highp float;\n\
  \out vec4 fragmentColor;\n\
  \void main() {\n\
  \  fragmentColor = vec4(1, 0, 0, 1);\n\
  \}"

screenVertShader :: String
screenVertShader =
  "#version 130\n\

  \in vec3 position;\n\
  \uniform ivec2 size;\n\

  \void main() {\n\
  \  gl_Position = vec4((vec2(position) / size) * vec2(2, -2) + vec2(-1, 1), 0, 1);\n\
  \}"

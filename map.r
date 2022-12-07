
# Change the content of customData and customGeoms the way you like

customData = data.frame(
  box    = c(1, 4, 7, 9, 12, 14, 16, 18, 24, 26, 28, 32, 35, 43, 54, 55, 60, 61, 62, 63, 64, 66, 67, 70, 74, 77, 78, 87, 95, 98, 100, 104, 108, 111, 115, 124, 125, 136, 140, 158, 175, 177, 178, 180, 187, 189, 192, 195, 197, 204, 206, 211, 218, 224, 227, 233, 238, 240, 244, 246, 251, 256, 260, 262, 264, 268, 271, 272, 275, 2, 6, 21, 25, 33, 44, 56, 86, 109, 163, 172, 174, 179, 191, 194, 228, 234, 252, 253, 257, 261, 269),
  your_category  = c(rep("Recorder", 69), rep("Micro (50m)", 22)),
  your_label = c(rep('', 69), 1, 7, 16, 24, 32, 26, 55, 78, 108, 140, 158, 175, 178, 192, 195, 227, 233, 251, 238, 271, 262, 268)
)

# next line is required
customData = merge(customData, boxesxy, by = 'box')

# create a list of ggplot geoms and scales.
# see e.g. http://docs.ggplot2.org/current/
customGeoms = list(
  geom_point(data = customData,
             aes(x = long, y = lat, shape = your_category),
             size = 10, col = 'red' ),
  scale_shape(solid = FALSE),

  geom_text(data = customData,
            hjust = 'right', nudge_x = 0, nudge_y = -13,
            aes(x = long, y = lat, label = your_label) )
)

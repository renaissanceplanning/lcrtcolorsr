#'Full Color List
#'
#'A full list of the individual colors used in this package, including name and
#'hex code
#'
#' @export
lcrt_color_list <- c(
#Logo Colors
  `Sky` = "#CBE8E3",
  `Ocean` = "#343873",
  `Sand` = "#F3EFE7",
  `Shore` = "#6D6155",

  #lcrt rainbow
  `Coral` = "#FF4548",
  `Orange` = "#F6921E",
  `Yellow` = "#FFDA3D",
  `Green` = "#8BC53F",
  `Aqua` = "#26BBB3",
  `Blue` = "#00ADEE",
  `Purple` = "#C963FF",
  `Pink` = "#F291BC"

  # Ramp ends
)

#'Retrieve a Color
#'
#'This function takes a string or group of strings and returns hex codes
#'for the corresponding colors
#'
#'A full list of the colors in this package can be returned by running the
#'function with no input
#'
#' @export
lcrt_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (lcrt_color_list)

  lcrt_color_list[cols]
}

#'Full Palette List
#'
#'A full list of the color palettes used in this package
#'
#' @export
lcrt_color_palettes <- list(
  #categorical ramps
  `logo` = lcrt_cols("Sky", "Ocean", "Sand", "Shore"),

  `lines` = lcrt_cols("Coral", "Orange", "Yellow", "Green",
                     "Aqua", "Blue", "Purple", "Pink"),
  # linear ramps
  `Coral_ramp` = lcrt_cols("Sand", "Coral"),
  `Orange_ramp` = lcrt_cols("Sand", "Orange"),
  `Yellow_ramp` = lcrt_cols("Sand", "Yellow"),
  `Green_ramp` = lcrt_cols("Sand", "Green"),
  `Aqua_ramp` = lcrt_cols("Sand", "Aqua"),
  `Blue_ramp` = lcrt_cols("Sand", "Blue"),
  `Purple_ramp` = lcrt_cols("Sand", "Purple"),
  `Pink_ramp` = lcrt_cols("Sand", "Pink"),

  # diverging ramps
  `cool_to_warm` = lcrt_cols("Orange", "Yellow", "Blue"),
  `cold_to_hot` = lcrt_cols("Coral", "Orange", "Yellow", "Aqua", "Blue")

)

#'Retrieve a Palette
#'
#'This function excepts a character string for a palette and returns a function
#'that can be turned into a palette
#'
#'adding a number after the function call will create a palette with that number
#'of steps
#'
#'@param palette A string representing a color palette, see https://bfroebrpg.github.io/lcrtcolorsr_pages/
#'for a full list of palettes
#'
#'@param reverse When true the default order of the color palette is reversed
#'
#'@return a function to create a color palette
#'
#' @export
lcrt_color_pal <- function(palette = "logo", reverse = FALSE, ...) {
  pal <- lcrt_color_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#'Create a Color Scale for ggplot
#'
#'This function is inserted into a ggplot call to replace a scale_color
#'command.
#'
#'By default the palette will be discrete.
#'
#'@param palette A string representing a color palette, see https://bfroebrpg.github.io/lcrtcolorsr_pages/
#'for a full list of palettes
#'
#'@param discrete Changes between discrete or continuous scales
#'
#'@param reverse When true the default order of the color palette is reversed
#'
#'@return a function to create a color palette
#' @export
scale_color_lcrt <- function(palette = "logo", discrete = TRUE, reverse = FALSE, ...) {
  pal <- lcrt_color_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("lcrt_color_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#'Create a Fill Scale for ggplot
#'
#'This function is inserted into a ggplot call to replace a scale_fill
#'command.
#'
#'By default the palette will be discrete.
#'
#'@param palette A string representing a color palette, see https://bfroebrpg.github.io/lcrtcolorsr_pages/
#'for a full list of palettes
#'
#'@param discrete Changes between discrete or continuous scales
#'
#'@param reverse When true the default order of the color palette is reversed
#'
#'@return a function to create a color palette
#' @export
scale_fill_lcrt <- function(palette = "logo", discrete = TRUE, reverse = FALSE, ...) {
  pal <- lcrt_color_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("lcrt_color_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


#' Create a Color Matrix for Mapdeck
#'
#' This funciton will create the color matrix required to a use a custom
#' palette while using mapdeck.
#'
#'@param palette A string representing a color palette, see https://bfroebrpg.github.io/lcrtcolorsr_pages/
#'for a full list of palettes
#'
#'@param reverse When true the default order of the color palette is reversed
#'
#'@param alpha sets the alpha for the colors, must be int between 1 and 256.
#'1 is transparent and 256 is solid.
#'
#'
#'@return a color matrix
#' @export
mapdeck_palette <- function(palette = "logo", reverse = FALSE, alpha = 256){
  steps <- length(lcrt_color_palettes[[palette]])
  colors <- lcrt_color_pal(palette = palette, reverse = reverse)(steps)
  matrix <- colorRamp(colors)( (1:256)/256 )
  matrix <- cbind(matrix, alpha)
  return(matrix)
}

#'lcrt Mapbox Base Maps
#'
#'URLs to use Mapbox vector tile services as an interactive basemap
#'
#' @export
rpg_basemaps <- list(
  `atlas_tmap` = paste0(
    "https://api.mapbox.com/styles/v1/renplan/cl4olqb9v000514ldljtf1omt/",
    "tiles/256/{z}/{x}/{y}@2x?access_token=pk.",
    "eyJ1IjoicmVucGxhbiIsImEiOiJjaWgzcHdjOTIweTJvdzdtNWxlYnZ5MXZjIn0",
    ".zMsUwMAoEu6DZvd7IYVtjg"
  ),
  `atlas_style` = paste0(
    "mapbox://styles/renplan/cl4olqb9v000514ldljtf1omt"
  ),
  `rpg_access_token` = paste0(
    "pk.eyJ1IjoicmVucGxhbiIsImEiOiJjaWgzcHdjOTIweTJ",
    "vdzdtNWxlYnZ5MXZjIn0.zMsUwMAoEu6DZvd7IYVtjg"
  )
)










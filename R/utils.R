

#' @importFrom stringr str_match str_split str_sub
#' @importFrom magrittr "%>%"
NULL


# CRAN sometimes issues spurious warnings about undefined variables
# utils::globalVariables(c(".", "%>%","x","y","c","value"))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# nimg class ----


#' nimg is a class for storing image data. Image data is stored as a 3 dimensional array with y, x, c format.
#' y and x are spatial coordinates, and c is a color dimension.
#' @title Create a nimg object
#' @param im a 3 dimensional numeric array
#' @param name a string for name attribute.
#' @return an object of class nimg
#' @examples
#' nimg(array(1,c(128,256,3)))
#' @export
nimg = function( im, name ){
  if( is.logical( im ) | is.integer( im ) ){
    im = im + 0.0
  }
  class( im ) = c( "nimg", "numeric" )
  if( ! base::missing( name ) ){
    attr( im, "name" ) = name
  } else if( is.null( attr( im, "name" ) ) ){
    attr( im, "name" ) = ""
  }
  im
}


##' Check if an object is a nimg object
##' @param x an object
##' @return logical. if an object is a nimg object.
##' @export
is.nimg = function( x ){
  methods::is(x,"cimg")
}


##' @export
print.nimg = function( x, ... ){
  d = dim( x )
  cat( sprintf( "image: %i [height] x %i [width] x %i [colour channels]\n", d[1], d[2], d[3] ) )
  invisible( x )
}


#' Display an image
#' @param x an image
#' @param rescale logical. if true, then pixel value is rescaled to range between 0 and 1.
#' @param ... other parameters to be passed to plot.default
#' @examples
#' plot(regatta)
#' @export
plot.nimg = function( x, rescale = FALSE, ... ){
  old.par = graphics::par( no.readonly = TRUE )
  on.exit( graphics::par( old.par ), add = TRUE )
  if( im_npix( x ) == 0 ){
    stop( "The image is empty." )
  }

  if( im_nchannel( x ) == 1 ){
    # a raster array must have exactly 3 or 4 planes
    x = rep_channel( x, 3 )
  }
  im = x[ ,,, drop = FALSE ]
  if( rescale ){
    im = rescaling01( im )
  } else if( max( im ) > 1 || min( im ) < 0 ){
    warning( "Pixcel value exceeds the range [0,1], and hence it was clamped when plotting.")
    im = clamping( im )
  }
  graphics::par( mar = c( 0, 0, 0, 0 ) )
  graphics::plot.new()
  graphics::plot.window( xlim = c(1,im_width(x)), ylim = c(im_height(x),1), asp = 1, xaxs = "i", yaxs = "i", ...)
  rst = grDevices::as.raster(
    im, rescale = FALSE, colorscale = NULL, colourscale = NULL, col.na = grDevices::rgb(0,0,0,0) )
  graphics::rasterImage( rst, 1, nrow( rst ), ncol( rst ), 1, interpolate = TRUE )
  invisible( x )
}


# file = "/Users/tsuhir/Dropbox/code/material/img/face11.jpg" # color jpg
# file = "/Users/tsuhir/Dropbox/code/material/img/aog.png" # color png
# file = "/Users/tsuhir/Dropbox/code/material/img/N055.bmp" # color bmp
# file = "/Users/tsuhir/Dropbox/code/material/img/gray_aog.jpg" # gray jpg
# file = "/Users/tsuhir/Dropbox/code/material/img/gray_aog.png" # gray png
# file = "/Users/tsuhir/Dropbox/code/material/img/kani_gray.bmp" # gray bmp
# file = "/Users/tsuhir/Dropbox/code/material/img/mono_aog.jpg" # mono jpg
# file = "/Users/tsuhir/Dropbox/code/material/img/mono_aog.png" # mono png
# file = "/Users/tsuhir/Dropbox/code/material/img/mono_aog.bmp" # mono bmp
# file = "/Users/tsuhir/Dropbox/code/material/img/aog_col_a.png" # color png with alpha

#' Load image from file or URL
#' @param file path to file or URL
#' @param name a string for name attribute. if missing, inffered from the file argument.
#' @return an array of image data
#' @examples
#' \dontrun{
#' # load an image from disk
#' im = im_load("path/to/your/image.jpg")
#' plot(im)
#' # load an image from URL
#' im = im_load("http://placehold.jp/150x150.png")
#' }
#' @export
im_load = function( file, name ){
  if( grepl("^(http|ftp)s?://", file) ){ # if URL
    url = file
    ext = stringr::str_extract_all( url, "\\.([A-Za-z0-9]+$)" )[[ 1 ]]
    if( length( ext ) > 0 ){
      file = tempfile( fileext = ext )
    } else {
      file = tempfile()
    }
    downloader::download( url, file, mode = "wb" )
    im = im_load( file, get_image_name( url ) )
    unlink( file )
    return( im )
  }
  ext = sub( ".*\\.([^.]{3,4})$", "\\1", file ) %>% tolower
  if( ext %in% c( "png", "bmp", "jpg", "jpeg" ) ){
    im = readbitmap::read.bitmap( file )
    dim( im )
    if( ! is.null( attr( im, "header" ) ) ){
      im = im / 255
    }
    if( length( dim( im ) ) == 2 ){ # gray-scale image
      dim( im ) = c( dim( im ), 1 )
    } else if( length( dim( im ) ) == 3 ){ # multiple channels
      if( dim( im )[ 3 ] %in% c( 2, 4 ) ){
        # remove alpha channel if it is uninformative
        if( min( im[ , , dim( im )[ 3 ] ] ) == max( im[ , , dim( im )[ 3 ] ] ) ){
          im = im[ , , 1:( dim( im )[ 3 ] - 1 ), drop = FALSE ]
        }
      }
    }
    im = nimg( im, ifelse( base::missing( name ), get_image_name( file ), name ) )
    return( im )
  } else {
    stop( "Only jpg, png, and bmp formats are supported." )
  }
}


#' Save an image to disk
#' @param im An image.
#' @param name Name of the image file.
#' @param path Path to file.
#' @param format Image format. Either "jpg", "png", "tiff", or "bmp". Default is "png".
#' @param quality (jpg only) default is 0.95. Higher quality means less compression.
#' @return No return value, called for side effects.
#' @examples
#' \dontrun{
#' # regatta.png is saved to the current working directory
#' imsave( regatta, path = getwd() )
#' # myimage.jpg is saved to a specified directory
#' imsave( regatta, name = "myimage", path = "path/to/image", format = "jpg" )
#' }
#' @export
im_save = function( im, name, path, format = "png", quality = .95 ){
  if( ! format %in% c( "jpg", "png" ) ){
    warning( "Incorrect imaeg format. Use either jpg or png." )
    return()
  }
  if( base::missing( name ) ){
    name = deparse( substitute( im ) )
  }
  if( im_nchannel( im ) == 1 ){
    im = rep_channel( im, 3 )
  }
  if( stringr::str_sub( path, stringr::str_length( path ) ) == "/" ){
    path = stringr::str_sub( path, end = stringr::str_length( path ) - 1 )
  }
  if( max( im ) > 1 || min( im ) < 0 ){
    warning( "Pixcel value exceeds the range [0,1], and hence it was clamped when saving.")
    im = clamping( im )
  }
  base::dir.create( path, showWarnings = FALSE, recursive = TRUE )
  file = paste0( path, "/", name, ".", format )
  if( format == "png" ){
    png::writePNG( im, file )
  } else if ( format == "jpeg" | format == "jpg" ){
    jpeg::writeJPEG( im, file, quality = quality )
  }
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# image info ----


#' Get image height
#' @param im an image
#' @return image's height
#' @examples
#' im_height(regatta)
#' @export
im_height = function( im ){
  dim( im )[ 1 ]
}


#' Get image width
#' @param im an image
#' @return image's width
#' @examples
#' im_width(regatta)
#' @export
im_width = function( im ){
  dim( im )[ 2 ]
}


#' Get image width and height
#' @param im an image
#' @examples
#' im_size(regatta)
#' @export
im_size = function( im ){
  unname( dim( im )[ 1:2 ] )
}


#' Total number of pixels of an image
#' @param im an image
#' @examples
#' im_npix(regatta)
#' @export
im_npix = function( im ){
  prod( dim( im ) )
}


#' The number of color channel of an image
#' @param im an image
#' @examples
#' im_nchannel(regatta)
#' @export
im_nchannel = function( im ){
  dim( im )[ 3 ]
}


#' The number of color channel of an image
#' @param im an image
#' @examples
#' im_nc(regatta)
#' @export
im_nc = function( im ){
  im_nchannel( im )
}


#' X coordinate of image center
#' @param im an image
#' @examples
#' im_cx(regatta)
#' @export
im_cx = function( im ){
  return( floor( im_width( im ) / 2 ) + 1 )
}


#' Y coordinate of image center
#' @param im an image
#' @examples
#' im_cy(regatta)
#' @export
im_cy = function( im ){
  return( floor( im_height( im ) / 2 ) + 1 )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# image transform ----


#' Replicate an image along color dimension
#' @param im an image
#' @param n number of repeat
#' @param channel which channel to extract
#' @export
rep_channel = function( im, n = 3, channel = 1 ){
  return( nimg( array( im[ ,,channel ], c( im_height( im ), im_width( im ), n ) ) ) )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# math ----


#' Rescale numeric vector to have a range between 0 to 1.
#'
#' @param x a numeric vector
#' @seealso \code{\link{rescaling}}
#' @examples
#' rescaling01( 1:5 )
#' @export
rescaling01 = function( x ){
  if( max( x ) == min( x ) ){
    return( x )
  } else {
    return( ( x - min( x ) ) / ( max( x ) - min( x ) ) )
  }
}


#' Rescale numeric vector to have a specified range.
#'
#' @param x a numeric vector
#' @param from lowest value
#' @param to highest value
#' @seealso \code{\link{rescaling01}}
#' @examples
#' rescaling( 1:5, from = 0, to = 10 )
#' @export
rescaling = function( x, from = 0, to = 1 ){
  if( max( x ) == min( x ) ){
    return( x )
  } else {
    return( from + ( to - from ) * rescaling01( x ) )
  }
}


#' Clamp values to a minimum and maximum value.
#'
#' @param x a numeric vector
#' @param min minimum value
#' @param max maximum value
#' @examples
#' clamping( -5:5, min = -3, max = 3 )
#' @export
clamping = function( x, min = 0, max = 1 ){
  x[ x < min ] = min
  x[ x > max ] = max
  return( x )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# string ----


get_image_name = function( file ){
  name = stringr::str_split( file, "/" )[[ 1 ]]
  name = name[ length( name ) ]
  name = stringr::str_split( name, "[.]" )[[ 1 ]]
  name[ 1 ]
}




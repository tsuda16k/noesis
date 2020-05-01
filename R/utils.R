

#' @importFrom stringr str_match str_split str_sub
#' @importFrom graphics plot
#' @importFrom magrittr "%>%"
NULL


# CRAN sometimes issues spurious warnings about undefined variables
utils::globalVariables( c( ".", "%>%", "x", "y", "c", "value" ) )


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# nimg class ----


#' nimg is a class for storing image data as a 3 dimensional array (y, x, c format).
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
  if( length( dim( im ) ) == 2 ){ # gray-scale image
    dim( im ) = c( dim( im ), 1 )
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
##' @examples
##' is.nimg(regatta)
##' @export
is.nimg = function( x ){
  methods::is(x,"nimg")
}


##' @export
print.nimg = function( x, ... ){
  d = dim( x )
  if( attr( x, "name" ) == "" || attr( x, "name" ) == "-" || is.null( attr( x, "name" ) ) ){
    name = "image"
  } else {
    name = attr( x, "name" )
  }
  cat( sprintf( "%s: %i [height] x %i [width] x %i [colour channels]\n", name, d[1], d[2], d[3] ) )
  # cat( sprintf( "image: %i [height] x %i [width] x %i [colour channels]\n", d[1], d[2], d[3] ) )
  invisible( x )
}


#' Display an image
#' @param x an image
#' @param rescale logical. if true, then pixel value is rescaled to range between 0 and 1.
#' @param ... other parameters to be passed to plot.default
#' @return No return value, called for side effects.
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
  graphics::plot.window(
    xlim = c(1,im_width(x)), ylim = c(im_height(x),1), asp = 1, xaxs = "i", yaxs = "i", ...)
  rst = grDevices::as.raster(
    im, rescale = FALSE, colorscale = NULL, colourscale = NULL, col.na = grDevices::rgb(0,0,0,0) )
  graphics::rasterImage( rst, 1, nrow( rst ), ncol( rst ), 1, interpolate = FALSE )
  invisible( x )
}


#' Display an image
#' @param im an image. either nimg object or an array object
#' @param rescale logical. if true, then pixel value is rescaled to range between 0 and 1.
#' @return No return value, called for side effects.
#' @examples
#' pplot(regatta)
#' @export
pplot = function( im, rescale = FALSE ){
  if( "nimg" %in% class( im ) ){
    graphics::plot( im )
  } else {
    graphics::plot( nimg( im ) )
  }
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
#' pplot(im)
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
    im = im_load( file, get_image_name_from_file( url ) )
    unlink( file )
    return( im )
  }
  ext = sub( ".*\\.([^.]{3,4})$", "\\1", file ) %>% tolower
  if( ext %in% c( "png", "bmp", "jpg", "jpeg" ) ){
    tryCatch({
      im = readbitmap::read.bitmap( file )
    },
    error = function(e) {
      stop( paste0( e, "Note: im_load() fails for binary (black/white) bmp image." ) )
    })
    # im = readbitmap::read.bitmap( file )
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
    im = nimg( im, ifelse( base::missing( name ), get_image_name_from_file( file ), name ) )
    return( im )
  } else {
    stop( "Only jpg, png, and bmp formats are supported." )
  }
}


#' Load all images in a directory and return them as a list.
#' @param path a directory path containig images
#' @return a list of images
#' @examples
#' \donttest{
#' im = im_load_dir( "path/to/image/folder" )
#' }
#' @export
im_load_dir = function( path ){
  names = list.files( path, pattern = "\\.(jpg|jpeg|png|bmp|JPG|JPEG|PNG|BMP)$" )
  l = vector( "list", length( names ) )
  for( i in 1:length( names ) ){
    tryCatch({
      l[[ i ]] = im_load( paste0( path, "/", names[ i ] ) )
    },
    error = function(e) {
      warning( paste0( names[ i ], " cannot be loaded, and ignored." ) )
      l[[ i ]] = NULL
    })
    names( l )[ i ] = stringr::str_split( names[ i ], "[.]" )[[ 1 ]][ 1 ]
  }
  # names( l ) = names
  return( l )
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


#' cimg to nimg conversion
#' @param im a cimg object
#' @return an nimg object
#' @export
cimg2nimg = function( im ){
  if( is.list( im ) ){
    im = lapply( im, function( x ){
      if( "nimg" %in% class( x ) ){
        x
      } else {
        cimg2nimg( x )
      }
    })
    return( im )
  } else if( "cimg" %in% class( im ) ){
    im = aperm( im, c( 2, 1, 4, 3 ) ) # (x, y, z, cc) to (y, x, cc, z)
    return( nimg( im[,,,1] ) )
  } else if( "nimg" %in% class( im ) ){
    return( im )
  } else {
    return( nimg( im ) )
  }
}


#' nimg to cimg conversion
#' @param im an nimg object
#' @return a cimg object
#' @export
nimg2cimg = function( im ){
  if( is.list( im ) ){
    im = lapply( im, function(x){
      if( "cimg" %in% class( x ) ){
        x
      } else {
        nimg2cimg( x )
      }
    })
    return( im )
  } else {
    if( "cimg" %in% class( im ) ) {
      return( im )
    } else if( length( dim( im ) ) == 2 ){ # (y, x) to (x, y)
      return( imager::as.cimg( t( im ) ) )
    } else if( length( dim( im ) ) == 4 ){ # (y, x, cc, z) to (x, y, z, cc)
      return( imager::as.cimg( aperm( im, c( 2, 1, 4, 3 ) ) ) )
    } else if( length( dim( im ) ) == 3 ){ # (y, x, cc) to (x, y, cc)
      im = aperm( im, c( 2, 1, 3 ) )
      im2 = array( 0, dim = c( dim( im )[ 1 ], dim( im )[ 2 ], 1, dim( im )[ 3 ] ) )
      im2[,,1,] = im
      return( imager::as.cimg( im2 ) )
    }
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
#' @return image's height
#' @examples
#' im_size(regatta)
#' @export
im_size = function( im ){
  unname( dim( im )[ 1:2 ] )
}


#' Total number of pixels of an image
#' @param im an image
#' @return total number of pixels
#' @examples
#' im_npix(regatta)
#' @export
im_npix = function( im ){
  prod( dim( im ) )
}


#' The number of color channel of an image
#' @param im an image
#' @return the number of color channel
#' @examples
#' im_nchannel(regatta)
#' @export
im_nchannel = function( im ){
  dim( im )[ 3 ]
}


#' The number of color channel of an image
#' @param im an image
#' @return the number of color channel
#' @examples
#' im_nc(regatta)
#' @export
im_nc = function( im ){
  im_nchannel( im )
}


#' X coordinate of image center
#' @param im an image
#' @return X coordinate of image center
#' @examples
#' im_cx(regatta)
#' @export
im_cx = function( im ){
  return( floor( im_width( im ) / 2 ) + 1 )
}


#' Y coordinate of image center
#' @param im an image
#' @return Y coordinate of image center
#' @examples
#' im_cy(regatta)
#' @export
im_cy = function( im ){
  return( floor( im_height( im ) / 2 ) + 1 )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# image slicing ----


#' Extract a color channel from an image
#' @param im an image
#' @param channel color channel to extract. either numeric or string.
#' @return an image
#' @examples
#' R = get_channel(regatta, 1)
#' pplot(R)
#' G = get_channel(regatta, "G")
#' pplot(G)
#' @export
get_channel = function( im, channel ){
  if( is.numeric( channel ) ){
    return( nimg( im[ , , channel, drop = F ] ) )
  } else {
    return( get_channel( im, match( channel, c( "R", "G", "B", "A" ) ) ) )
  }
}


#' Extract the Red channel from an image
#' @param im an image
#' @return an image
#' @export
get_R = function( im ){
  return( get_channel( im, 1 ) )
}


#' Extract the Green channel from an image
#' @param im an image
#' @return an image
#' @export
get_G = function( im ){
  return( get_channel( im, 2 ) )
}


#' Extract the Blue channel from an image
#' @param im an image
#' @return an image
#' @export
get_B = function( im ){
  return( get_channel( im, 3 ) )
}


#' Extract the Alpha channel from an image
#' @param im an image
#' @return an image
#' @export
get_A = function( im ){
  return( get_channel( im, 4 ) )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# image transform ----


#' Replicate a channel along color dimension
#' @param im an image
#' @param n number of repeat
#' @param channel which channel to extract
#' @return an image
#' @export
rep_channel = function( im, n = 3, channel = 1 ){
  return( nimg( array( im[ ,,channel ], c( im_height( im ), im_width( im ), n ) ) ) )
}


#' Fprce an image to have three and only three color channels
#' @param im an image
#' @return an image
#' @examples
#' R = get_R(regatta)
#' im = force_tricolor(R)
#' @export
force_tricolor = function( im ){
  n = im_nc( im )
  if( n < 3 ){
    return( rep_channel( im, 3, 1 ) )
  } else if( n > 3 ){
    return( get_channel( im, 1:3 ) )
  } else {
    return( im )
  }
}


#' Pad image with n pixels
#'
#' @param im an image
#' @param n number of pixels to pad with
#' @param method either "zero", "mean", "repeat", "mirror", or a numeric value
#' @examples
#' # zero padding
#' im = im_pad(regatta, 20, "zero")
#' pplot(im)
#' # mirror padding
#' im = im_pad(regatta, 100, "mirror")
#' pplot(im)
#' @export
im_pad = function( im, n, method = "mirror" ){
  if( n == 0 ) return( im )

  w = im_width( im )
  h = im_height( im )

  if( any( n > c( w, h ) ) ){
    warning( "n must be equal or smaller than image width (and height)." )
    return( im )
  }

  # create an empty matrix
  x = ifelse( is.numeric( method ), method, ifelse( method == "mean", mean( im ), 0 ) )
  mat = array( x, c( h + 2 * n, w + 2 * n, dim( im )[ 3 ] ) )

  # put the image
  mat[ ( n + 1 ):( n + h ), ( n + 1 ):( n + w ), ] = im

  # padding
  if( method == "zero" || method == "mean" || is.numeric( method ) ){
    # do nothing
  } else if( method == "repeat" ){
    # top left
    mat[ 1:n, 1:n, ] = im[ (h-n+1):h, (w-n+1):w, ]
    # top
    mat[ 1:n, (n+1):(n+w), ] = im[ (h-n+1):h, 1:w, ]
    # top right
    mat[ 1:n, (n+w+1):(2*n+w), ] = im[ (h-n+1):h, 1:n, ]
    # left
    mat[ (n+1):(n+h), 1:n, ] = im[ 1:h, (w-n+1):w, ]
    # right
    mat[ (n+1):(n+h), (n+w+1):(2*n+w), ] = im[ 1:h, 1:n, ]
    # bottom left
    mat[ (n+h+1):(2*n+h), 1:n, ] = im[ 1:n, (w-n+1):w, ]
    # bottom
    mat[ (n+h+1):(2*n+h), (n+1):(n+w), ] = im[ 1:n, 1:w, ]
    # bottom right
    mat[ (n+h+1):(2*n+h), (n+w+1):(2*n+w), ] = im[ 1:n, 1:n, ]
  } else if( method == "mirror" ){
    # top left
    mat[ 1:n, 1:n, ] = im[ n:1, n:1, ]
    # top
    mat[ 1:n, (n+1):(n+w), ] = im[ n:1, 1:w, ]
    # top right
    mat[ 1:n, (n+w+1):(2*n+w), ] = im[ n:1, w:(w-n+1), ]
    # left
    mat[ (n+1):(n+h), 1:n, ] = im[ 1:h, n:1, ]
    # right
    mat[ (n+1):(n+h), (n+w+1):(2*n+w), ] = im[ 1:h, w:(w-n+1), ]
    # bottom left
    mat[ (n+h+1):(2*n+h), 1:n, ] = im[ h:(h-n+1), n:1, ]
    # bottom
    mat[ (n+h+1):(2*n+h), (n+1):(n+w), ] = im[ h:(h-n+1), 1:w, ]
    # bottom right
    mat[ (n+h+1):(2*n+h), (n+w+1):(2*n+w), ] = im[ h:(h-n+1), w:(w-n+1), ]
  }

  im = nimg( mat )
  return( im )
}


#' Image shift operation
#' @param im an image
#' @param axis either "x" or "y"
#' @param lag a numeric
#' @return an image
#' @examples
#' # shift a image 100 pixels to the right
#' im = im_shift(regatta, axis = "x", lag = 100)
#' pplot(im)
#' @export
im_shift = function( im, axis = "x", lag = 0 ){
  if( axis == "x" ){
    index = vec_shift( 1:dim(im)[2], lag )
    im = im[ ,index, , drop = F ]
  } else if( axis == "y" ){
    index = vec_shift( 1:dim(im)[1], lag )
    im = im[ index, , , drop = F ]
  }
  return( nimg( im ) )
}


# margin: (top/bottom/left/right) or (top/bottom, left/right) or (top, right, bottom, right)
#' Image cropping
#' @param im an image
#' @param margin a numeric vector.
#' @return an image
#' @examples
#' pplot(im_crop(regatta, 100)) # crop all four sides with the same value
#' pplot(im_crop(regatta, c(100, 50))) # vertical, horizontal
#' pplot(im_crop(regatta, c(0, 50, 50, 200))) # top, right, bottom, left
#' @export
im_crop = function( im, margin ){
  if( length( margin ) == 1 ){
    top = bottom = left = right = margin
  } else if( length( margin ) == 2 ){
    top = bottom = margin[ 1 ]
    left = right = margin[ 2 ]
  } else if( length( margin ) == 3 ){
    warning( "margin length must be 1, 2, or 4!" )
  } else if( length( margin ) == 4 ){
    top = margin[ 1 ]
    right = margin[ 2 ]
    bottom = margin[ 3 ]
    left = margin[ 4 ]
  }
  im = im[ (1 + top):(im_height( im ) - bottom), (1 + left):(im_width( im ) - right), , drop = F ]
  return( nimg( im ) )
}


#' Make an image square shaped
#' @param im an image
#' @param position square center. a numeric value between 0 and 1.
#' @return an image
#' @examples
#' pplot(im_crop_square(regatta))
#' pplot(im_crop_square(regatta, position = 0))
#' pplot(im_crop_square(regatta, position = 0.8))
#' @export
im_crop_square = function( im, position = 0.5 ){
  position = clamping( position )
  diff = im_width( im ) - im_height( im )
  position = 2 * position - 1 # range [-1,1]
  size = min( im_size( im ) )
  erode = abs( diff ) / 2
  center = max( im_size( im ) ) / 2
  start = floor( center - size / 2 + erode * position )
  if( start < 1 ) start = 1
  end = start + size - 1
  if( diff > 0 ){ # wide
    im = im_crop( im, c( 0, im_width( im ) - end, 0, start - 1 ) )
  } else { # tall
    im = im_crop( im, c( start - 1, 0, im_height( im ) - end, 0 ) )
  }
  return( nimg( im ) )
}


#' Rotate an image
#' @param im an image
#' @param angle rotation angle in degrees
#' @param expand either FALSE (default, does not change image size) or TRUE
#' @param cx center of rotation along x
#' @param cy center of rotation along y
#' @param interpolation Type of interpolation. Either 0 (nearest), 1 (linear), or 2 (cubic).
#' @param pad Type of padding. Either "zero", "neumann", or "repeat".
#' @return an image
#' @examples
#' pplot(im_rotate(regatta, 30))
#' pplot(im_rotate(regatta, 30, expand = TRUE))
#' pplot(im_rotate(regatta, 30, expand = TRUE, pad = "repeat"))
#' @export
im_rotate = function( im, angle, expand = FALSE, cx = NULL, cy = NULL, interpolation = 2, pad = "zero" ){
  cimg = nimg2cimg( im )
  boundary = 0
  if( pad == "neumann" ){
    boundary = 1
  } else if( pad == "repeat" ){
    boundary = 2
  }
  if( is.null( cx ) && is.null( cy ) ){
    if( expand ){
      im = imager::imrotate( cimg, angle, interpolation = interpolation, boundary = boundary )
    } else {
      im = imager::imrotate( cimg, angle, im_width(im)/2, im_height(im)/2, interpolation, boundary )
    }
  } else if( ! is.null( cx ) && ! is.null( cy ) ){
    im = imager::imrotate( cimg, angle, cx, cy, interpolation, boundary )
  } else {
    warning( "You must specify both cx and cy." )
    return( NULL )
  }
  return( cimg2nimg( clamping( im ) ) )
}


#' Resize image




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# math ----


#' Rescale numeric vector to have a range between 0 to 1.
#' @param x a numeric vector
#' @return a rescaled numeric vector
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
#' @param x a numeric vector
#' @param from lowest value
#' @param to highest value
#' @return a rescaled numeric vector
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
#' @param x a numeric vector
#' @param min minimum value
#' @param max maximum value
#' @return a numeric vector
#' @examples
#' clamping( -5:5, min = -3, max = 3 )
#' @export
clamping = function( x, min = 0, max = 1 ){
  x[ x < min ] = min
  x[ x > max ] = max
  return( x )
}


#' Calculate a cubic spline.
#' @param x a numeric vector
#' @param low minimum value of output
#' @param high maxmum value of output
#' @return a numeric vector
#' @examples
#' x = seq( from = 0, to = 1, by = 0.01 )
#' plot( x, cubic_spline( x ) )
#' @export
cubic_spline = function( x, low = 0, high = 1 ){
  if( low == high ){
    warning( "low and high must be different!" )
  } else if( low > high ){
    return( 1 - ( cubic_spline( x, high, low ) ) )
  }
  x2 = x
  t = x[ x > low & x < high ]
  t = ( t - low ) / ( high - low )
  x2[ x > low & x < high ] = t^2 * ( 3 - 2 * t )
  x2[ x <= low ] = 0
  x2[ x >= high ] = 1
  return( x2 )
}


#' Calculate the Euclidean distance between two points
#' @param x1 x coordinate of point 1
#' @param y1 y coordinate of point 1
#' @param x2 x coordinate of point 2
#' @param y2 y coordinate of point 2
#' @return distance between point 1 and 2
#' @export
eucdist = function( x1, y1, x2, y2 ){
  return( sqrt( ( x1 - x2 )^2 + ( y1 - y2 )^2 ) )
}


#' Shift operation
#' @param v a numeric vector
#' @param lag a numeric
#' @return a lagged vector
#' @export
vec_shift = function( v, lag = 0 ){
  if( lag == 0 || abs( lag ) == length( v ) ){
    return( v )
  }
  index = 1:length( v )
  lag = lag %% length( index )
  index = c( index[ length( index ) - lag + 1 ]:max( index ), 1:index[ length( index ) - lag ] )
  return( index )
}


#' Power operation
#' @param x a numeric vector
#' @param p power term
#' @return a numeric vector
#' @export
pow = function( x, p ){
  return( x^p )
}


#' Calculate range
#' @param x a numeric vector
#' @return the range of x
#' @export
MinMax = MaxMin = function( x ){
  return( max( x ) - min( x ) )
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# string ----


get_image_name_from_file = function( file ){
  tryCatch({
    name = stringr::str_split( file, "/" )[[ 1 ]]
    name = name[ length( name ) ]
    name = stringr::str_split( name, "[.]" )[[ 1 ]]
    return( name[ 1 ] )
  },
  error = function(e) {
    return( "-" )
  })

}




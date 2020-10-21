

#' @title Read bitmap file in PCX format.
#'
#' @param filepath character string, path to the file including extension
#'
#' @param hdr logical, whether to return full list with header
#'
#' @param hdr_only logical, whether to read only the header
#'
#' @return array with color data, or pcx instance (named list) if `hdr` is `TRUE`
#'
#' @examples
#' \dontrun{
#'    pcxf = '~/data/q2_pak/models/items/quaddama/skin.pcx';
#'    pcx = read.pcx(pcxf);
#'    plot(imager::as.cimg(pcx$colors))
#'    # show palette:
#'    plot(1:256, col=rgb(pcx$palette, maxColorValue = 255))
#'
#' }
#'
#' @export
read.pcx <- function(filepath, hdr = TRUE, hdr_only = FALSE) {
  fh = file(filepath, "rb");
  on.exit({ close(fh) });

  endian = 'little';

  pcx = list();
  header = list();

  header$ident = readBin(fh, integer(), n = 1, size = 1, endian = endian);
  if(header$ident != 10L) {
    stop("File not in PCX format.");
  }
  header$painbrush_version = readBin(fh, integer(), n = 1, size = 1, endian = endian);
  header$encoding_type = readBin(fh, integer(), n = 1, size = 1, endian = endian); # 0 = none, 1 = runlength enc.
  header$bitpix = readBin(fh, integer(), n = 1, size = 1, endian = endian); # bits per pixel, defines number of possible colors in image. 1 = 2, 2 = 4, 4 = 16, 8 = 256.

  header$minx = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  header$miny = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  header$maxx = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  header$maxy = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  header$res_horizontal = readBin(fh, integer(), n = 1, size = 2, endian = endian); # DPI
  header$res_vertical = readBin(fh, integer(), n = 1, size = 2, endian = endian); # DPI
  header$ega_palette = readBin(fh, integer(), n = 16 * 3L, size = 1, endian = endian); # the EGA palette, used for 16-color images (pitpix = 3).
  header$reserved1 = readBin(fh, integer(), n = 1, size = 1, endian = endian);
  header$num_channels = readBin(fh, integer(), n = 1, size = 1, endian = endian);
  header$bytes_per_channels_line = readBin(fh, integer(), n = 1, size = 2, endian = endian);
  header$palette_mode = readBin(fh, integer(), n = 1, size = 2, endian = endian); # 1 = color/monochrome, 2=grayscale

  # The next 2 fields are only present for later format versions (painbrush_version >= 4). In earlier versions, the reserved2 field is 4 bytes longer. Check whether they make sense before using them.
  header$screen_size_horizontal = readBin(fh, integer(), n = 1, size = 2, endian = endian); # horizontal screen resolution of source system
  header$screen_size_vertical = readBin(fh, integer(), n = 1, size = 2, endian = endian); # vertical
  header$reserved2 = readBin(fh, integer(), n = 54, size = 1, endian = endian);

  img_width = header$maxx - header$minx + 1L;
  img_height = header$maxy - header$miny + 1L;

  header$width = img_width;
  header$height = img_height;
  header$derived = list();

  #cat(sprintf("img_dim = %d x %d\n", img_width, img_height));

  class(header) = c(class(header), 'pcxheader');


  img_num_pixels = img_width * img_height;
  img_num_values = img_num_pixels * header$num_channels;
  img_data = array(rep(NA, img_num_values), dim = c(img_width, img_height, header$num_channels));

  # The following is the length of an UNENCODED scanline in bytes:
  scan_line_num_bytes = header$num_channels * header$bytes_per_channels_line;

  bb = 8L;  # padding setting: the byte block size, lines are padded to be a multiple of the bb.
  scanline_padding_size = (scan_line_num_bytes * (bb / header$bitpix)) - img_width;
  header$derived$scanline_padding_size = scanline_padding_size;

  cat(sprintf("'%s': bitpix = %d,  scanline_padding_size = %d\n", filepath, header$bitpix, scanline_padding_size));
  if(scanline_padding_size != 0L) {
    cat(sprintf("'%s': scan_line_num_bytes = %d,  img_width = %d\n", filepath, scan_line_num_bytes, img_width));
  }

  if(hdr_only) {
    return(header);
  }
  pcx$header = header;


  seek(fh, where = 128L, origin = "start");

  # Read and decompress color data
  for(i in 1:img_height) {
    #cat(sprintf("Scanning image line %d of %d.\n", i, img_height));
    for(j in 1:header$num_channels) {
      #cat(sprintf(" * Scanning channel %d of %d [line %d of %d].\n", j, header$num_channels, i, img_height));
      row_pixel_index = 1L;
      bytes_read_this_line = 0L;
      bytes_expanded_this_line = 0L;
      while(bytes_expanded_this_line < header$bytes_per_channels_line) {
        #cat(sprintf(" *   Scanning byte %d of %d [channel %d of %d, line %d of %d].\n", (bytes_read_this_line+1L), header$bytes_per_channels_line, j, header$num_channels, i, img_height));
        raw_value = readBin(fh, integer(), n = 1L, size = 1L, signed = FALSE);
        bytes_read_this_line = bytes_read_this_line + 1L;
        if(length(raw_value) < 1L) {
          break; # last line may be shorter.
          #stop(sprintf("Reached end of file, but expected more data at byte %d of %d [channel %d of %d, line %d of %d].\n", bytes_read_this_line, header$bytes_per_channels_line, j, header$num_channels, i, img_height));
        }

        if(raw_value > 192L) { # repeat
          repeat_times = raw_value - 192L;
          if(repeat_times < 1L) {
            cat(sprintf("Repeat zero times.\n"));
          }
          repeat_color = readBin(fh, integer(), n = 1L, size = 1L, signed = FALSE);
          if(length(repeat_color) < 1L) {
            break;  # last line may be shorter than bytes_per_channels_line
            #stop(sprintf("Reached end of file, but expected repeat color at byte %d of %d [channel %d of %d, line %d of %d].\n", bytes_read_this_line, header$bytes_per_channels_line, j, header$num_channels, i, img_height));
          }
          bytes_read_this_line = bytes_read_this_line + 1L;
          bytes_expanded_this_line = bytes_expanded_this_line + repeat_times;
          if(row_pixel_index <= img_width) { # in image data (as opposed to padding)
            if(repeat_times > 0L) {
              for(l in 1:repeat_times) {
                if(row_pixel_index <= img_width) { # in image data
                  #cat(sprintf(" *    - In Repeat: Set %d pixels to %d.\n", repeat_times, repeat_color));
                  img_data[row_pixel_index, i, j] = repeat_color;
                  row_pixel_index = row_pixel_index + 1L;
                }
              }
            }
          }
        } else { # low value: direct color
          if(row_pixel_index <= img_width) { # in image data
            #cat(sprintf("line %d, channel %d: *    - Set 1 pixel (#%d of %d) to %d. [byte %d of %d per channel]\n", i, j, row_pixel_index, img_width, raw_value, k, header$bytes_per_channels_line));
            img_data[row_pixel_index, i, j] = raw_value;
          }
          row_pixel_index = row_pixel_index + 1L;
          bytes_expanded_this_line = bytes_expanded_this_line + 1L;
        }

        #cat(sprintf("row_pixel_index=%d, bytes_read_this_line=%d, bytes_expanded_this_line=%d.\n", row_pixel_index, bytes_read_this_line, bytes_expanded_this_line));
      }
    }
  }

  # check for palette
  seek(fh, where = -(768L +1L), origin = "end");
  palette_check = readBin(fh, integer(), n = 1L, size = 1L);
  if(palette_check == 12L) {
    pcx$header$has_palette_at_end = TRUE;
    palette = array(rep(0L, (256 * 4L)), dim = c(256L, 4L));
    for(i in 1:256) {
      for(j in 1:3) {  # the 4th entry is 'reserved', it is NOT to be read from the file.
        palette[i,j] = readBin(fh, integer(), n = 1L, size = 1L, signed = FALSE);
      }
    }
    pcx$palette = palette;
    pcx$palette_rgb = palette[,1:3];

    # apply palette
    if(dim(img_data)[3] == 1L) {
      # only 1 channel, use palette.
      pcx$colors = matrix(pcx$palette_rgb[drop(img_data)], nrow = pcx$header$height, byrow = TRUE);
    }

  } else {
    pcx$header$has_palette_at_end = FALSE;
    pcx$colors = img_data;
  }

  pcx$data = img_data;
  class(pcx) = c(class(pcx), 'pcx');
  return(pcx);
}


#' @title S3 print function for pcx header.
#'
#' @param x a pcx.header instance.
#'
#' @param ... extra args, not used.
#'
#' @export
print.pcxheader <- function(x, ...) {
  pcx = list('data' = NULL, 'header'=x);
  class(pcx) = c(class(pcx), 'pcx');
  print(pcx);
}


#' @title S3 print function for pcx image.
#'
#' @param x a pcx instance.
#'
#' @param ... extra args, not used.
#'
#' @export
print.pcx <- function(x, ...) {
  cat(sprintf("PCX bitmap image, dimension %d x %d pixels, %d channels.\n", x$header$width, x$header$height, x$header$num_channels));

  str_encoding_type = "INVALID";
  if(x$header$encoding_type == 1L) {
    str_encoding_type = 'runlength';
  } else if(x$header$encoding_type == 2L) {
    str_encoding_type = 'none';
  }


  is_indexed = FALSE;
  if(x$header$num_channels == 1L) {
    if(x$header$bitpix %in% c(4L, 8L)) {
      is_indexed = TRUE;
    }
  }

  if(is_indexed) {
    if(x$header$palette_mode == 1L) {
      str_palette_mode = "color/monochrome";
    } else if(x$header$palette_mode == 2L) {
      str_palette_mode = "grayscale";
    } else {
      str_palette_mode = "unknown";
    }
    cat(sprintf("Bits per pixel=%d, indexed (with %s palette type): %d different colors possible. Encoding = %s.\n", x$header$bitpix, str_palette_mode, (2 * x$header$num_channels) ** x$header$bitpix, str_encoding_type));
  } else {
    cat(sprintf("Bits per pixel=%d, not indexed: %d different colors possible. Encoding = %s.\n", x$header$bitpix, (2 * x$header$num_channels) ** x$header$bitpix, str_encoding_type));
  }

}

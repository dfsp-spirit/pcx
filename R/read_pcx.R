
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
#'    pcxf = '~/data/q2_pak0_extracted/models/items/quaddama/skin.pcx';
#'    pcx = read.pcx(pcxf);
#'    plot(imager::as.cimg(pcx$colors));
#'    # show palette:
#'    plot(1:256, col=rgb(pcx$palette, maxColorValue = 255));
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

  header$paintbrush_version = readBin(fh, integer(), n = 1, size = 1, endian = endian);
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

  # The next 2 fields are only present for later format versions (paintbrush_version >= 4). In earlier versions, the reserved2 field is 4 bytes longer. Check whether they make sense before using them.
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

  guessed_graphics_mode = guess.graphics.mode(header);
  img_num_pixels = img_width * img_height;
  img_num_values = img_num_pixels * header$num_channels;
  img_data = array(rep(NA, img_num_values), dim = c(img_width, img_height, header$num_channels));

  # The following is the length of an UNENCODED scanline in bytes:
  scan_line_num_bytes = header$num_channels * header$bytes_per_channels_line;

  bb = 8L;  # padding setting: the byte block size, lines are padded to be a multiple of the bb.
  scanline_padding_size = (scan_line_num_bytes * (bb / header$bitpix)) - img_width;
  header$derived$scanline_padding_size = scanline_padding_size;
  header$derived$guessed_graphics_mode = guessed_graphics_mode;

  #cat(sprintf("'%s': bitpix = %d,  scanline_padding_size = %d\n", filepath, header$bitpix, scanline_padding_size));
  #if(scanline_padding_size != 0L) {
  #  cat(sprintf("'%s': scan_line_num_bytes = %d,  img_width = %d\n", filepath, scan_line_num_bytes, img_width));
  #}

  if(hdr_only) {
    return(header);
  }

  pcx$header = header;
  seek(fh, where = 128L, origin = "start");

  num_zero_repeats_total = 0L;

  # Read and decompress color data
  for(i in 1:img_height) {
    #cat(sprintf("Scanning image line %d of %d.\n", i, img_height));
    for(j in 1:header$num_channels) {
      #cat(sprintf(" * Scanning channel %d of %d [line %d of %d].\n", j, header$num_channels, i, img_height));
      row_pixel_index = 1L;
      bytes_read_this_line = 0L;
      bytes_expanded_this_line = 0L;
      num_skipped_padding = 0L;
      while(bytes_expanded_this_line < header$bytes_per_channels_line) {
        #cat(sprintf(" *   Scanning byte %d of %d [channel %d of %d, line %d of %d].\n", (bytes_read_this_line+1L), header$bytes_per_channels_line, j, header$num_channels, i, img_height));
        raw_value = readBin(fh, integer(), n = 1L, size = 1L, signed = FALSE);
        bytes_read_this_line = bytes_read_this_line + 1L;
        if(length(raw_value) < 1L) {
          cat(sprintf("Reached end of file, but expected more data at byte %d of %d [channel %d of %d, line %d of %d].\n", bytes_read_this_line, header$bytes_per_channels_line, j, header$num_channels, i, img_height));
          break; # last line may be shorter.
        }

        if(raw_value > 192L) { # repeat
          repeat_times = raw_value - 192L;
          if(repeat_times < 1L) {
            cat(sprintf("Repeat zero times.\n"));
          }
          repeat_color = readBin(fh, integer(), n = 1L, size = 1L, signed = FALSE);
          if(length(repeat_color) < 1L) {
            cat(sprintf("Reached end of file, but expected repeat color at byte %d of %d [channel %d of %d, line %d of %d].\n", bytes_read_this_line, header$bytes_per_channels_line, j, header$num_channels, i, img_height));
            break;  # last line may be shorter than bytes_per_channels_line
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
            } else {
              num_zero_repeats_total = num_zero_repeats_total + 1L;
            }
          } else {
            num_skipped_padding = num_skipped_padding + repeat_times;
          }
        } else { # low value: direct color
          if(row_pixel_index <= img_width) { # in image data
            #cat(sprintf("line %d, channel %d: *    - Set 1 pixel (#%d of %d) to %d. [byte %d of %d per channel]\n", i, j, row_pixel_index, img_width, raw_value, k, header$bytes_per_channels_line));
            img_data[row_pixel_index, i, j] = raw_value;
            row_pixel_index = row_pixel_index + 1L;
          } else {
            num_skipped_padding = num_skipped_padding + 1L;
          }
          bytes_expanded_this_line = bytes_expanded_this_line + 1L;
        }

        #cat(sprintf("row_pixel_index=%d, bytes_read_this_line=%d, bytes_expanded_this_line=%d.\n", row_pixel_index, bytes_read_this_line, bytes_expanded_this_line));
      }
    }
  }

  is_indexed = is.pcx.indexed(pcx$header);
  guessed_graphics_mode = guess.graphics.mode(pcx$header);

  # The img_data is decoded, but read as 1-byte integers. We need to honor bitpix to get the correct values.
  img_data_bitpix = img_data;
  #img_data_bitpix = uint8split(img_data, output_bits_per_int = pcx$header$bitpix);

  # check for VGA palette at end of file
  if(is_indexed & guessed_graphics_mode == 'VGA') {
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
      #if(dim(img_data)[3] == 1L) {
        # only 1 channel, use palette.
        pcx$colors = matrix(pcx$palette_rgb[drop(img_data_bitpix)], nrow = pcx$header$height, byrow = TRUE);
      #}
    } else {
      pcx$header$has_palette_at_end = FALSE;
    }
  }

  # This is no indexed VGA image, but we may need to apply the CGA/EGA palette from the header.
  if(is_indexed & guessed_graphics_mode %in% c('CGA', 'EGA')) {
    cat(sprintf("Computing header CGA/EGA palette for indexed non-VGA image (guessed: %s).\n", guessed_graphics_mode));

    palette = compute.header.palette.colors(pcx$header, gfx_mode = guessed_graphics_mode, raw_image_data = img_data);
    if(! is.null(palette)) {
      pcx$colors = matrix(palette[drop(img_data_bitpix)], nrow = pcx$header$height, byrow = TRUE);
    }
  }

  if(! is_indexed) {
    pcx$colors = img_data_bitpix;
  }

  pcx$data = img_data;
  class(pcx) = c(class(pcx), 'pcx');
  return(pcx);
}




#' @title Compute the colors for the CGA/EGA palette from the header.
#'
#' @param pcxheader a PCX header instance
#'
#' @param gfx_mode the graphics mode, one of 'CGA', 'EGA', or 'AUTO' to guess the mode from the data.
#'
#' @param raw_image_data the raw image data as stored in the file, decoded (extracted from the RLE) but without further modification like application of a palette, optional. Can be used to check the validity of the computed palette if given.
#'
#' @return integer matrix, the palette colors.
#' @export
compute.header.palette.colors <- function(pcxheader, gfx_mode = 'AUTO', raw_image_data = NULL) {

  if(! gfx_mode %in% c('AUTO', 'CGA', 'EGA')) {
    stop("Parameter 'gfx_mode' must be one of 'AUTO', 'CGA', or 'EGA'.");
  }
  if(gfx_mode == 'AUTO') {
    gfx_mode = guess.graphics.mode(pcxheader);
  }
  if(! gfx_mode %in% c('CGA', 'EGA')) {
    warning(sprintf("The PCX image seems to use %s mode, cannot use header CGA/EGA palette.\n", gfx_mode));
    return(NULL);
  }

  ega = ega.palette.default16();
  cga_palette0_dark_ega_indices = c(2L, 4L, 6L);
  cga_palette0_bright_ega_indices = c(10L, 12L, 14L);
  cga_palette1_dark_ega_indices = c(3L, 5L, 7L);
  cga_palette1_bright_ega_indices = c(11L, 13L, 15L);
  #cga_palette2_dark_ega_indices = c(3L, 4L, 7L);   # unoffical palette 2, currently not used. see colorburst.
  #cga_palette2_bright_ega_indices = c(11L, 12L, 15L);

  if(gfx_mode == 'CGA') {

    # Check whether the image data could be indexed CGA with 4 colors.
    if(! is.null(raw_image_data)) {
      #print(raw_image_data);
      if(max(raw_image_data, na.rm = TRUE) >= 4L) {
        warning(sprintf("Raw image data contains max value '%d', it does not look like an indexed CGA image: expected max value 4.\n", max(raw_image_data, na.rm = TRUE)));
      }
    }

    # The upper 4 bits of the first byte define the palette background color (its first color).
    # The integer value represented by them is an index into the default EGA palette. The other 2 bytes of
    # the first triplet are ignored.
    background_color_bytes = pcxheader$ega_palette[1:3];
    default_ega_palette_index = bitwShiftR(background_color_bytes[1], 4L);
    palette_background_color = ega.palette.default16()$palette[, default_ega_palette_index];
    num_colors_max = (2 * pcxheader$num_channels) ** pcxheader$bitpix;
    if(num_colors_max == 2L) {
      return(cbind(palette_background_color, c(ega.palette.default16()$palette[, 1L]))); # set 2nd color to black.
    } else { # 4 color mode.
      if(pcxheader$palette_mode > 0L) {
        # file uses the old way of setting status bits.
        status_byte = pcxheader$ega_palette[4];
        #bit_colorburst = as.integer(intToBits(status_byte)[8]);  # we ignore colorburst, see references.
        bit_palette = as.integer(intToBits(status_byte)[7]);
        bit_intensity = as.integer(intToBits(status_byte)[6]);
        if(bit_palette == 0L) {
         # use palette 0
          if(bit_intensity == 1L) {
            return(ega$colors[,cga_palette0_bright_ega_indices]);
          } else {
            return(ega$colors[,cga_palette0_dark_ega_indices]);
          }
        } else { # use palette 1
          if(bit_intensity == 1L) {
            return(ega$colors[,cga_palette1_bright_ega_indices]);

          } else {
            return(ega$colors[,cga_palette1_dark_ega_indices]);
          }
        }
      } else {
        # the file uses the new way
        byte_2nd_entry_green = pcxheader$ega_palette[5];
        byte_2nd_entry_blue = pcxheader$ega_palette[6];
        if(byte_2nd_entry_green > byte_2nd_entry_blue) {
          if(byte_2nd_entry_green > 200) {
            bit_intensity = 1L;
            return(ega$colors[,cga_palette0_bright_ega_indices]);
          } else {
            bit_intensity = 0L;
            return(ega$colors[,cga_palette0_dark_ega_indices]);
          }
        } else {
          if(byte_2nd_entry_green > 200) {
            bit_intensity = 1L;
            return(ega$colors[,cga_palette1_bright_ega_indices]);
          } else {
            bit_intensity = 0L;
            return(ega$colors[,cga_palette1_dark_ega_indices]);
          }
        }
      }
    }
  } else if(gfx_mode == 'EGA') {

    # Check whether the image data could be indexed EGA with 16 colors.
    if(! is.null(raw_image_data)) {
      if(max(raw_image_data, na.rm = TRUE) >= 16L) {
        warning(sprintf("Raw image data contain max value %d, does not look like an indexed EGA image: expected max value 15.\n", max(raw_image_data, na.rm = TRUE)));
      }
    }

    if(pcxheader$paintbrush_version %in% c(0L, 3L)) {
      return(ega.palette.default16()$colors);
    } else {
      # read palette from header.
      if(pcxheader$bitpix == 4L) {
        return(as.matrix(pcxheader$ega_palette[1:(16*3)], ncol=3, byrow=TRUE));
      } else if(pcxheader$bitpix == 3L) {
        return(as.matrix(pcxheader$ega_palette[1:(8*3)], ncol=3, byrow=TRUE));
      } else {
        warning(sprintf("Cannot interprete EGA palette for image with %d bits per pixel (expected 3 or 4).\n", pcxheader$bitpix));
        return(NULL);
      }
    }
  }
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


#' @title S3 plot function for pcx image.
#'
#' @param x a pcx instance.
#'
#' @param ... extra args, not used.
#'
#' @export
#' @importFrom graphics plot
plot.pcx <- function(x, ...) {
  if(requireNamespace('imager', quietly = TRUE)) {
    graphics::plot(imager::as.cimg(x$colors));
  } else {
    stop("The 'imager' package must be installed to plot PCX images.");
  }
}


#' @title Determine from PCX header whether the image is indexed, i.e., whether it uses a fixed palette.
#'
#' @param pcxheader a PCX header instance
#'
#' @return logical, whether the image is indexed.
#'
#' @keywords internal
is.pcx.indexed <- function(pcxheader) {
  is_indexed = FALSE;
  if(pcxheader$num_channels == 1L) {
    if(pcxheader$bitpix %in% c(2L, 4L, 8L)) {
      is_indexed = TRUE;
    }
  }
  return(is_indexed);
}


#' @title S3 print function for pcx image.
#'
#' @param x a pcx instance.
#'
#' @param ... extra args, not used.
#'
#' @export
print.pcx <- function(x, ...) {
  cat(sprintf("%d-bit PCX bitmap image, dimension %d x %d pixels, %d channels. Format version %d.\n", (x$header$num_channels * x$header$bitpix), x$header$width, x$header$height, x$header$num_channels, x$header$paintbrush_version));

  str_encoding_type = "INVALID";
  if(x$header$encoding_type == 1L) {
    str_encoding_type = 'Runlength';
  } else if(x$header$encoding_type == 2L) {
    str_encoding_type = 'No';
  }

  is_indexed = is.pcx.indexed(x$header);
  num_colors_max = (2 * x$header$num_channels) ** x$header$bitpix;
  guessed_graphics_mode = guess.graphics.mode(x$header);

  if(is_indexed) {
    if(x$header$palette_mode == 1L) {
      str_palette_mode = "color/monochrome";
    } else if(x$header$palette_mode == 2L) {
      str_palette_mode = "grayscale";
    } else {
      str_palette_mode = "unspecified";
    }
    cat(sprintf("Indexed with %s palette type: %d different colors possible, assuming %s mode. %s encoding.\n", str_palette_mode, num_colors_max, guessed_graphics_mode, str_encoding_type));
  } else {
    cat(sprintf("Not indexed: %d different colors possible, assuming %s mode. %s encoding.\n", num_colors_max, guessed_graphics_mode, str_encoding_type));
  }
}


#' @title Guess graphics mode from header.
#'
#' @param pcxheader PCX header instance
#'
#' @return one of 'CGA', 'EGA', or 'VGA'.
#'
#' @keywords internal
guess.graphics.mode <- function(pcxheader, raw_image_data = NULL) {
  num_colors_max = (2 * pcxheader$num_channels) ** pcxheader$bitpix;

  max_raw_image_data_value = NULL;
  if(! is.null(raw_image_data)) {
    max_raw_image_data_value = max(raw_image_data, na.rm = TRUE);
  }

  if(pcxheader$bitpix < 8L) {
    if(num_colors_max %in% c(2L, 4L)) {
      guessed_graphics_mode = 'CGA';
    } else if(num_colors_max %in% c(8L, 16L)) {
      guessed_graphics_mode = 'EGA';
    } else {
      warning(sprintf("Detected image with %d bits per pixel per channel, %d channels and %d colors max. Guessing EGA mode.\n", pcxheader$bitpix, pcxheader$num_channels, num_colors_max));
      guessed_graphics_mode = 'EGA';
    }
  } else { # bitpix >= 8
    guessed_graphics_mode = 'VGA';
    if(num_colors_max < 256L) {
      warning(sprintf("Detected image with %d bits per pixel per channel, %d channels and %d colors max. Guessing VGA mode.\n", pcxheader$bitpix, pcxheader$num_channels, num_colors_max));
    }
  }
  return(guessed_graphics_mode);
}


#' @title Get default 16 colors 6-bit EGA palette.
#'
#' @return named list with entries (1) `info`: data.frame with entries `index`, `color_names`, and `color_code_decimal` and (2) `colors`: 16x3 integer matrix representing RGB colors, in range 0-255.
#'
#' @note EGA allowed one to create palettes by selecting 16 colors out of 64 possible ones. This is the default palette, see the \code{info$color_code_decimal} field to see which of the 64 colors were selected.
#'
#' @keywords internal
#' @importFrom grDevices col2rgb
ega.palette.default16 <- function() {
  color_names = c('black', 'blue', 'green', 'cyan', 'red', 'magenta', 'yellow', 'light gray',
                  'dark gray', 'bright blue', 'bright green', 'bright cyan', 'bright red', 'bright magenta', 'bright yellow', 'white');
  color_code_decimal = c(0L, 1L, 2L, 3L, 4L, 5L, 20L, 7L,
                         56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L);
  info_df = data.frame('index' = seq.int(0, 15), 'name' = color_names, 'color_code_decimal' = color_code_decimal);

  col_matrix = grDevices::col2rgb(c('#000000', '#0000AA', '#00AA00', '#00AAAA', '#AA0000', '#AA00AA', '#AA5500', '#AAAAAA',
                         '#555555', '#AAAAFF', '#55FF55', '#55FFFF', '#FF55FF', '#FF5555', '#FFFF55', '#FFFFFF'));

  return(list('info' = info_df, 'colors' = col_matrix));
}


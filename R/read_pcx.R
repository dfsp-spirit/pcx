

# pcxf = '~/data/q2_pak/models/items/quaddama/skin.pcx';
# pcx = read.pcx(pcxf);
#' @title Read bitmap file in PCX format.
#'
#' @param filepath character string, path to the file including extension
#'
#' @param hdr logical, whether to return full list with header
#'
#' @return array with color data, or pcx instance (named list) if `hdr` is `TRUE`
#'
#' @export
read.pcx <- function(filepath, hdr = TRUE) {
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
  header$bitpix = readBin(fh, integer(), n = 1, size = 1, endian = endian); # bits per pixel, defines number of possible colors in image. 1 = 2, 2 = 4, 3 = 16, 4 = 256.

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
  header$screen_size_horizontal = readBin(fh, integer(), n = 1, size = 2, endian = endian); # horizontal screen resolution of source system
  header$screen_size_vertical = readBin(fh, integer(), n = 1, size = 2, endian = endian); # vertical
  header$reserved2 = readBin(fh, integer(), n = 54, size = 1, endian = endian);

  pcx$header = header;

  img_width = header$maxx - header$minx + 1L;
  img_height = header$maxy - header$miny + 1L;

  img_num_pixels = img_width * img_height;
  img_num_values = img_num_pixels * header$num_channels;
  img_data = array(rep(NA, img_num_values), dim = c(img_width, img_height, header$num_channels));

  scan_line_num_bytes = header$num_channels * header$bytes_per_channels_line;
  bb = 8L;
  if(scan_line_num_bytes %% bb != 0L) {
    scan_line_num_bytes = ((scan_line_num_bytes / bb) + 1L) * bb; # lines are padded to next full byte.
  }

  seek(fh, where = 128, origin = "start");

  # Read and decompress color data
  for(i in 1:img_height) {
    for(j in 1:header$num_channels) {
      row_pixel_index = 1L;
      for(k in 1:header$bytes_per_channels_line) {
        raw_value = readBin(fh, integer(), n = 1L, size = 1L);
        if(raw_value > 192L) { # repeat
          repeat_times = raw_value - 192L;
          repeat_color = readBin(fh, integer(), n = 1L, size = 1L);
          k = k + 1L;
          if(row_pixel_index <= img_width) { # in image data
            if(repeat_times > 0L) {
              for(l in 1:repeat_times) {
                img_data[i, row_pixel_index, j] = repeat_color;
                row_pixel_index = row_pixel_index + 1L;
              }
            }
          }
        } else { # low value: direct color
          if(row_pixel_index <= img_width) { # in image data
            img_data[i, row_pixel_index, j] = raw_value;
          }
        }
      }
    }
  }

  # check for palette

  pcx$data = img_data;
  return(pcx);
}

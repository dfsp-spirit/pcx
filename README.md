# pcx
Read pcx bitmap image files in R.

## About

The PCX (Picture Exchange) format is an old bitmap image format that is compressed using runlength encoding and typically indexed, i.e. used with a fixed palette. It was used together with the WAL format for parts of the assets (skybox textures, model skins, UI pictures, sprites) in the early Quake series games (id tech1 and tech2 engines).

## Installation

**This is still work in progress.** You are of course free to try it, the best way is `devtools::install_github("dfsp-spirit/pcx")`.

## Package API

This is work in progress, but here is the current state:

    # Read a PCX image from a file at filepath:
    pcx = pcx::read.pcx(filepath);

    # print information from the PCX header
    pcx;


Here are some ideas on what to do with your image:

    # show the image using 'imager' package
    plot(imager::as.cimg(pcx$colors));

    # plot the palette (indexed images with VGA palette only):
    plot(1:nrow(pcx$palette), col=rgb(pcx$palette, maxColorValue = 255));

    # export the image to JPEG format (requires the 'jpeg' package):
    jpeg::writeJPEG(pcx$colors/255., target = '~/myimage.jpg');


### Return value details

The returned `pcx` object in the example code above is a named list with the following entries:

* `colors`: this is what you want, the image. An array of integers in range 0-255 representing RGB colors, with three dimensions in the following order: width, height, channels. The channels are in order R, G, B. If the image is indexed, this has been created by applying the `palette` to the `data` (see below).
* `header`: named list, containing the header fields and values from the file.
* `palette`: the optional VGA image palette (256 fixed colors), a vector of intensities (for images with 1 channel) or a matrix of RGB colors (for multi-channel images). This is `NULL` if the file does not contain a VGA palette. Note that for very old PCX files (CGA/EGA with <= 16 fixed colors), the palette is stored in the `header$ega_palette` field instead.
* `data`: the raw image data, as read from the file. The palette has *not* been applied to this. Usually not needed, but you could use this to apply a custom palette to the image data.


### A note on the interpretation of indexed PCX color data

The interpretation of indexed PCX images, especially in CGA/EGA mode, is not well defined and thus the same image may be displayed differently by different viewers, as explained in the references. If the `colors` returned by this package do not match your expectations, feel free to apply your own interpretation using the `header` information and `data`.

However, if you know the PCX specs well and feel confident enough that the interpretation of this package is definitely *wrong* for a file, please open an issue and attach the file together with a detailed description of your expectations. If possible, please include a screenshot of the rendering of the image in some standard software that matches your expectations.


## References

* The [PCX spec](http://bespin.org/~qz/pc-gpe/pcx.txt)
* A general [PCX format description at Wikipedia](https://en.wikipedia.org/wiki/PCX)
* More details and assembly reading code at the [Shikadi Modding Wiki](http://www.shikadi.net/moddingwiki/PCX_Format). Also checkout the references section.


## Unit tests and CI

<!-- badges: start -->
[![R-CMD-check](https://github.com/dfsp-spirit/pcx/workflows/R-CMD-check/badge.svg)](https://github.com/dfsp-spirit/pcx/actions)

[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/dfsp-spirit/pcx?branch=master&svg=true)](https://ci.appveyor.com/project/dfsp-spirit/pcx) AppVeyor CI under Windows
<!-- badges: end -->


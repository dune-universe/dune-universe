========================================================
CamlImages - Objective Caml image processing library
========================================================

What is CamlImages ?
========================================================

This is an image processing library, which provides some basic
functions of image processing and loading/saving various image file
formats. In addition the library can handle huge images that cannot be
(or can hardly be) stored into the memory (the library automatically
creates swap files and escapes them to reduce the memory usage).

Installation
========================================================

Read the file INSTALL.txt

Using CamlImages
========================================================

Color models
--------------------------------------------------------

CamlImages supports the following color models:

* Rgb24 -- 24bit depth full color image
* Index8 -- 8bit depth indexed image with transparent information
* Index16 -- 16bit depth indexed image with transparent information

For each color models, the corresponding module is provided. Use the module
Rgb24 if you want to access 24bit depth full color images, for example.

Load/Save image files and other fancy features
--------------------------------------------------------

CamlImages supports loading and saving of the following file formats:

* Bitmap (.bmp)
* Tiff (.tiff or .tif), color only
* Jpeg (.jpeg or .jpg)
* Png  (.png)
* Ppm (.pbm, .pgm, .ppm), portable pixmaps
* PS (.ps, .eps), PostScript files
* X Pixmap (.xpm), no saving
* Gif (.gif) (not recommended)
* EXIF tag

For each image format, we provide a separate module. For instance,
there is a Tiff module to load and save images stored in the tiff file
format.

If you do not want to specify the file format, you can use Image.load:
this function automatically analyses the header of the image file at hand
and loads the image into the memory, if the library supports this format.

CamlImages also provides an interface to the internal image format of
O'Caml's Graphics library (this way you can draw your image files into 
the Graphics window).

You can also draw strings on images using the Freetype library, which 
is an external library to load and render TrueType fonts.

Class interface
--------------------------------------------------------

The modules begins the letter 'o' are the class interface for CamlImages.

Image swap
--------------------------------------------------------

When you create/load a huge image, the computer memory may not be
sufficient to contain all the data. (For example, this may happen if
you are working with a scanned image of A4, 720dpi, 24bit fullcolor,
even if you have up to 128Mb of memory!) 
(Well, my son, the first version of this document was written around 1998,
and computers had less memory at that time.)
To work with such huge
images, CamlImages provides image swaps, which can escape part of the
images into files stored on the hard disk. A huge image is thus
partitioned into several blocks and if there is not enough free
memory, the blocks which have not been accessed recently are swapped
to temporary files.  If a program requests to access to such a swapped
block, the library silently loads it back into memory.

By default, image swapping is disabled, because it slows down the
programs. To activate this function, you have to modify
Bitmap.maximum_live and Bitmap.maximum_block_size. Bitmap.maximum_live
is the maximum heap live data size of the program (in words) and
Bitmap.maximum_block_size is the maximum size of swap blocks (in
words).

For example, if you do not want to use more than 10M words (that is
40Mb for a 32bit architecture or 80Mb for a 64bit architecture), set
Bitmap.maximum_live to 10000000. You may (and you should) enable heap
compaction, look at the GC interface file, gc.mli, in the standard
library for more details (you should change the compaction configuration).

Bitmap.maximum_block_size affects the speed and frequency of image
block swapping. If it is larger, each swapping becomes slower. If it
is smaller, more swappings will occur. Too large and too small
maximum_block_size, both may make the program slower. I suggest to
have maximum_block_size set to !Bitmap.maximum_live / 10.

If you activated image swapping, cache files for unused swapped 
blocks will be removed automatically by Caml GC finalization, 
but you may free them explicitly by hand also. The functions and methods 
named "destroy" will free those blocks. 

The swap files are usually created in the /tmp directory.  If you
set the environment variable "CAMLIMAGESTMPDIR", then its value
replaces the default "/tmp" directory. The temporary files are erased
when the program exits successfully. In other situations, for instance
in case of spurious exception, you may need to erase temporary files
manually.

Where to report issues?
==========================================================

https://bitbucket.org/camlspotter/camlimages/issues?status=new&status=open

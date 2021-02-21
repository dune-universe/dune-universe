0.2 2021-02-20
--------------

- *Breaking change*: `Palette.t` now represent a single palette.
  Constants such as `ylgn`,... are now of type `t list`.
- The [Matplotlib palettes](https://bids.github.io/colormap/) have
  been added.
- Make possible to create gradients from palettes.
- New functions `to_gray`, `Gradient.cmyk`, `Palette.get_rgb`,
  `Palette.get_cmyk`, `Palette.ty`, `Palette.blind`, `Palette.print`,
  `Palette.copy`, and `Palette.lcd`.
- The test `tests/gradient.ml` displays all available palettes.

0.1 2019-12-09
--------------

- Initial release.

0.2 2019-12-08
--------------

- New function `to_latex_channel`.
- Allow to specify the color when converting to LaTeX.
- Automatically divide the path into several PGF/TikZ paths when it is
  too long for LaTeX capacity.  This is configurable.
- LaTeX output can draw arrows on paths.
- Improve the sampling procedure: better determine the slope at
  endpoints, be less reactive to small zigzags that may be due to
  rough estimates, and use viewport scaling to estimate all costs.
- Use an internal random state and not the global one.

0.1 2018-11-28
--------------

- Initial release

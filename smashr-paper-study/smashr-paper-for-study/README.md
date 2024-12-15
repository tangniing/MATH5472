# smashr: smoothing using Adaptive Shrinkage in R

This R package implements fast, wavelet-based Empirical Bayes
shrinkage methods for signal denoising. This includes smoothing
Poisson-distributed data and Gaussian-distributed data, with possibly
heteroskedastic error. The algorithms implement the methods described
in [Xing, Carbonetto & Stephens (2021)][smash-jmlr].

If you find a bug, please post an [issue][issues].

## License

Copyright (c) 2016-2021, Zhengrong Xing, Peter Carbonetto and Matthew
Stephens.

All source code and software in this repository is free software; you
can redistribute it and/or modify it under the terms of the
[GNU General Public License][gpl] as published by the
[Free Software Foundation][fsf]; either version 3 of the License, or
(at your option) any later version. See the [LICENSE](LICENSE) file
for the full text of the license.

## Citing this work

If you find that this R package useful for your work, please cite our
paper:

> Zhengrong Xing, Peter Carbonetto and Matthew Stephens (2021).
> [Flexible signal denoising via flexible empirical Bayes shrinkage.][smash-jmlr]
> *Journal of Machine Learning Research* 22(93), 1-28.

## Quick Start

Follow these steps to quickly get started using smashr.

1. In R, install the latest version of smashr using [devtools][devtools]:

   ```R
   install.packages("devtools")
   library(devtools)
   install_github("stephenslab/smashr")
   ```

   If you are interested in replicating results from the paper,
   we recommendg installing smashr 1.2-7:

   ```R
   install_github("stephenslab/smashr@v1.2-7")
   ```

   This will build the smashr package *without* the vignettes. To
   build with the vignettes, do this instead:

   ```R
   install_github("stephenslab/smashr",build_vignettes = TRUE)
   ```
   
   We caution that some of the simulation examples may take a long
   time to run (20--30 minutes, or possibly longer). Also note that the
   `install_github` call should also install any missing packages that
   are required for smashr to work.

2. Load the smashr package, and run the smashr demo:

   ```R
   library(smashr)
   demo("smashr")
   ```
   
3. To learn more, see the smashr package help and the smashr vignette
   (which you can also view [here][smashr-web]):

   ```R
   help(package = "smashr")
   vignette("smashr")
   ```
   
## Credits

This R package was developed by [Zhengrong Xing][zhengrong] and
[Matthew Stephens][matthew] at the University of Chicago, with
contributions from [Peter Carbonetto][peter].

[smash-jmlr]: https://jmlr.org/papers/v22/19-042.html
[issues]: https://github.com/stephenslab/smashr/issues
[gpl]: http://www.gnu.org/licenses/gpl.html
[fsf]: https://www.fsf.org
[smashr-web]: https://stephenslab.github.io/smashr
[devtools]: https://github.com/r-lib/devtools
[zhengrong]: https://github.com/zrxing
[matthew]: http://stephenslab.uchicago.edu
[peter]: http://pcarbo.github.io

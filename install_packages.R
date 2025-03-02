# install required packages to run - the package version used is shown for reference
# if you need to install an older version of a package you can find the urls on CRAN
# https://stackoverflow.com/questions/17082341/installing-older-version-of-r-package
# e.g. for zoo the archive can be found here https://cran.r-project.org/src/contrib/Archive/zoo/

install.packages('tidyverse') # version 2.0.0
install.packages('zoo') # version 1.8.12
install.packages('RCurl') # version 1.98.1.8
install.packages('lubridate') # version 1.9.3
install.packages('tsibble') # version 1.1.5
install.packages('remotes') # 2.4.2.1

# some plotting packages
install.packages('ggpubr') # version 0.6.0
install.packages('ggridges') # version 0.5.6
install.packages('gratia') # version 0.9.2

# a plotting package installation from Github (the CRAN version doesn't work atm)
remotes::install_github('teunbrand/ggh4x') # version 0.3.0.9000

# required statistical packages
install.packages('car')
install.packages('broom') # version 1.0.5
install.packages('lmer4')
install.packages('mgcv') # version 1.91
install.packages('marginaleffects') # version 0.24.0

# install required packages to run
install.packages(c('tidyverse', 'zoo', 'RCurl', 'lubridate', 'tsibble', 'remotes'))

# some plotting packages
install.packages(c('ggpubr', 'ggridges'))

# a plotting package installation from Github (the CRAN version doesn't work atm)
remotes::install_github('teunbrand/ggh4x')

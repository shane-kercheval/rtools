FROM r-base

RUN apt-get update -y && apt-get install zsh -y
RUN PATH="$PATH:/usr/bin/zsh"

RUN mkdir /code
WORKDIR /code
ENV PATH "$PATH:/code"

RUN apt-get install -y libxml2-dev libcurl4-openssl-dev libssl-dev libgit2-dev pandoc

RUN R -e "install.packages('broom')"
RUN R -e "install.packages('ChannelAttribution')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('DescTools')"
RUN R -e "install.packages('forcats')"
RUN R -e "install.packages('fpp2')"
RUN R -e "install.packages('gapminder')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('grid')"
RUN R -e "install.packages('gridExtra')"
RUN R -e "install.packages('Hmisc')"
RUN R -e "install.packages('lubridate')"
RUN R -e "install.packages('magrittr')"
RUN R -e "install.packages('modelr')"
RUN R -e "install.packages('moments')"
RUN R -e "install.packages('networkD3')"
RUN R -e "install.packages('nycflights13')"
RUN R -e "install.packages('purrr')"
RUN R -e "install.packages('reshape2')"
RUN R -e "install.packages('rsample')"
RUN R -e "install.packages('scales')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('timeDate')"
RUN R -e "install.packages('tidyr')"
RUN R -e "install.packages('tidyselect')"
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('tibble')"
RUN R -e "install.packages('zoo')"
RUN R -e "install.packages('testthat')"
RUN R -e "install.packages('devtools')"
RUN R -e "install.packages('webshot')"

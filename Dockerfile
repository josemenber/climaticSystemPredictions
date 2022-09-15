FROM opencpu/rstudio:latest


#    R Package Manager fix  https://github.com/rocker-org/rocker-versioned2/issues/301#issuecomment-984679756
#RUN echo 'options(repos = c(CRAN = "https://cloud.r-project.org"))' >> ${R_HOME}/etc/Rprofile.site


# OPENCPU ---> https://opencpu.github.io/server-manual/opencpu-server.pdf
#    apache mail server fix  https://stackoverflow.com/questions/40890011/ubuntu-dockerfile-mailutils-install
ENV DEBIAN_FRONTEND="noninteractive"
RUN apt-get update && apt-get install -y mailutils # https://stackoverflow.com/questions/40890011/ubuntu-dockerfile-mailutils-install

RUN sudo apt-get update
RUN sudo apt-get upgrade -y
RUN sudo apt-get install software-properties-common -y
RUN sudo add-apt-repository ppa:opencpu/opencpu-2.2 -y
RUN sudo apt-get update
RUN sudo apt-get install -y opencpu-server
RUN sudo apt-get install libcurl4-openssl-dev libssl-dev
RUN sudo apt -y install libfontconfig1-dev
RUN sudo apt -y install libharfbuzz-dev libfribidi-dev
RUN sudo apt install -y libtiff-dev

# LIBRERIAS ML
RUN R -e "options(repos = 'https://cloud.r-project.org/')"
RUN R -e "install.packages(c('ragg'), dependencies = T)"
RUN R -e "install.packages(c('devtools'), dependencies = T)"
RUN R -e "install.packages(c('randomForest'), dependencies = T)"
RUN R -e "install.packages(c('stats'), dependencies = T)"
RUN R -e "install.packages(c('R.utils'), dependencies = T)"
RUN R -e "install.packages(c('ranger'), dependencies = T)"

# NUESTRO PAQUETE
RUN R -e "devtools::install_github('https://github.com/josemenber/climaticSystemPredictions.git', ref = 'master')"

#KERAS + TENSORFLOW ---> https://tensorflow.rstudio.com/installation/
#RUN R -e "install.packages('tensorflow')"
#RUN R -e "tensorflow::install_tensorflow()"
#RUN R -e "install.packages('keras')"

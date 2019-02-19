FROM debian:testing
RUN apt-get update && \
    apt-get install -y sudo wget unzip openjdk-8-jre ocaml llvm-dev \
      libclang-dev libc6-dev-i386
RUN useradd -m -s /bin/bash ci
RUN echo ci      ALL=\(ALL\) NOPASSWD:ALL >/etc/sudoers
USER ci
ENV HOME /home/ci
WORKDIR $HOME
SHELL ["/bin/bash", "-c"]
ENV SONAR_VERSION=3.3.0.1492
RUN wget https://binaries.sonarsource.com/Distribution/sonar-scanner-cli/sonar-scanner-cli-$SONAR_VERSION-linux.zip
RUN unzip sonar-scanner-cli-$SONAR_VERSION-linux.zip
RUN ln -s sonar-scanner-$SONAR_VERSION-linux sonar-scanner
ENV SONAR_ROOT $HOME/sonar-scanner
RUN echo export SONAR_ROOT=$SONAR_ROOT >$HOME/.bash_env
RUN echo export PATH=\$SONAR_ROOT/bin:\$PATH >>$HOME/.bash_env
ENV BASH_ENV $HOME/.bash_env
RUN wget --no-check-certificate \
    https://sonarqube.bordeaux.inria.fr/static/digicertca.crt
RUN keytool -import -alias inria -storepass "changeit" \
    -keystore $SONAR_ROOT/jre/lib/security/cacerts -file digicertca.crt
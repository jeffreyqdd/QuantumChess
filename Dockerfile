FROM ubuntu:latest as base_image

# install dev tools and ocaml
RUN apt update && \
  apt install -y build-essential zip unzip && \
  apt install -y opam && \
  apt -y clean all && \
  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /root.cache/

# create user. It's considered safer to have a user than to be purely root
RUN useradd -ms /bin/bash software 

# init OPAM environment
FROM base_image as opam_init
USER software
RUN opam init --bare -a -y
RUN opam switch create final-project-sp23 ocaml-base-compiler.4.14.0
COPY install/opam_packages.sh /tmp/
RUN /tmp/opam_packages.sh
USER root 
RUN rm /tmp/opam_packages.sh



# finalized development environment
FROM base_image
USER software
WORKDIR /home/software/quantum_chess
COPY --from=opam_init /home/software/.opam /home/software/.opam
RUN echo "[[ ! -r /home/software/.opam/opam-init/init.sh ]] || source \
        /home/software/.opam/opam-init/init.sh  > /dev/null 2> /dev/null" \
         >> ~/.bashrc
RUN echo "alias ls='ls --color'" >> ~/.bashrc


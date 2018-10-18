FROM heroku/heroku:18

ENV LANG C.UTF-8

# Install required packages.
RUN apt-get update
RUN apt-get upgrade -y --assume-yes
# Install packages for stack and ghc.
RUN apt-get install -y --assume-yes xz-utils gcc libgmp-dev zlib1g-dev
# Install packages needed for libraries used by our app.
RUN apt-get install -y --assume-yes libpq-dev
# Install convenience utilities, like tree, ping, and vim.
RUN apt-get install -y --assume-yes tree iputils-ping vim-nox

# Recently added 
RUN apt-get install -y --assume-yes libc6 libssl-dev libcurl4-gnutls-dev

# Remove apt caches to reduce the size of our container.
RUN rm -rf /var/lib/apt/lists/*

# Install stack to /opt/stack/bin.
RUN mkdir -p /opt/stack/bin
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'

# Create /opt/api/bin and /opt/api/src.  Set
# /opt/api/src as the working directory.
RUN mkdir -p /opt/api/src
RUN mkdir -p /opt/api/bin
WORKDIR /opt/api/src

# Set the PATH for the root user so they can use stack.
ENV PATH "$PATH:/opt/stack/bin:/opt/api/bin"

# Install GHC using stack, based on your app's stack.yaml file.
COPY ./stack.yaml /opt/api/src/stack.yaml
RUN stack --no-terminal setup

# Install all dependencies in app's .cabal file.
COPY ./api.cabal /opt/api/src/api.cabal

RUN stack --no-terminal test --only-dependencies

# Build application.
COPY . /opt/api/src
RUN stack -v --no-terminal build

# Install application binaries to /opt/api/bin.
RUN stack --no-terminal --local-bin-path /opt/api/bin install

# Remove source code.
#RUN rm -rf /opt/api/src

# Add the apiuser and setup their PATH.
RUN useradd -ms /bin/bash apiuser
RUN chown -R apiuser:apiuser /opt/api
USER apiuser
ENV PATH "$PATH:/opt/stack/bin:/opt/api/bin"

# Set the working directory as /opt/api/.
WORKDIR /opt/api

EXPOSE 3000
CMD /opt/api/bin/api


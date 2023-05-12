FROM haskell:9.4.5-buster
WORKDIR /
COPY . .
RUN cabal update
RUN cabal build
CMD cabal run
EXPOSE 8080

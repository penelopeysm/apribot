FROM penelopeysm/apribot:latest
WORKDIR /
COPY . .

# This line circumvents a spurious error with cabal.project's git submodule.
# It should go away if I ever upload reddit-oauth2 to Hackage
RUN git config --global --add safe.directory '*'
RUN cabal update
RUN cabal build

CMD cabal run
EXPOSE 8080

FROM penelopeysm/apribot:latest

WORKDIR /
# Directories
COPY ./app ./app
COPY ./python ./python
# Files
COPY ./apribot.cabal .
COPY ./cabal.project .
COPY ./cabal.project.freeze .

RUN git config --global --add safe.directory '*' \
    && cabal update \
    && cabal build

CMD cabal run
EXPOSE 8080

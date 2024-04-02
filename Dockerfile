# Build server
FROM node:20.5.1 AS server-builder

WORKDIR /web
COPY web/package-lock.json .
COPY web/package.json .
COPY web/svelte.config.js .
COPY web/tsconfig.json .
COPY web/vite.config.ts .
COPY web/src ./src
COPY web/static ./static
RUN npm install \
    && npm run build \
    && npm prune --production

FROM penelopeysm/apribot:latest

# Install Haskell bot
WORKDIR /hs
COPY ./hs/app ./app
COPY ./hs/python ./python
COPY ./hs/static ./static
COPY ./hs/apribot.cabal .
COPY ./hs/cabal.project .
COPY ./hs/cabal.project.freeze .
RUN git config --global --add safe.directory '*' \
    && cabal update \
    && cabal build

# Install Python requirements
WORKDIR /hs/python
RUN python -m pip install -r requirements.txt

# Copy built files for web server
WORKDIR /web
COPY --from=server-builder ./web/node_modules ./node_modules
COPY --from=server-builder ./web/build ./build
COPY --from=server-builder ./web/package.json .

# Launch nginx
WORKDIR /
COPY ./nginx.conf /etc/nginx/conf.d
RUN ln -sf /dev/stdout /var/log/nginx/access.log && ln -sf /dev/stderr /var/log/nginx/error.log

# Launch supervisor
COPY ./supervisord.conf .
CMD ["supervisord", "-c", "supervisord.conf"]
EXPOSE 80

# syntax = docker/dockerfile:1
FROM node:latest AS base

LABEL fly_launch_runtime="Bun.js"

RUN npm install -g bun

FROM base AS installer
COPY packages /app/packages
RUN find /app/packages \! -name "package.json" -mindepth 2 -maxdepth 2 -print | xargs rm -rf
COPY package.json bun.lockb /app/
RUN cd /app && bun install

FROM installer AS builder
WORKDIR /app
COPY applications/test-solid-start-bun-effect /app/applications/test-solid-start-bun-effect
COPY entrypoint.sh /app/entrypoint.sh
RUN bun install
RUN bun run nx run @code9/test-solid-start-bun-effect:nx-build

FROM builder AS runner
EXPOSE 3000

ENTRYPOINT []
CMD ["/app/entrypoint.sh"]

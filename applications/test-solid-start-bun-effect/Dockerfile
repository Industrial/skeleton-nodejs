# syntax = docker/dockerfile:1

# Adjust NODE_VERSION as desired
ARG NODE_VERSION=20.18.0
# Adjust BUN_VERSION as desired
ARG BUN_VERSION=1.2.0
FROM oven/bun:${BUN_VERSION} AS base

LABEL fly_launch_runtime="Node.js"

WORKDIR /app

COPY package.json /app
RUN bun install

COPY . .
RUN bun nx run @code9/test-solid-start-bun-effect:nx-build

USER bun
EXPOSE 3000

CMD ["run" "nx" "run" "@code9/test-solid-start-bun-effect:nx-start"]

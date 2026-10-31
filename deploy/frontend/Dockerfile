# Builds a static bfweb or bfsite bundle and serves it with nginx.
#
# Build args:
#   APP           - "bfweb" or "bfsite" (default: bfweb)
#   VITE_API_BASE - absolute origin of the bfdb API, e.g. https://api.example.com
#                   (no trailing slash). Baked into the JS bundle at build time.
#
# Example:
#   docker build -f deploy/frontend/Dockerfile \
#     --build-arg APP=bfweb \
#     --build-arg VITE_API_BASE=https://api.example.com \
#     -t vector-dashboard .
#
#   docker build -f deploy/frontend/Dockerfile \
#     --build-arg APP=bfsite \
#     --build-arg VITE_API_BASE=https://api.example.com \
#     -t vector-site .

FROM node:20-alpine AS build
ARG APP=bfweb
ARG VITE_API_BASE=
WORKDIR /src
COPY ${APP}/package.json ${APP}/package-lock.json ./
RUN npm ci
COPY ${APP}/ ./
ENV VITE_API_BASE=${VITE_API_BASE}
# bfsite defaults its Vite `base` to /site/ (for when it's embedded in bfdb);
# standalone it should be served at the domain root instead.
RUN npx tsc -b && \
    if [ "$APP" = "bfsite" ]; then npx vite build --base=/; else npx vite build; fi

FROM nginx:alpine AS serve
COPY deploy/frontend/nginx.conf /etc/nginx/conf.d/default.conf
COPY --from=build /src/dist /usr/share/nginx/html
EXPOSE 80

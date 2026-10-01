# The image scripts/build_manual.py builds the manual in. The base image is
# pinned by digest so a retagged upstream image cannot change the toolchain.
FROM asciidoctor/docker-asciidoctor:1.100.0@sha256:93ea06944afbc8e7a74ea31348b5f0624f1dfc69fa038ead3e7c4838faaa27d7

RUN apk add --no-cache chromium nodejs npm poppler-utils python3 \
    && npm install --global @mermaid-js/mermaid-cli@11.16.0 \
    && npm cache clean --force

ENV PUPPETEER_EXECUTABLE_PATH=/usr/bin/chromium-browser

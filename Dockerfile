FROM node:alpine
RUN apk add --no-cache fish git
WORKDIR /app
COPY . .
CMD ["fish", "scripts/entrypoint.fish"]

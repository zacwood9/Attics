FROM nixos/nix AS builder

# update packages
RUN nix-channel --update nixpkgs

# speed up compile time by using digitallyinduced's cachix cache
RUN nix-env -i cachix
RUN cachix use digitallyinduced

RUN mkdir -p /app/web

RUN nix-env -i git
RUN git clone https://github.com/digitallyinduced/ihp.git /app/web/IHP

ADD web /app/web
WORKDIR /app

ADD build.nix .
ADD default.nix .

RUN nix-build


# Store all runtime dependencies in a folder
RUN mkdir /tmp/nix-store-closure
RUN cp -R $(nix-store -qR result/) /tmp/nix-store-closure

FROM scratch

# Copy over runtime dependencies, application code, and library
COPY --from=builder /tmp/nix-store-closure /nix/store
COPY --from=builder /app/result /app
COPY --from=builder /app/web/IHP /app/IHP

# certs for HTTPS requests
COPY --from=builder /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/ca-certificates.crt

WORKDIR /app

CMD ["bin/RunProdServer"]

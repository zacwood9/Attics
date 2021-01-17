FROM nixos/nix AS builder

RUN mkdir -p /app/web

ADD web /app/web
WORKDIR /app

RUN nix-channel --update nixpkgs

RUN nix-env -i cachix
RUN cachix use digitallyinduced

RUN nix-env -i git
RUN git clone https://github.com/digitallyinduced/ihp.git web/IHP

ADD build.nix .
ADD default.nix .

RUN nix-build

RUN mkdir /tmp/nix-store-closure
RUN cp -R $(nix-store -qR result/) /tmp/nix-store-closure

FROM scratch

COPY --from=builder /tmp/nix-store-closure /nix/store
COPY --from=builder /app/result /app
COPY --from=builder /app/web/IHP /app/web/IHP

WORKDIR /app/web

LABEL org.opencontainers.image.source=https://github.com/zacwood9/Attics

CMD ["bin/RunProdServer"]

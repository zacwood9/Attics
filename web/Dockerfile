FROM nixos/nix

RUN nix-env --install cachix
RUN cachix use digitallyinduced
RUN nix-env --install git

RUN mkdir /var/app
COPY . /var/app
WORKDIR /var/app

RUN nix-shell --run "make build/bin/RunUnoptimizedProdServer"
RUN nix-shell --run "make static/prod.css"
RUN nix-shell --run "make static/prod.js"

ENV PORT 8000

CMD ["nix-shell", "--run" "'./build/bin/RunProdServer'"]


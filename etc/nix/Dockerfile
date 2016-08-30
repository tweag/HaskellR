# Build environment for HaskellR
FROM nixos/nix
MAINTAINER Mathieu Boespflug <m@tweag.io>

RUN nix-env -f '<nixpkgs>' -iA python34Packages.ipython R zeromq pkgconfig zlib
RUN nix-env -f '<nixpkgs>' -iA ncurses python34Packages.jupyter_client python34Packages.notebook
RUN cp -R /root/.nix-profile /nix-profile
ENV \
    ENV=/etc/profile \
    PATH=/:/nix-profile/bin:/nix-profile/sbin:/bin:/sbin:/usr/bin:/usr/sbin \
    GIT_SSL_CAINFO=/nix-profile/etc/ssl/certs/ca-bundle.crt \
    SSL_CERT_FILE=/nix-profile/etc/ssl/certs/ca-bundle.crt \
    NIX_PATH=/nix/var/nix/profiles/per-user/root/channels/
RUN apk update && apk add ca-certificates xz make && chmod a+rwx /root
ADD stack /

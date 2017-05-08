# Build environment for HaskellR
FROM debian
# Modified Dockerfile from NixOS/nix: misc/docker/Dockerfile

RUN apt-get update && apt-get install -y wget bzip2

RUN wget -O- http://nixos.org/releases/nix/nix-1.11.2/nix-1.11.2-x86_64-linux.tar.bz2 | bzcat - | tar xf - \
    && echo "nixbld:x:30000:nixbld1,nixbld2,nixbld3,nixbld4,nixbld5,nixbld6,nixbld7,nixbld8,nixbld9,nixbld10,nixbld11,nixbld12,nixbld13,nixbld14,nixbld15,nixbld16,nixbld17,nixbld18,nixbld19,nixbld20,nixbld21,nixbld22,nixbld23,nixbld24,nixbld25,nixbld26,nixbld27,nixbld28,nixbld29,nixbld30" >> /etc/group \
    && for i in $(seq 1 30); do echo "nixbld$i:x:$((30000 + $i)):30000:::" >> /etc/passwd; done \
    && mkdir -m 0755 /nix && USER=root sh nix-*-x86_64-linux/install \
    && echo ". /root/.nix-profile/etc/profile.d/nix.sh" >> /etc/profile \
    && rm -r /nix-*-x86_64-linux

ONBUILD ENV \
    ENV=/etc/profile \
    PATH=/root/.nix-profile/bin:/root/.nix-profile/sbin:/bin:/sbin:/usr/bin:/usr/sbin \
    GIT_SSL_CAINFO=/root/.nix-profile/etc/ssl/certs/ca-bundle.crt \
    SSL_CERT_FILE=/root/.nix-profile/etc/ssl/certs/ca-bundle.crt

RUN cp -R /root/.nix-profile /nix-profile && chmod a+rwx /root
ENV \
    ENV=/etc/profile \
    PATH=/:/nix-profile/bin:/nix-profile/sbin:/bin:/sbin:/usr/bin:/usr/sbin \
    GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt \
    SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt \
    NIX_PATH=/nix/var/nix/profiles/per-user/root/channels/

RUN nix-channel --update && nix-env -f '<nixpkgs>' -iA \
python34Packages.ipython R zeromq pkgconfig zlib ncurses \
python34Packages.jupyter_client python34Packages.notebook \
haskell.compiler.ghc7103


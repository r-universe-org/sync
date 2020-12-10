FROM rhub/r-minimal:4.0.1

RUN installr -d -t "bash openssl-dev libgit2-dev" -a "openssl libgit2 git" gert gh remotes

COPY . /pkg
COPY entrypoint.sh /entrypoint.sh

RUN R -e 'remotes::install_local("/pkg")'

ENTRYPOINT ["sh","/entrypoint.sh"]

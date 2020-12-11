FROM rhub/r-minimal

RUN installr -d -t "gcc bash openssl-dev libgit2-dev" -a "openssl libgit2 git" gert gh remotes

COPY . /pkg
COPY entrypoint.sh /entrypoint.sh

RUN R -e 'remotes::install_local("/pkg")'

ENTRYPOINT ["sh","/entrypoint.sh"]

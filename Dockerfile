FROM rhub/r-minimal

COPY . /pkg
COPY entrypoint.sh /entrypoint.sh

RUN installr -d -t "openssl-dev libgit2-dev" -a "openssl libgit2 git" local::/pkg

ENTRYPOINT ["sh","/entrypoint.sh"]

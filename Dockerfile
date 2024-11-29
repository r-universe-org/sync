FROM rhub/r-minimal

COPY . /pkg
COPY entrypoint.sh /entrypoint.sh

RUN installr -d -t "openssl-dev libgit2-dev" -a "openssl libgit2 git" rlang local::/pkg

RUN echo 'rlang::global_entrace()' >> "$(R RHOME)/etc/Rprofile.site"

ENTRYPOINT ["sh","/entrypoint.sh"]

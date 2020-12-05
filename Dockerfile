FROM haskell

RUN curl -OL https://github.com/haskell/haskell-language-server/releases/download/0.6.0/haskell-language-server-Linux-8.10.2.gz

RUN gunzip haskell-language-server-Linux-8.10.2.gz

RUN mv haskell-language-server-Linux-8.10.2 /usr/local/bin/haskell-language-server
RUN chmod u+x /usr/local/bin/haskell-language-server

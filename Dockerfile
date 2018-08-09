FROM haskell:8

WORKDIR /usr/src/app

COPY . .

CMD ["ghci", "./LogAnalysis.hs"]

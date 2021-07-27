# plutus-fhw
Plutus Contract code to sell mint and sell NFT:s in a decentralized way

## Setup 

#### Step 1

[Clone the plutus repository in a convenient location](https://github.com/input-output-hk/plutus)

#### Step 2 

Checkout the correct plutus commit as specified in the cabal.project file. 
This repository uses nix so I recommend using nix to install plutus.

#### Step 3

In the top plutus directory use

```bash
nix-shell
```

Voila! Everything should work like magic.

## Commands

### Building

```bash
cd path/to/plutus
nix-shell
cd path/to/here
cabal build
```

### REPL

```bash
cd path/to/plutus
nix-shell
cd path/to/here
cabal repl
```

### Plutus Docs

#### Enter the shell

```bash
cd path/to/plutus
nix-shell
```

#### Build the docs if necessary

```bash
nix-build -A plutus-playground.haddock
```

#### Run the webserver

You want to serve the files in 'result/share/doc' there are many ways of doing it
but python is one simple option

```bash
cd result/share/doc
python3 -m http.server 8081
```

## Editor

If you are using VSCode you want to start it from the command line and make sure that you 
are inside of nix-shell.

## Useful Links

* [Community docs](https://docs.plutus-community.com/)
# HypERLedger
A protoyupe blockchain and client application written in Erlang.

**Discalimer:** This has nothing to do with [this Hyperledger](https://www.hyperledger.org/) or [this one](https://www.ibm.com/blockchain/hyperledger) project. It's just a blockchain written in erlang and hyperledger is a blockchain-related-word containing the letters ERL which are short for Erlang. #PunIntended #ProbablyLessFunnyThanWeThought 

# Usage
Using the application locally is very easy. First you need to clone the repo and cd into the newly created directory.
```bash
$ git clone https://github.com/Pewepow/HypERLedger.git
$ cd HypERLedger
```
Typing the following commands will then compile everything that needs to be compiled (all the files that have not been compiled yet or are not since the last modification), enter the erlang shell and run the test program. This will start the Central Authority and open a client application. From there on, the program should be fairly self-explanatory.

```bash
$ erl -make
$ erl
> main:init().
> client:init().
```

Inside the client application, you can then choose between registering as a new client, logging in or visualizing the hypERLedger blockchain. You can use the numbers 1-3 followed by a period to choose between these options. 

!()[images/choose.png]


# Developers
@pewepow and @sirfrankiecrisp

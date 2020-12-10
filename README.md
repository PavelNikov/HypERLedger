# HypERLedger
A protoyupe blockchain and client application written in Erlang.

**Discalimer:** This has nothing to do with [this Hyperledger](https://www.hyperledger.org/) or [this one](https://www.ibm.com/blockchain/hyperledger) project. It's just a blockchain written in erlang and hyperledger is a blockchain-related-word containing the letters ERL which are short for Erlang. #PunIntended #ProbablyLessFunnyThanWeThought 

# Usage
Using the application locally is very easy. First you need to clone the repo and cd into the newly created directory.
```bash
$ git clone https://github.com/Pewepow/HypERLedger.git
$ cd HypERLedger
```
Typing the following commands will then compile everything that needs to be compiled (all the files that have not been compiled yet or are not since the last modification) and start an Erlang node with the short name "main" and the secretcookie "topsecret". You can change these parameters as you wish, but make sure to then also change them in the upcoming steps. `main:init()` will then initialize the main function on the main node, which will spawn the Central Authority and the blockchain miners/nodes on the same Erlang node (in our program they are called nodes... Erlang nodes are not the same as our miner nodes).

```bash
$ erl -make
$ erl -sname main -secretcookie topsecret
(main@host)1> main:init().
```

In a second terminal window or split run the commands listed below. These will start a new Erlang node using the same secretcookie and initialize the client application. This procedure also works on different machines inside the same network. Here we want to give the client the name of the Erlang node where the Central Authority is running. You can either input it directly as a paramenter inside the brackets like this: `client:init('main@host')` or otherwise do it in two different steps and provide the Central Authoritie's host node name when prompted (as shown below). Adapt the node's name to what is shown in the prompt of the other terminal window where you started main. Now your client should start up and you should be ready to create a new account, login, check out the other transactions on the blockchain and send/receive Hypercoins!

```bash
$ erl -sname client -secretcookie topsecret
(client@host)1> client:init().

```

Inside the client application, you can then choose between registering as a new client, logging in or visualizing the hypERLedger blockchain. You can use the numbers 1-3 followed by a period to choose between these options. 

!()[images/choose.png]


# Developers
@pewepow and @sirfrankiecrisp

# Catalyst Fund5 
## The metadata challenge

In this folder you will find our POC code for the multiparty en-/decryption process. Which is build similar to an broadcast encryption.

## Encryptionapi

Has the API service which delivers endpoint for the en-/decryption.

## Encryptionkey Decryptor Console

Is a simple console application to check if en-/decryption works.

## Metatdata Plublicapi
Contains to code for adding metadat to the Cardano Blockchain. This part of the code was decoupled form our core application and contains only the part regarding fund5 metadata challenge.

As part of the metadata challenge Profila is running a Cardano Wallet API at http://dcardano.profila.com:8090/v2/

Currently the Wallet Api is open - we might consider to secure the API in a laterphase.
## General idea

General idea is that we encypt the metadata with synchronous encryption, which will be added to the blockchain.

The encryptionkey we used for synchronous encryption will be then send to each involved party in asychronous encrypted way. So each an every party needs a private/public keypair.

## Demonstration 

https://www.youtube.com/watch?v=D7IKZm_R9Y4

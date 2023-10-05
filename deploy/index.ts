import { InMemorySigner } from "@taquito/signer";
import { MichelsonMap, TezosToolkit } from "@taquito/taquito";
import { char2Bytes } from "@taquito/utils";

import nftContract from "../compiled/fa2/nft/nft.impl.mligo.json";

const RPC_ENDPOINT = "https://ghostnet.tezos.marigold.dev";

async function main() {
  const Tezos = new TezosToolkit(RPC_ENDPOINT);

  //set alice key
  Tezos.setProvider({
    signer: await InMemorySigner.fromSecretKey(
      "edskRpm2mUhvoUjHjXgMoDRxMKhtKfww1ixmWiHCWhHuMEEbGzdnz8Ks4vgarKDtxok7HmrEo1JzkXkdkvyw7Rtw6BNtSd7MJ7"
    ),
  });

  const ledger = new MichelsonMap();
  ledger.set(0, "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb");

  const token_metadata = new MichelsonMap();
  const token_info = new MichelsonMap();
  token_info.set("name", char2Bytes("My super token"));
  token_info.set("description", char2Bytes("Lorem ipsum ..."));
  token_info.set("symbol", char2Bytes("XXX"));
  token_info.set("decimals", char2Bytes("0"));

  token_metadata.set(0, { token_id: 0, token_info });

  const metadata = new MichelsonMap();
  metadata.set("", char2Bytes("tezos-storage:data"));
  metadata.set(
    "data",
    char2Bytes(`{
    "name":"FA2",
    "description":"Example FA2 implementation",
    "version":"0.1.0",
    "license":{"name":"MIT"},
    "authors":["Benjamin Fuentes<benjamin.fuentes@marigold.dev>"],
    "homepage":"",
    "source":{"tools":["Ligo"], "location":"https://github.com/ligolang/contract-catalogue/tree/main/lib/fa2"},
    "interfaces":["TZIP-012"],
    "errors":[],
    "views":[]
  
  }`)
  );

  const operators = new MichelsonMap();

  const initialStorage = {
    ledger,
    metadata,
    token_metadata,
    operators,
  };

  try {
    const originated = await Tezos.contract.originate({
      code: nftContract,
      storage: initialStorage,
    });
    console.log(
      `Waiting for nftContract ${originated.contractAddress} to be confirmed...`
    );
    await originated.confirmation(2);
    console.log("confirmed contract: ", originated.contractAddress);
  } catch (error: any) {
    console.log(error);
  }
}

main();

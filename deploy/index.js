"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const signer_1 = require("@taquito/signer");
const taquito_1 = require("@taquito/taquito");
const utils_1 = require("@taquito/utils");
const nft_impl_mligo_json_1 = __importDefault(require("../compiled/fa2/nft/nft.impl.mligo.json"));
const RPC_ENDPOINT = "https://ghostnet.tezos.marigold.dev";
function main() {
    return __awaiter(this, void 0, void 0, function* () {
        const Tezos = new taquito_1.TezosToolkit(RPC_ENDPOINT);
        //set alice key
        Tezos.setProvider({
            signer: yield signer_1.InMemorySigner.fromSecretKey("edskRpm2mUhvoUjHjXgMoDRxMKhtKfww1ixmWiHCWhHuMEEbGzdnz8Ks4vgarKDtxok7HmrEo1JzkXkdkvyw7Rtw6BNtSd7MJ7"),
        });
        const ledger = new taquito_1.MichelsonMap();
        ledger.set(0, "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb");
        const token_metadata = new taquito_1.MichelsonMap();
        const token_info = new taquito_1.MichelsonMap();
        token_info.set("name", (0, utils_1.char2Bytes)("My super token"));
        token_info.set("description", (0, utils_1.char2Bytes)("Lorem ipsum ..."));
        token_info.set("symbol", (0, utils_1.char2Bytes)("XXX"));
        token_info.set("decimals", (0, utils_1.char2Bytes)("0"));
        token_metadata.set(0, { token_id: 0, token_info });
        const metadata = new taquito_1.MichelsonMap();
        metadata.set("", (0, utils_1.char2Bytes)("tezos-storage:data"));
        metadata.set("data", (0, utils_1.char2Bytes)(`{
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
  
  }`));
        const operators = new taquito_1.MichelsonMap();
        const initialStorage = {
            ledger,
            metadata,
            token_metadata,
            operators,
        };
        try {
            const originated = yield Tezos.contract.originate({
                code: nft_impl_mligo_json_1.default,
                storage: initialStorage,
            });
            console.log(`Waiting for nftContract ${originated.contractAddress} to be confirmed...`);
            yield originated.confirmation(2);
            console.log("confirmed contract: ", originated.contractAddress);
        }
        catch (error) {
            console.log(error);
        }
    });
}
main();

import * as dotenv from 'dotenv'

import { importKey } from '@taquito/signer'
import { TezosToolkit, MichelsonMap } from '@taquito/taquito'

import contract from "../../../compiled/fa2/nft/NFT_mligo.json"

dotenv.config(({ path: __dirname + '/.env' }))

const RPC_ENDPOINT = process.env.RPC_ENDPOINT || "https://hangzhounet.api.tez.ie"

const ADMIN_PKH = process.env.FAUCET_PKH
const ADMIN_MNEMONIC = (process.env.FAUCET_MNEMONIC || "").split(",").join(" ")
const ADMIN_EMAIL = process.env.FAUCET_EMAIL
const ADMIN_PASSWORD = process.env.FAUCET_PASSWORD
const ADMIN_ACTIVATION_CODE = process.env.FAUCET_ACTIVATION_CODE

const NFT_TOKEN_IDS = (process.env.NFT_TOKEN_IDS || "").split(",").map((id) => parseInt(id))

async function main() {
    const Tezos = new TezosToolkit(RPC_ENDPOINT)
    await importKey(Tezos, ADMIN_EMAIL, ADMIN_PASSWORD, ADMIN_MNEMONIC, ADMIN_ACTIVATION_CODE)

    const ledger = NFT_TOKEN_IDS.reduce((ledger, token_id) => {
        let _ = ledger.set(token_id, ADMIN_PKH)
        return ledger
    }, new MichelsonMap())

    const token_metadata = NFT_TOKEN_IDS.reduce((token_metadata, token_id) => {
        let _ = token_metadata.set(token_id, { token_id, token_info: new MichelsonMap() })
        return token_metadata
    }, new MichelsonMap())

    const operators = new MichelsonMap()
    operators.set([ADMIN_PKH, ADMIN_PKH], NFT_TOKEN_IDS)

    const initialStorage = {
        ledger,
        token_metadata,
        operators,
        token_ids: NFT_TOKEN_IDS,
    }

    try {
        const originated = await Tezos.contract.originate({
            code: contract,
            storage: initialStorage
        })
        console.log(`Waiting for contract ${originated.contractAddress} to be confirmed...`)
        await originated.confirmation(2)
        console.log('confirmed contract: ', originated.contractAddress)
    } catch (error: any) {
        console.log(error)
    }

}

main()


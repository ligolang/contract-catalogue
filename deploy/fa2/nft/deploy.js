"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function() { return m[k]; } });
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (_) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
exports.__esModule = true;
var dotenv = __importStar(require("dotenv"));
var signer_1 = require("@taquito/signer");
var taquito_1 = require("@taquito/taquito");
var NFT_mligo_json_1 = __importDefault(require("../../../compiled/fa2/nft/NFT_mligo.json"));
dotenv.config(({ path: __dirname + '/.env' }));
var RPC_ENDPOINT = process.env.RPC_ENDPOINT || "https://hangzhounet.api.tez.ie";
var ADMIN_PKH = process.env.FAUCET_PKH;
var ADMIN_MNEMONIC = (process.env.FAUCET_MNEMONIC || "").split(",").join(" ");
var ADMIN_EMAIL = process.env.FAUCET_EMAIL;
var ADMIN_PASSWORD = process.env.FAUCET_PASSWORD;
var ADMIN_ACTIVATION_CODE = process.env.FAUCET_ACTIVATION_CODE;
var NFT_TOKEN_IDS = (process.env.NFT_TOKEN_IDS || "").split(",").map(function (id) { return parseInt(id); });
function main() {
    return __awaiter(this, void 0, void 0, function () {
        var Tezos, ledger, token_metadata, operators, initialStorage, originated, error_1;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    Tezos = new taquito_1.TezosToolkit(RPC_ENDPOINT);
                    return [4 /*yield*/, (0, signer_1.importKey)(Tezos, ADMIN_EMAIL, ADMIN_PASSWORD, ADMIN_MNEMONIC, ADMIN_ACTIVATION_CODE)];
                case 1:
                    _a.sent();
                    ledger = NFT_TOKEN_IDS.reduce(function (ledger, token_id) {
                        var _ = ledger.set(token_id, ADMIN_PKH);
                        return ledger;
                    }, new taquito_1.MichelsonMap());
                    token_metadata = NFT_TOKEN_IDS.reduce(function (token_metadata, token_id) {
                        var _ = token_metadata.set(token_id, { token_id: token_id, token_info: new taquito_1.MichelsonMap() });
                        return token_metadata;
                    }, new taquito_1.MichelsonMap());
                    operators = new taquito_1.MichelsonMap();
                    operators.set([ADMIN_PKH, ADMIN_PKH], NFT_TOKEN_IDS);
                    initialStorage = {
                        ledger: ledger,
                        token_metadata: token_metadata,
                        operators: operators,
                        token_ids: NFT_TOKEN_IDS
                    };
                    _a.label = 2;
                case 2:
                    _a.trys.push([2, 5, , 6]);
                    return [4 /*yield*/, Tezos.contract.originate({
                            code: NFT_mligo_json_1["default"],
                            storage: initialStorage
                        })];
                case 3:
                    originated = _a.sent();
                    console.log("Waiting for nft ".concat(originated.contractAddress, " to be confirmed..."));
                    return [4 /*yield*/, originated.confirmation(2)];
                case 4:
                    _a.sent();
                    console.log('confirmed nft: ', originated.contractAddress);
                    return [3 /*break*/, 6];
                case 5:
                    error_1 = _a.sent();
                    console.log(error_1);
                    return [3 /*break*/, 6];
                case 6: return [2 /*return*/];
            }
        });
    });
}
main();

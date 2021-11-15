using CardanoSharp.Wallet;
using CardanoSharp.Wallet.Models.Keys;
using Microsoft.Extensions.Configuration;
using MongoDB.Bson.IO;
using MongoDB.Driver;
using RightAssertion.API.Common;
using RightAssertion.API.Data.DataModels;
using RightAssertion.API.Models;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;
using System.Net.Http;
using System.Text.Json;
using System.Net.Http.Json;
using BarGameApi.Common;

namespace RightAsserion.Cardano.Service
{
    public class CardanoServices : ICardanoServices
    {
        IMongoCollection<PWallet> _wallets;
        IMongoCollection<TAddress> _Addresses;
        IMongoCollection<User> _users;
        IMongoCollection<Right> _rights;
        KeyService keyService;
        AddressService addressService;

        public CardanoServices(IConfiguration config)
        {
            var client = new MongoClient(config.GetConnectionString("DatabaseUrl"));
            var database = client.GetDatabase(config.GetConnectionString("Database"));
            _wallets = database.GetCollection<PWallet>("Wallets");
            _Addresses = database.GetCollection<TAddress>("Addresses");
            _users = database.GetCollection<User>("Users");
            _rights = database.GetCollection<Right>("Rights");
            keyService = new KeyService();
            addressService = new AddressService();
        }

        public PWallet ChkExistingWallet(string userId)
        {
            var wallet = _wallets.Find(x => x.UserId == userId && x.WalletType == WalletTypes.CardanoWallet).FirstOrDefault();
            return wallet;
        }

        public Right CreateTransaction(CreateTransactionModel model, string walletBlockChainId, CreateTransactionRequestModel createModel)
        {
            string json =Newtonsoft.Json.JsonConvert.SerializeObject(model);
            json = json.Replace("_1337", "1337");
            StringContent content = new StringContent(json, Encoding.UTF8, "application/json");
            HttpClient client = new HttpClient();
            var uri = new Uri(CardanoConstants.ApiBase + $"wallets/{walletBlockChainId}/transactions");
            var response = client.PostAsync(uri, content).Result;
            if (response.IsSuccessStatusCode)
            {
                json = response.Content.ReadAsStringAsync().Result;
                TransactionResultModel tresult =Newtonsoft.Json.JsonConvert.DeserializeObject<TransactionResultModel>(json);
                Right right = new Right();
                right.AssertedBy = createModel.AssertedBy;
                right.AssertedTo = createModel.AssertedTo;
                right.BrandEmail = createModel.BrandEmail;
                right.ClientEmail = createModel.ClientEmail;
                right.CreatedOn = DateTime.Now;
                right.TransactionId = tresult.id;
                right.UpdatedOn = DateTime.Now;
                right.AssertedRightAddress = model.payments.FirstOrDefault().address;
                _rights.InsertOne(right);
                return right;
            }
            return null;
        }

        public async Task<PWallet> CreateWalletThroughApi(User user)
        {
            Mnemonic mnc = keyService.Generate(15, CardanoSharp.Wallet.Enums.WordLists.English);
            var words = mnc.Words.ToString();
            var requestObject = new RequestObject();
            requestObject.name = "MyAPIWallet";
            requestObject.mnemonic_sentence = words.Split(' ').ToList();
            requestObject.passphrase = user.Password;
            string json =Newtonsoft.Json.JsonConvert.SerializeObject(requestObject);

            StringContent content = new StringContent(json, Encoding.UTF8, "application/json");
            var uri = new Uri(CardanoConstants.ApiBase + "wallets");
            HttpClient client = new HttpClient();
            var response = await client.PostAsync(uri, content);
            if (response.IsSuccessStatusCode)
            {
                var result = await response.Content.ReadAsStringAsync();
                Root walletResponse =Newtonsoft.Json.JsonConvert.DeserializeObject<Root>(result);
                PWallet w = new PWallet();
                w.CreatedOn = DateTime.Now;
                w.UpdatedOn = DateTime.Now;
                w.WalletBlockChainId = walletResponse.id;
                w.UserId = user.Id;
                w.WalletType = WalletTypes.CardanoWallet;
                w.Words = Psecurity.Encrypy(words);
                _wallets.InsertOne(w);
                return w;

            }
            return null;
        }

        public async Task<TAddress> GenerateAddress(PWallet wallet)
        {
            var user = _users.Find(x => x.Id == wallet.UserId).FirstOrDefault();

            // ADDRESS USING CARDANO WALLET API


            HttpClient client = new HttpClient() { BaseAddress = new Uri(CardanoConstants.ApiBase) };
            var addresses = await client.GetFromJsonAsync<JsonElement>($"wallets/{wallet.WalletBlockChainId}/addresses?status=unused");
            var address = addresses[0].GetProperty("id");

            TAddress tAddress = new TAddress();
            tAddress.Address = address.ToString();
            tAddress.Balance = 0;
            tAddress.CreatedOn = DateTime.Now;
            tAddress.UpdatedOn = DateTime.Now;
            tAddress.WalletId = wallet.Id;
            tAddress.WalletType = WalletTypes.CardanoWallet;
            _Addresses.InsertOne(tAddress);
            return tAddress;
        }

        public List<TAddress> GetAddressesByWalletId(string walletId)
        {
            return _Addresses.Find(x => x.WalletId == walletId).ToList();
        }

        public TAddress GetDetailsByAdress(string address)
        {
            throw new NotImplementedException();
        }

        public PWallet GetWalletByUserId(string UserId)
        {
            throw new NotImplementedException();
        }

        public List<PWallet> GetWalletsByUserId(string UserId)
        {
            return _wallets.Find(x => x.UserId == UserId && x.WalletType == WalletTypes.CardanoWallet).ToList()
               .OrderByDescending(x => x.CreatedOn).ToList();
        }

        public Right GetRightByTransactionId(string trans)
        {
            return _rights.Find(x => x.TransactionId == trans).FirstOrDefault();
        }

        public string GetWalletIdByAddress(string address)
        {
            var add = _Addresses.Find(x => x.Address == address).FirstOrDefault();
            if (add != null)
                return add.WalletId;
            return null;
        }

        public PWallet GetWalletById(string walletId)
        {
            return _wallets.Find(x => x.Id == walletId).FirstOrDefault();
        }

        public TransactionResultModel GetTransactionDetails(string trans, string walletBlockChainId)
        {
            HttpClient client = new HttpClient() { BaseAddress = new Uri(CardanoConstants.ApiBase) };
            var response = client.GetAsync($"wallets/{walletBlockChainId}/transactions/{trans}").Result;
            if (response.IsSuccessStatusCode)
            {
                var json = response.Content.ReadAsStringAsync().Result;
                var transResult =Newtonsoft.Json.JsonConvert.DeserializeObject<TransactionResultModel>(json);
                return transResult;
            }
            return null;
        }
    }
}

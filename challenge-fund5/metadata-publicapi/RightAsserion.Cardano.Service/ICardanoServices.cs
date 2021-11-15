using RightAssertion.API.Data.DataModels;
using RightAssertion.API.Models;
using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace RightAsserion.Cardano.Service
{
    public interface ICardanoServices
    {
        Task<PWallet> CreateWalletThroughApi(User user);

        Task<TAddress> GenerateAddress(PWallet wallet);
        PWallet GetWalletByUserId(string UserId);

        List<PWallet> GetWalletsByUserId(string UserId);

        Right CreateTransaction(CreateTransactionModel model, string walletBlockChainId,
            CreateTransactionRequestModel createModel);

        TransactionResultModel GetTransactionDetails(string trans, string walletBlockChainId);

        Right GetRightByTransactionId(string trans);

        string GetWalletIdByAddress(string address);
        PWallet GetWalletById(string walletId);
    }
}

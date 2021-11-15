using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using RightAsserion.Cardano.Service;
using RightAssertion.API.Data.DataModels;
using RightAssertion.API.Models;
using RightAssertion.API.Services.Abstract;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace RightAssertion.API.Api.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class CardanoController : ControllerBase
    {
        ICardanoServices _cardanoService;
        IUserService _userService;

        public CardanoController(ICardanoServices cardanoServices, IUserService userService)
        {
            _cardanoService = cardanoServices;
            _userService = userService;
        }

     


        [Authorize]
        [HttpPost("GenerateAddress")]
        public IActionResult GenerateAddress(string userId)
        {
            var currentUserId = User.Identity.Name;
            if (currentUserId == userId)
            {
                    var wallet = _cardanoService.CreateWalletThroughApi(_userService.Get(userId)).Result;
                    TAddress address = _cardanoService.GenerateAddress(wallet).Result;
                    return Ok(new { address = address });
            }
            return BadRequest(new { Message = "Provided User Id Dsnt Match with logged In User" });
        }

        [Authorize]
        [HttpPost("AssertRight")]
        public IActionResult CreateTransaction(CreateTransactionRequestModel model)
        {
            try
            {
                var currentUserId = User.Identity.Name;
                var currentUser = _userService.Get(currentUserId);
                var walletBlockChainId = GetWalletBlockChainId(model.payments.Address);
                CreateTransactionModel transModel = new CreateTransactionModel();
                transModel.passphrase = currentUser.Password;
                Payment payment = new Payment();
                payment.address = model.payments.Address;
                payment.amount.quantity = model.payments.Amount;
                payment.amount.unit = "lovelace";
                transModel.payments.Add(payment);
                Map map = new Map();
                map.k.@string = model.rights.BrandName;
                map.v.@string = model.rights.RightMessage;
                transModel.metadata._1337.map.Add(map);
                var right = _cardanoService.CreateTransaction(transModel, walletBlockChainId, model);
                if (right != null)
                {
                    return Ok(right);
                }
                return Ok(new { Message = "Process was not successfull because of some unknown Error Please try Again Later" });

            }
            catch (Exception ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        private string GetWalletBlockChainId(string address)
        {
            var walletId = _cardanoService.GetWalletIdByAddress(address);
            var wallet = _cardanoService.GetWalletById(walletId);
            return wallet.WalletBlockChainId;
        }

        [HttpGet("GetTransactionDetails")]
        public IActionResult GetTransaction(string transactionId)
        {
            try
            {
                var right = _cardanoService.GetRightByTransactionId(transactionId);
                if (right != null)
                {
                    var blockChainId = GetWalletBlockChainId(right.AssertedRightAddress);
                    var tResult = _cardanoService.GetTransactionDetails(transactionId, blockChainId);
                    if (tResult != null)
                    {
                        return Ok(tResult);
                    }
                    return StatusCode(500, new { Message = "Internal Error. Please try Again..." });
                }

                return StatusCode(204, new { Message = "No Transaction Found with provided Id" });
            }
            catch (Exception ex)
            {
                return StatusCode(500, ex.Message);
            }
        }
    }
}

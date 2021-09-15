using BarGameApi.Common;
using EncryptionAPI.Data.DataModels;
using EncryptionAPI.Models;
using EncryptionAPI.Services.Abstract;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace EncryptionAPI.Api.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class RightsController : ControllerBase
    {
        private readonly IRightAssertionService _rightAssertion;
        private readonly IUserService _userService;

        public RightsController(IRightAssertionService rightAssertion,IUserService userService)
        {
            _rightAssertion = rightAssertion;
            _userService = userService;
        }

        [Authorize]
        [HttpPost("AssertRight")]
        public IActionResult Assertright([FromBody] RightAssertionModel model)
        {
            var currentuser = User.Identity.Name;
            if (model.RightAssertedBy == currentuser)
            {
                Right right = new Right();
                right.CreatedOn = DateTime.Now;
                right.UpdatedOn = DateTime.Now;
                right.Message = model.Message;
                right.RightAssertedBy = model.RightAssertedBy;
                right.RightAssertedTo = model.RightAssertedTo;
                _rightAssertion.AssertRight(right);
                return Ok(new { Message = "Right Asserted Successfully" });
            }
            return Ok(new { Message = "Invalid Request. Current User Id doesnot match with RightAssetedBy property" });
        }

        [Authorize]
        [HttpGet("GetAssertedRightsByUser")]
        public IActionResult GetRightsByUser(string userId)
        {
            var rights = _rightAssertion.GetRightAssertedBy(userId);
            var user = _userService.Get(User.Identity.Name);
            rights.ForEach(x =>
            {
                x.EncryptionKey = Psecurity.AsymetricKeyEncryption(x.EncryptionKey, user.PublicKey);
            });
            return Ok(rights);
        }
        [Authorize]
        [HttpGet("GetByrightId")]
        public IActionResult GetById(string rightId)
        {
            var currentuserId = User.Identity.Name;
            var user = _userService.Get(currentuserId);
            var right = _rightAssertion.GetRight(rightId);
            if (right != null)
            {
                right.EncryptionKey = Psecurity.AsymetricKeyEncryption(right.EncryptionKey, user.PublicKey);
                return Ok(right);
            }
            return Ok(new { Message = "Invalid Right Id" });
        }

        [HttpGet("DecryptMessage")]
        public IActionResult DecryptMessage(string decryptedKey, string encryptedMessage)
        {
            var msg = Psecurity.DecryptMessage(encryptedMessage, decryptedKey);
            return Ok(msg);
        }
        
    }
}

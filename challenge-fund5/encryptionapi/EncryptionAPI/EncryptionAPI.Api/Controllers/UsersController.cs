using EncryptionAPI.Data.DataModels;
using EncryptionAPI.Models;
using EncryptionAPI.Services.Abstract;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Configuration;
using Microsoft.IdentityModel.Tokens;
using System;
using System.Collections.Generic;
using System.IdentityModel.Tokens.Jwt;
using System.IO;
using System.Linq;
using System.Security.Claims;
using System.Security.Cryptography;
using System.Text;
using System.Threading.Tasks;
using System.Xml.Serialization;

namespace EncryptionAPI.Api.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class UsersController : ControllerBase
    {
        RSACryptoServiceProvider csp = new RSACryptoServiceProvider(2048);
        IUserService _userService;
        IConfiguration _config;
        public UsersController(IUserService userService, IConfiguration config)
        {
            _userService = userService;
            _config = config;
        }

        [AllowAnonymous]
        [HttpPost("Login")]
        public IActionResult Login([FromBody] LoginModel loginModel)
        {
            IActionResult response = NotFound(new { Message = "User with this username and password not found" });
            var user = _userService.AuthenticateUser(loginModel);
            if (user != null)
            {
                var tokenstring = GenerateJWTToken(user);
                response = Ok(new { token = tokenstring });
            }
            return response;
        }
        private string GenerateJWTToken(User user)
        {
            var securityKey = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(_config["JWT:Key"]));
            var cardinals = new SigningCredentials(securityKey, SecurityAlgorithms.HmacSha256);
            var claims = new[]
            {
                new Claim(ClaimTypes.Name,user.Id),
                new Claim(ClaimTypes.Email,user.Email)
            };
            var token = new JwtSecurityToken(_config["JWT:Issuer"], _config["JWT:Issuer"], claims, expires: DateTime.Now.AddMinutes(120),
                signingCredentials: cardinals);
            return new JwtSecurityTokenHandler().WriteToken(token);

        }

        [AllowAnonymous]
        [HttpPost("Craete")]
        public IActionResult Create([FromBody] UserModel user)
        {
            
            var _publicKey = csp.ExportParameters(false);
            var _privateKey = csp.ExportParameters(true);
            var publicKey = convertKeyToString(false);
            var privateKey = convertKeyToString(true);
            var cuser = _userService.Create(user,publicKey);
            if (cuser != null)
            {
                CreateResponseModel responseModel = new CreateResponseModel();
                responseModel.Address = cuser.Address;
                responseModel.Contacts = cuser.Contacts;
                responseModel.Email = cuser.Email;
                responseModel.Id = cuser.Id;
                responseModel.Name = cuser.Name;
                responseModel.Password = cuser.Password;
                responseModel.PrivateKey = privateKey;
                return Ok(responseModel);
            }
            return StatusCode(StatusCodes.Status500InternalServerError, new { Message = "USer with this Email address already exists" });
        }

        private string convertKeyToString(bool key)
        {
            return csp.ToXmlString(key);
            /*
            var sw = new StringWriter();
            var xs = new XmlSerializer(typeof(RSAParameters));
            xs.Serialize(sw, Key);
            return sw.ToString();*/
        }

        [Authorize]
        [HttpGet("GetById")]
        public ActionResult<User> GetByid(string id)
        {
            var currentUserId = User.Identity.Name;
            if (currentUserId != null)
            {
                if (id == currentUserId)
                {
                    var user = _userService.Get(id);
                    if (user != null)
                    {
                        return Ok(user);
                    }
                    return NotFound();
                }
                return Forbid();
            }
            return BadRequest();
        }

        [Authorize]
        [HttpPost("UpdateUser")]

        public IActionResult Update([FromBody] UserModel model)
        {
            var currentid = User.Identity.Name;
            if (currentid != null && currentid == model.Id)
            {
                var user = _userService.Update(model);
                if (user == null)
                {
                    return Ok(new { Message = "No User Found with this ID" });
                }
                else
                {
                    return Ok(user);
                }
            }
            return Ok(new { Message = "Either youa re not logged in or You are logged in with different ID" });
        }
    }
}

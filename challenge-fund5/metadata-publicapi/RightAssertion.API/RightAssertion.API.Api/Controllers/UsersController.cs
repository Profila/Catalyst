using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Configuration;
using Microsoft.IdentityModel.Tokens;
using RightAssertion.API.Data.DataModels;
using RightAssertion.API.Models;
using RightAssertion.API.Services.Abstract;
using System;
using System.Collections.Generic;
using System.IdentityModel.Tokens.Jwt;
using System.Linq;
using System.Security.Claims;
using System.Text;
using System.Threading.Tasks;

namespace RightAssertion.API.Api.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class UsersController : ControllerBase
    {
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
            var cuser = _userService.Create(user);
            if (cuser != null)
            {
                return Ok(cuser);
            }
            return StatusCode(StatusCodes.Status500InternalServerError, new { Message = "USer with this Email address already exists" });
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

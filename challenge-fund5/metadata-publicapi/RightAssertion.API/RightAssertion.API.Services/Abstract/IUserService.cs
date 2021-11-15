using RightAssertion.API.Data.DataModels;
using RightAssertion.API.Models;
using System;
using System.Collections.Generic;
using System.Text;

namespace RightAssertion.API.Services.Abstract
{
    public interface IUserService
    {
        User AuthenticateUser(LoginModel model);
        User Create(UserModel model);
        List<User> Get();
        User Get(string Id);
        User Update(UserModel model);
    }
}

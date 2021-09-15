using EncryptionAPI.Data.DataModels;
using EncryptionAPI.Models;
using System;
using System.Collections.Generic;
using System.Text;

namespace EncryptionAPI.Services.Abstract
{
    public interface IUserService
    {
        User AuthenticateUser(LoginModel model);
        User Create(UserModel model, string publicKey);
        List<User> Get();
        User Get(string Id);
        User Update(UserModel model);
    }
}

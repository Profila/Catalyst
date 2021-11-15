using BarGameApi.Common;
using Microsoft.Extensions.Configuration;
using MongoDB.Driver;
using RightAssertion.API.Data.DataModels;
using RightAssertion.API.Models;
using RightAssertion.API.Services.Abstract;
using System;
using System.Collections.Generic;
using System.Text;

namespace RightAssertion.API.Services.Implementation
{
    public class UserService : IUserService
    {
        IMongoCollection<User> _user;

        public UserService(IConfiguration config)
        {
            var client = new MongoClient(config.GetConnectionString("DatabaseUrl"));
            var database = client.GetDatabase(config.GetConnectionString("Database"));
            _user = database.GetCollection<User>("Users");
        }

        public User AuthenticateUser(LoginModel model)
        {
            var user = _user.Find(x => x.Email == model.email).FirstOrDefault();
            if (user != null && Psecurity.Decrypt(user.Password) == model.passWord)
            {
                return user;
            }
            return user;
        }

        public User Create(UserModel model)
        {
            var u = _user.Find(x => x.Email.ToLower() == model.Email.ToLower()).FirstOrDefault();
            if (u == null)
            {
                User user = new User();
                user.Address = model.Address;
                user.CreatedOn = DateTime.Now.Date;
                user.UpdatedOn = DateTime.Now.Date;
                user.Email = model.Email;
                user.Name = model.Name;
                user.Password = Psecurity.Encrypy(model.Password);
                _user.InsertOne(user);
                return user;
            }
            return null;
        }

        public List<User> Get()
        {
            return _user.Find(x => true).ToList();
        }

        public User Get(string Id)
        {
            return _user.Find(x => x.Id == Id).FirstOrDefault();
        }

        public User Update(UserModel model)
        {
            var user = _user.Find(x => x.Id == model.Id).FirstOrDefault();
            if (user != null)
            {
                user.Address = model.Address;
                user.UpdatedOn = DateTime.Now.Date;
                user.Email = model.Email;
                user.Name = model.Name;
                user.Password = Psecurity.Encrypy(model.Password);
                var filter = Builders<User>.Filter.Eq(x => x.Id, model.Id);
                var result = _user.ReplaceOne(filter, user);

            }
            return user;
        }
    }
}

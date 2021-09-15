using BarGameApi.Common;
using EncryptionAPI.Data.DataModels;
using EncryptionAPI.Services.Abstract;
using Microsoft.Extensions.Configuration;
using MongoDB.Driver;
using System;
using System.Collections.Generic;
using System.IO;
using System.Security.Cryptography;
using System.Text;

namespace EncryptionAPI.Services.Implementation
{
    public class RightAssertionService : IRightAssertionService
    {
        IMongoCollection<Right> _asserRight;

        public RightAssertionService(IConfiguration config)
        {
            var client = new MongoClient(config.GetConnectionString("DatabaseUrl"));
            var database = client.GetDatabase(config.GetConnectionString("Database"));
            _asserRight = database.GetCollection<Right>("Rights");
        }

        public void AssertRight(Right model)
        {
            var key = GenerateRandomString();
            model.Message = Psecurity.EncryptMessage(model.Message, key);
            model.EncryptionKey = key;
            _asserRight.InsertOne(model);
        }

        public Right GetRight(string rightId)
        {
            return _asserRight.Find(x => x.Id == rightId).FirstOrDefault();
        }

        public List<Right> GetRightAssertedBy(string rightsAssertedBy)
        {
            return _asserRight.Find(x => x.RightAssertedBy == rightsAssertedBy).ToList();
        }

        public List<Right> GetRightAssertedTo(string rightAssertedTo)
        {
            throw new NotImplementedException();
        }

        private string GenerateRandomString()
        {
            const string src = "abcdefghijklmnopqrstuvwxyz0123456789@!#$%^&*";
            int length = 16;
            var sb = new StringBuilder();
            Random RNG = new Random();
            for (var i = 0; i < length; i++)
            {
                var c = src[RNG.Next(0, src.Length)];
                sb.Append(c);
            }
            return sb.ToString();
        }

       
    }
}

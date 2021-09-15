using System;
using System.Collections.Generic;
using System.Text;

namespace EncryptionAPI.Models
{
    public class CreateResponseModel
    {
        public string Id { get; set; }
        public string Name { get; set; }
        public string Email { get; set; }
        public string Contacts { get; set; }
        public string Address { get; set; }
        public string Password { get; set; }
        
        public string PrivateKey { get; set; }
    }
}

using System;
using System.Collections.Generic;
using System.Text;

namespace EncryptionAPI.Data.DataModels
{
    public class User : BaseEntity
    {
        public string Name { get; set; }
        public string Email { get; set; }
        public string Contacts { get; set; }
        public string Address { get; set; }
        public string Password { get; set; }

        public string PublicKey { get; set; }
    }
}

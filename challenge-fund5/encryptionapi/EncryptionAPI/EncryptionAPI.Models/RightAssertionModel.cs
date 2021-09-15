using System;
using System.Collections.Generic;
using System.Text;

namespace EncryptionAPI.Models
{
    public class RightAssertionModel
    {
        public string Id { get; set; }
        public string RightAssertedBy { get; set; }
        public string RightAssertedTo { get; set; }
        public string Message { get; set; }
    }
}

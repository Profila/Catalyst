using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Text;

namespace EncryptionAPI.Models
{
    public class LoginModel
    {
        [Required]
        public string email { get; set; }
        [Required]
        public string passWord { get; set; }
    }
}

using System;
using System.Collections.Generic;
using System.Text;

namespace RightAssertion.API.Models
{
    public class RequestObject
    {
        public string name { get; set; }
        public List<string> mnemonic_sentence { get; set; }
        public string passphrase { get; set; }
    }
}

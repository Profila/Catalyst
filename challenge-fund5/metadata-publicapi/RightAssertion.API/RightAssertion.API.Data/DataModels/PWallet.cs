using System;
using System.Collections.Generic;
using System.Text;

namespace RightAssertion.API.Data.DataModels
{
    public class PWallet : BaseEntity
    {
        public string UserId { get; set; }
        public string Words { get; set; }
        public string WalletBlockChainId { get; set; }
        public string WalletType { get; set; }
    }
}

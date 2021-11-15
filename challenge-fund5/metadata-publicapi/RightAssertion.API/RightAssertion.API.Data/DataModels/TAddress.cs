using System;
using System.Collections.Generic;
using System.Text;

namespace RightAssertion.API.Data.DataModels
{
    public class TAddress : BaseEntity
    {
        public string WalletId { get; set; }
        public string WalletType { get; set; }
        public string Address { get; set; }

        public decimal Balance { get; set; }
    }
}

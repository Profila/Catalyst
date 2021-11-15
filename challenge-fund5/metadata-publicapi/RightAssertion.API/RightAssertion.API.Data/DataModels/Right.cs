using System;
using System.Collections.Generic;
using System.Text;

namespace RightAssertion.API.Data.DataModels
{
    public class Right : BaseEntity
    {
        public string TransactionId { get; set; }
        public string AssertedBy { get; set; }
        public string BrandEmail { get; set; }
        public string ClientEmail { get; set; }
        public string AssertedTo { get; set; }
        public string AssertedRightAddress { get; set; }
    }
}

using System;
using System.Collections.Generic;
using System.Text;

namespace RightAssertion.API.Models
{
    public class CreateTransactionRequestModel
    {
        public string AssertedBy { get; set; }
        public string ClientEmail { get; set; }

        public string AssertedTo { get; set; }
        public string BrandEmail { get; set; }
        public Payments payments { get; set; }
        public RightDetails rights { get; set; }
    }


    public class Payments
    {
        public string Address { get; set; }
        public long Amount { get; set; }

    }

    public class RightDetails
    {
        public string BrandName { get; set; }
        public string RightMessage { get; set; }
    }
}

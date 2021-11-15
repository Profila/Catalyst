using System;
using System.Collections.Generic;
using System.Text;

namespace RightAssertion.API.Models
{
    public class K
    {
        public string @string { get; set; }
    }

    public class V
    {
        public string @string { get; set; }
    }

    public class Map
    {
        public K k { get; set; } = new K();
        public V v { get; set; } = new V();
    }

    public class _1337
    {
        public List<Map> map { get; set; } = new List<Map>();
    }

    public class Metadata
    {
        public _1337 _1337 { get; set; } = new _1337();
    }





    public class CreateTransactionModel
    {
        public string passphrase { get; set; }
        public List<Payment> payments { get; set; } = new List<Payment>();
        public Metadata metadata { get; set; } = new Metadata();


    }

    public class Payment
    {
        public string address { get; set; }
        public Amount amount { get; set; } = new Amount();
    }
    public class Amount
    {
        public long quantity { get; set; }
        public string unit { get; set; }
    }
}

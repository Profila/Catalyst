using System;
using System.Collections.Generic;
using System.Text;

namespace RightAssertion.API.Models
{
    public class Fee
    {
        public int quantity { get; set; }
        public string unit { get; set; }
    }

    public class Deposit
    {
        public int quantity { get; set; }
        public string unit { get; set; }
    }



    public class InsertedAt
    {
        public int absolute_slot_number { get; set; }
        public int slot_number { get; set; }
        public int epoch_number { get; set; }
        public DateTime time { get; set; }
        public Height height { get; set; }
    }

    public class ExpiresAt
    {
        public int absolute_slot_number { get; set; }
        public int epoch_number { get; set; }
        public int slot_number { get; set; }
        public DateTime time { get; set; }
    }

    public class PendingSince
    {
        public int absolute_slot_number { get; set; }
        public int slot_number { get; set; }
        public int epoch_number { get; set; }
        public DateTime time { get; set; }
        public Height height { get; set; }
    }

    public class Depth
    {
        public int quantity { get; set; }
        public string unit { get; set; }
    }

    public class Asset
    {
        public string policy_id { get; set; }
        public string asset_name { get; set; }
        public int quantity { get; set; }
    }

    public class Input
    {
        public string address { get; set; }
        public Amount amount { get; set; }
        public List<Asset> assets { get; set; }
        public string id { get; set; }
        public int index { get; set; }
    }

    public class Output
    {
        public string address { get; set; }
        public Amount amount { get; set; }
        public List<Asset> assets { get; set; }
    }

    public class Collateral
    {
        public string address { get; set; }
        public Amount amount { get; set; }
        public string id { get; set; }
        public int index { get; set; }
    }

    public class Withdrawal
    {
        public string stake_address { get; set; }
        public Amount amount { get; set; }
    }

    public class Mint
    {
        public string policy_id { get; set; }
        public string asset_name { get; set; }
        public string fingerprint { get; set; }
        public int quantity { get; set; }
    }






    public class TransactionResultModel
    {
        public string id { get; set; }
        public Amount amount { get; set; }
        public Fee fee { get; set; }
        public Deposit deposit { get; set; }
        public InsertedAt inserted_at { get; set; }
        public ExpiresAt expires_at { get; set; }
        public PendingSince pending_since { get; set; }
        public Depth depth { get; set; }
        public string direction { get; set; }
        public List<Input> inputs { get; set; }
        public List<Output> outputs { get; set; }
        public List<Collateral> collateral { get; set; }
        public List<Withdrawal> withdrawals { get; set; }
        public List<Mint> mint { get; set; }
        public string status { get; set; }
        public Metadata metadata { get; set; }
        public string script_validity { get; set; }
    }
}

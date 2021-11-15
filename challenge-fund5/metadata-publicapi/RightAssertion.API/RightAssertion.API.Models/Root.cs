using System;
using System.Collections.Generic;
using System.Text;

namespace RightAssertion.API.Models
{
    public class Available
    {
        public int quantity { get; set; }
        public string unit { get; set; }
        public string policy_id { get; set; }
        public string asset_name { get; set; }
    }

    public class Reward
    {
        public int quantity { get; set; }
        public string unit { get; set; }
    }

    public class Total
    {
        public int quantity { get; set; }
        public string unit { get; set; }
        public string policy_id { get; set; }
        public string asset_name { get; set; }
    }

    public class Balance
    {
        public Available available { get; set; }
        public Reward reward { get; set; }
        public Total total { get; set; }
    }

    public class Assets
    {
        public List<Available> available { get; set; }
        public List<Total> total { get; set; }
    }

    public class Active
    {
        public string status { get; set; }
        public string target { get; set; }
    }

    public class ChangesAt
    {
        public int epoch_number { get; set; }
        public DateTime epoch_start_time { get; set; }
    }

    public class Next
    {
        public string status { get; set; }
        public ChangesAt changes_at { get; set; }
    }

    public class Delegation
    {
        public Active active { get; set; }
        public List<Next> next { get; set; }
    }

    public class Passphrase
    {
        public DateTime last_updated_at { get; set; }
    }

    public class State
    {
        public string status { get; set; }
    }

    public class Height
    {
        public int quantity { get; set; }
        public string unit { get; set; }
    }

    public class Tip
    {
        public int absolute_slot_number { get; set; }
        public int slot_number { get; set; }
        public int epoch_number { get; set; }
        public DateTime time { get; set; }
        public Height height { get; set; }
    }

    public class Root
    {
        public string id { get; set; }
        public int address_pool_gap { get; set; }
        public Balance balance { get; set; }
        public Assets assets { get; set; }
        public Delegation delegation { get; set; }
        public string name { get; set; }
        public Passphrase passphrase { get; set; }
        public State state { get; set; }
        public Tip tip { get; set; }
    }
}

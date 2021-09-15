using System;
using System.Collections.Generic;
using System.Text;

namespace EncryptionAPI.Data.DataModels
{
    public class Right : BaseEntity
    {
        public string RightAssertedBy { get; set; }  // User Id
        public string RightAssertedTo { get; set; } //  Beand Id which is actually a user for us

        public string Message { get; set; }
        public string EncryptionKey { get; set; }

    }
}

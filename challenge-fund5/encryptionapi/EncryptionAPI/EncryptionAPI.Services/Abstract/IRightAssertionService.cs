using EncryptionAPI.Data.DataModels;
using System;
using System.Collections.Generic;
using System.Text;

namespace EncryptionAPI.Services.Abstract
{
    public interface IRightAssertionService
    {
        void AssertRight(Right model);
        List<Right> GetRightAssertedBy(string rightsAssertedBy); // Get Right asserted by User.
        List<Right> GetRightAssertedTo(string rightAssertedTo); // Get Rights By Brand. 

        Right GetRight(string rightId);
    }
}

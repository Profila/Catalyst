using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using System.Xml.Serialization;

namespace EncrytpionTest1
{
    class Program
    {
        public class RsaEncryption
        {
            private RSACryptoServiceProvider csp = new RSACryptoServiceProvider(2048);
            private RSAParameters _privateKey;
            private RSAParameters _publicKey;

            public RsaEncryption()
            {
                _privateKey = csp.ExportParameters(true);
                _publicKey = csp.ExportParameters(false);
            }

            public string GetPublicKey()
            {
                return csp.ToXmlString(false);
                /*
                var sw = new StringWriter();
                var xs = new XmlSerializer(typeof(RSAParameters));
                xs.Serialize(sw, _publicKey);
                return sw.ToString();*/
            }

            public string GetPrivateKey()
            {
                
                var sw = new StringWriter();
                var xs = new XmlSerializer(typeof(RSAParameters));
                xs.Serialize(sw, _privateKey);
                return sw.ToString();
            }
            public string Encrypt(string plainText)
            {
                csp = new RSACryptoServiceProvider();
                csp.ImportParameters(_publicKey);
                Console.WriteLine($"The key used to encrypt is {GetPublicKey()}");
                var data = Encoding.Unicode.GetBytes(plainText);
                var cypher = csp.Encrypt(data, false);
                return Convert.ToBase64String(cypher);
            }

            public string Decrypt(string cypher, string privatekey)
            {
                csp = new RSACryptoServiceProvider();
                var dataBytes = Convert.FromBase64String(cypher);
                csp.FromXmlString(privatekey);
                var plaintext = csp.Decrypt(dataBytes, false);
                return Encoding.Unicode.GetString(plaintext);
            }
            public void AESEncryption(string text)
            {
                AesCryptoServiceProvider Aes = new AesCryptoServiceProvider();
                Aes.GenerateIV();
                Aes.GenerateKey();

                byte[] clearBytes = Encoding.Unicode.GetBytes(text);

                using (MemoryStream ms = new MemoryStream())
                {
                    using (CryptoStream cs = new CryptoStream(ms, Aes.CreateEncryptor(), CryptoStreamMode.Write))
                    {
                        cs.Write(clearBytes, 0, clearBytes.Length);
                        cs.Close();
                    }
                    text = Convert.ToBase64String(ms.ToArray());
                }
                Console.WriteLine(text);
                var sw = new StringWriter();
                var xs = new XmlSerializer(typeof(Aes));
                xs.Serialize(sw, Aes.Key);
                Console.WriteLine(xs.ToString());
                

            }

            public void GenerateRandomString()
            {
                const string src = "abcdefghijklmnopqrstuvwxyz0123456789@!#$%^&*";
                int length = 16;
                var sb = new StringBuilder();
                Random RNG = new Random();
                for (var i = 0; i < length; i++)
                {
                    var c = src[RNG.Next(0, src.Length)];
                    sb.Append(c);
                }
                Console.WriteLine(sb.ToString());
            }

        }

        
        static void Main(string[] args)
        {
            RsaEncryption rsa = new RsaEncryption();
            //rsa.GenerateRandomString();
          
          
            Console.WriteLine("Enter Cypher");
            string cypher = Console.ReadLine();
            Console.WriteLine("Enter Your Private Key");
            string privateKey = Console.ReadLine();
            var text = rsa.Decrypt(cypher, privateKey);
            Console.WriteLine($"Decypted Text using private Key = {text}");
            Console.ReadLine();
                
            
        }
    }
}

module RSA (rsa) where

type PrivateKey = Integer
type PublicKey = (Integer,Integer)

type Message = String
type EncryptedMessage = Integer

data KeyPair = KeyPair (PrivateKey, PublicKey) deriving (Show, Eq)

phiForPrimes :: [Integer] -> Integer
phiForPrimes = foldr (*) 1 . map (flip (-) 1)

generatePublicKey :: Integer -> Integer -> PublicKey
generatePublicKey n e = (n, e)

generatePrivateKey :: Integer -> Integer -> PrivateKey
generatePrivateKey p = inverse
  where
    inverse e = head [d | d <- [1..p - 1], (d * e) `mod` p == 1]

generateKeyPair :: Integer -> Integer -> KeyPair
generateKeyPair p q = KeyPair (privateKey, publicKey)
  where
    phiPQ = phiForPrimes [p, q]
    e = last [m | m <- [1..phiPQ - 1], gcd m phiPQ  == 1]
    privateKey = generatePrivateKey phiPQ e :: PrivateKey
    publicKey = let n = p * q in generatePublicKey n e :: PublicKey

-- TODO: Implement this.
encodeMessage :: Message -> Integer
encodeMessage = id

encryptWithPublicKey :: PublicKey -> Message -> EncryptedMessage
encryptWithPublicKey (n, e) m = encodedMessage ^ e `mod` n
  where
    encodedMessage = encodeMessage m

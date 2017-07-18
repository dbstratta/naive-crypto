module RSA (rsa) where

type PrivateKey = (Integer, Integer)
type PublicKey = (Integer, Integer)

type Message = String
type EncodedMessage = Integer
type EncryptedMessage = Integer
type Sign = Integer
type SignedMessage = (Sign, Message)

data KeyPair = KeyPair (PrivateKey, PublicKey) deriving (Show, Eq)

phiForPrimes :: [Integer] -> Integer
phiForPrimes = foldr (*) 1 . map (flip (-) 1)

generatePublicKey :: Integer -> Integer -> PublicKey
generatePublicKey n e = (n, e)

generatePrivateKey :: Integer -> Integer -> Integer -> PrivateKey
generatePrivateKey n p e = (inverse e, n)
  where
    inverse e = head [d | d <- [1..p - 1], (d * e) `mod` p == 1]

generateKeyPair :: Integer -> Integer -> KeyPair
generateKeyPair p q = KeyPair (privateKey, publicKey)
  where
    n = p * q
    phiN = phiForPrimes [p, q]
    e = last [m | m <- [1..phiN - 1], gcd m phiN  == 1]
    privateKey = generatePrivateKey n phiN e :: PrivateKey
    publicKey = let n = p * q in generatePublicKey n e :: PublicKey

-- TODO: Implement this.
encodeMessage :: Message -> EncodedMessage
encodeMessage = id

-- TODO: Implement this.
decodeMessage :: EncodedMessage -> Message
decodeMessage = id

encrypt :: PublicKey -> Message -> EncryptedMessage
encrypt (n, e) m = encodedMessage ^ e `mod` n
  where
    encodedMessage = encodeMessage m

decrypt :: PrivateKey -> EncryptedMessage -> Message
decrypt (d, n) encryptedMessage = encryptedMessage

sign :: PrivateKey -> Message -> EncryptedMessage
sign (d, n) m = encodedMessage ^ d `mod` n
  where
    encodedMessage = encodeMessage m

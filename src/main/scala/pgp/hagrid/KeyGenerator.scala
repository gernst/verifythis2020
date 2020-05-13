package pgp.hagrid

import java.math.BigInteger
import java.security.SecureRandom
import java.util.Date

import org.bouncycastle.bcpg.sig.{Features, KeyFlags}
import org.bouncycastle.bcpg.{HashAlgorithmTags, PublicKeyAlgorithmTags, SymmetricKeyAlgorithmTags}
import org.bouncycastle.crypto.generators.RSAKeyPairGenerator
import org.bouncycastle.crypto.params.RSAKeyGenerationParameters
import org.bouncycastle.openpgp._
import org.bouncycastle.openpgp.operator.bc._
import pgp.Identity

// export PATH=/path/to/fake/sendmail/bin
// #!/bin/bash
// cat > email.txt
// echo "$@" >> email-log.txt
// cat >> email-log.txt
// echo >> email-log.txt

/**
 * Key generation ONLY works if the selected identity size per key is <= 2!
 * This is fine for our use case, but still unfortunate.
 */
object KeyGenerator {

  val passPhrase = "hagrid"

  def genPublicKey(identities: Set[Identity]): PGPPublicKey = {
    val passPhrase = "hagrid"
    val tail = identities.tail map (_.email)
    val keyRingGenerator =
      generateKeyRingGenerator(identities.head.email, tail.toList, passPhrase)

    //    val publicKeyRing = keyRingGenerator
    //    val output = new ByteArrayOutputStream
    //    val armored = new ArmoredOutputStream(output)
    //
    //    publicKeyRing.encode(armored)
    //
    //    output.close()
    //    armored.close()
    //
    //    new String(output.toByteArray)

    val publicKeyRing = keyRingGenerator

    val publicKey = keyRingGenerator.getPublicKey

    publicKey
  }

  def generateKeyRingGenerator(identity: String,
                               tail: List[String],
                               pass: String): PGPPublicKeyRing = {

    val keyPairGenerator = new RSAKeyPairGenerator

    keyPairGenerator.init(
      new RSAKeyGenerationParameters(
        BigInteger.valueOf(0x10001),
        new SecureRandom(),
        2048,
        12
      )
    )

    val signingMaster = new BcPGPKeyPair(
      3, // RSA_SIGN
      keyPairGenerator.generateKeyPair(),
      new Date()
    )

    val encryptionKey =
      new BcPGPKeyPair(2, keyPairGenerator.generateKeyPair(), new Date())
    val signHashGen = new PGPSignatureSubpacketGenerator

    signHashGen setKeyFlags(false, KeyFlags.SIGN_DATA | KeyFlags.CERTIFY_OTHER)
    signHashGen setFeature(false, Features.FEATURE_MODIFICATION_DETECTION)

    val subPacketGen = new PGPSignatureSubpacketGenerator
    subPacketGen setKeyFlags(false, KeyFlags.ENCRYPT_COMMS | KeyFlags.ENCRYPT_STORAGE)

    val sha1Calc =
      new BcPGPDigestCalculatorProvider().get(HashAlgorithmTags.SHA1)
    val sha256Calc =
      new BcPGPDigestCalculatorProvider().get(HashAlgorithmTags.SHA256)

    val secretKeyEnc =
      new BcPBESecretKeyEncryptorBuilder(SymmetricKeyAlgorithmTags.AES_256)
        .build(pass.toCharArray)

    val keyRingGen = new PGPKeyRingGenerator(
      PGPSignature.POSITIVE_CERTIFICATION,
      signingMaster,
      identity,
      sha1Calc,
      signHashGen.generate(),
      null,
      new BcPGPContentSignerBuilder(
        signingMaster.getPublicKey.getAlgorithm,
        HashAlgorithmTags.SHA1
      ),
      secretKeyEnc
    )
    keyRingGen.addSubKey(encryptionKey, subPacketGen.generate(), null)
    val keyRing = keyRingGen.generatePublicKeyRing()

    val (privateKey, publicKey) = extractSecretKey(
      keyRingGen.generateSecretKeyRing()
    )

    var ringGen2 = keyRingGen

    val finalRing = tail.foldLeft(keyRing) { (ring, id) =>
      val (key, gen) = genSubKeyForIdentity(subPacketGen, ringGen2, id)
      ringGen2 = gen

      PGPPublicKeyRing.insertPublicKey(ring, key)
    }

    finalRing
  }

  def extractSecretKey(
                        keyRing: PGPSecretKeyRing
                      ): (PGPPrivateKey, PGPPublicKey) = {
    val pgpSecretKey = keyRing.getSecretKey
    val digestCalculator = new BcPGPDigestCalculatorProvider
    val decryptor = new BcPBESecretKeyDecryptorBuilder(digestCalculator)

    val privateKey =
      pgpSecretKey.extractPrivateKey(decryptor.build(passPhrase.toCharArray))
    val publicKey = pgpSecretKey.getPublicKey

    (privateKey, publicKey)
  }

  def genSubKeyForIdentity(
                            subpacketGenerator: PGPSignatureSubpacketGenerator,
                            keyRingGen: PGPKeyRingGenerator,
                            identity: String
                          ): (PGPPublicKey, PGPKeyRingGenerator) = {
    val signatureGenerator = new PGPSignatureGenerator(
      new BcPGPContentSignerBuilder(
        PublicKeyAlgorithmTags.RSA_SIGN,
        HashAlgorithmTags.SHA1
      )
    )

    val signHashGen = new PGPSignatureSubpacketGenerator

    signHashGen setKeyFlags(false, KeyFlags.SIGN_DATA | KeyFlags.CERTIFY_OTHER)
    signHashGen setFeature(false, Features.FEATURE_MODIFICATION_DETECTION)

    val keyRing = keyRingGen.generateSecretKeyRing()

    val (privateKey, publicKey) = extractSecretKey(keyRing)

    signatureGenerator.init(PGPSignature.POSITIVE_CERTIFICATION, privateKey)
    signatureGenerator.setHashedSubpackets(signHashGen.generate())

    val certification =
      signatureGenerator.generateCertification(identity, publicKey)
    val key =
      PGPPublicKey.addCertification(publicKey, identity, certification)

    keyRingGen.addSubKey(
      new PGPKeyPair(key, privateKey),
      subpacketGenerator.generate(),
      null
    )

    (key, keyRingGen)

  }

}

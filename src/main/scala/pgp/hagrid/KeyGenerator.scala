package pgp.hagrid

import java.io.ByteArrayOutputStream
import java.math.BigInteger
import java.security.SecureRandom
import java.util.Date

import org.bouncycastle.bcpg.sig.{Features, KeyFlags}
import org.bouncycastle.bcpg.{ArmoredOutputStream, HashAlgorithmTags, SymmetricKeyAlgorithmTags}
import org.bouncycastle.crypto.generators.RSAKeyPairGenerator
import org.bouncycastle.crypto.params.RSAKeyGenerationParameters
import org.bouncycastle.openpgp.operator.bc.{BcPBESecretKeyEncryptorBuilder, BcPGPContentSignerBuilder, BcPGPDigestCalculatorProvider, BcPGPKeyPair}
import org.bouncycastle.openpgp.{PGPKeyRingGenerator, PGPSignature, PGPSignatureSubpacketGenerator}
import pgp.Identity

// export PATH=/path/to/fake/sendmail/bin
// #!/bin/bash
// cat > email.txt
// echo "$@" >> email-log.txt
// cat >> email-log.txt
// echo >> email-log.txt

object KeyGenerator {

  def genPublicKey(identities: Set[Identity]): String = {
    val passPhrase = "hagrid"
    val keyRingGenerator = generateKeyRingGenerator(identities.head.email, passPhrase)

    val publicKeyRing = keyRingGenerator.generatePublicKeyRing()
    val output = new ByteArrayOutputStream
    val armored = new ArmoredOutputStream(output)

    publicKeyRing.encode(armored)

    output.close()

    new String(output.toByteArray)
  }


  def generateKeyRingGenerator(identity: String, pass: String): PGPKeyRingGenerator = {

    val keyPairGenerator = new RSAKeyPairGenerator

    keyPairGenerator.init(
      new RSAKeyGenerationParameters(
        BigInteger.valueOf(0x10001),
        new SecureRandom(),
        2048,
        12)
    )

    val signingMaster = new BcPGPKeyPair(
      3, // RSA_SIGN
      keyPairGenerator.generateKeyPair(),
      new Date())

    val encryptionKey = new BcPGPKeyPair(2, keyPairGenerator.generateKeyPair(), new Date())
    val signHashGen = new PGPSignatureSubpacketGenerator

    signHashGen setKeyFlags(false, KeyFlags.SIGN_DATA | KeyFlags.CERTIFY_OTHER)
    signHashGen setFeature(false, Features.FEATURE_MODIFICATION_DETECTION)

    val subPacketGen = new PGPSignatureSubpacketGenerator
    subPacketGen setKeyFlags(false, KeyFlags.ENCRYPT_COMMS | KeyFlags.ENCRYPT_STORAGE)

    val sha1Calc = new BcPGPDigestCalculatorProvider().get(HashAlgorithmTags.SHA1)
    val sha256Calc = new BcPGPDigestCalculatorProvider().get(HashAlgorithmTags.SHA256)

    val secretKeyEnc =
      new BcPBESecretKeyEncryptorBuilder(
        SymmetricKeyAlgorithmTags.AES_256,
        sha256Calc,
        0xc0 // magic
      ).build(pass.toCharArray)

    val keyRingGen = new PGPKeyRingGenerator(
      PGPSignature.POSITIVE_CERTIFICATION,
      signingMaster,
      identity,
      sha1Calc,
      signHashGen.generate(),
      null,
      new BcPGPContentSignerBuilder(signingMaster.getPublicKey.getAlgorithm, HashAlgorithmTags.SHA1),
      secretKeyEnc
    )

    keyRingGen.addSubKey(encryptionKey, subPacketGen.generate(), null)

    keyRingGen
  }

}

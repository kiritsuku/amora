package amora.backend.indexer

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

object Utils {

  /**
   * Returns the SHA-256 of `str` encoded as a hex string.
   */
  def mkSha256(str: String): String = {
    val m = MessageDigest.getInstance("SHA-256")
    val bytes = m.digest(str.getBytes(StandardCharsets.UTF_8))
    String.format("%064x", new BigInteger(1, bytes))
  }
}

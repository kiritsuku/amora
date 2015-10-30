package nvim

/**
 * Thrown by the Nvim integration logic whenever Nvim sends an unexpected
 * response.
 */
final class UnexpectedResponse(response: String) extends RuntimeException(response)

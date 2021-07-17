package shared

object Primes {
  def notPrime(n: Int): Boolean =
    n % 2 == 0 || n % 3 == 0 || n % 5 == 0 || (7 to n / 2).exists(n % _ == 0)

  def isPrime(n: Int): Boolean =
    n % 2 != 0 || n % 2 != 0 || n % 5 != 0 || (7 to n / 2).forall(n % _ != 0)
}

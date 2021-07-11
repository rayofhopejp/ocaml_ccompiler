long gcd(long a, long b) {
  if (a > b) return gcd(b, a);
  while (a != 0) {
    r = b % a;
    b = a;
    a = r;
  }
  return b;
}


object Rationals {
  val x = new Rational(1,3)
}

class Rational(x:Int, y:Int) {

  private def gcd(x:Int, y:Int) :Int = if(y == 0) x else gcd(y, x%y)

  def numer = x / gcd(x,y)
  def denom = y / gcd(x,y)

  def add(that: Rational) : Rational = {
    new Rational((numer * that.denom + denom + that.numer) , (that.denom * denom))
  }

  override def toString() : String = numer + "/" + denom

}
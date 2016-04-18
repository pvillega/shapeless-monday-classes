import shapeless.ops.hlist.{Mapper, Reverse}
import shapeless.{HList, HNil, Poly, Poly1}

val hlist = 23 :: "foo" :: true :: HNil


/*
Below fails: all Hlist methods are implemented as Ops
extension methods.

HList.reverse expects an implicit Reverse[L], we are
not providing.

Error:
Error:(5, 37) could not find implicit value for parameter reverse: shapeless.ops.hlist.Reverse[shapeless.HList]
def reverseIt(l: HList): HList = l.reverse
                                   ^
 */
//def reverseIt(l: HList): HList = l.reverse

/*
Below fails: HList by itself is useless, just a supertrait

We are missing the specific definition of Reverse for the HList
*/
//def reverseIt(l: HList)(implicit rev: Reverse[HList]): HList = l.reverse
//reverseIt(hlist)

// Works as you can only perform operations in HList in scopes where you have
// the corresponding Witness available
def reverseIt[L <: HList](l: L)(implicit rev: Reverse[L]): rev.Out = l.reverse
reverseIt(hlist)

//Poly -> polymorphic functions that allow to use map over HList (we have multiple
// inputs for heterogeneous HList)
object inc extends Poly1 {
  implicit def caseInt = at[Int](_ + 1)
  implicit def caseString = at[String](_ + "!")
  implicit def caseBoolean = at[Boolean](!_)
}
inc
inc(23)
inc("ha")
inc(true)

hlist map inc

// As before, we need evidence. In this case the evidence depends
// on both L (the HList) and the Poly type we will use to
// map over with
//def mapIt[L <: HList](l: L, f: Poly)(implicit m: Mapper[f.type, L]): m.Out = l map f
// Below alternative reusing previously defined Poly
def mapIt[L <: HList](l: L)(implicit m: Mapper[inc.type, L]): m.Out = l map inc
mapIt(hlist)

// Below fails due to restrictions on how argument lists work
//def mapRev[L <: HList](l: L)(implicit m: Mapper[inc.type, L], rev: Reverse[m.Out]): rev.Out = (l map inc).reverse

//def point(x: Int)(y: Int = x)
//point(2)(3)

// Below fails due to same issue as above mapRev
//def point(x: Int, y: Int = x)

// Solution: Aux
def mapRev[L <: HList, Out <: HList](l: L)(implicit m: Mapper.Aux[inc.type, L, Out], r: Reverse[Out]): r.Out = (l map inc).reverse
mapRev(hlist)

// let's compress the definition before to 1 implicit param only, instead of Aux thingy
object Lemma {

  // we keep the Poly in scope for the mapping, so Lemma is self-contained
  // (we could use the `inc` object defined above if we didn't care about that)
  object polyInc extends Poly1 {
    implicit def caseInt = at[Int](_ + 1)
    implicit def caseString = at[String](_ + "!")
    implicit def caseBoolean = at[Boolean](!_)
  }

  // notice only 1 implicit evidence value!
  def mapRev[L <: HList](l: L)(implicit m: MapRev[L]): m.Out = m(l)

  // trait for implicit with apply and out values
  trait MapRev[L <: HList] {
    type Out <: HList
    def apply(l: L): Out
  }

  // companion object so it is in implicit scope
  object MapRev {

    // we use this otherwise return of `implicit def mapRev` below
    // would be `MapRev[L] { type Out = r.Out }` which is... ugly
    type Aux[L <: HList, Out0 <: HList] = MapRev[L] { type Out = Out0 }

    implicit def mapRev[L <: HList, Out0 <: HList]
      (implicit m: Mapper.Aux[polyInc.type, L, Out0],
                r: Reverse[Out0]
      ): Aux[L, r.Out] =
      new MapRev[L] {
        type Out = r.Out
        def apply(l: L): Out = (l map polyInc).reverse
      }

  }

}


// ta-daa
Lemma.mapRev(hlist)

package fp

/**
  * ===Fast Map===
  *
  * Implements a key vlaue map like data structure which is optimized to handle
  * primitive as a primary key
  *
  * We are using here an immutable unbalanced binary tree like data structure that can be traversed using a zipper
  * pearl as presented here [[http://learnyouahaskell.com/zippers]]
  *
  */
package object ftree extends FTreeOps with FTreeConstructors with FTreeNavigation with FTreeCatamorphism

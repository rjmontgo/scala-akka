object MyModule {

  def isSorted[A] (arr: Array[A], ordered: (A , A) => Boolean): Boolean = {
    if (arr.length <= 1)
      true

    def loop(idx: Int): Boolean = {
      if (idx >= arr.length)
        true
      else if (!ordered(arr(idx - 1), arr(idx)))
        false
      else
        loop(idx + 1)
    }

    loop(1)
  }
}

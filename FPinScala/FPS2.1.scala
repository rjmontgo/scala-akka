object MyModule {

  def fibonacci(arg: Int): Int = {
    def loop(times: Int, prev: Int, acc: Int): Int = 
      if (times <= 0)
        acc
      else 
        loop(times - 1, acc, prev + acc)

    loop(arg, 1, 0)

  }
}

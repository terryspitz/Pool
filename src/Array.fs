module MyArray2D


///Define my own 2D array since Fable doesn't offer one
type MyArray2D(r, c) =
    //member or even mutable member doesn't seem to allow 
    //setting from Item method, use class local variable
    let _arr = Array.create (r*c) 0.0
    member this.r = r
    member this.c = c
    member this.arr = _arr

    ///Can't override (.[,]) so take a tuple instead
    //member this.(.[]) (i : int * int) = 
    member this.Item
      with get (i : int * int) : float =
        // assert ((fst i)<r && (snd i)<c)
        _arr.[(fst i)*c + (snd i)]
      and set (i : int * int) (value : float) =
        assert ((fst i)<r && (snd i)<c)
        _arr.[(fst i)*c + (snd i)] <- value


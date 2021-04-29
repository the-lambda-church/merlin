module type Monad = sig
  type 'a t
end
module type Monad_option =
  Monad
    with type 'a t = 'a option
    constraint 'a = int

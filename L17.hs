
pure id <> v = <> const id 1
              = const 1 (id 1)
              = 1
              = v

pure (.) <> u <> v <> w = pure (.) <> const x <> const y <> const z
                           = const (x.y) <> const z
                           = const ((x.y) z)
                           = const (x $ y z) = const x <> const (y z)
                           = const x <> (const y <> const z)
                           = u <> (v <> w)

pure f <> pure x = const f <> const x
                  = const (f x)
                  = pure (f x) 


u <> pure y = const f <> pure y
             = const (f y)
             = const (f $ y) = const (($ y) f)
             = const ($ y) <> const f
             = pure ($ y) <> u


             
-- | Faster Coroutine Pipelines: A Reconstruction

module Main where

main = return ()

data Pipe i o a = In (i -> Pipe i o a)
                | Out o (Pipe i o a)
                | Return a

-- Scott Encoding
-- inCon f    = \i o r -> i f
-- outCon v p = \i o r -> o v p
-- retCon a   = \i o r -> r a
newtype PipeS i o a = PipeS
  { unPipeS:: forall r.
              ((i -> PipeS i o a) -> r)
           -> (o -> PipeS i o a -> r)
           -> (a -> r)
           -> r
  }

newtype RelaxedPipeS i o a = RelaxedPipeS
  { unRelaxedPipeS :: forall r.
              (i -> RelaxedPipeS i o a -> r)
           -> (o -> RelaxedPipeS i o a -> r)
           -> (a -> r)
           -> r
  }

data RelaxedPipe i o a = In i (RelaxedPipe i o a)
                       | Out o (RelaxedPipe i o a)
                       | Return a

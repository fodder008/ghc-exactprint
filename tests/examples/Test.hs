-- |Comment not in a SrcSpan
foo x = -- Compute foo
  let a = 4 -- using a
  in a + x

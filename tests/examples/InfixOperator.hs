{-# LANGUAGE BangPatterns, CPP, OverloadedStrings #-}

#define BACKSLASH 92
#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define COMMA 44
#define DOUBLE_QUOTE 34
#define OPEN_CURLY 123
#define OPEN_SQUARE 91
#define C_0 48
#define C_9 57
#define C_A 65
#define C_F 70
#define C_a 97
#define C_f 102
#define C_n 110
#define C_t 116

json_ :: Parser Value -> Parser Value -> Parser Value
json_ obj ary = do
  w <- skipSpace *> A.satisfy (\w -> w == OPEN_CURLY || w == OPEN_SQUARE)
  if w == OPEN_CURLY
    then obj
    else ary
{-# INLINE json_ #-}


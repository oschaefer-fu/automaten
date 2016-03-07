abstype regexp
with
  allOf   :: [char] -> regexp
  oneOf   :: [char] -> regexp
  star    :: regexp -> regexp
  matches :: regexp -> [char] -> bool
  alpha_  :: regexp
  digit_  :: regexp

%insert "regexpimpl.m"

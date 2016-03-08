abstype ra
with
  ||         name      zeichenkette
  allOf   :: [char] -> [char] -> ra
  oneOf   :: [char] -> [char] -> ra
  raOr    :: ra -> ra -> ra
  raAnd   :: ra -> ra -> ra
  raStar  :: ra -> ra
  matches :: ra -> [char] -> bool
  alpha_  :: ra
  digit_  :: ra
  upper_  :: ra
  lower_  :: ra
  showra  :: ra -> [char]

%insert "raimpl.m"

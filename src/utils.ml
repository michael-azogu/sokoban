let ord n =
  let suffix =
    let j, k = (n mod 10, n mod 100) in
      if j = 1 && k <> 11 then
        "st"
      else if j = 2 && k <> 12 then
        "nd"
      else if j = 3 && k <> 13 then
        "rd"
      else
        "th"
  in
    string_of_int n ^ suffix
